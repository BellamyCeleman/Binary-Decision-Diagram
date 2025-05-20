#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <time.h>

#define HASH_SIZE 2000

typedef struct BDDNode {
    char var;
    struct BDDNode *low;
    struct BDDNode *high;
} BDDNode;

typedef struct HashEntry {
    char var;
    BDDNode *low;
    BDDNode *high;
    BDDNode *node;
    struct HashEntry *next;        // for collisions
} HashEntry;

typedef struct HashTable {
    HashEntry **list;
    int size;
    int num_nodes;
} HashTable;

typedef struct BDD {
    BDDNode *root;
    int size;
    char *var_order;
    HashTable *hash_table;
} BDD;

typedef struct Minterm {
    int zero_flag;
    char *vars;
    int var_count;
    struct Minterm *next;
} Minterm;

typedef struct Expression {
    Minterm *head;
    int zero_flag;
    int one_flag;
    int minterm_length;
} Expression;

static BDDNode TRUE = {'1', NULL, NULL};        // we will have only 2 nodes for 1 and
static BDDNode FALSE = {'0', NULL, NULL};

unsigned int hash(char symbol, BDDNode *low, BDDNode *high) {
    unsigned long hash = 0;
    hash += (unsigned long)symbol * 31;
    hash += (unsigned long)low * 17;
    hash += (unsigned long)high * 13;
    return (unsigned int)(hash % HASH_SIZE);
}

BDDNode *search(HashTable *table, char var, BDDNode *low, BDDNode *high) {
    if (table == NULL) return NULL;

    unsigned int idx = hash(var, low, high);
    HashEntry *current = table->list[idx];

    while (current != NULL) {
        if (current->var == var &&
            current->low == low &&
            current->high == high) {
            return current->node;
        }
        current = current->next;
    }

    return NULL;
}

// insert node to the hash table
void insert_node(HashTable *table, char var, BDDNode *low, BDDNode *high, BDDNode *node) {
    if (table == NULL || node == NULL) return;

    unsigned int idx = hash(var, low, high);
    HashEntry *new_entry = malloc(sizeof(HashEntry));
    if (!new_entry) return;

    new_entry->var = var;
    new_entry->low = low;
    new_entry->high = high;
    new_entry->node = node;
    new_entry->next = table->list[idx];
    table->list[idx] = new_entry;

    table->num_nodes++;
}

BDDNode *create_node(char var) {
    BDDNode *new_node = calloc(1, sizeof(BDDNode));
    if (!new_node) return NULL;

    new_node->var = var;
    return new_node;
}

void free_expression(Expression *expr) {
    if (!expr) return;

    Minterm *current = expr->head;
    Minterm *next;
    while (current) {
        next = current->next;
        free(current->vars);
        free(current);
        current = next;
    }

    free(expr);
}

void free_hash_table(HashTable *table) {
    if (!table) return;

    for (int i = 0; i < table->size; i++) {
        HashEntry *entry = table->list[i];
        while (entry) {
            HashEntry *next = entry->next;
            free(entry->node);
            free(entry);
            entry = next;
        }
    }

    free(table->list);
    free(table);
}

void free_bdd(BDD *bdd) {
    if (!bdd) return;

    free_hash_table(bdd->hash_table);
    free(bdd->var_order);
    free(bdd);
}

// this function helps us safely work with the Expression through copying it
Expression *clone_expression_full(Expression *expr) {
    Expression *copy = calloc(1, sizeof(Expression));

    copy->zero_flag = expr->zero_flag;
    copy->one_flag = expr->one_flag;
    copy->minterm_length = expr->minterm_length;

    Minterm *expr_current = expr->head;
    Minterm *prev_copy = NULL;

    while (expr_current) {
        Minterm *copy_minterm = calloc(1, sizeof(Minterm));

        copy_minterm->zero_flag = expr_current->zero_flag;
        copy_minterm->var_count = expr_current->var_count;

        if (expr_current->var_count > 0) {
            copy_minterm->vars = malloc(expr_current->var_count * sizeof(signed char));
            memcpy(copy_minterm->vars, expr_current->vars, expr_current->var_count * sizeof(signed char));
        }

        if (!copy->head) {
            copy->head = copy_minterm;
        } else {
            prev_copy->next = copy_minterm;
        }

        prev_copy = copy_minterm;
        expr_current = expr_current->next;
    }

    return copy;
}

// helps to work with Minterms, adds a new variable to minterm
void add_letter(Minterm *current, signed char letter) {
    for (int i = 0; i < current->var_count; i++) {
        if (current->vars[i] == -letter) {

        }
        if (current->vars[i] == letter) {
            return;
        } 
    }

    current->vars = realloc(current->vars, (current->var_count + 1) * sizeof(char));
    current->vars[current->var_count] = letter;
    current->var_count++;
}

// here we parse our initial expression into the Expression struct to work conveniently with it
Expression *parse(char *expr) {
    int negate = 0;
    char c;

    if (!expr || !*expr) {
        Expression *expression = calloc(1, sizeof(Expression));
        expression->zero_flag = 1;
        return expression;
    }

    Expression *expression = calloc(1, sizeof(Expression));
    Minterm *current = calloc(1, sizeof(Minterm));
    expression->head = current;
    expression->minterm_length = 1;

    int i = 0;
    while ((c = expr[i++])) {
        if (c == '!') {
            negate = 1;
            continue;
        }

        if (c >= 'a' && c <= 'z') {
            add_letter(current, negate ? -c : c);
            negate = 0;
            continue;
        }

        if (c == '+') {
            Minterm *new_minterm = calloc(1, sizeof(Minterm));
            current->next = new_minterm;
            current = new_minterm;
            expression->minterm_length++;
        }
    }

    return expression;
}

// this function we need to simplify our expression in 1 step down of bdd level
Expression *substitution(Expression *expr, signed char letter) {
    if (!expr) {
        return calloc(1, sizeof(Expression));
    }
    if (expr->one_flag || expr->zero_flag) {
        return clone_expression_full(expr);
    }

    Expression *result = clone_expression_full(expr);
    Minterm *item = result->head;
    int zero_result = 1;

    while (item) {      // we go through all minterms
        int i = 0;
        while (i < item->var_count) {       // then through each variable in each minterm
            if (item->vars[i] == letter) {
                memmove(&item->vars[i], &item->vars[i + 1],                 // delete that variable from the vars
                        (item->var_count - i - 1) * sizeof(signed char));   // if we found it
                item->var_count--;
                if (item->var_count == 0) {     // if after deletion we went out of variable than our term is 1
                    result->one_flag = 1;
                    result->zero_flag = 0;
                    return result;
                }
                continue;
            }
            if (item->vars[i] == -letter) {     // if we found an opposite variable (with !) than our term is 0
                item->zero_flag = 1;
                break;
            }
            i++;
        }

        if (!item->zero_flag) {     // if at least 1 term doesn't equal to zero than we won't assign 0 to the expression
            zero_result = 0;
        }
        item = item->next;
    }

    if (zero_result) {
        result->zero_flag = 1;
    }

    return result;
}

// it doesn't let existing node to be created again
BDDNode *find_or_add_unique_node(HashTable *hash_table, char var, BDDNode *low, BDDNode *high) {
    if (low == high) return low;

    BDDNode *existing = search(hash_table, var, low, high);
    if (existing) {return existing;}

    BDDNode *node = create_node(var);
    node->low = low;
    node->high = high;

    insert_node(hash_table, var, low, high, node);

    return node;
}

HashTable* create_hash_table(int size) {
    HashTable *table = calloc(1, sizeof(HashTable));
    table->size = size;
    table->num_nodes = 0;
    table->list = calloc(size, sizeof(HashEntry*));

    return table;
}

BDDNode *build_bdd(Expression *expression, char *var_order, int level, HashTable *hash_table) {
    if (expression->zero_flag) return &FALSE;       // if our expression got to the basic case than return it
    if (expression->one_flag) return &TRUE;

    char current = var_order[level];
    if (!current) {                                                 // if variables in Expression ended we go through  
        Minterm *m = expression->head;                              // all minterms and check if there is one with no variables left
        while (m) {                                                 // and zero_flag isn't 1, return true else false
            if (!m->zero_flag && m->var_count == 0) return &TRUE;
            m = m->next;
        }
        return &FALSE;
    }

    int found = 0;                                                  // here we check if there is this variable used in                           
    for (Minterm *m =expression->head; m && !found; m = m->next) {  // a minterm
        for (int i = 0; i < m->var_count; i++) {
            if (abs(m->vars[i]) == current) {
                found = 1;
                break;
            }
        }
    }

    if (!found) {               // if it isn't used than skip
        return build_bdd(expression, var_order, level + 1, hash_table);
    }

    Expression *f_high = substitution(expression, current);
    Expression *f_low = substitution(expression, -current);

    BDDNode *high_node = build_bdd(f_high, var_order, level + 1, hash_table);
    BDDNode *low_node = build_bdd(f_low, var_order, level + 1, hash_table);

    free_expression(f_high);
    free_expression(f_low);

    if (high_node == low_node) {
        return high_node;
    }

    return find_or_add_unique_node(hash_table, current, low_node, high_node);
}


BDD* create_BDD(char *expression, char *var_seq) {
    BDD *bdd = calloc(1, sizeof(BDD));

    bdd->hash_table = create_hash_table(HASH_SIZE);
    bdd->size = 0;

    char *vars = strdup(var_seq);

    for (int i = 0; var_seq[i]; i++) {
        vars[i] = tolower(var_seq[i]);
    }
    bdd->var_order = vars;

    Expression *expr = parse(expression);

    if (expr->one_flag == 1) {
        bdd->root = &TRUE;
        free_expression(expr);
        return bdd;
    } else if (expr->zero_flag == 1) {
        bdd->root = &FALSE;
        free_expression(expr);
        return bdd;
    }

    bdd->root = build_bdd(expr, vars, 0, bdd->hash_table);
    free_expression(expr);

    return bdd;
}

BDD *create_BDD_with_best_order(char *expr, char *var_seq) {
    int count = strlen(var_seq);

    if (count == 0) { // if var_seq has no chars in it then expression is a constant 
        Expression *parsed_expr = parse(expr);

        BDD *b = calloc(1, sizeof(BDD));
        b->var_order = strdup("");
        b->hash_table = create_hash_table(HASH_SIZE);

        if (parsed_expr->one_flag == 1) {       // if it is 1 than we assign true to the root
            b->root = &TRUE;
        } else if (parsed_expr->zero_flag == 1) {   // if it is 0 assign false
            b->root = &FALSE;
        }

        free_expression(parsed_expr);
        return b;
    }

    char *vars = strdup(var_seq);

    BDD *best = NULL;
    int best_size = INT_MAX;

    for (int i = 0; i < count; i++) {
        char *order = strdup(vars);

        if (i > 0) {        // (i > 0) to check the default case
            char temp = order[0];
            memmove(order, order + 1, count - 1);       // here we make the rotation to check different orders
            order[count - 1] = temp;                    // abcd -> bcda -> cdab -> dabc
        }

        BDD *b = create_BDD(expr, order);

        if (!best || b->hash_table->num_nodes < best_size) {       // if bdd has better result, than we assign it
            free_bdd(best);
            best = b;
            best_size = b->hash_table->num_nodes;
        } else {
            free_bdd(b);
        }

        free(order);
    }

    free(vars);
    return best;
}

char BDD_use(BDD *bdd, char *input_bits) {
    if (!bdd || !input_bits) return -1;

    const BDDNode *node = bdd->root;
    if (node == &TRUE) return '1';
    if (node == &FALSE) return '0';

    if (!bdd->var_order) return -1;

    char bit_map[26];
    for (int i = 0; i < 26; ++i) bit_map[i] = -1;

    for (int i = 0; input_bits[i] && i < 26; ++i) {
        char ch = input_bits[i];
        if (ch == '0') bit_map[i] = 0;
        else if (ch == '1') bit_map[i] = 1;
        else return -1;
    }

    while (node && node != &TRUE && node != &FALSE) {
        char var = node->var;
        int idx = var - 'a';
        if (idx < 0 || idx >= 26) return -1;

        int decision = bit_map[idx];
        if (decision == 0)
            node = node->low;
        else if (decision == 1)
            node = node->high;
        else
            return -1;
    }

    return (node == &TRUE) ? '1' : '0';
}

// void test_efficiency(char *expr, char *default_order) {
//     clock_t start, end;

//     printf("Тест выражения: %s\n", expr);

//     start = clock();
//     BDD *bdd1 = create_BDD(expr, default_order);
//     end = clock();
//     int size1 = bdd1->hash_table->num_nodes;
//     double time1 = (double)(end - start) / CLOCKS_PER_SEC;

//     start = clock();
//     BDD *bdd2 = create_BDD_with_best_order(expr, default_order);
//     end = clock();
//     int size2 = bdd2->hash_table->num_nodes;
//     double time2 = (double)(end - start) / CLOCKS_PER_SEC;

//     printf("create_BDD: size = %d, time = %.6f sec\n", size1, time1);
//     printf("create_BDD_with_best_order: size = %d, time = %.6f sec\n", size2, time2);

//     if (size2 < size1) {
//         printf("→ The second function is more efficient in size (%d < %d)\n", size2, size1);
//     } else {
//         printf("→ The first function is not worse in size (%d >= %d)\n", size2, size1);
//     }

//     free_bdd(bdd1);
//     free_bdd(bdd2);
// }

// int main() {
//     char expression[] = "abc+cd+f+aef+bd";
//     char vars[] = "abcdef";

//     test_efficiency(expression, vars);

//     return 0;
// }