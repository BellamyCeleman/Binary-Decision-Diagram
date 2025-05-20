#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "bdd.c"

char evaluate_expression(char *expression, char *vars, int var_count, char *values) {
    char *copy_expr = strdup(expression);
    if (!copy_expr) return '0';

    char *token = strtok(copy_expr, "+");
    while (token) {
        int match = 1;
        for (int i = 0; token[i] != '\0'; i++) {
            int is_negated = 0;

            if (token[i] == '!') {
                is_negated = 1;
                i++;
            }

            char var = token[i];
            int found = 0;

            for (int j = 0; j < var_count; j++) {
                if (vars[j] == var) {
                    char val = values[j];
                    if ((is_negated && val == '1') || (!is_negated && val == '0')) {
                        match = 0; 
                    }
                    found = 1;
                    break;
                }
            }

            if (!found || !match) {
                match = 0;
                break;
            }
        }

        if (match) {
            free(copy_expr);
            return '1';
        }

        token = strtok(NULL, "+");
    }

    free(copy_expr);
    return '0';
}

char *generate_random_boolean_function(int num_vars) {
    int number_terms = rand() % (num_vars + 1) + 1;
    char *function = malloc(5000);
    function[0] = '\0';

    for (int i = 0; i < number_terms; i++) {
        int term_length = rand() % num_vars + 1;

        for (int j = 0; j < term_length; j++) {
            if (rand() % 2 == 0) {
                strcat(function, "!");
            }
            char variable = 'a' + (rand() % num_vars);
            strncat(function, &variable, 1);
        }

        if (i < number_terms - 1) {
            strcat(function, "+");
        }
    }

    return function;
}

void all_combinations(int num_vars, char **combinations) {
    int number_combinations = 1 << num_vars;
    for (int i = 0; i < number_combinations; i++) {
        for (int j = 0; j < num_vars; j++) {
            combinations[i][j] = (i & (1 << (num_vars - j - 1))) ? '1' : '0';
        }
        combinations[i][num_vars] = '\0';
    }
}

int test_accuracy(BDD *bdd, char *expr, char *vars, int num_vars) {
    int number_combinations = 1 << num_vars;
    char **combinations = malloc(number_combinations * sizeof(char*));
    for (int i = 0; i < number_combinations; i++) {
        combinations[i] = malloc((num_vars + 1) * sizeof(char));
    }

    all_combinations(num_vars, combinations);
    for (int i = 0; i < number_combinations; i++) {
        char expected = evaluate_expression(expr, vars, num_vars, combinations[i]);
        char result = BDD_use(bdd, combinations[i]);
        if (expected != result) {
            for (int j = 0; j < number_combinations; j++) {
                free(combinations[j]);
            }
            free(combinations);
            return 0;
        }
    }

    for (int i = 0; i < number_combinations; i++) {
        free(combinations[i]);
    }
    free(combinations);
    return 1;
}

double evaluate_reduction(int original_size, int reduced_size) {
    if (original_size == 0) return 0.0;
    return ((double)(original_size - reduced_size) / original_size) * 100.0;
}

void test_bdd(int num_vars, int num_func) {
    char *order = malloc((num_vars + 1) * sizeof(char));
    for (int i = 0; i < num_vars; i++) {
        order[i] = 'a' + i;
    }
    order[num_vars] = '\0';

    int total_correct = 0;
    double total_reduction = 0.0;
    double total_best_bdd_reduction = 0.0;

    double total_bdd_time = 0.0;
    double total_best_bdd_time = 0.0;

    int num_nodes = 0;
    int num_nodes_bo = 0; 

    for (int i = 0; i < num_func; i++) {
        char *expression = generate_random_boolean_function(num_vars);

        clock_t start_bdd = clock();
        BDD *bdd = create_BDD(expression, order);
        clock_t end_bdd = clock();
        total_bdd_time += (double)(end_bdd - start_bdd) / CLOCKS_PER_SEC;

        clock_t start_best_bdd = clock();
        BDD *best_bdd = create_BDD_with_best_order(expression, order);
        clock_t end_best_bdd = clock();
        total_best_bdd_time += (double)(end_best_bdd - start_best_bdd) / CLOCKS_PER_SEC;

        if (test_accuracy(bdd, expression, bdd->var_order, num_vars)) {
            total_correct++;
        }

        num_nodes += bdd->hash_table->num_nodes;
        num_nodes_bo += best_bdd->hash_table->num_nodes;

        int full_size = (1 << (num_vars + 1)) - 1;
        double reduction = evaluate_reduction(full_size, bdd->hash_table->num_nodes);
        double best_bdd_reduction = evaluate_reduction(bdd->hash_table->num_nodes, best_bdd->hash_table->num_nodes);

        total_reduction += reduction;
        total_best_bdd_reduction += best_bdd_reduction;

        free_bdd(bdd);
        free_bdd(best_bdd);
        free(expression);
    }

    free(order);

    printf("Num of variables: %d\n", num_vars);
    printf("Num of expressions: %d\n", num_func);
    printf("Accuracy: %.2f%%\n", (double)total_correct / num_func * 100.0);
    printf("Reduction: %.2f%%\n", total_reduction / num_func);
    printf("Best order reduction: %.2f%%\n", total_best_bdd_reduction / num_func);
    printf("Time for BDD creation: %.2f seconds\n", total_bdd_time);
    printf("Time for BDD with best order creation: %.2f seconds\n", total_best_bdd_time);
    printf("Number of nodes: %d\n", num_nodes / num_func);
    printf("Number of nodes best order: %d\n", num_nodes_bo / num_func);
}

int main() {
    srand(time(NULL));
    int num_vars = 12;
    int num_func = 100;

    test_bdd(num_vars, num_func);
    return 0;
}
