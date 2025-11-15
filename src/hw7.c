#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
        
    //create new bst if root is null
    if (root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));
        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }

    bst_sf *cur = root;

    while (cur != NULL) {
        //insert left 
        if (mat->name < cur->mat->name) {
          
            if (cur->left_child == NULL) {
                bst_sf *node = malloc(sizeof(bst_sf));
                node->mat = mat;
                node->left_child = NULL;
                node->right_child = NULL;
                cur->left_child = node;
                break;
            } else {
                cur = cur->left_child;
            }
        }
        else { //insert right
           
            if (cur->right_child == NULL) {
                bst_sf *node = malloc(sizeof(bst_sf));
                node->mat = mat;
                node->left_child = NULL;
                node->right_child = NULL;
                cur->right_child = node;
                break;
            } else {
                cur = cur->right_child;
            }
        }
    }

    return root;  
   
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    bst_sf *cur = root;

    while (cur != NULL) {
        if (name == cur->mat->name) {
            return cur->mat;     
        }
        else if (name < cur->mat->name) {
            cur = cur->left_child;
        }
        else {
            cur = cur->right_child;
        }
    }
    return NULL; 

}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) return;

    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);

    free(root->mat);

    free(root);
    
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
  
    if (mat1->num_rows != mat2->num_rows || mat1->num_cols != mat2->num_cols)
        return NULL;

    unsigned int rows = mat1->num_rows;
    unsigned int cols = mat1->num_cols;
    int result[rows * cols];

    for (unsigned int i = 0; i < rows * cols; i++)
        result[i] = mat1->values[i] + mat2->values[i];

    return copy_matrix(rows, cols, result);

}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
   if (mat1->num_cols != mat2->num_rows)
        return NULL;

    unsigned int m = mat1->num_rows;
    unsigned int n = mat1->num_cols;
    unsigned int p = mat2->num_cols;
    int result[m * p];

    for (unsigned int i = 0; i < m; i++) {
        for (unsigned int j = 0; j < p; j++) {
            int sum = 0;
            for (unsigned int k = 0; k < n; k++)
                sum += mat1->values[i*n + k] * mat2->values[k*p + j];
            result[i*p + j] = sum;
        }
    }

    return copy_matrix(m, p, result);

}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if (!mat) return NULL;

    unsigned int rows = mat->num_rows;
    unsigned int cols = mat->num_cols;
    int result[rows * cols];

    for (unsigned int i = 0; i < rows; i++) {
        for (unsigned int j = 0; j < cols; j++) {
            result[j * rows + i] = mat->values[i * cols + j];
        }
    }

    return copy_matrix(cols, rows, result);
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
   
   const char *p = expr;

    //parse rows 
    while (isspace((unsigned char)*p)) p++;

    unsigned int rows = 0;
    while (isdigit((unsigned char)*p)) {
        rows = rows * 10 + (*p - '0');
        p++;
    }

    //parse cols
    while (isspace((unsigned char)*p)) p++;

    unsigned int cols = 0;
    while (isdigit((unsigned char)*p)) {
        cols = cols * 10 + (*p - '0');
        p++;
    }

    //move to [
    while (*p && *p != '[') p++;
    if (*p != '[') return NULL;
    p++;      

    //allocate for matrix 
    int total = rows * cols;
    matrix_sf *m = malloc(sizeof(matrix_sf) + total * sizeof(int));
    m->name = name;
    m->num_rows = rows;
    m->num_cols = cols;

    //parse integers
    int index = 0;

    while (index < total && *p) {

        
        while (isspace((unsigned char)*p) || *p == ';')
            p++;

        
        int sign = 1;
        if (*p == '-') {
            sign = -1;
            p++;
        }

        //parse integer value
        int value = 0;
        int saw_digits = 0;

        while (isdigit((unsigned char)*p)) {
            value = value * 10 + (*p - '0');
            p++;
            saw_digits = 1;
        }

        //store integer in matrix 
        if (saw_digits) {
            m->values[index++] = sign * value;
        }

        //advance to next character in expr 
        while (*p && !isdigit((unsigned char)*p) &&
               *p != '-' &&
               *p != ']' &&
               *p != ';')
        {
            p++;
        }
    }

    return m;


}

static int precedence(char op) {
    if (op == '\'') return 3;   // highest
    if (op == '*')  return 2;
    if (op == '+')  return 1;
    return 0;
}


char* infix2postfix_sf(char *infix) {
    char opstack[1024];
    int top = -1;

  
    char *post = malloc(strlen(infix) * 2 + 1);
    int k = 0;

    for (int i = 0; infix[i] != '\0'; i++) {

        char c = infix[i];

        if (isspace((unsigned char)c))
            continue;

        //matrix names 
        if (c >= 'A' && c <= 'Z') {
            post[k++] = c;
        }

        /* left parenthesis */
        else if (c == '(') {
            opstack[++top] = c;
        }

        /* right parenthesis */
        else if (c == ')') {
            while (top >= 0 && opstack[top] != '(')
                post[k++] = opstack[top--];
            if (top >= 0) top--;  
        }

        
        else {
            while (top >= 0 &&
                   opstack[top] != '(' &&
                   precedence(opstack[top]) >= precedence(c)) {

                post[k++] = opstack[top--];
            }
            opstack[++top] = c;  // push operator
        }
    }

    
    while (top >= 0)
        post[k++] = opstack[top--];

    post[k] = '\0';
    return post;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    
    //convert infix to postfix 
    char *postfix = infix2postfix_sf(expr);

    int capacity = 1024;
    int top = -1;
    matrix_sf **stack = malloc(sizeof(matrix_sf*) * capacity);

    
    for (int i = 0; postfix[i] != '\0'; i++) {

        char c = postfix[i];

        if (isspace((unsigned char)c))
            continue;

        
        if (c >= 'A' && c <= 'Z') {
            matrix_sf *m = find_bst_sf(c, root);
            stack[++top] = m;   
        }

        else if (c == '\'') {

            matrix_sf *m = stack[top--];
            matrix_sf *t = transpose_mat_sf(m);

            if (!(m->name >= 'A' && m->name <= 'Z'))
                free(m);

            t->name = '?';
            stack[++top] = t;
        }

        else if (c == '+' || c == '*') {

            matrix_sf *right = stack[top--];
            matrix_sf *left  = stack[top--];

            matrix_sf *res;

            if (c == '+')
                res = add_mats_sf(left, right);
            else
                res = mult_mats_sf(left, right);

            if (!(left->name >= 'A' && left->name <= 'Z'))
                free(left);

            if (!(right->name >= 'A' && right->name <= 'Z'))
                free(right);

            res->name = '?';
            stack[++top] = res;
        }
    }

    
    matrix_sf *final = stack[top--];

    free(postfix);
    free(stack);
    
    final->name = name;
    return final;
}

matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) return NULL;

    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;

    char *str = NULL;
    size_t max_len = MAX_LINE_LEN;   

    while (getline(&str, &max_len, file) != -1) {

        char *p = str;
        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == '\0') continue;

        char name = 0;
        for (int i = 0; str[i] != '\0'; i++) {
            if (str[i] >= 'A' && str[i] <= 'Z') {
                name = str[i];
                break;
            }
        }
        if (!name) continue;   

        char *eq = strchr(str, '=');
        if (!eq) continue;
        eq++;  

        while (*eq && isspace((unsigned char)*eq)) eq++;

        int literal = 0;
        for (int i = 0; eq[i] != '\0'; i++) {
            if (eq[i] == '[') {
                literal = 1;
                break;
            }
        }

        matrix_sf *mat;

        if (literal) {
            mat = create_matrix_sf(name, eq);
        }
        else {
            mat = evaluate_expr_sf(name, eq, root);
        }

        root = insert_bst_sf(mat, root);
        last_matrix = mat;
    }

    free(str);
    fclose(file);

    return last_matrix;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
