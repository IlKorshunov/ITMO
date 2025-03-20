#include "matrix.h"

int main() {
    matrix<int> my_matrix(3, 3);
    my_matrix(0, 0) = 1;
    my_matrix(1, 1) = 2;
    return 0;
}