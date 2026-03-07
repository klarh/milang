#include <stdlib.h>

void *nullable_make_ptr(int val) {
    int *p = (int *)malloc(sizeof(int));
    *p = val;
    return p;
}

void *nullable_make_null(void) {
    return NULL;
}

int nullable_deref_ptr(int *p) {
    return *p;
}
