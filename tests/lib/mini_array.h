// Minimal array FFI for tests — integer array backed by malloc
#ifndef MINI_ARRAY_H
#define MINI_ARRAY_H
#include <stdlib.h>

typedef struct { int *data; int len; int cap; } MiniArray;

static MiniArray *mini_array_new(int cap) {
    MiniArray *a = (MiniArray *)malloc(sizeof(MiniArray));
    a->data = (int *)calloc(cap > 0 ? cap : 4, sizeof(int));
    a->len = 0;
    a->cap = cap > 0 ? cap : 4;
    return a;
}

static void mini_array_push(MiniArray *a, int val) {
    if (a->len >= a->cap) {
        a->cap *= 2;
        a->data = (int *)realloc(a->data, a->cap * sizeof(int));
    }
    a->data[a->len++] = val;
}

static int mini_array_get(MiniArray *a, int idx) {
    if (idx < 0 || idx >= a->len) return 0;
    return a->data[idx];
}

static int mini_array_len(MiniArray *a) {
    return a->len;
}

static void mini_array_free(MiniArray *a) {
    if (a) { free(a->data); free(a); }
}

#endif
