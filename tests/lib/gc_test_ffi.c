#include "gc_test_ffi.h"
#include <stdlib.h>

GcTestObj* gc_test_alloc(long val) {
    GcTestObj* obj = (GcTestObj*)malloc(sizeof(GcTestObj));
    obj->value = val;
    return obj;
}

void gc_test_free(GcTestObj* obj) {
    if (obj) free(obj);
}

long gc_test_read(GcTestObj* obj) {
    return obj->value;
}
