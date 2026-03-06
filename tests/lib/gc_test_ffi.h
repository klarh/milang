#ifndef GC_TEST_FFI_H
#define GC_TEST_FFI_H

typedef struct { long value; } GcTestObj;

GcTestObj* gc_test_alloc(long val);
void gc_test_free(GcTestObj* obj);
long gc_test_read(GcTestObj* obj);

#endif
