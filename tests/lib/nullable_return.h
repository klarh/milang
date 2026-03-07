#ifndef NULLABLE_RETURN_TEST_H
#define NULLABLE_RETURN_TEST_H

void *nullable_make_ptr(int val);
void *nullable_make_null(void);
int nullable_deref_ptr(int *p);

#endif
