#ifndef CC_FLAGS_TEST_H
#define CC_FLAGS_TEST_H

int get_value(void);

#ifdef CC_FLAGS_IMPL
int get_value(void) { return 42; }
#endif

#endif
