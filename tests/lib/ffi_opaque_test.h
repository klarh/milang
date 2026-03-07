#ifndef FFI_OPAQUE_TEST_H
#define FFI_OPAQUE_TEST_H

typedef struct {
    int type;
    struct { int code; int value; } detail;
} Event;

/* Returns pointer to a static Event (for testing) */
Event* create_event(int type, int code, int value);

#endif
