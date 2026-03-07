#include "ffi_opaque_test.h"
#include <stdlib.h>

Event* create_event(int type, int code, int value) {
    Event *e = (Event*)malloc(sizeof(Event));
    e->type = type;
    e->detail.code = code;
    e->detail.value = value;
    return e;
}
