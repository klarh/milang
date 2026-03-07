#include "ffi_include_test.h"
#include "ffi_include_types.h"
#include <stdlib.h>

void* create_packet(uint32_t id, int32_t x, int32_t y) {
    Packet* p = malloc(sizeof(Packet));
    p->id = id;
    p->x = x;
    p->y = y;
    return p;
}
