#include "nullable_test.h"
#include <string.h>

int is_null(void *ptr) {
    return ptr == NULL ? 1 : 0;
}

int copy_rect(TestRect *rect, TestRect *out) {
    if (rect == NULL) {
        memset(out, 0, sizeof(TestRect));
        return 0;
    }
    *out = *rect;
    return 1;
}

void make_rect(int x, int y, int w, int h, TestRect *out) {
    out->x = x;
    out->y = y;
    out->w = w;
    out->h = h;
}
