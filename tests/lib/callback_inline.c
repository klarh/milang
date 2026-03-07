#include "callback_inline.h"

int apply_callback(void (*func)(int status, void *data), void *data) {
    func(42, data);
    return 0;
}

int add_one(int x) {
    return x + 1;
}
