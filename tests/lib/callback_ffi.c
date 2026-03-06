#include "callback_ffi.h"

long apply_fn(IntFn f, long x) {
    return f(x);
}

long apply_twice(IntFn f, long x) {
    return f(f(x));
}

long fold_range(IntBinFn f, long init, long n) {
    long acc = init;
    for (long i = 0; i < n; i++) {
        acc = f(acc, i);
    }
    return acc;
}

void call_void(VoidFn f, long x) {
    f(x);
}
