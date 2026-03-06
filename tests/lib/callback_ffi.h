#ifndef CALLBACK_FFI_H
#define CALLBACK_FFI_H

typedef long (*IntFn)(long);
typedef long (*IntBinFn)(long, long);
typedef void (*VoidFn)(long);

long apply_fn(IntFn f, long x);
long apply_twice(IntFn f, long x);
long fold_range(IntBinFn f, long init, long n);
void call_void(VoidFn f, long x);

#endif
