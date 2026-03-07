#ifndef CALLBACK_INLINE_H
#define CALLBACK_INLINE_H

/* Uses inline function pointer syntax (not typedef) like on_exit(3) */
int apply_callback(void (*func)(int status, void *data), void *data);
int add_one(int x);

#endif
