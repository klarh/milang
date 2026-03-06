#ifndef ENUM_FFI_H
#define ENUM_FFI_H

typedef enum { RED = 0, GREEN = 1, BLUE = 2 } Color;

int color_value(Color c);
const char* color_name(Color c);

#endif
