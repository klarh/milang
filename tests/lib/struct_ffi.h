#ifndef STRUCT_FFI_H
#define STRUCT_FFI_H

typedef struct { float x; float y; } Vec2;

Vec2 vec2_new(float x, float y);
Vec2 vec2_add(Vec2 a, Vec2 b);
float vec2_dot(Vec2 a, Vec2 b);

#endif
