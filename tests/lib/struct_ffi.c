#include "struct_ffi.h"

Vec2 vec2_new(float x, float y) { return (Vec2){ x, y }; }
Vec2 vec2_add(Vec2 a, Vec2 b) { return (Vec2){ a.x + b.x, a.y + b.y }; }
float vec2_dot(Vec2 a, Vec2 b) { return a.x * b.x + a.y * b.y; }
