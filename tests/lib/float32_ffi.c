#include "float32_ffi.h"

float f32_add(float a, float b) { return a + b; }
float f32_mul(float a, float b) { return a * b; }
float f32_from_int(int n) { return (float)n; }
int f32_to_int(float f) { return (int)f; }
double f32_to_double(float f) { return (double)f; }
