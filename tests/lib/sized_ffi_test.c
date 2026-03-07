#include "sized_ffi_test.h"

int8_t  add_i8(int8_t a, int8_t b)   { return a + b; }
int16_t add_i16(int16_t a, int16_t b) { return a + b; }
int     add_i32(int a, int b)         { return a + b; }
int64_t add_i64(int64_t a, int64_t b) { return a + b; }

uint8_t  add_u8(uint8_t a, uint8_t b)   { return a + b; }
uint16_t add_u16(uint16_t a, uint16_t b) { return a + b; }
uint32_t add_u32(uint32_t a, uint32_t b) { return a + b; }
uint64_t add_u64(uint64_t a, uint64_t b) { return a + b; }

int8_t  overflow_i8(void)  { return (int8_t)(127 + 1); }
uint8_t overflow_u8(void)  { return (uint8_t)(255 + 1); }

uint8_t  max_u8(void)  { return 255; }
uint16_t max_u16(void) { return 65535; }
uint32_t max_u32(void) { return 4294967295U; }

float  add_f32(float a, float b)   { return a + b; }
double add_f64(double a, double b) { return a + b; }

int identity_u8_to_i32(uint8_t x) { return (int)x; }
