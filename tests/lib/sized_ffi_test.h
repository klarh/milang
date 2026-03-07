#ifndef SIZED_FFI_TEST_H
#define SIZED_FFI_TEST_H

#include <stdint.h>

// Signed integer operations at various widths
int8_t  add_i8(int8_t a, int8_t b);
int16_t add_i16(int16_t a, int16_t b);
int     add_i32(int a, int b);
int64_t add_i64(int64_t a, int64_t b);

// Unsigned integer operations
uint8_t  add_u8(uint8_t a, uint8_t b);
uint16_t add_u16(uint16_t a, uint16_t b);
uint32_t add_u32(uint32_t a, uint32_t b);
uint64_t add_u64(uint64_t a, uint64_t b);

// Overflow tests: return whether overflow wraps correctly
int8_t  overflow_i8(void);   // 127 + 1 → -128
uint8_t overflow_u8(void);   // 255 + 1 → 0

// Signedness: unsigned max values
uint8_t  max_u8(void);   // 255
uint16_t max_u16(void);  // 65535
uint32_t max_u32(void);  // 4294967295

// Float widths
float  add_f32(float a, float b);
double add_f64(double a, double b);

// Mixed: takes uint8_t, returns int32_t
int identity_u8_to_i32(uint8_t x);

#endif
