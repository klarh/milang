#ifndef FFI_INCLUDE_TEST_H
#define FFI_INCLUDE_TEST_H
#include <stdint.h>

/* The Packet type is defined in a separate header (ffi_include_types.h).
   Milang only sees this API header — it doesn't know Packet's layout. */

void* create_packet(uint32_t id, int32_t x, int32_t y);

#endif
