#include "enum_ffi.h"

int color_value(Color c) { return (int)c * 10; }
const char* color_name(Color c) {
    switch(c) {
        case RED: return "red";
        case GREEN: return "green";
        case BLUE: return "blue";
        default: return "unknown";
    }
}
