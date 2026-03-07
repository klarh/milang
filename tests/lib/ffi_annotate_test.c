#include "ffi_annotate_test.h"

int point_sum(Point p) { return p.x + p.y; }
Point point_scale(Point p, int factor) { return (Point){ p.x * factor, p.y * factor }; }
void point_components(Point p, int *out_x, int *out_y) { *out_x = p.x; *out_y = p.y; }
