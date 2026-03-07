#ifndef FFI_ANNOTATE_TEST_H
#define FFI_ANNOTATE_TEST_H

typedef struct { int x; int y; } Point;

int point_sum(Point p);
Point point_scale(Point p, int factor);
void point_components(Point p, int *out_x, int *out_y);

#endif
