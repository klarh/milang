#include "struct_param.h"

int point_sum(const Point *p) {
    return p->x + p->y;
}

int point_product(Point p) {
    return p.x * p.y;
}

Point point_make(int x, int y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}
