#ifndef STRUCT_PARAM_H
#define STRUCT_PARAM_H

typedef struct {
    int x;
    int y;
} Point;

/* Takes a struct pointer and returns the sum of fields */
int point_sum(const Point *p);

/* Takes a struct by value and returns the product of fields */
int point_product(Point p);

/* Creates a Point and returns it by value */
Point point_make(int x, int y);

#endif
