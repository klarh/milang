#ifndef NULLABLE_TEST_H
#define NULLABLE_TEST_H

typedef struct { int x; int y; int w; int h; } TestRect;

/* Returns 1 if ptr is NULL, 0 otherwise */
int is_null(void *ptr);

/* Copies rect to out if rect is non-NULL, otherwise sets out to {0,0,0,0}.
   Returns 1 if rect was non-NULL. */
int copy_rect(TestRect *rect, TestRect *out);

/* Fills a TestRect out-param with x,y,w,h values */
void make_rect(int x, int y, int w, int h, TestRect *out);

#endif
