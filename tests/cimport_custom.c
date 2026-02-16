#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

typedef enum { MI_INT, MI_FLOAT, MI_STRING, MI_RECORD, MI_CLOSURE, MI_POINTER } MiType;

typedef struct MiVal {
  MiType type;
  union {
    int64_t i;
    double f;
    char *s;
    struct { const char *tag; const char **names; struct MiVal *fields; int nfields; } rec;
    struct { struct MiVal (*fn)(struct MiVal, void*); void *env; } closure;
    void *ptr;
  } as;
} MiVal;

static MiVal mi_int(int64_t v) { MiVal r; r.type = MI_INT; r.as.i = v; return r; }
static MiVal mi_float(double v) { MiVal r; r.type = MI_FLOAT; r.as.f = v; return r; }
static MiVal mi_string(const char *s) { MiVal r; r.type = MI_STRING; r.as.s = strdup(s); return r; }
static MiVal mi_pointer(void *p) { MiVal r; r.type = MI_POINTER; r.as.ptr = p; return r; }

static MiVal mi_closure(MiVal (*fn)(MiVal, void*), void *env) {
  MiVal r; r.type = MI_CLOSURE; r.as.closure.fn = fn; r.as.closure.env = env; return r;
}

static MiVal mi_apply(MiVal f, MiVal arg) {
  if (f.type != MI_CLOSURE) { fprintf(stderr, "apply on non-closure\n"); exit(1); }
  return f.as.closure.fn(arg, f.as.closure.env);
}

static double mi_to_float(MiVal v) {
  return v.type == MI_FLOAT ? v.as.f : (double)v.as.i;
}

static MiVal mi_add(MiVal a, MiVal b) {
  if (a.type == MI_STRING && b.type == MI_STRING) {
    size_t la = strlen(a.as.s), lb = strlen(b.as.s);
    char *r = malloc(la + lb + 1);
    memcpy(r, a.as.s, la); memcpy(r+la, b.as.s, lb+1);
    MiVal v; v.type = MI_STRING; v.as.s = r; return v;
  }
  if (a.type == MI_FLOAT || b.type == MI_FLOAT)
    return mi_float(mi_to_float(a) + mi_to_float(b));
  return mi_int(a.as.i + b.as.i);
}
static MiVal mi_sub(MiVal a, MiVal b) {
  if (a.type == MI_FLOAT || b.type == MI_FLOAT)
    return mi_float(mi_to_float(a) - mi_to_float(b));
  return mi_int(a.as.i - b.as.i);
}
static MiVal mi_mul(MiVal a, MiVal b) {
  if (a.type == MI_FLOAT || b.type == MI_FLOAT)
    return mi_float(mi_to_float(a) * mi_to_float(b));
  return mi_int(a.as.i * b.as.i);
}
static MiVal mi_div(MiVal a, MiVal b) {
  if (a.type == MI_FLOAT || b.type == MI_FLOAT)
    return mi_float(mi_to_float(a) / mi_to_float(b));
  if (b.as.i == 0) { fprintf(stderr, "division by zero\n"); exit(1); }
  return mi_int(a.as.i / b.as.i);
}
static MiVal mi_pow(MiVal a, MiVal b) {
  if (a.type == MI_FLOAT || b.type == MI_FLOAT)
    return mi_float(pow(mi_to_float(a), mi_to_float(b)));
  int64_t result = 1, base = a.as.i, exp = b.as.i;
  for (; exp > 0; exp--) result *= base;
  return mi_int(result);
}

static MiVal mi_eq(MiVal a, MiVal b) { return mi_int(a.as.i == b.as.i); }
static MiVal mi_neq(MiVal a, MiVal b) { return mi_int(a.as.i != b.as.i); }
static MiVal mi_lt(MiVal a, MiVal b) { return mi_int(a.as.i < b.as.i); }
static MiVal mi_gt(MiVal a, MiVal b) { return mi_int(a.as.i > b.as.i); }
static MiVal mi_le(MiVal a, MiVal b) { return mi_int(a.as.i <= b.as.i); }
static MiVal mi_ge(MiVal a, MiVal b) { return mi_int(a.as.i >= b.as.i); }

static void mi_print_val(MiVal v) {
  switch (v.type) {
    case MI_INT:    printf("%ld", v.as.i); break;
    case MI_FLOAT:  printf("%g", v.as.f); break;
    case MI_STRING: printf("%s", v.as.s); break;
    case MI_RECORD:
      printf("%s {", v.as.rec.tag);
      for (int i = 0; i < v.as.rec.nfields; i++) {
        if (i > 0) printf(", ");
        if (v.as.rec.names) printf("%s = ", v.as.rec.names[i]);
        mi_print_val(v.as.rec.fields[i]);
      }
      printf("}");
      break;
    case MI_CLOSURE: printf("<closure>"); break;
    case MI_POINTER: printf("<ptr:%p>", v.as.ptr); break;
  }
}

static void mi_println_val(MiVal v) {
  mi_print_val(v); printf("\n");
}

static MiVal mi_field(MiVal rec, const char *name) {
  if (rec.type != MI_RECORD) { fprintf(stderr, "field access on non-record\n"); exit(1); }
  for (int i = 0; i < rec.as.rec.nfields; i++) {
    if (rec.as.rec.names && strcmp(rec.as.rec.names[i], name) == 0)
      return rec.as.rec.fields[i];
  }
  fprintf(stderr, "field '%s' not found in record '%s'\n", name, rec.as.rec.tag);
  exit(1);
}

static MiVal mi_builtin_print(MiVal v, void *env) {
  (void)env; mi_print_val(v); return mi_int(0);
}
static MiVal mi_builtin_println(MiVal v, void *env) {
  (void)env; mi_println_val(v); return mi_int(0);
}

#include "tests/lib/mylib.h"

struct mi_cffi_0_env {
  MiVal _a0;
};

static MiVal mi_cffi_0(MiVal _arg, void *_env) {
  struct mi_cffi_0_env *_e = (struct mi_cffi_0_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_int((int64_t)(add_ints((int)(_a0.as.i), (int)(_arg.as.i))));
}

static MiVal mi_cffi_1(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_0_env *_ne = malloc(sizeof(struct mi_cffi_0_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_0, _ne);
}

struct mi_cffi_2_env {
  MiVal _a0;
};

static MiVal mi_cffi_2(MiVal _arg, void *_env) {
  struct mi_cffi_2_env *_e = (struct mi_cffi_2_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(multiply(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_3(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_2_env *_ne = malloc(sizeof(struct mi_cffi_2_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_2, _ne);
}

static MiVal mi_cffi_4(MiVal _arg, void *_env) {
  (void)_arg;   (void)_env;
  return mi_string(greeting());
}

struct mi_cffi_5_env {
  MiVal _a0;
};

static MiVal mi_cffi_5(MiVal _arg, void *_env) {
  struct mi_cffi_5_env *_e = (struct mi_cffi_5_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_int((int64_t)(add_ints((int)(_a0.as.i), (int)(_arg.as.i))));
}

static MiVal mi_cffi_6(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_5_env *_ne = malloc(sizeof(struct mi_cffi_5_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_5, _ne);
}

struct mi_cffi_7_env {
  MiVal _a0;
};

static MiVal mi_cffi_7(MiVal _arg, void *_env) {
  struct mi_cffi_7_env *_e = (struct mi_cffi_7_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_int((int64_t)(add_ints((int)(_a0.as.i), (int)(_arg.as.i))));
}

static MiVal mi_cffi_8(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_7_env *_ne = malloc(sizeof(struct mi_cffi_7_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_7, _ne);
}

struct mi_cffi_9_env {
  MiVal _a0;
};

static MiVal mi_cffi_9(MiVal _arg, void *_env) {
  struct mi_cffi_9_env *_e = (struct mi_cffi_9_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(multiply(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_10(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_9_env *_ne = malloc(sizeof(struct mi_cffi_9_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_9, _ne);
}

struct mi_cffi_11_env {
  MiVal _a0;
};

static MiVal mi_cffi_11(MiVal _arg, void *_env) {
  struct mi_cffi_11_env *_e = (struct mi_cffi_11_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(multiply(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_12(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_11_env *_ne = malloc(sizeof(struct mi_cffi_11_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_11, _ne);
}

static MiVal mi_cffi_13(MiVal _arg, void *_env) {
  (void)_arg;   (void)_env;
  return mi_string(greeting());
}

static MiVal mi_cffi_14(MiVal _arg, void *_env) {
  (void)_arg;   (void)_env;
  return mi_string(greeting());
}


int main(void) {
  MiVal mi_print = mi_closure(mi_builtin_print, NULL);
  MiVal mi_println = mi_closure(mi_builtin_println, NULL);

  MiVal Lib = ({
    MiVal add_ints = mi_closure(mi_cffi_1, NULL);
    MiVal multiply = mi_closure(mi_cffi_3, NULL);
    MiVal greeting = mi_closure(mi_cffi_4, NULL);
    greeting;
  });
  MiVal a = mi_apply(mi_apply(mi_closure(mi_cffi_6, NULL), mi_int(3)), mi_int(4));
  MiVal main1 = mi_apply(mi_println, mi_apply(mi_apply(mi_closure(mi_cffi_8, NULL), mi_int(3)), mi_int(4)));
  MiVal b = mi_apply(mi_apply(mi_closure(mi_cffi_10, NULL), mi_float(2.5)), mi_float(4.0));
  MiVal main2 = mi_apply(mi_println, mi_apply(mi_apply(mi_closure(mi_cffi_12, NULL), mi_float(2.5)), mi_float(4.0)));
  MiVal c = mi_apply(mi_closure(mi_cffi_13, NULL), mi_int(0));
  MiVal main3 = mi_apply(mi_println, mi_apply(mi_closure(mi_cffi_14, NULL), mi_int(0)));

  printf("Lib = "); mi_print_val(Lib); printf("\n");
  printf("a = "); mi_print_val(a); printf("\n");
  printf("main1 = "); mi_print_val(main1); printf("\n");
  printf("b = "); mi_print_val(b); printf("\n");
  printf("main2 = "); mi_print_val(main2); printf("\n");
  printf("c = "); mi_print_val(c); printf("\n");
  printf("main3 = "); mi_print_val(main3); printf("\n");
  return 0;
}
