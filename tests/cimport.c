#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

typedef enum { MI_INT, MI_FLOAT, MI_STRING, MI_RECORD, MI_CLOSURE } MiType;

typedef struct MiVal {
  MiType type;
  union {
    int64_t i;
    double f;
    char *s;
    struct { const char *tag; const char **names; struct MiVal *fields; int nfields; } rec;
    struct { struct MiVal (*fn)(struct MiVal, void*); void *env; } closure;
  } as;
} MiVal;

static MiVal mi_int(int64_t v) { MiVal r; r.type = MI_INT; r.as.i = v; return r; }
static MiVal mi_float(double v) { MiVal r; r.type = MI_FLOAT; r.as.f = v; return r; }
static MiVal mi_string(const char *s) { MiVal r; r.type = MI_STRING; r.as.s = strdup(s); return r; }

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

#include "/usr/include/math.h"

static MiVal mi_cffi_0(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acos(mi_to_float(_arg))));
}

static MiVal mi_cffi_1(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asin(mi_to_float(_arg))));
}

static MiVal mi_cffi_2(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atan(mi_to_float(_arg))));
}

static MiVal mi_cffi_3(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_4_env *_ne = malloc(sizeof(struct mi_cffi_4_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_4, _ne);
}

struct mi_cffi_4_env {
  MiVal _a0;
};

static MiVal mi_cffi_4(MiVal _arg, void *_env) {
  struct mi_cffi_4_env *_e = (struct mi_cffi_4_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(atan2(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_5(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cos(mi_to_float(_arg))));
}

static MiVal mi_cffi_6(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sin(mi_to_float(_arg))));
}

static MiVal mi_cffi_7(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tan(mi_to_float(_arg))));
}

static MiVal mi_cffi_8(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cosh(mi_to_float(_arg))));
}

static MiVal mi_cffi_9(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sinh(mi_to_float(_arg))));
}

static MiVal mi_cffi_10(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tanh(mi_to_float(_arg))));
}

static MiVal mi_cffi_11(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acosh(mi_to_float(_arg))));
}

static MiVal mi_cffi_12(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asinh(mi_to_float(_arg))));
}

static MiVal mi_cffi_13(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atanh(mi_to_float(_arg))));
}

static MiVal mi_cffi_14(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(exp(mi_to_float(_arg))));
}

static MiVal mi_cffi_15(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_16_env *_ne = malloc(sizeof(struct mi_cffi_16_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_16, _ne);
}

struct mi_cffi_16_env {
  MiVal _a0;
};

static MiVal mi_cffi_16(MiVal _arg, void *_env) {
  struct mi_cffi_16_env *_e = (struct mi_cffi_16_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(frexp(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_17(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_18_env *_ne = malloc(sizeof(struct mi_cffi_18_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_18, _ne);
}

struct mi_cffi_18_env {
  MiVal _a0;
};

static MiVal mi_cffi_18(MiVal _arg, void *_env) {
  struct mi_cffi_18_env *_e = (struct mi_cffi_18_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(ldexp(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_19(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log(mi_to_float(_arg))));
}

static MiVal mi_cffi_20(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log10(mi_to_float(_arg))));
}

static MiVal mi_cffi_21(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_22_env *_ne = malloc(sizeof(struct mi_cffi_22_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_22, _ne);
}

struct mi_cffi_22_env {
  MiVal _a0;
};

static MiVal mi_cffi_22(MiVal _arg, void *_env) {
  struct mi_cffi_22_env *_e = (struct mi_cffi_22_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(modf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_23(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(expm1(mi_to_float(_arg))));
}

static MiVal mi_cffi_24(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log1p(mi_to_float(_arg))));
}

static MiVal mi_cffi_25(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(logb(mi_to_float(_arg))));
}

static MiVal mi_cffi_26(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(exp2(mi_to_float(_arg))));
}

static MiVal mi_cffi_27(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log2(mi_to_float(_arg))));
}

static MiVal mi_cffi_28(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_29_env *_ne = malloc(sizeof(struct mi_cffi_29_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_29, _ne);
}

struct mi_cffi_29_env {
  MiVal _a0;
};

static MiVal mi_cffi_29(MiVal _arg, void *_env) {
  struct mi_cffi_29_env *_e = (struct mi_cffi_29_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(pow(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_30(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sqrt(mi_to_float(_arg))));
}

static MiVal mi_cffi_31(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_32_env *_ne = malloc(sizeof(struct mi_cffi_32_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_32, _ne);
}

struct mi_cffi_32_env {
  MiVal _a0;
};

static MiVal mi_cffi_32(MiVal _arg, void *_env) {
  struct mi_cffi_32_env *_e = (struct mi_cffi_32_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(hypot(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_33(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cbrt(mi_to_float(_arg))));
}

static MiVal mi_cffi_34(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(ceil(mi_to_float(_arg))));
}

static MiVal mi_cffi_35(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(fabs(mi_to_float(_arg))));
}

static MiVal mi_cffi_36(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(floor(mi_to_float(_arg))));
}

static MiVal mi_cffi_37(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_38_env *_ne = malloc(sizeof(struct mi_cffi_38_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_38, _ne);
}

struct mi_cffi_38_env {
  MiVal _a0;
};

static MiVal mi_cffi_38(MiVal _arg, void *_env) {
  struct mi_cffi_38_env *_e = (struct mi_cffi_38_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmod(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_39(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isinf(mi_to_float(_arg))));
}

static MiVal mi_cffi_40(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(finite(mi_to_float(_arg))));
}

static MiVal mi_cffi_41(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_42_env *_ne = malloc(sizeof(struct mi_cffi_42_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_42, _ne);
}

struct mi_cffi_42_env {
  MiVal _a0;
};

static MiVal mi_cffi_42(MiVal _arg, void *_env) {
  struct mi_cffi_42_env *_e = (struct mi_cffi_42_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(drem(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_43(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(significand(mi_to_float(_arg))));
}

static MiVal mi_cffi_44(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_45_env *_ne = malloc(sizeof(struct mi_cffi_45_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_45, _ne);
}

struct mi_cffi_45_env {
  MiVal _a0;
};

static MiVal mi_cffi_45(MiVal _arg, void *_env) {
  struct mi_cffi_45_env *_e = (struct mi_cffi_45_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(copysign(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_46(MiVal _arg, void *_env) {
  (void)_arg; (void)_env;
  return mi_float((double)(nan()));
}

static MiVal mi_cffi_47(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isnan(mi_to_float(_arg))));
}

static MiVal mi_cffi_48(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j0(mi_to_float(_arg))));
}

static MiVal mi_cffi_49(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j1(mi_to_float(_arg))));
}

static MiVal mi_cffi_50(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_51_env *_ne = malloc(sizeof(struct mi_cffi_51_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_51, _ne);
}

struct mi_cffi_51_env {
  MiVal _a0;
};

static MiVal mi_cffi_51(MiVal _arg, void *_env) {
  struct mi_cffi_51_env *_e = (struct mi_cffi_51_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(jn(_a0.as.i, mi_to_float(_arg))));
}

static MiVal mi_cffi_52(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y0(mi_to_float(_arg))));
}

static MiVal mi_cffi_53(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y1(mi_to_float(_arg))));
}

static MiVal mi_cffi_54(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_55_env *_ne = malloc(sizeof(struct mi_cffi_55_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_55, _ne);
}

struct mi_cffi_55_env {
  MiVal _a0;
};

static MiVal mi_cffi_55(MiVal _arg, void *_env) {
  struct mi_cffi_55_env *_e = (struct mi_cffi_55_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(yn(_a0.as.i, mi_to_float(_arg))));
}

static MiVal mi_cffi_56(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erf(mi_to_float(_arg))));
}

static MiVal mi_cffi_57(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erfc(mi_to_float(_arg))));
}

static MiVal mi_cffi_58(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(lgamma(mi_to_float(_arg))));
}

static MiVal mi_cffi_59(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tgamma(mi_to_float(_arg))));
}

static MiVal mi_cffi_60(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(gamma(mi_to_float(_arg))));
}

static MiVal mi_cffi_61(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_62_env *_ne = malloc(sizeof(struct mi_cffi_62_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_62, _ne);
}

struct mi_cffi_62_env {
  MiVal _a0;
};

static MiVal mi_cffi_62(MiVal _arg, void *_env) {
  struct mi_cffi_62_env *_e = (struct mi_cffi_62_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(lgamma_r(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_63(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(rint(mi_to_float(_arg))));
}

static MiVal mi_cffi_64(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_65_env *_ne = malloc(sizeof(struct mi_cffi_65_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_65, _ne);
}

struct mi_cffi_65_env {
  MiVal _a0;
};

static MiVal mi_cffi_65(MiVal _arg, void *_env) {
  struct mi_cffi_65_env *_e = (struct mi_cffi_65_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nextafter(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_66(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_67_env *_ne = malloc(sizeof(struct mi_cffi_67_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_67, _ne);
}

struct mi_cffi_67_env {
  MiVal _a0;
};

static MiVal mi_cffi_67(MiVal _arg, void *_env) {
  struct mi_cffi_67_env *_e = (struct mi_cffi_67_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nexttoward(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_68(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_69_env *_ne = malloc(sizeof(struct mi_cffi_69_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_69, _ne);
}

struct mi_cffi_69_env {
  MiVal _a0;
};

static MiVal mi_cffi_69(MiVal _arg, void *_env) {
  struct mi_cffi_69_env *_e = (struct mi_cffi_69_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(remainder(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_70(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_71_env *_ne = malloc(sizeof(struct mi_cffi_71_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_71, _ne);
}

struct mi_cffi_71_env {
  MiVal _a0;
};

static MiVal mi_cffi_71(MiVal _arg, void *_env) {
  struct mi_cffi_71_env *_e = (struct mi_cffi_71_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbn(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_72(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(ilogb(mi_to_float(_arg))));
}

static MiVal mi_cffi_73(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_74_env *_ne = malloc(sizeof(struct mi_cffi_74_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_74, _ne);
}

struct mi_cffi_74_env {
  MiVal _a0;
};

static MiVal mi_cffi_74(MiVal _arg, void *_env) {
  struct mi_cffi_74_env *_e = (struct mi_cffi_74_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbln(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_75(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(nearbyint(mi_to_float(_arg))));
}

static MiVal mi_cffi_76(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(round(mi_to_float(_arg))));
}

static MiVal mi_cffi_77(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(trunc(mi_to_float(_arg))));
}

static MiVal mi_cffi_78(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_79_env *_ne = malloc(sizeof(struct mi_cffi_79_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_79, _ne);
}

struct mi_cffi_79_env {
  MiVal _a0;
};

static MiVal mi_cffi_79(MiVal _arg, void *_env) {
  struct mi_cffi_79_env *_e = (struct mi_cffi_79_env *)_env;
  struct mi_cffi_80_env *_ne = malloc(sizeof(struct mi_cffi_80_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_80, _ne);
}

struct mi_cffi_80_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_80(MiVal _arg, void *_env) {
  struct mi_cffi_80_env *_e = (struct mi_cffi_80_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(remquo(mi_to_float(_a0), mi_to_float(_a1), _arg.as.i)));
}

static MiVal mi_cffi_81(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lrint(mi_to_float(_arg))));
}

static MiVal mi_cffi_82(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lround(mi_to_float(_arg))));
}

static MiVal mi_cffi_83(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_84_env *_ne = malloc(sizeof(struct mi_cffi_84_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_84, _ne);
}

struct mi_cffi_84_env {
  MiVal _a0;
};

static MiVal mi_cffi_84(MiVal _arg, void *_env) {
  struct mi_cffi_84_env *_e = (struct mi_cffi_84_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fdim(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_85(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_86_env *_ne = malloc(sizeof(struct mi_cffi_86_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_86, _ne);
}

struct mi_cffi_86_env {
  MiVal _a0;
};

static MiVal mi_cffi_86(MiVal _arg, void *_env) {
  struct mi_cffi_86_env *_e = (struct mi_cffi_86_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmax(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_87(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_88_env *_ne = malloc(sizeof(struct mi_cffi_88_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_88, _ne);
}

struct mi_cffi_88_env {
  MiVal _a0;
};

static MiVal mi_cffi_88(MiVal _arg, void *_env) {
  struct mi_cffi_88_env *_e = (struct mi_cffi_88_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmin(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_89(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_90_env *_ne = malloc(sizeof(struct mi_cffi_90_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_90, _ne);
}

struct mi_cffi_90_env {
  MiVal _a0;
};

static MiVal mi_cffi_90(MiVal _arg, void *_env) {
  struct mi_cffi_90_env *_e = (struct mi_cffi_90_env *)_env;
  struct mi_cffi_91_env *_ne = malloc(sizeof(struct mi_cffi_91_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_91, _ne);
}

struct mi_cffi_91_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_91(MiVal _arg, void *_env) {
  struct mi_cffi_91_env *_e = (struct mi_cffi_91_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(fma(mi_to_float(_a0), mi_to_float(_a1), mi_to_float(_arg))));
}

static MiVal mi_cffi_92(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_93_env *_ne = malloc(sizeof(struct mi_cffi_93_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_93, _ne);
}

struct mi_cffi_93_env {
  MiVal _a0;
};

static MiVal mi_cffi_93(MiVal _arg, void *_env) {
  struct mi_cffi_93_env *_e = (struct mi_cffi_93_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalb(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_94(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acosf(mi_to_float(_arg))));
}

static MiVal mi_cffi_95(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asinf(mi_to_float(_arg))));
}

static MiVal mi_cffi_96(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atanf(mi_to_float(_arg))));
}

static MiVal mi_cffi_97(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_98_env *_ne = malloc(sizeof(struct mi_cffi_98_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_98, _ne);
}

struct mi_cffi_98_env {
  MiVal _a0;
};

static MiVal mi_cffi_98(MiVal _arg, void *_env) {
  struct mi_cffi_98_env *_e = (struct mi_cffi_98_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(atan2f(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_99(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cosf(mi_to_float(_arg))));
}

static MiVal mi_cffi_100(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sinf(mi_to_float(_arg))));
}

static MiVal mi_cffi_101(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tanf(mi_to_float(_arg))));
}

static MiVal mi_cffi_102(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(coshf(mi_to_float(_arg))));
}

static MiVal mi_cffi_103(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sinhf(mi_to_float(_arg))));
}

static MiVal mi_cffi_104(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tanhf(mi_to_float(_arg))));
}

static MiVal mi_cffi_105(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acoshf(mi_to_float(_arg))));
}

static MiVal mi_cffi_106(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asinhf(mi_to_float(_arg))));
}

static MiVal mi_cffi_107(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atanhf(mi_to_float(_arg))));
}

static MiVal mi_cffi_108(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(expf(mi_to_float(_arg))));
}

static MiVal mi_cffi_109(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_110_env *_ne = malloc(sizeof(struct mi_cffi_110_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_110, _ne);
}

struct mi_cffi_110_env {
  MiVal _a0;
};

static MiVal mi_cffi_110(MiVal _arg, void *_env) {
  struct mi_cffi_110_env *_e = (struct mi_cffi_110_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(frexpf(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_111(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_112_env *_ne = malloc(sizeof(struct mi_cffi_112_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_112, _ne);
}

struct mi_cffi_112_env {
  MiVal _a0;
};

static MiVal mi_cffi_112(MiVal _arg, void *_env) {
  struct mi_cffi_112_env *_e = (struct mi_cffi_112_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(ldexpf(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_113(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(logf(mi_to_float(_arg))));
}

static MiVal mi_cffi_114(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log10f(mi_to_float(_arg))));
}

static MiVal mi_cffi_115(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_116_env *_ne = malloc(sizeof(struct mi_cffi_116_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_116, _ne);
}

struct mi_cffi_116_env {
  MiVal _a0;
};

static MiVal mi_cffi_116(MiVal _arg, void *_env) {
  struct mi_cffi_116_env *_e = (struct mi_cffi_116_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(modff(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_117(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(expm1f(mi_to_float(_arg))));
}

static MiVal mi_cffi_118(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log1pf(mi_to_float(_arg))));
}

static MiVal mi_cffi_119(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(logbf(mi_to_float(_arg))));
}

static MiVal mi_cffi_120(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(exp2f(mi_to_float(_arg))));
}

static MiVal mi_cffi_121(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log2f(mi_to_float(_arg))));
}

static MiVal mi_cffi_122(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_123_env *_ne = malloc(sizeof(struct mi_cffi_123_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_123, _ne);
}

struct mi_cffi_123_env {
  MiVal _a0;
};

static MiVal mi_cffi_123(MiVal _arg, void *_env) {
  struct mi_cffi_123_env *_e = (struct mi_cffi_123_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(powf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_124(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sqrtf(mi_to_float(_arg))));
}

static MiVal mi_cffi_125(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_126_env *_ne = malloc(sizeof(struct mi_cffi_126_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_126, _ne);
}

struct mi_cffi_126_env {
  MiVal _a0;
};

static MiVal mi_cffi_126(MiVal _arg, void *_env) {
  struct mi_cffi_126_env *_e = (struct mi_cffi_126_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(hypotf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_127(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cbrtf(mi_to_float(_arg))));
}

static MiVal mi_cffi_128(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(ceilf(mi_to_float(_arg))));
}

static MiVal mi_cffi_129(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(fabsf(mi_to_float(_arg))));
}

static MiVal mi_cffi_130(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(floorf(mi_to_float(_arg))));
}

static MiVal mi_cffi_131(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_132_env *_ne = malloc(sizeof(struct mi_cffi_132_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_132, _ne);
}

struct mi_cffi_132_env {
  MiVal _a0;
};

static MiVal mi_cffi_132(MiVal _arg, void *_env) {
  struct mi_cffi_132_env *_e = (struct mi_cffi_132_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmodf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_133(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isinff(mi_to_float(_arg))));
}

static MiVal mi_cffi_134(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(finitef(mi_to_float(_arg))));
}

static MiVal mi_cffi_135(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_136_env *_ne = malloc(sizeof(struct mi_cffi_136_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_136, _ne);
}

struct mi_cffi_136_env {
  MiVal _a0;
};

static MiVal mi_cffi_136(MiVal _arg, void *_env) {
  struct mi_cffi_136_env *_e = (struct mi_cffi_136_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(dremf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_137(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(significandf(mi_to_float(_arg))));
}

static MiVal mi_cffi_138(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_139_env *_ne = malloc(sizeof(struct mi_cffi_139_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_139, _ne);
}

struct mi_cffi_139_env {
  MiVal _a0;
};

static MiVal mi_cffi_139(MiVal _arg, void *_env) {
  struct mi_cffi_139_env *_e = (struct mi_cffi_139_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(copysignf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_140(MiVal _arg, void *_env) {
  (void)_arg; (void)_env;
  return mi_float((double)(nanf()));
}

static MiVal mi_cffi_141(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isnanf(mi_to_float(_arg))));
}

static MiVal mi_cffi_142(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j0f(mi_to_float(_arg))));
}

static MiVal mi_cffi_143(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j1f(mi_to_float(_arg))));
}

static MiVal mi_cffi_144(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_145_env *_ne = malloc(sizeof(struct mi_cffi_145_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_145, _ne);
}

struct mi_cffi_145_env {
  MiVal _a0;
};

static MiVal mi_cffi_145(MiVal _arg, void *_env) {
  struct mi_cffi_145_env *_e = (struct mi_cffi_145_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(jnf(_a0.as.i, mi_to_float(_arg))));
}

static MiVal mi_cffi_146(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y0f(mi_to_float(_arg))));
}

static MiVal mi_cffi_147(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y1f(mi_to_float(_arg))));
}

static MiVal mi_cffi_148(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_149_env *_ne = malloc(sizeof(struct mi_cffi_149_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_149, _ne);
}

struct mi_cffi_149_env {
  MiVal _a0;
};

static MiVal mi_cffi_149(MiVal _arg, void *_env) {
  struct mi_cffi_149_env *_e = (struct mi_cffi_149_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(ynf(_a0.as.i, mi_to_float(_arg))));
}

static MiVal mi_cffi_150(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erff(mi_to_float(_arg))));
}

static MiVal mi_cffi_151(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erfcf(mi_to_float(_arg))));
}

static MiVal mi_cffi_152(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(lgammaf(mi_to_float(_arg))));
}

static MiVal mi_cffi_153(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tgammaf(mi_to_float(_arg))));
}

static MiVal mi_cffi_154(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(gammaf(mi_to_float(_arg))));
}

static MiVal mi_cffi_155(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_156_env *_ne = malloc(sizeof(struct mi_cffi_156_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_156, _ne);
}

struct mi_cffi_156_env {
  MiVal _a0;
};

static MiVal mi_cffi_156(MiVal _arg, void *_env) {
  struct mi_cffi_156_env *_e = (struct mi_cffi_156_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(lgammaf_r(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_157(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(rintf(mi_to_float(_arg))));
}

static MiVal mi_cffi_158(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_159_env *_ne = malloc(sizeof(struct mi_cffi_159_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_159, _ne);
}

struct mi_cffi_159_env {
  MiVal _a0;
};

static MiVal mi_cffi_159(MiVal _arg, void *_env) {
  struct mi_cffi_159_env *_e = (struct mi_cffi_159_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nextafterf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_160(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_161_env *_ne = malloc(sizeof(struct mi_cffi_161_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_161, _ne);
}

struct mi_cffi_161_env {
  MiVal _a0;
};

static MiVal mi_cffi_161(MiVal _arg, void *_env) {
  struct mi_cffi_161_env *_e = (struct mi_cffi_161_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nexttowardf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_162(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_163_env *_ne = malloc(sizeof(struct mi_cffi_163_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_163, _ne);
}

struct mi_cffi_163_env {
  MiVal _a0;
};

static MiVal mi_cffi_163(MiVal _arg, void *_env) {
  struct mi_cffi_163_env *_e = (struct mi_cffi_163_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(remainderf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_164(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_165_env *_ne = malloc(sizeof(struct mi_cffi_165_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_165, _ne);
}

struct mi_cffi_165_env {
  MiVal _a0;
};

static MiVal mi_cffi_165(MiVal _arg, void *_env) {
  struct mi_cffi_165_env *_e = (struct mi_cffi_165_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbnf(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_166(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(ilogbf(mi_to_float(_arg))));
}

static MiVal mi_cffi_167(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_168_env *_ne = malloc(sizeof(struct mi_cffi_168_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_168, _ne);
}

struct mi_cffi_168_env {
  MiVal _a0;
};

static MiVal mi_cffi_168(MiVal _arg, void *_env) {
  struct mi_cffi_168_env *_e = (struct mi_cffi_168_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalblnf(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_169(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(nearbyintf(mi_to_float(_arg))));
}

static MiVal mi_cffi_170(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(roundf(mi_to_float(_arg))));
}

static MiVal mi_cffi_171(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(truncf(mi_to_float(_arg))));
}

static MiVal mi_cffi_172(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_173_env *_ne = malloc(sizeof(struct mi_cffi_173_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_173, _ne);
}

struct mi_cffi_173_env {
  MiVal _a0;
};

static MiVal mi_cffi_173(MiVal _arg, void *_env) {
  struct mi_cffi_173_env *_e = (struct mi_cffi_173_env *)_env;
  struct mi_cffi_174_env *_ne = malloc(sizeof(struct mi_cffi_174_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_174, _ne);
}

struct mi_cffi_174_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_174(MiVal _arg, void *_env) {
  struct mi_cffi_174_env *_e = (struct mi_cffi_174_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(remquof(mi_to_float(_a0), mi_to_float(_a1), _arg.as.i)));
}

static MiVal mi_cffi_175(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lrintf(mi_to_float(_arg))));
}

static MiVal mi_cffi_176(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lroundf(mi_to_float(_arg))));
}

static MiVal mi_cffi_177(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_178_env *_ne = malloc(sizeof(struct mi_cffi_178_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_178, _ne);
}

struct mi_cffi_178_env {
  MiVal _a0;
};

static MiVal mi_cffi_178(MiVal _arg, void *_env) {
  struct mi_cffi_178_env *_e = (struct mi_cffi_178_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fdimf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_179(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_180_env *_ne = malloc(sizeof(struct mi_cffi_180_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_180, _ne);
}

struct mi_cffi_180_env {
  MiVal _a0;
};

static MiVal mi_cffi_180(MiVal _arg, void *_env) {
  struct mi_cffi_180_env *_e = (struct mi_cffi_180_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmaxf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_181(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_182_env *_ne = malloc(sizeof(struct mi_cffi_182_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_182, _ne);
}

struct mi_cffi_182_env {
  MiVal _a0;
};

static MiVal mi_cffi_182(MiVal _arg, void *_env) {
  struct mi_cffi_182_env *_e = (struct mi_cffi_182_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fminf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_183(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_184_env *_ne = malloc(sizeof(struct mi_cffi_184_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_184, _ne);
}

struct mi_cffi_184_env {
  MiVal _a0;
};

static MiVal mi_cffi_184(MiVal _arg, void *_env) {
  struct mi_cffi_184_env *_e = (struct mi_cffi_184_env *)_env;
  struct mi_cffi_185_env *_ne = malloc(sizeof(struct mi_cffi_185_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_185, _ne);
}

struct mi_cffi_185_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_185(MiVal _arg, void *_env) {
  struct mi_cffi_185_env *_e = (struct mi_cffi_185_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(fmaf(mi_to_float(_a0), mi_to_float(_a1), mi_to_float(_arg))));
}

static MiVal mi_cffi_186(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_187_env *_ne = malloc(sizeof(struct mi_cffi_187_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_187, _ne);
}

struct mi_cffi_187_env {
  MiVal _a0;
};

static MiVal mi_cffi_187(MiVal _arg, void *_env) {
  struct mi_cffi_187_env *_e = (struct mi_cffi_187_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbf(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_188(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acosl(mi_to_float(_arg))));
}

static MiVal mi_cffi_189(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asinl(mi_to_float(_arg))));
}

static MiVal mi_cffi_190(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atanl(mi_to_float(_arg))));
}

static MiVal mi_cffi_191(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_192_env *_ne = malloc(sizeof(struct mi_cffi_192_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_192, _ne);
}

struct mi_cffi_192_env {
  MiVal _a0;
};

static MiVal mi_cffi_192(MiVal _arg, void *_env) {
  struct mi_cffi_192_env *_e = (struct mi_cffi_192_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(atan2l(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_193(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cosl(mi_to_float(_arg))));
}

static MiVal mi_cffi_194(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sinl(mi_to_float(_arg))));
}

static MiVal mi_cffi_195(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tanl(mi_to_float(_arg))));
}

static MiVal mi_cffi_196(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(coshl(mi_to_float(_arg))));
}

static MiVal mi_cffi_197(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sinhl(mi_to_float(_arg))));
}

static MiVal mi_cffi_198(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tanhl(mi_to_float(_arg))));
}

static MiVal mi_cffi_199(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(acoshl(mi_to_float(_arg))));
}

static MiVal mi_cffi_200(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(asinhl(mi_to_float(_arg))));
}

static MiVal mi_cffi_201(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(atanhl(mi_to_float(_arg))));
}

static MiVal mi_cffi_202(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(expl(mi_to_float(_arg))));
}

static MiVal mi_cffi_203(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_204_env *_ne = malloc(sizeof(struct mi_cffi_204_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_204, _ne);
}

struct mi_cffi_204_env {
  MiVal _a0;
};

static MiVal mi_cffi_204(MiVal _arg, void *_env) {
  struct mi_cffi_204_env *_e = (struct mi_cffi_204_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(frexpl(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_205(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_206_env *_ne = malloc(sizeof(struct mi_cffi_206_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_206, _ne);
}

struct mi_cffi_206_env {
  MiVal _a0;
};

static MiVal mi_cffi_206(MiVal _arg, void *_env) {
  struct mi_cffi_206_env *_e = (struct mi_cffi_206_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(ldexpl(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_207(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(logl(mi_to_float(_arg))));
}

static MiVal mi_cffi_208(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log10l(mi_to_float(_arg))));
}

static MiVal mi_cffi_209(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_210_env *_ne = malloc(sizeof(struct mi_cffi_210_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_210, _ne);
}

struct mi_cffi_210_env {
  MiVal _a0;
};

static MiVal mi_cffi_210(MiVal _arg, void *_env) {
  struct mi_cffi_210_env *_e = (struct mi_cffi_210_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(modfl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_211(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(expm1l(mi_to_float(_arg))));
}

static MiVal mi_cffi_212(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log1pl(mi_to_float(_arg))));
}

static MiVal mi_cffi_213(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(logbl(mi_to_float(_arg))));
}

static MiVal mi_cffi_214(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(exp2l(mi_to_float(_arg))));
}

static MiVal mi_cffi_215(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(log2l(mi_to_float(_arg))));
}

static MiVal mi_cffi_216(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_217_env *_ne = malloc(sizeof(struct mi_cffi_217_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_217, _ne);
}

struct mi_cffi_217_env {
  MiVal _a0;
};

static MiVal mi_cffi_217(MiVal _arg, void *_env) {
  struct mi_cffi_217_env *_e = (struct mi_cffi_217_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(powl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_218(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sqrtl(mi_to_float(_arg))));
}

static MiVal mi_cffi_219(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_220_env *_ne = malloc(sizeof(struct mi_cffi_220_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_220, _ne);
}

struct mi_cffi_220_env {
  MiVal _a0;
};

static MiVal mi_cffi_220(MiVal _arg, void *_env) {
  struct mi_cffi_220_env *_e = (struct mi_cffi_220_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(hypotl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_221(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(cbrtl(mi_to_float(_arg))));
}

static MiVal mi_cffi_222(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(ceill(mi_to_float(_arg))));
}

static MiVal mi_cffi_223(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(fabsl(mi_to_float(_arg))));
}

static MiVal mi_cffi_224(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(floorl(mi_to_float(_arg))));
}

static MiVal mi_cffi_225(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_226_env *_ne = malloc(sizeof(struct mi_cffi_226_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_226, _ne);
}

struct mi_cffi_226_env {
  MiVal _a0;
};

static MiVal mi_cffi_226(MiVal _arg, void *_env) {
  struct mi_cffi_226_env *_e = (struct mi_cffi_226_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmodl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_227(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isinfl(mi_to_float(_arg))));
}

static MiVal mi_cffi_228(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(finitel(mi_to_float(_arg))));
}

static MiVal mi_cffi_229(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_230_env *_ne = malloc(sizeof(struct mi_cffi_230_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_230, _ne);
}

struct mi_cffi_230_env {
  MiVal _a0;
};

static MiVal mi_cffi_230(MiVal _arg, void *_env) {
  struct mi_cffi_230_env *_e = (struct mi_cffi_230_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(dreml(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_231(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(significandl(mi_to_float(_arg))));
}

static MiVal mi_cffi_232(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_233_env *_ne = malloc(sizeof(struct mi_cffi_233_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_233, _ne);
}

struct mi_cffi_233_env {
  MiVal _a0;
};

static MiVal mi_cffi_233(MiVal _arg, void *_env) {
  struct mi_cffi_233_env *_e = (struct mi_cffi_233_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(copysignl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_234(MiVal _arg, void *_env) {
  (void)_arg; (void)_env;
  return mi_float((double)(nanl()));
}

static MiVal mi_cffi_235(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(isnanl(mi_to_float(_arg))));
}

static MiVal mi_cffi_236(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j0l(_arg.as.i)));
}

static MiVal mi_cffi_237(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(j1l(_arg.as.i)));
}

static MiVal mi_cffi_238(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_239_env *_ne = malloc(sizeof(struct mi_cffi_239_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_239, _ne);
}

struct mi_cffi_239_env {
  MiVal _a0;
};

static MiVal mi_cffi_239(MiVal _arg, void *_env) {
  struct mi_cffi_239_env *_e = (struct mi_cffi_239_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(jnl(_a0.as.i, _arg.as.i)));
}

static MiVal mi_cffi_240(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y0l(_arg.as.i)));
}

static MiVal mi_cffi_241(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(y1l(_arg.as.i)));
}

static MiVal mi_cffi_242(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_243_env *_ne = malloc(sizeof(struct mi_cffi_243_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_243, _ne);
}

struct mi_cffi_243_env {
  MiVal _a0;
};

static MiVal mi_cffi_243(MiVal _arg, void *_env) {
  struct mi_cffi_243_env *_e = (struct mi_cffi_243_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(ynl(_a0.as.i, _arg.as.i)));
}

static MiVal mi_cffi_244(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erfl(_arg.as.i)));
}

static MiVal mi_cffi_245(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(erfcl(_arg.as.i)));
}

static MiVal mi_cffi_246(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(lgammal(_arg.as.i)));
}

static MiVal mi_cffi_247(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(tgammal(_arg.as.i)));
}

static MiVal mi_cffi_248(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(gammal(_arg.as.i)));
}

static MiVal mi_cffi_249(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_250_env *_ne = malloc(sizeof(struct mi_cffi_250_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_250, _ne);
}

struct mi_cffi_250_env {
  MiVal _a0;
};

static MiVal mi_cffi_250(MiVal _arg, void *_env) {
  struct mi_cffi_250_env *_e = (struct mi_cffi_250_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(lgammal_r(_a0.as.i, _arg.as.i)));
}

static MiVal mi_cffi_251(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(rintl(mi_to_float(_arg))));
}

static MiVal mi_cffi_252(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_253_env *_ne = malloc(sizeof(struct mi_cffi_253_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_253, _ne);
}

struct mi_cffi_253_env {
  MiVal _a0;
};

static MiVal mi_cffi_253(MiVal _arg, void *_env) {
  struct mi_cffi_253_env *_e = (struct mi_cffi_253_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nextafterl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_254(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_255_env *_ne = malloc(sizeof(struct mi_cffi_255_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_255, _ne);
}

struct mi_cffi_255_env {
  MiVal _a0;
};

static MiVal mi_cffi_255(MiVal _arg, void *_env) {
  struct mi_cffi_255_env *_e = (struct mi_cffi_255_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(nexttowardl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_256(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_257_env *_ne = malloc(sizeof(struct mi_cffi_257_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_257, _ne);
}

struct mi_cffi_257_env {
  MiVal _a0;
};

static MiVal mi_cffi_257(MiVal _arg, void *_env) {
  struct mi_cffi_257_env *_e = (struct mi_cffi_257_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(remainderl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_258(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_259_env *_ne = malloc(sizeof(struct mi_cffi_259_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_259, _ne);
}

struct mi_cffi_259_env {
  MiVal _a0;
};

static MiVal mi_cffi_259(MiVal _arg, void *_env) {
  struct mi_cffi_259_env *_e = (struct mi_cffi_259_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbnl(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_260(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(ilogbl(mi_to_float(_arg))));
}

static MiVal mi_cffi_261(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_262_env *_ne = malloc(sizeof(struct mi_cffi_262_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_262, _ne);
}

struct mi_cffi_262_env {
  MiVal _a0;
};

static MiVal mi_cffi_262(MiVal _arg, void *_env) {
  struct mi_cffi_262_env *_e = (struct mi_cffi_262_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalblnl(mi_to_float(_a0), _arg.as.i)));
}

static MiVal mi_cffi_263(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(nearbyintl(mi_to_float(_arg))));
}

static MiVal mi_cffi_264(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(roundl(mi_to_float(_arg))));
}

static MiVal mi_cffi_265(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(truncl(mi_to_float(_arg))));
}

static MiVal mi_cffi_266(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_267_env *_ne = malloc(sizeof(struct mi_cffi_267_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_267, _ne);
}

struct mi_cffi_267_env {
  MiVal _a0;
};

static MiVal mi_cffi_267(MiVal _arg, void *_env) {
  struct mi_cffi_267_env *_e = (struct mi_cffi_267_env *)_env;
  struct mi_cffi_268_env *_ne = malloc(sizeof(struct mi_cffi_268_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_268, _ne);
}

struct mi_cffi_268_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_268(MiVal _arg, void *_env) {
  struct mi_cffi_268_env *_e = (struct mi_cffi_268_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(remquol(mi_to_float(_a0), mi_to_float(_a1), _arg.as.i)));
}

static MiVal mi_cffi_269(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lrintl(mi_to_float(_arg))));
}

static MiVal mi_cffi_270(MiVal _arg, void *_env) {
  (void)_env;
  return mi_int((int64_t)(lroundl(mi_to_float(_arg))));
}

static MiVal mi_cffi_271(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_272_env *_ne = malloc(sizeof(struct mi_cffi_272_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_272, _ne);
}

struct mi_cffi_272_env {
  MiVal _a0;
};

static MiVal mi_cffi_272(MiVal _arg, void *_env) {
  struct mi_cffi_272_env *_e = (struct mi_cffi_272_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fdiml(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_273(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_274_env *_ne = malloc(sizeof(struct mi_cffi_274_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_274, _ne);
}

struct mi_cffi_274_env {
  MiVal _a0;
};

static MiVal mi_cffi_274(MiVal _arg, void *_env) {
  struct mi_cffi_274_env *_e = (struct mi_cffi_274_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fmaxl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_275(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_276_env *_ne = malloc(sizeof(struct mi_cffi_276_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_276, _ne);
}

struct mi_cffi_276_env {
  MiVal _a0;
};

static MiVal mi_cffi_276(MiVal _arg, void *_env) {
  struct mi_cffi_276_env *_e = (struct mi_cffi_276_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(fminl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_277(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_278_env *_ne = malloc(sizeof(struct mi_cffi_278_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_278, _ne);
}

struct mi_cffi_278_env {
  MiVal _a0;
};

static MiVal mi_cffi_278(MiVal _arg, void *_env) {
  struct mi_cffi_278_env *_e = (struct mi_cffi_278_env *)_env;
  struct mi_cffi_279_env *_ne = malloc(sizeof(struct mi_cffi_279_env));
  _ne->_a0 = _e->_a0;
  _ne->_a1 = _arg;
  return mi_closure(mi_cffi_279, _ne);
}

struct mi_cffi_279_env {
  MiVal _a0;
  MiVal _a1;
};

static MiVal mi_cffi_279(MiVal _arg, void *_env) {
  struct mi_cffi_279_env *_e = (struct mi_cffi_279_env *)_env;
  MiVal _a0 = _e->_a0;
  MiVal _a1 = _e->_a1;
  return mi_float((double)(fmal(mi_to_float(_a0), mi_to_float(_a1), mi_to_float(_arg))));
}

static MiVal mi_cffi_280(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_281_env *_ne = malloc(sizeof(struct mi_cffi_281_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_281, _ne);
}

struct mi_cffi_281_env {
  MiVal _a0;
};

static MiVal mi_cffi_281(MiVal _arg, void *_env) {
  struct mi_cffi_281_env *_e = (struct mi_cffi_281_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(scalbl(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_282(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sin(mi_to_float(_arg))));
}

static MiVal mi_cffi_283(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sin(mi_to_float(_arg))));
}

static MiVal mi_cffi_284(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sqrt(mi_to_float(_arg))));
}

static MiVal mi_cffi_285(MiVal _arg, void *_env) {
  (void)_env;
  return mi_float((double)(sqrt(mi_to_float(_arg))));
}

static MiVal mi_cffi_286(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_287_env *_ne = malloc(sizeof(struct mi_cffi_287_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_287, _ne);
}

struct mi_cffi_287_env {
  MiVal _a0;
};

static MiVal mi_cffi_287(MiVal _arg, void *_env) {
  struct mi_cffi_287_env *_e = (struct mi_cffi_287_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(pow(mi_to_float(_a0), mi_to_float(_arg))));
}

static MiVal mi_cffi_288(MiVal _arg, void *_env) {
  (void)_env;
  struct mi_cffi_289_env *_ne = malloc(sizeof(struct mi_cffi_289_env));
  _ne->_a0 = _arg;
  return mi_closure(mi_cffi_289, _ne);
}

struct mi_cffi_289_env {
  MiVal _a0;
};

static MiVal mi_cffi_289(MiVal _arg, void *_env) {
  struct mi_cffi_289_env *_e = (struct mi_cffi_289_env *)_env;
  MiVal _a0 = _e->_a0;
  return mi_float((double)(pow(mi_to_float(_a0), mi_to_float(_arg))));
}


int main(void) {
  MiVal mi_print = mi_closure(mi_builtin_print, NULL);
  MiVal mi_println = mi_closure(mi_builtin_println, NULL);

  MiVal M = ({
    MiVal acos = mi_closure(mi_cffi_0, NULL);
    MiVal asin = mi_closure(mi_cffi_1, NULL);
    MiVal atan = mi_closure(mi_cffi_2, NULL);
    MiVal atan2 = mi_closure(mi_cffi_3, NULL);
    MiVal cos = mi_closure(mi_cffi_5, NULL);
    MiVal sin = mi_closure(mi_cffi_6, NULL);
    MiVal tan = mi_closure(mi_cffi_7, NULL);
    MiVal cosh = mi_closure(mi_cffi_8, NULL);
    MiVal sinh = mi_closure(mi_cffi_9, NULL);
    MiVal tanh = mi_closure(mi_cffi_10, NULL);
    MiVal acosh = mi_closure(mi_cffi_11, NULL);
    MiVal asinh = mi_closure(mi_cffi_12, NULL);
    MiVal atanh = mi_closure(mi_cffi_13, NULL);
    MiVal exp = mi_closure(mi_cffi_14, NULL);
    MiVal frexp = mi_closure(mi_cffi_15, NULL);
    MiVal ldexp = mi_closure(mi_cffi_17, NULL);
    MiVal log = mi_closure(mi_cffi_19, NULL);
    MiVal log10 = mi_closure(mi_cffi_20, NULL);
    MiVal modf = mi_closure(mi_cffi_21, NULL);
    MiVal expm1 = mi_closure(mi_cffi_23, NULL);
    MiVal log1p = mi_closure(mi_cffi_24, NULL);
    MiVal logb = mi_closure(mi_cffi_25, NULL);
    MiVal exp2 = mi_closure(mi_cffi_26, NULL);
    MiVal log2 = mi_closure(mi_cffi_27, NULL);
    MiVal pow = mi_closure(mi_cffi_28, NULL);
    MiVal sqrt = mi_closure(mi_cffi_30, NULL);
    MiVal hypot = mi_closure(mi_cffi_31, NULL);
    MiVal cbrt = mi_closure(mi_cffi_33, NULL);
    MiVal ceil = mi_closure(mi_cffi_34, NULL);
    MiVal fabs = mi_closure(mi_cffi_35, NULL);
    MiVal floor = mi_closure(mi_cffi_36, NULL);
    MiVal fmod = mi_closure(mi_cffi_37, NULL);
    MiVal isinf = mi_closure(mi_cffi_39, NULL);
    MiVal finite = mi_closure(mi_cffi_40, NULL);
    MiVal drem = mi_closure(mi_cffi_41, NULL);
    MiVal significand = mi_closure(mi_cffi_43, NULL);
    MiVal copysign = mi_closure(mi_cffi_44, NULL);
    MiVal nan = mi_closure(mi_cffi_46, NULL);
    MiVal isnan = mi_closure(mi_cffi_47, NULL);
    MiVal j0 = mi_closure(mi_cffi_48, NULL);
    MiVal j1 = mi_closure(mi_cffi_49, NULL);
    MiVal jn = mi_closure(mi_cffi_50, NULL);
    MiVal y0 = mi_closure(mi_cffi_52, NULL);
    MiVal y1 = mi_closure(mi_cffi_53, NULL);
    MiVal yn = mi_closure(mi_cffi_54, NULL);
    MiVal erf = mi_closure(mi_cffi_56, NULL);
    MiVal erfc = mi_closure(mi_cffi_57, NULL);
    MiVal lgamma = mi_closure(mi_cffi_58, NULL);
    MiVal tgamma = mi_closure(mi_cffi_59, NULL);
    MiVal gamma = mi_closure(mi_cffi_60, NULL);
    MiVal lgamma_r = mi_closure(mi_cffi_61, NULL);
    MiVal rint = mi_closure(mi_cffi_63, NULL);
    MiVal nextafter = mi_closure(mi_cffi_64, NULL);
    MiVal nexttoward = mi_closure(mi_cffi_66, NULL);
    MiVal remainder = mi_closure(mi_cffi_68, NULL);
    MiVal scalbn = mi_closure(mi_cffi_70, NULL);
    MiVal ilogb = mi_closure(mi_cffi_72, NULL);
    MiVal scalbln = mi_closure(mi_cffi_73, NULL);
    MiVal nearbyint = mi_closure(mi_cffi_75, NULL);
    MiVal round = mi_closure(mi_cffi_76, NULL);
    MiVal trunc = mi_closure(mi_cffi_77, NULL);
    MiVal remquo = mi_closure(mi_cffi_78, NULL);
    MiVal lrint = mi_closure(mi_cffi_81, NULL);
    MiVal lround = mi_closure(mi_cffi_82, NULL);
    MiVal fdim = mi_closure(mi_cffi_83, NULL);
    MiVal fmax = mi_closure(mi_cffi_85, NULL);
    MiVal fmin = mi_closure(mi_cffi_87, NULL);
    MiVal fma = mi_closure(mi_cffi_89, NULL);
    MiVal scalb = mi_closure(mi_cffi_92, NULL);
    MiVal acosf = mi_closure(mi_cffi_94, NULL);
    MiVal asinf = mi_closure(mi_cffi_95, NULL);
    MiVal atanf = mi_closure(mi_cffi_96, NULL);
    MiVal atan2f = mi_closure(mi_cffi_97, NULL);
    MiVal cosf = mi_closure(mi_cffi_99, NULL);
    MiVal sinf = mi_closure(mi_cffi_100, NULL);
    MiVal tanf = mi_closure(mi_cffi_101, NULL);
    MiVal coshf = mi_closure(mi_cffi_102, NULL);
    MiVal sinhf = mi_closure(mi_cffi_103, NULL);
    MiVal tanhf = mi_closure(mi_cffi_104, NULL);
    MiVal acoshf = mi_closure(mi_cffi_105, NULL);
    MiVal asinhf = mi_closure(mi_cffi_106, NULL);
    MiVal atanhf = mi_closure(mi_cffi_107, NULL);
    MiVal expf = mi_closure(mi_cffi_108, NULL);
    MiVal frexpf = mi_closure(mi_cffi_109, NULL);
    MiVal ldexpf = mi_closure(mi_cffi_111, NULL);
    MiVal logf = mi_closure(mi_cffi_113, NULL);
    MiVal log10f = mi_closure(mi_cffi_114, NULL);
    MiVal modff = mi_closure(mi_cffi_115, NULL);
    MiVal expm1f = mi_closure(mi_cffi_117, NULL);
    MiVal log1pf = mi_closure(mi_cffi_118, NULL);
    MiVal logbf = mi_closure(mi_cffi_119, NULL);
    MiVal exp2f = mi_closure(mi_cffi_120, NULL);
    MiVal log2f = mi_closure(mi_cffi_121, NULL);
    MiVal powf = mi_closure(mi_cffi_122, NULL);
    MiVal sqrtf = mi_closure(mi_cffi_124, NULL);
    MiVal hypotf = mi_closure(mi_cffi_125, NULL);
    MiVal cbrtf = mi_closure(mi_cffi_127, NULL);
    MiVal ceilf = mi_closure(mi_cffi_128, NULL);
    MiVal fabsf = mi_closure(mi_cffi_129, NULL);
    MiVal floorf = mi_closure(mi_cffi_130, NULL);
    MiVal fmodf = mi_closure(mi_cffi_131, NULL);
    MiVal isinff = mi_closure(mi_cffi_133, NULL);
    MiVal finitef = mi_closure(mi_cffi_134, NULL);
    MiVal dremf = mi_closure(mi_cffi_135, NULL);
    MiVal significandf = mi_closure(mi_cffi_137, NULL);
    MiVal copysignf = mi_closure(mi_cffi_138, NULL);
    MiVal nanf = mi_closure(mi_cffi_140, NULL);
    MiVal isnanf = mi_closure(mi_cffi_141, NULL);
    MiVal j0f = mi_closure(mi_cffi_142, NULL);
    MiVal j1f = mi_closure(mi_cffi_143, NULL);
    MiVal jnf = mi_closure(mi_cffi_144, NULL);
    MiVal y0f = mi_closure(mi_cffi_146, NULL);
    MiVal y1f = mi_closure(mi_cffi_147, NULL);
    MiVal ynf = mi_closure(mi_cffi_148, NULL);
    MiVal erff = mi_closure(mi_cffi_150, NULL);
    MiVal erfcf = mi_closure(mi_cffi_151, NULL);
    MiVal lgammaf = mi_closure(mi_cffi_152, NULL);
    MiVal tgammaf = mi_closure(mi_cffi_153, NULL);
    MiVal gammaf = mi_closure(mi_cffi_154, NULL);
    MiVal lgammaf_r = mi_closure(mi_cffi_155, NULL);
    MiVal rintf = mi_closure(mi_cffi_157, NULL);
    MiVal nextafterf = mi_closure(mi_cffi_158, NULL);
    MiVal nexttowardf = mi_closure(mi_cffi_160, NULL);
    MiVal remainderf = mi_closure(mi_cffi_162, NULL);
    MiVal scalbnf = mi_closure(mi_cffi_164, NULL);
    MiVal ilogbf = mi_closure(mi_cffi_166, NULL);
    MiVal scalblnf = mi_closure(mi_cffi_167, NULL);
    MiVal nearbyintf = mi_closure(mi_cffi_169, NULL);
    MiVal roundf = mi_closure(mi_cffi_170, NULL);
    MiVal truncf = mi_closure(mi_cffi_171, NULL);
    MiVal remquof = mi_closure(mi_cffi_172, NULL);
    MiVal lrintf = mi_closure(mi_cffi_175, NULL);
    MiVal lroundf = mi_closure(mi_cffi_176, NULL);
    MiVal fdimf = mi_closure(mi_cffi_177, NULL);
    MiVal fmaxf = mi_closure(mi_cffi_179, NULL);
    MiVal fminf = mi_closure(mi_cffi_181, NULL);
    MiVal fmaf = mi_closure(mi_cffi_183, NULL);
    MiVal scalbf = mi_closure(mi_cffi_186, NULL);
    MiVal acosl = mi_closure(mi_cffi_188, NULL);
    MiVal asinl = mi_closure(mi_cffi_189, NULL);
    MiVal atanl = mi_closure(mi_cffi_190, NULL);
    MiVal atan2l = mi_closure(mi_cffi_191, NULL);
    MiVal cosl = mi_closure(mi_cffi_193, NULL);
    MiVal sinl = mi_closure(mi_cffi_194, NULL);
    MiVal tanl = mi_closure(mi_cffi_195, NULL);
    MiVal coshl = mi_closure(mi_cffi_196, NULL);
    MiVal sinhl = mi_closure(mi_cffi_197, NULL);
    MiVal tanhl = mi_closure(mi_cffi_198, NULL);
    MiVal acoshl = mi_closure(mi_cffi_199, NULL);
    MiVal asinhl = mi_closure(mi_cffi_200, NULL);
    MiVal atanhl = mi_closure(mi_cffi_201, NULL);
    MiVal expl = mi_closure(mi_cffi_202, NULL);
    MiVal frexpl = mi_closure(mi_cffi_203, NULL);
    MiVal ldexpl = mi_closure(mi_cffi_205, NULL);
    MiVal logl = mi_closure(mi_cffi_207, NULL);
    MiVal log10l = mi_closure(mi_cffi_208, NULL);
    MiVal modfl = mi_closure(mi_cffi_209, NULL);
    MiVal expm1l = mi_closure(mi_cffi_211, NULL);
    MiVal log1pl = mi_closure(mi_cffi_212, NULL);
    MiVal logbl = mi_closure(mi_cffi_213, NULL);
    MiVal exp2l = mi_closure(mi_cffi_214, NULL);
    MiVal log2l = mi_closure(mi_cffi_215, NULL);
    MiVal powl = mi_closure(mi_cffi_216, NULL);
    MiVal sqrtl = mi_closure(mi_cffi_218, NULL);
    MiVal hypotl = mi_closure(mi_cffi_219, NULL);
    MiVal cbrtl = mi_closure(mi_cffi_221, NULL);
    MiVal ceill = mi_closure(mi_cffi_222, NULL);
    MiVal fabsl = mi_closure(mi_cffi_223, NULL);
    MiVal floorl = mi_closure(mi_cffi_224, NULL);
    MiVal fmodl = mi_closure(mi_cffi_225, NULL);
    MiVal isinfl = mi_closure(mi_cffi_227, NULL);
    MiVal finitel = mi_closure(mi_cffi_228, NULL);
    MiVal dreml = mi_closure(mi_cffi_229, NULL);
    MiVal significandl = mi_closure(mi_cffi_231, NULL);
    MiVal copysignl = mi_closure(mi_cffi_232, NULL);
    MiVal nanl = mi_closure(mi_cffi_234, NULL);
    MiVal isnanl = mi_closure(mi_cffi_235, NULL);
    MiVal j0l = mi_closure(mi_cffi_236, NULL);
    MiVal j1l = mi_closure(mi_cffi_237, NULL);
    MiVal jnl = mi_closure(mi_cffi_238, NULL);
    MiVal y0l = mi_closure(mi_cffi_240, NULL);
    MiVal y1l = mi_closure(mi_cffi_241, NULL);
    MiVal ynl = mi_closure(mi_cffi_242, NULL);
    MiVal erfl = mi_closure(mi_cffi_244, NULL);
    MiVal erfcl = mi_closure(mi_cffi_245, NULL);
    MiVal lgammal = mi_closure(mi_cffi_246, NULL);
    MiVal tgammal = mi_closure(mi_cffi_247, NULL);
    MiVal gammal = mi_closure(mi_cffi_248, NULL);
    MiVal lgammal_r = mi_closure(mi_cffi_249, NULL);
    MiVal rintl = mi_closure(mi_cffi_251, NULL);
    MiVal nextafterl = mi_closure(mi_cffi_252, NULL);
    MiVal nexttowardl = mi_closure(mi_cffi_254, NULL);
    MiVal remainderl = mi_closure(mi_cffi_256, NULL);
    MiVal scalbnl = mi_closure(mi_cffi_258, NULL);
    MiVal ilogbl = mi_closure(mi_cffi_260, NULL);
    MiVal scalblnl = mi_closure(mi_cffi_261, NULL);
    MiVal nearbyintl = mi_closure(mi_cffi_263, NULL);
    MiVal roundl = mi_closure(mi_cffi_264, NULL);
    MiVal truncl = mi_closure(mi_cffi_265, NULL);
    MiVal remquol = mi_closure(mi_cffi_266, NULL);
    MiVal lrintl = mi_closure(mi_cffi_269, NULL);
    MiVal lroundl = mi_closure(mi_cffi_270, NULL);
    MiVal fdiml = mi_closure(mi_cffi_271, NULL);
    MiVal fmaxl = mi_closure(mi_cffi_273, NULL);
    MiVal fminl = mi_closure(mi_cffi_275, NULL);
    MiVal fmal = mi_closure(mi_cffi_277, NULL);
    MiVal scalbl = mi_closure(mi_cffi_280, NULL);
    scalbl;
  });
  MiVal a = mi_apply(mi_closure(mi_cffi_282, NULL), mi_float(1.0));
  MiVal main1 = mi_apply(mi_println, mi_apply(mi_closure(mi_cffi_283, NULL), mi_float(1.0)));
  MiVal b = mi_apply(mi_closure(mi_cffi_284, NULL), mi_float(144.0));
  MiVal main2 = mi_apply(mi_println, mi_apply(mi_closure(mi_cffi_285, NULL), mi_float(144.0)));
  MiVal c = mi_apply(mi_apply(mi_closure(mi_cffi_286, NULL), mi_float(2.0)), mi_float(10.0));
  MiVal main3 = mi_apply(mi_println, mi_apply(mi_apply(mi_closure(mi_cffi_288, NULL), mi_float(2.0)), mi_float(10.0)));

  printf("M = "); mi_print_val(M); printf("\n");
  printf("a = "); mi_print_val(a); printf("\n");
  printf("main1 = "); mi_print_val(main1); printf("\n");
  printf("b = "); mi_print_val(b); printf("\n");
  printf("main2 = "); mi_print_val(main2); printf("\n");
  printf("c = "); mi_print_val(c); printf("\n");
  printf("main3 = "); mi_print_val(main3); printf("\n");
  return 0;
}
