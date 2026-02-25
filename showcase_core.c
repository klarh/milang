#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <ctype.h>

// ── Arena allocator ──
#define MI_ARENA_BLOCK_SIZE (1024 * 1024)
typedef struct MiArenaBlock { char *data; size_t used; size_t cap; struct MiArenaBlock *next; } MiArenaBlock;
typedef struct { MiArenaBlock *head; } MiArena;
static MiArena mi_arena = {0};

static void *mi_alloc(size_t size) {
  size = (size + 7) & ~7;
  MiArenaBlock *b = mi_arena.head;
  if (!b || b->used + size > b->cap) {
    size_t cap = MI_ARENA_BLOCK_SIZE;
    if (size > cap) cap = size;
    b = (MiArenaBlock*)malloc(sizeof(MiArenaBlock) + cap);
    b->data = (char*)(b + 1); b->used = 0; b->cap = cap;
    b->next = mi_arena.head; mi_arena.head = b;
  }
  void *p = b->data + b->used; b->used += size;
  memset(p, 0, size);
  return p;
}

static char *mi_strdup(const char *s) {
  size_t n = strlen(s) + 1;
  char *p = (char*)mi_alloc(n);
  memcpy(p, s, n);
  return p;
}

// ── Forward declarations ──
typedef struct MiVal MiVal;
typedef struct MiExpr MiExpr;
typedef struct MiEnv MiEnv;
typedef struct MiBinding MiBinding;
typedef struct MiAlt MiAlt;
typedef struct MiPat MiPat;

// ── MiVal: runtime values ──
typedef enum { MI_INT, MI_FLOAT, MI_STRING, MI_RECORD, MI_CLOSURE, MI_NATIVE } MiType;

struct MiVal {
  MiType type;
  union {
    int64_t i;
    double f;
    struct { char *data; int len; } str;
    struct { const char *tag; const char **names; MiVal *fields; int nfields; } rec;
    struct { MiExpr *body; const char *param; MiEnv *env; } closure;
    struct { MiVal (*fn)(MiVal, void*); void *env; } native;
  } as;
};

// ── MiExpr: expression tree ──
typedef enum {
  EXPR_INT, EXPR_FLOAT, EXPR_STRING, EXPR_NAME, EXPR_BINOP, EXPR_APP,
  EXPR_LAM, EXPR_WITH, EXPR_RECORD, EXPR_FIELD, EXPR_NAMESPACE,
  EXPR_CASE, EXPR_THUNK, EXPR_VAL
} ExprType;

struct MiExpr {
  ExprType type;
  const char *loc;
  union {
    int64_t i;
    double f;
    char *s;
    char *name;
    struct { char *op; MiExpr *left; MiExpr *right; } binop;
    struct { MiExpr *fn; MiExpr *arg; } app;
    struct { char *param; MiExpr *body; } lam;
    struct { MiExpr *body; int nbindings; MiBinding *bindings; } with;
    struct { char *tag; int nbindings; MiBinding *bindings; } record;
    struct { MiExpr *expr; char *field; } field;
    struct { int nbindings; MiBinding *bindings; } ns;
    struct { MiExpr *scrut; int nalts; MiAlt *alts; } cas;
    struct { MiExpr *body; } thunk;
    MiVal val;
  } as;
};

struct MiBinding {
  char *name;
  int lazy;
  int nparams;
  char **params;
  MiExpr *body;
};

// ── MiPat ──
typedef enum { PAT_VAR, PAT_WILD, PAT_INT, PAT_STRING, PAT_REC } PatType;

struct MiPat {
  PatType type;
  union {
    char *var;
    int64_t i;
    char *s;
    struct { char *tag; int nfields; char **field_names; MiPat **field_pats; } rec;
  } as;
};

struct MiAlt {
  MiPat *pat;
  MiExpr *guard;
  MiExpr *body;
};

// ── MiEnv ──
struct MiEnv {
  char *name;
  MiVal val;
  MiEnv *next;
  MiEnv *parent;
};

// ── Value constructors ──
static MiVal mi_int(int64_t v) { MiVal r; r.type = MI_INT; r.as.i = v; return r; }
static MiVal mi_float(double v) { MiVal r; r.type = MI_FLOAT; r.as.f = v; return r; }
static MiVal mi_string(const char *s) { MiVal r; r.type = MI_STRING; int n = strlen(s); r.as.str.data = mi_alloc(n+1); memcpy(r.as.str.data, s, n+1); r.as.str.len = n; return r; }
static MiVal mi_stringn(const char *s, int n) { MiVal r; r.type = MI_STRING; r.as.str.data = mi_alloc(n+1); memcpy(r.as.str.data, s, n); r.as.str.data[n] = '\0'; r.as.str.len = n; return r; }
static MiVal mi_nil(void) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Nil"; r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL; return r;
}
static MiVal mi_cons(MiVal head, MiVal tail) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Cons"; r.as.rec.nfields = 2;
  r.as.rec.names = mi_alloc(2 * sizeof(const char*)); r.as.rec.names[0] = "head"; r.as.rec.names[1] = "tail";
  r.as.rec.fields = mi_alloc(2 * sizeof(MiVal)); r.as.rec.fields[0] = head; r.as.rec.fields[1] = tail;
  return r;
}
static MiVal mi_nothing(void) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Nothing"; r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL; return r;
}
static MiVal mi_just(MiVal val) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Just"; r.as.rec.nfields = 1;
  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = "val";
  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = val;
  return r;
}
static MiVal mi_native(MiVal (*fn)(MiVal, void*)) {
  MiVal r; r.type = MI_NATIVE; r.as.native.fn = fn; r.as.native.env = NULL; return r;
}
static MiVal mi_native_env(MiVal (*fn)(MiVal, void*), void *env) {
  MiVal r; r.type = MI_NATIVE; r.as.native.fn = fn; r.as.native.env = env; return r;
}

// ── Error helpers ──
static void mi_error(const char *loc, const char *msg) {
  if (loc) fprintf(stderr, "%s: %s\n", loc, msg);
  else fprintf(stderr, "%s\n", msg);
  exit(1);
}
static void mi_errorf(const char *loc, const char *fmt, ...) {
  if (loc) fprintf(stderr, "%s: ", loc);
  va_list args; va_start(args, fmt);
  vfprintf(stderr, fmt, args); va_end(args);
  fprintf(stderr, "\n"); exit(1);
}

// ── Env operations ──
static MiEnv *mi_env_new(MiEnv *parent) {
  MiEnv *e = mi_alloc(sizeof(MiEnv)); e->parent = parent; return e;
}

static void mi_env_set(MiEnv *env, const char *name, MiVal val) {
  MiEnv *entry = mi_alloc(sizeof(MiEnv));
  entry->name = mi_strdup(name); entry->val = val;
  entry->next = env->next; entry->parent = NULL;
  env->next = entry;
}

static MiVal mi_env_get_loc(MiEnv *env, const char *name, const char *loc) {
  MiEnv *frame = env;
  while (frame) {
    for (MiEnv *e = frame; e; e = e->next)
      if (e->name && strcmp(e->name, name) == 0) return e->val;
    frame = frame->parent;
  }
  if (name[0] >= 'A' && name[0] <= 'Z') {
    MiVal r; r.type = MI_RECORD; r.as.rec.tag = name;
    r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL;
    return r;
  }
  mi_errorf(loc, "unbound variable: %s", name);
  exit(1);
}
static MiVal mi_env_get(MiEnv *env, const char *name) { return mi_env_get_loc(env, name, NULL); }

// ── Expr constructors ──
static MiExpr *mi_expr_int(int64_t v) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_INT; e->as.i = v; return e;
}
static MiExpr *mi_expr_float(double v) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FLOAT; e->as.f = v; return e;
}
static MiExpr *mi_expr_string(const char *s) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_STRING; e->as.s = mi_strdup(s); return e;
}
static MiExpr *mi_expr_name(const char *n) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_NAME; e->as.name = mi_strdup(n); return e;
}
static MiExpr *mi_expr_binop(const char *op, MiExpr *l, MiExpr *r) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_BINOP;
  e->as.binop.op = mi_strdup(op); e->as.binop.left = l; e->as.binop.right = r; return e;
}
static MiExpr *mi_expr_app(MiExpr *fn, MiExpr *arg) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_APP;
  e->as.app.fn = fn; e->as.app.arg = arg; return e;
}
static MiExpr *mi_expr_lam(const char *param, MiExpr *body) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_LAM;
  e->as.lam.param = mi_strdup(param); e->as.lam.body = body; return e;
}
static MiExpr *mi_expr_val(MiVal v) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_VAL; e->as.val = v; return e;
}
static MiExpr *mi_expr_field(MiExpr *expr, const char *field) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FIELD;
  e->as.field.expr = expr; e->as.field.field = mi_strdup(field); return e;
}
static MiExpr *mi_expr_thunk(MiExpr *body) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_THUNK; e->as.thunk.body = body; return e;
}

static MiBinding mi_binding(const char *name, int lazy, int nparams, ...) {
  MiBinding b; b.name = mi_strdup(name); b.lazy = lazy; b.nparams = nparams;
  va_list args; va_start(args, nparams);
  if (nparams > 0) {
    b.params = mi_alloc(nparams * sizeof(char*));
    for (int i = 0; i < nparams; i++) b.params[i] = mi_strdup(va_arg(args, char*));
  } else { b.params = NULL; }
  b.body = va_arg(args, MiExpr*); va_end(args); return b;
}

static MiExpr *mi_expr_with(MiExpr *body, int n, ...) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_WITH;
  e->as.with.body = body; e->as.with.nbindings = n;
  e->as.with.bindings = mi_alloc(n * sizeof(MiBinding));
  va_list args; va_start(args, n);
  for (int i = 0; i < n; i++) e->as.with.bindings[i] = va_arg(args, MiBinding);
  va_end(args); return e;
}

static MiExpr *mi_expr_record(const char *tag, int n, ...) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_RECORD;
  e->as.record.tag = mi_strdup(tag); e->as.record.nbindings = n;
  e->as.record.bindings = mi_alloc(n * sizeof(MiBinding));
  va_list args; va_start(args, n);
  for (int i = 0; i < n; i++) e->as.record.bindings[i] = va_arg(args, MiBinding);
  va_end(args); return e;
}

static MiExpr *mi_expr_namespace(int n, ...) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_NAMESPACE;
  e->as.ns.nbindings = n; e->as.ns.bindings = mi_alloc(n * sizeof(MiBinding));
  va_list args; va_start(args, n);
  for (int i = 0; i < n; i++) e->as.ns.bindings[i] = va_arg(args, MiBinding);
  va_end(args); return e;
}

static MiExpr *mi_expr_loc(MiExpr *e, const char *loc) { e->loc = loc; return e; }

// ── Pat constructors ──
static MiPat *mi_pat_var(const char *v) {
  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_VAR; p->as.var = mi_strdup(v); return p;
}
static MiPat *mi_pat_wild(void) { MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_WILD; return p; }
static MiPat *mi_pat_int(int64_t v) {
  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_INT; p->as.i = v; return p;
}
static MiPat *mi_pat_string(const char *s) {
  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_STRING; p->as.s = mi_strdup(s); return p;
}
static MiPat *mi_pat_rec(const char *tag, int n, ...) {
  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_REC;
  p->as.rec.tag = mi_strdup(tag); p->as.rec.nfields = n;
  p->as.rec.field_names = mi_alloc(n * sizeof(char*));
  p->as.rec.field_pats = mi_alloc(n * sizeof(MiPat*));
  va_list args; va_start(args, n);
  for (int i = 0; i < n; i++) {
    p->as.rec.field_names[i] = mi_strdup(va_arg(args, char*));
    p->as.rec.field_pats[i] = va_arg(args, MiPat*);
  }
  va_end(args); return p;
}

static MiAlt mi_alt(MiPat *pat, MiExpr *guard, MiExpr *body) { MiAlt a; a.pat = pat; a.guard = guard; a.body = body; return a; }

static MiExpr *mi_expr_case(MiExpr *scrut, int n, ...) {
  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_CASE;
  e->as.cas.scrut = scrut; e->as.cas.nalts = n;
  e->as.cas.alts = mi_alloc(n * sizeof(MiAlt));
  va_list args; va_start(args, n);
  for (int i = 0; i < n; i++) e->as.cas.alts[i] = va_arg(args, MiAlt);
  va_end(args); return e;
}

// ── Helpers ──
static double mi_to_float(MiVal v) {
  return v.type == MI_FLOAT ? v.as.f : (double)v.as.i;
}

static int mi_positional_idx(const char *name) {
  if (name[0] != '_' || name[1] == '\0') return -1;
  for (int i = 1; name[i]; i++) if (name[i] < '0' || name[i] > '9') return -1;
  return atoi(name + 1);
}

static MiVal mi_field_loc(MiVal rec, const char *name, const char *loc) {
  if (rec.type != MI_RECORD) { mi_error(loc, "field access on non-record"); }
  int idx = mi_positional_idx(name);
  if (idx >= 0 && idx < rec.as.rec.nfields) return rec.as.rec.fields[idx];
  for (int i = 0; i < rec.as.rec.nfields; i++)
    if (rec.as.rec.names && strcmp(rec.as.rec.names[i], name) == 0)
      return rec.as.rec.fields[i];
  mi_errorf(loc, "field '%s' not found in record '%s'", name, rec.as.rec.tag);
  exit(1);
}
static MiVal mi_field(MiVal rec, const char *name) { return mi_field_loc(rec, name, NULL); }

static MiVal mi_apply(MiVal f, MiVal arg);
static MiVal mi_eval(MiExpr *expr, MiEnv *env);
static int64_t mi_truthy(MiVal v);

// ── Printing ──
static int mi_is_cons_list(MiVal v);
static void mi_print_cons_items(MiVal v, int first);

static void mi_print_val(MiVal v) {
  switch (v.type) {
    case MI_INT:    printf("%ld", v.as.i); break;
    case MI_FLOAT:  printf("%g", v.as.f); break;
    case MI_STRING: printf("%.*s", v.as.str.len, v.as.str.data); break;
    case MI_RECORD:
      if (mi_is_cons_list(v)) {
        printf("["); mi_print_cons_items(v, 1); printf("]");
      } else {
        printf("%s {", v.as.rec.tag);
        for (int i = 0; i < v.as.rec.nfields; i++) {
          if (i > 0) printf(", ");
          if (v.as.rec.names) printf("%s = ", v.as.rec.names[i]);
          mi_print_val(v.as.rec.fields[i]);
        }
        printf("}");
      }
      break;
    case MI_CLOSURE: printf("<closure>"); break;
    case MI_NATIVE:  printf("<closure>"); break;
  }
}
static int mi_is_cons_list(MiVal v) {
  if (v.type != MI_RECORD) return 0;
  if (strcmp(v.as.rec.tag, "Nil") == 0 && v.as.rec.nfields == 0) return 1;
  if (strcmp(v.as.rec.tag, "Cons") == 0 && v.as.rec.nfields == 2) return mi_is_cons_list(v.as.rec.fields[1]);
  return 0;
}
static void mi_print_cons_items(MiVal v, int first) {
  if (strcmp(v.as.rec.tag, "Nil") == 0) return;
  if (!first) printf(", ");
  mi_print_val(v.as.rec.fields[0]);
  mi_print_cons_items(v.as.rec.fields[1], 0);
}
static void mi_println_val(MiVal v) { mi_print_val(v); printf("\n"); }

// ── Application ──
static MiVal mi_apply(MiVal f, MiVal arg) {
  if (f.type == MI_NATIVE) return f.as.native.fn(arg, f.as.native.env);
  if (f.type == MI_CLOSURE) {
    MiEnv *call_env = mi_env_new(f.as.closure.env);
    mi_env_set(call_env, f.as.closure.param, arg);
    return mi_eval(f.as.closure.body, call_env);
  }
  mi_error(NULL, "apply on non-function");
}

// ── Force thunk ──
static MiVal mi_force(MiVal v, MiEnv *env) {
  (void)env;
  if (v.type == MI_CLOSURE && v.as.closure.param && strcmp(v.as.closure.param, "_thunk_") == 0)
    return mi_eval(v.as.closure.body, v.as.closure.env);
  return v;
}

// ── Pattern matching ──
static int mi_match(MiPat *pat, MiVal val, MiEnv *env) {
  switch (pat->type) {
    case PAT_VAR:    mi_env_set(env, pat->as.var, val); return 1;
    case PAT_WILD:   return 1;
    case PAT_INT:    return val.type == MI_INT && val.as.i == pat->as.i;
    case PAT_STRING: return val.type == MI_STRING && strcmp(val.as.str.data, pat->as.s) == 0;
    case PAT_REC:
      if (val.type != MI_RECORD || strcmp(val.as.rec.tag, pat->as.rec.tag) != 0) return 0;
      for (int i = 0; i < pat->as.rec.nfields; i++) {
        int idx = mi_positional_idx(pat->as.rec.field_names[i]);
        if (idx >= 0 && idx < val.as.rec.nfields) {
          if (!mi_match(pat->as.rec.field_pats[i], val.as.rec.fields[idx], env)) return 0;
          continue;
        }
        int found = 0;
        for (int j = 0; j < val.as.rec.nfields; j++) {
          if (strcmp(val.as.rec.names[j], pat->as.rec.field_names[i]) == 0) {
            if (!mi_match(pat->as.rec.field_pats[i], val.as.rec.fields[j], env)) return 0;
            found = 1; break;
          }
        }
        if (!found) return 0;
      }
      return 1;
  }
  return 0;
}

// ── Structural equality ──
static int mi_vals_equal(MiVal a, MiVal b);
static int mi_vals_equal(MiVal a, MiVal b) {
  if (a.type != b.type) return 0;
  switch (a.type) {
    case MI_INT: return a.as.i == b.as.i;
    case MI_FLOAT: return a.as.f == b.as.f;
    case MI_STRING: return a.as.str.len == b.as.str.len && memcmp(a.as.str.data, b.as.str.data, a.as.str.len) == 0;
    case MI_RECORD:
      if (strcmp(a.as.rec.tag, b.as.rec.tag) != 0 || a.as.rec.nfields != b.as.rec.nfields) return 0;
      for (int i = 0; i < a.as.rec.nfields; i++) {
        if (strcmp(a.as.rec.names[i], b.as.rec.names[i]) != 0) return 0;
        if (!mi_vals_equal(a.as.rec.fields[i], b.as.rec.fields[i])) return 0;
      }
      return 1;
    default: return 0;
  }
}

// ── Arithmetic ──
static MiVal mi_binop(const char *op, MiVal a, MiVal b) {
  if (strcmp(op, ":") == 0) return mi_cons(a, b);
  if (strcmp(op, "==") == 0) return mi_int(mi_vals_equal(a, b));
  if (strcmp(op, "/=") == 0) return mi_int(!mi_vals_equal(a, b));
  if (strcmp(op, "+") == 0 && a.type == MI_STRING && b.type == MI_STRING) {
    int la = a.as.str.len, lb = b.as.str.len;
    char *r = mi_alloc(la + lb + 1); memcpy(r, a.as.str.data, la); memcpy(r+la, b.as.str.data, lb); r[la+lb] = '\0';
    MiVal v; v.type = MI_STRING; v.as.str.data = r; v.as.str.len = la + lb; return v;
  }
  if (a.type == MI_STRING && b.type == MI_STRING) {
    int cmp = strcmp(a.as.str.data, b.as.str.data);
    if (strcmp(op, "<") == 0) return mi_int(cmp < 0);
    if (strcmp(op, ">") == 0) return mi_int(cmp > 0);
    if (strcmp(op, "<=") == 0) return mi_int(cmp <= 0);
    if (strcmp(op, ">=") == 0) return mi_int(cmp >= 0);
  }
  if (a.type == MI_FLOAT || b.type == MI_FLOAT) {
    double fa = mi_to_float(a), fb = mi_to_float(b);
    if (strcmp(op, "+") == 0) return mi_float(fa + fb);
    if (strcmp(op, "-") == 0) return mi_float(fa - fb);
    if (strcmp(op, "*") == 0) return mi_float(fa * fb);
    if (strcmp(op, "/") == 0) return mi_float(fa / fb);
    if (strcmp(op, "**") == 0 && b.type == MI_INT && b.as.i >= 0) { double r=1.0; for(int64_t e=b.as.i;e>0;e--) r*=fa; return mi_float(r); }
    if (strcmp(op, "<") == 0) return mi_int(fa < fb);
    if (strcmp(op, ">") == 0) return mi_int(fa > fb);
    if (strcmp(op, "<=") == 0) return mi_int(fa <= fb);
    if (strcmp(op, ">=") == 0) return mi_int(fa >= fb);
    fprintf(stderr, "unsupported operator %s for float operands\n", op); exit(1);
  }
  int64_t ia = a.as.i, ib = b.as.i;
  if (strcmp(op, "+") == 0) return mi_int(ia + ib);
  if (strcmp(op, "-") == 0) return mi_int(ia - ib);
  if (strcmp(op, "*") == 0) return mi_int(ia * ib);
  if (strcmp(op, "/") == 0) { if (ib==0) return mi_int(0); return mi_int(ia / ib); }
  if (strcmp(op, "%") == 0) { if (ib==0) return mi_int(0); return mi_int(ia % ib); }
  if (strcmp(op, "**") == 0) { int64_t r=1; for(int64_t e=ib;e>0;e--) r*=ia; return mi_int(r); }
  if (strcmp(op, "<") == 0) return mi_int(ia < ib);
  if (strcmp(op, ">") == 0) return mi_int(ia > ib);
  if (strcmp(op, "<=") == 0) return mi_int(ia <= ib);
  if (strcmp(op, ">=") == 0) return mi_int(ia >= ib);
  fprintf(stderr, "unknown operator: %s\n", op); exit(1);
}

// ── Wrap params ──
static MiExpr *mi_wrap_lambda(int nparams, char **params, MiExpr *body) {
  MiExpr *e = body;
  for (int i = nparams - 1; i >= 0; i--) e = mi_expr_lam(params[i], e);
  return e;
}

// ── Evaluator (with TCO trampoline) ──
static MiVal mi_eval(MiExpr *expr, MiEnv *env) {
  eval_top:;
  switch (expr->type) {
    case EXPR_INT:    return mi_int(expr->as.i);
    case EXPR_FLOAT:  return mi_float(expr->as.f);
    case EXPR_STRING: return mi_string(expr->as.s);
    case EXPR_VAL:    return expr->as.val;
    case EXPR_NAME:   return mi_env_get_loc(env, expr->as.name, expr->loc);

    case EXPR_BINOP: {
      MiVal l = mi_eval(expr->as.binop.left, env);
      MiVal r = mi_eval(expr->as.binop.right, env);
      return mi_binop(expr->as.binop.op, l, r);
    }

    case EXPR_APP: {
      MiVal fn = mi_eval(expr->as.app.fn, env);
      MiVal a = mi_eval(expr->as.app.arg, env);
      if (fn.type == MI_CLOSURE) {
        MiEnv *call_env = mi_env_new(fn.as.closure.env);
        mi_env_set(call_env, fn.as.closure.param, a);
        expr = fn.as.closure.body; env = call_env; goto eval_top;
      }
      if (fn.type == MI_NATIVE) {
        MiVal result = fn.as.native.fn(a, fn.as.native.env);
        if (result.type == MI_CLOSURE && result.as.closure.param
            && strcmp(result.as.closure.param, "_thunk_") == 0) {
          expr = result.as.closure.body; env = result.as.closure.env; goto eval_top;
        }
        return result;
      }
      mi_error(expr->loc, "apply on non-function");
    }

    case EXPR_LAM: {
      MiVal r; r.type = MI_CLOSURE;
      r.as.closure.body = expr->as.lam.body;
      r.as.closure.param = expr->as.lam.param;
      r.as.closure.env = env;
      return r;
    }

    case EXPR_WITH: {
      MiEnv *inner = mi_env_new(env);
      for (int i = 0; i < expr->as.with.nbindings; i++) {
        MiBinding *b = &expr->as.with.bindings[i];
        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;
        if (b->lazy) {
          MiVal thunk; thunk.type = MI_CLOSURE;
          thunk.as.closure.body = body; thunk.as.closure.param = "_thunk_";
          thunk.as.closure.env = inner;
          mi_env_set(inner, b->name, thunk);
        } else {
          mi_env_set(inner, b->name, mi_eval(body, inner));
        }
      }
      expr = expr->as.with.body; env = inner; goto eval_top;
    }

    case EXPR_RECORD: {
      int n = expr->as.record.nbindings;
      MiVal *fields = mi_alloc(n * sizeof(MiVal));
      const char **names = mi_alloc(n * sizeof(char*));
      for (int i = 0; i < n; i++) {
        MiBinding *b = &expr->as.record.bindings[i];
        names[i] = b->name;
        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;
        fields[i] = mi_eval(body, env);
      }
      MiVal r; r.type = MI_RECORD;
      r.as.rec.tag = expr->as.record.tag; r.as.rec.names = names;
      r.as.rec.fields = fields; r.as.rec.nfields = n;
      return r;
    }

    case EXPR_FIELD: {
      MiVal v = mi_eval(expr->as.field.expr, env);
      v = mi_force(v, env);
      return mi_field_loc(v, expr->as.field.field, expr->loc);
    }

    case EXPR_NAMESPACE: {
      MiEnv *inner = mi_env_new(env);
      int n = expr->as.ns.nbindings;
      MiVal *vals = mi_alloc(n * sizeof(MiVal));
      const char **names = mi_alloc(n * sizeof(const char *));
      for (int i = 0; i < n; i++) {
        MiBinding *b = &expr->as.ns.bindings[i];
        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;
        names[i] = b->name;
        if (b->lazy) {
          MiVal thunk; thunk.type = MI_CLOSURE;
          thunk.as.closure.body = body; thunk.as.closure.param = "_thunk_";
          thunk.as.closure.env = inner;
          mi_env_set(inner, b->name, thunk); vals[i] = thunk;
        } else {
          vals[i] = mi_eval(body, inner);
          mi_env_set(inner, b->name, vals[i]);
        }
      }
      MiVal rec; rec.type = MI_RECORD;
      rec.as.rec.tag = "_module_";
      rec.as.rec.nfields = n;
      rec.as.rec.names = names;
      rec.as.rec.fields = vals;
      return rec;
    }

    case EXPR_CASE: {
      MiVal scrut = mi_eval(expr->as.cas.scrut, env);
      scrut = mi_force(scrut, env);
      for (int i = 0; i < expr->as.cas.nalts; i++) {
        MiEnv *match_env = mi_env_new(env);
        if (mi_match(expr->as.cas.alts[i].pat, scrut, match_env)) {
          if (expr->as.cas.alts[i].guard) {
            MiVal gv = mi_eval(expr->as.cas.alts[i].guard, match_env);
            if (!mi_truthy(gv)) continue;
          }
          expr = expr->as.cas.alts[i].body; env = match_env; goto eval_top;
        }
      }
      mi_error(expr->loc, "match: no matching pattern");
    }

    case EXPR_THUNK: {
      MiVal r; r.type = MI_CLOSURE;
      r.as.closure.body = expr->as.thunk.body;
      r.as.closure.param = "_thunk_";
      r.as.closure.env = env;
      return r;
    }
  }
  mi_errorf(expr->loc, "eval: unknown expr type %d", expr->type);
}

// ── Built-ins ──
static int64_t mi_truthy(MiVal v) {
  switch (v.type) {
    case MI_INT:    return v.as.i != 0;
    case MI_FLOAT:  return v.as.f != 0.0;
    case MI_STRING: return v.as.str.len != 0;
    case MI_RECORD:
      if (strcmp(v.as.rec.tag, "False") == 0 && v.as.rec.nfields == 0) return 0;
      if (strcmp(v.as.rec.tag, "Nil") == 0 && v.as.rec.nfields == 0) return 0;
      return 1;
    default: return 1;
  }
}
static MiVal mi_builtin_truthy(MiVal v, void *e) { (void)e; return mi_int(mi_truthy(v)); }
static MiVal mi_builtin_print(MiVal v, void *e) { (void)e; mi_print_val(v); return mi_int(0); }
static MiVal mi_builtin_println(MiVal v, void *e) { (void)e; mi_println_val(v); return mi_int(0); }

// if: 3-arg curried
struct mi_if_env2 { MiVal cond; MiVal then_val; };
static MiVal mi_builtin_if3(MiVal else_val, void *env) {
  struct mi_if_env2 *e = (struct mi_if_env2 *)env;
  if (mi_truthy(e->cond)) return e->then_val;
  return else_val;
}
struct mi_if_env1 { MiVal cond; };
static MiVal mi_builtin_if2(MiVal then_val, void *env) {
  struct mi_if_env1 *e = (struct mi_if_env1 *)env;
  struct mi_if_env2 *e2 = mi_alloc(sizeof(struct mi_if_env2));
  e2->cond = e->cond; e2->then_val = then_val;
  return mi_native_env(mi_builtin_if3, e2);
}
static MiVal mi_builtin_if(MiVal cond, void *env) {
  (void)env; struct mi_if_env1 *e = mi_alloc(sizeof(struct mi_if_env1));
  e->cond = cond; return mi_native_env(mi_builtin_if2, e);
}

// len
static MiVal mi_builtin_len(MiVal v, void *e) {
  (void)e;
  if (v.type == MI_STRING) return mi_int(v.as.str.len);
  if (v.type == MI_RECORD) {
    int64_t n = 0; MiVal cur = v;
    while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, "Cons") == 0) { n++; cur = cur.as.rec.fields[1]; }
    return mi_int(n);
  }
  return mi_int(0);
}

// charAt
struct mi_charAt_env { MiVal str; };
static MiVal mi_builtin_charAt2(MiVal idx, void *env) {
  struct mi_charAt_env *e = (struct mi_charAt_env *)env;
  int i = (int)idx.as.i;
  if (i < 0 || i >= e->str.as.str.len) return mi_nothing();
  return mi_just(mi_stringn(e->str.as.str.data + i, 1));
}
static MiVal mi_builtin_charAt(MiVal str, void *env) {
  (void)env; struct mi_charAt_env *e = mi_alloc(sizeof(struct mi_charAt_env));
  e->str = str; return mi_native_env(mi_builtin_charAt2, e);
}

// slice
struct mi_slice_env1 { MiVal val; };
struct mi_slice_env2 { MiVal val; int start; };
static MiVal mi_builtin_slice3(MiVal end_val, void *env) {
  struct mi_slice_env2 *e = (struct mi_slice_env2 *)env;
  int s = e->start;
  int end = (int)end_val.as.i;
  if (e->val.type == MI_STRING) {
    int n = e->val.as.str.len;
    if (s < 0) s = 0; if (end > n) end = n; if (s > end) s = end;
    return mi_stringn(e->val.as.str.data + s, end - s);
  }
  MiVal cur = e->val;
  for (int i = 0; i < s && cur.type == MI_RECORD && strcmp(cur.as.rec.tag, "Cons") == 0; i++) cur = cur.as.rec.fields[1];
  int count = end - s; if (count < 0) count = 0;
  MiVal result = mi_nil();
  for (int i = 0; i < count && cur.type == MI_RECORD && strcmp(cur.as.rec.tag, "Cons") == 0; i++) {
    result = mi_cons(cur.as.rec.fields[0], result); cur = cur.as.rec.fields[1];
  }
  MiVal rev = mi_nil();
  while (result.type == MI_RECORD && strcmp(result.as.rec.tag, "Cons") == 0) {
    rev = mi_cons(result.as.rec.fields[0], rev); result = result.as.rec.fields[1];
  }
  return rev;
}
static MiVal mi_builtin_slice2(MiVal start, void *env) {
  struct mi_slice_env1 *e1 = (struct mi_slice_env1 *)env;
  struct mi_slice_env2 *e2 = mi_alloc(sizeof(struct mi_slice_env2));
  e2->val = e1->val; e2->start = (int)start.as.i;
  return mi_native_env(mi_builtin_slice3, e2);
}
static MiVal mi_builtin_slice(MiVal val, void *env) {
  (void)env; struct mi_slice_env1 *e = mi_alloc(sizeof(struct mi_slice_env1));
  e->val = val; return mi_native_env(mi_builtin_slice2, e);
}

// indexOf
struct mi_indexOf_env { MiVal haystack; };
static MiVal mi_builtin_indexOf2(MiVal needle, void *env) {
  struct mi_indexOf_env *e = (struct mi_indexOf_env *)env;
  char *p = strstr(e->haystack.as.str.data, needle.as.str.data);
  return mi_int(p ? (int64_t)(p - e->haystack.as.str.data) : -1);
}
static MiVal mi_builtin_indexOf(MiVal haystack, void *env) {
  (void)env; struct mi_indexOf_env *e = mi_alloc(sizeof(struct mi_indexOf_env));
  e->haystack = haystack; return mi_native_env(mi_builtin_indexOf2, e);
}

// split
struct mi_split_env { MiVal str; };
static MiVal mi_builtin_split2(MiVal delim, void *env) {
  struct mi_split_env *e = (struct mi_split_env *)env;
  int dlen = delim.as.str.len;
  if (dlen == 0) {
    int n = e->str.as.str.len;
    MiVal lst = mi_nil();
    for (int i = n - 1; i >= 0; i--) lst = mi_cons(mi_stringn(e->str.as.str.data + i, 1), lst);
    return lst;
  }
  int cap = 16; int count = 0;
  MiVal *items = (MiVal*)malloc(cap * sizeof(MiVal));
  const char *p = e->str.as.str.data;
  const char *end = p + e->str.as.str.len;
  while (p <= end) {
    const char *found = strstr(p, delim.as.str.data);
    if (!found || found >= end) { found = end; }
    if (count >= cap) { cap *= 2; items = (MiVal*)realloc(items, cap * sizeof(MiVal)); }
    items[count++] = mi_stringn(p, (int)(found - p));
    if (found == end) break;
    p = found + dlen;
  }
  MiVal lst = mi_nil();
  for (int i = count - 1; i >= 0; i--) lst = mi_cons(items[i], lst);
  free(items);
  return lst;
}
static MiVal mi_builtin_split(MiVal str, void *env) {
  (void)env; struct mi_split_env *e = mi_alloc(sizeof(struct mi_split_env));
  e->str = str; return mi_native_env(mi_builtin_split2, e);
}

// trim
static MiVal mi_builtin_trim(MiVal str, void *env) {
  (void)env;
  const char *s = str.as.str.data; int n = str.as.str.len;
  int start = 0; while (start < n && (s[start]==' '||s[start]=='\t'||s[start]=='\n'||s[start]=='\r')) start++;
  int end = n; while (end > start && (s[end-1]==' '||s[end-1]=='\t'||s[end-1]=='\n'||s[end-1]=='\r')) end--;
  return mi_stringn(s + start, end - start);
}

// toUpper / toLower
static MiVal mi_builtin_toUpper(MiVal str, void *env) {
  (void)env; int n = str.as.str.len; char *buf = mi_alloc(n+1);
  for (int i = 0; i < n; i++) buf[i] = toupper((unsigned char)str.as.str.data[i]);
  buf[n] = '\0';
  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = n; return r;
}
static MiVal mi_builtin_toLower(MiVal str, void *env) {
  (void)env; int n = str.as.str.len; char *buf = mi_alloc(n+1);
  for (int i = 0; i < n; i++) buf[i] = tolower((unsigned char)str.as.str.data[i]);
  buf[n] = '\0';
  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = n; return r;
}

// replace
struct mi_replace_env1 { MiVal str; };
struct mi_replace_env2 { MiVal str; MiVal old; };
static MiVal mi_builtin_replace3(MiVal new_str, void *env) {
  struct mi_replace_env2 *e = (struct mi_replace_env2 *)env;
  if (e->old.as.str.len == 0) return e->str;
  int count = 0;
  const char *p = e->str.as.str.data;
  while ((p = strstr(p, e->old.as.str.data)) != NULL) { count++; p += e->old.as.str.len; }
  if (count == 0) return e->str;
  int new_len = e->str.as.str.len + count * (new_str.as.str.len - e->old.as.str.len);
  char *buf = mi_alloc(new_len + 1); int pos = 0;
  p = e->str.as.str.data;
  while (*p) {
    const char *found = strstr(p, e->old.as.str.data);
    if (!found) { int rem = e->str.as.str.len - (int)(p - e->str.as.str.data); memcpy(buf+pos, p, rem); pos += rem; break; }
    int seg = (int)(found - p); memcpy(buf+pos, p, seg); pos += seg;
    memcpy(buf+pos, new_str.as.str.data, new_str.as.str.len); pos += new_str.as.str.len;
    p = found + e->old.as.str.len;
  }
  buf[new_len] = '\0';
  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = new_len; return r;
}
static MiVal mi_builtin_replace2(MiVal old, void *env) {
  struct mi_replace_env1 *e1 = (struct mi_replace_env1 *)env;
  struct mi_replace_env2 *e2 = mi_alloc(sizeof(struct mi_replace_env2));
  e2->str = e1->str; e2->old = old;
  return mi_native_env(mi_builtin_replace3, e2);
}
static MiVal mi_builtin_replace(MiVal str, void *env) {
  (void)env; struct mi_replace_env1 *e = mi_alloc(sizeof(struct mi_replace_env1));
  e->str = str; return mi_native_env(mi_builtin_replace2, e);
}

// toString
static void mi_format_val_buf(MiVal v, char **buf, int *len, int *cap);
static void mi_buf_append(char **buf, int *len, int *cap, const char *s) {
  int slen = strlen(s);
  while (*len + slen >= *cap) { *cap *= 2; *buf = realloc(*buf, *cap); }
  memcpy(*buf + *len, s, slen); *len += slen; (*buf)[*len] = 0;
}
static void mi_format_val_buf(MiVal v, char **buf, int *len, int *cap) {
  char tmp[64];
  v = mi_force(v, NULL);
  switch (v.type) {
    case MI_INT: snprintf(tmp, sizeof(tmp), "%ld", v.as.i); mi_buf_append(buf,len,cap,tmp); break;
    case MI_FLOAT: snprintf(tmp, sizeof(tmp), "%g", v.as.f); mi_buf_append(buf,len,cap,tmp); break;
    case MI_STRING: {
      while (*len + v.as.str.len >= *cap) { *cap *= 2; *buf = realloc(*buf, *cap); }
      memcpy(*buf + *len, v.as.str.data, v.as.str.len); *len += v.as.str.len; (*buf)[*len] = 0;
      break; }
    case MI_RECORD:
      if (mi_is_cons_list(v)) {
        mi_buf_append(buf,len,cap,"[");
        int first = 1; MiVal cur = v;
        while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, "Cons") == 0) {
          if (!first) mi_buf_append(buf,len,cap,", ");
          mi_format_val_buf(cur.as.rec.fields[0], buf, len, cap);
          cur = mi_force(cur.as.rec.fields[1], NULL); first = 0;
        }
        mi_buf_append(buf,len,cap,"]");
      } else {
        if (v.as.rec.tag[0]) { mi_buf_append(buf,len,cap,v.as.rec.tag); mi_buf_append(buf,len,cap," {"); }
        else mi_buf_append(buf,len,cap,"{");
        for (int i = 0; i < v.as.rec.nfields; i++) {
          if (i > 0) mi_buf_append(buf,len,cap,", ");
          if (v.as.rec.names) { mi_buf_append(buf,len,cap,v.as.rec.names[i]); mi_buf_append(buf,len,cap," = "); }
          mi_format_val_buf(v.as.rec.fields[i], buf, len, cap);
        }
        mi_buf_append(buf,len,cap,"}");
      }
      break;
    case MI_CLOSURE: mi_buf_append(buf,len,cap,"<closure>"); break;
    case MI_NATIVE:  mi_buf_append(buf,len,cap,"<closure>"); break;
  }
}
static MiVal mi_builtin_toString(MiVal v, void *env) {
  (void)env;
  int len = 0, cap = 256;
  char *buf = malloc(cap);
  buf[0] = 0;
  mi_format_val_buf(v, &buf, &len, &cap);
  MiVal result = mi_string(buf);
  free(buf);
  return result;
}

// toInt
static MiVal mi_builtin_toInt(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_INT) return mi_just(v);
  if (v.type == MI_FLOAT) return mi_just(mi_int((int64_t)v.as.f));
  if (v.type == MI_STRING) {
    char *end; long long n = strtoll(v.as.str.data, &end, 10);
    if (end != v.as.str.data && *end == '\0') return mi_just(mi_int(n));
    return mi_nothing();
  }
  return mi_nothing();
}

// toFloat
static MiVal mi_builtin_toFloat(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_FLOAT) return mi_just(v);
  if (v.type == MI_INT) return mi_just(mi_float((double)v.as.i));
  if (v.type == MI_STRING) {
    char *end; double n = strtod(v.as.str.data, &end);
    if (end != v.as.str.data && *end == '\0') return mi_just(mi_float(n));
    return mi_nothing();
  }
  return mi_nothing();
}

// float / round / floor / ceil
static MiVal mi_builtin_float(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_FLOAT) return v;
  return mi_float((double)v.as.i);
}
static MiVal mi_builtin_round(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_INT) return v;
  double d = v.as.f; return mi_int((int64_t)(d < 0 ? d - 0.5 : d + 0.5));
}
static MiVal mi_builtin_floor(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_INT) return v;
  double d = v.as.f; return mi_int((int64_t)(d >= 0 ? d : d - 1));
}
static MiVal mi_builtin_ceil(MiVal v, void *env) {
  (void)env;
  if (v.type == MI_INT) return v;
  double d = v.as.f; int64_t i = (int64_t)d; return mi_int(d > (double)i ? i + 1 : i);
}

// ── Record introspection ──
static MiVal mi_builtin_fields(MiVal v, void *env) {
  (void)env; v = mi_force(v, NULL);
  if (v.type != MI_RECORD) return mi_nil();
  MiVal lst = mi_nil();
  for (int i = v.as.rec.nfields - 1; i >= 0; i--) lst = mi_cons(v.as.rec.fields[i], lst);
  return lst;
}
static MiVal mi_builtin_fieldNames(MiVal v, void *env) {
  (void)env; v = mi_force(v, NULL);
  if (v.type != MI_RECORD) return mi_nil();
  MiVal lst = mi_nil();
  for (int i = v.as.rec.nfields - 1; i >= 0; i--) lst = mi_cons(mi_string(v.as.rec.names[i]), lst);
  return lst;
}
static MiVal mi_builtin_tag(MiVal v, void *env) {
  (void)env; v = mi_force(v, NULL);
  if (v.type != MI_RECORD) return mi_string("");
  return mi_string(v.as.rec.tag);
}
struct mi_getField_env { MiVal rec; };
static MiVal mi_builtin_getField2(MiVal name, void *env) {
  struct mi_getField_env *e = (struct mi_getField_env *)env;
  if (name.type != MI_STRING) return mi_nothing();
  for (int i = 0; i < e->rec.as.rec.nfields; i++) {
    if (strcmp(e->rec.as.rec.names[i], name.as.str.data) == 0) return mi_just(e->rec.as.rec.fields[i]);
  }
  return mi_nothing();
}
static MiVal mi_builtin_getField2_notfound(MiVal name, void *env) {
  (void)name; (void)env; return mi_nothing();
}
static MiVal mi_builtin_getField(MiVal rec, void *env) {
  (void)env; rec = mi_force(rec, NULL);
  if (rec.type != MI_RECORD) return mi_native_env(mi_builtin_getField2_notfound, NULL);
  struct mi_getField_env *e = mi_alloc(sizeof(struct mi_getField_env));
  e->rec = rec; return mi_native_env(mi_builtin_getField2, e);
}
struct mi_setField_env { MiVal rec; };
struct mi_setField_env2 { MiVal rec; const char *name; };
static MiVal mi_builtin_setField3(MiVal val, void *env) {
  struct mi_setField_env2 *e = (struct mi_setField_env2 *)env;
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = e->rec.as.rec.tag;
  int n = e->rec.as.rec.nfields;
  int found = 0;
  for (int i = 0; i < n; i++) if (strcmp(e->rec.as.rec.names[i], e->name) == 0) found = 1;
  int nn = found ? n : n + 1;
  r.as.rec.nfields = nn;
  r.as.rec.names = mi_alloc(sizeof(const char*) * nn);
  r.as.rec.fields = mi_alloc(sizeof(MiVal) * nn);
  for (int i = 0; i < n; i++) {
    r.as.rec.names[i] = e->rec.as.rec.names[i];
    r.as.rec.fields[i] = (strcmp(e->rec.as.rec.names[i], e->name) == 0) ? val : e->rec.as.rec.fields[i];
  }
  if (!found) { r.as.rec.names[n] = e->name; r.as.rec.fields[n] = val; }
  return r;
}
static MiVal mi_builtin_setField2(MiVal name, void *env) {
  struct mi_setField_env *e = (struct mi_setField_env *)env;
  if (name.type != MI_STRING) return e->rec;
  struct mi_setField_env2 *e2 = mi_alloc(sizeof(struct mi_setField_env2));
  e2->rec = e->rec; e2->name = name.as.str.data;
  return mi_native_env(mi_builtin_setField3, e2);
}
static MiVal mi_builtin_setField(MiVal rec, void *env) {
  (void)env; rec = mi_force(rec, NULL);
  if (rec.type != MI_RECORD) return rec;
  struct mi_setField_env *e = mi_alloc(sizeof(struct mi_setField_env));
  e->rec = rec; return mi_native_env(mi_builtin_setField2, e);
}

// ── World / IO ──
static MiVal mi_ok(MiVal v) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Ok";
  r.as.rec.nfields = 1;
  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = "value";
  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = v;
  return r;
}
static MiVal mi_err(const char *msg) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "Err";
  r.as.rec.nfields = 1;
  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = "msg";
  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = mi_string(msg);
  return r;
}
static MiVal mi_none(void) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = "None";
  r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL;
  return r;
}

static MiVal mi_world_readFile(MiVal path, void *env) {
  (void)env;
  FILE *f = fopen(path.as.str.data, "r");
  if (!f) {
    char buf[512]; snprintf(buf, sizeof(buf), "Cannot open file: %s", path.as.str.data);
    return mi_err(buf);
  }
  fseek(f, 0, SEEK_END); long len = ftell(f); fseek(f, 0, SEEK_SET);
  char *contents = mi_alloc(len + 1);
  long nread = fread(contents, 1, len, f); contents[nread] = '\0'; fclose(f);
  return mi_ok(mi_stringn(contents, (int)nread));
}

struct mi_wf_env { MiVal path; };
static MiVal mi_world_writeFile2(MiVal contents, void *env) {
  struct mi_wf_env *e = (struct mi_wf_env *)env;
  FILE *f = fopen(e->path.as.str.data, "w");
  if (!f) {
    char buf[512]; snprintf(buf, sizeof(buf), "Cannot write file: %s", e->path.as.str.data);
    return mi_err(buf);
  }
  fwrite(contents.as.str.data, 1, contents.as.str.len, f); fclose(f);
  return mi_ok(mi_int(0));
}
static MiVal mi_world_writeFile(MiVal path, void *env) {
  (void)env; struct mi_wf_env *e = mi_alloc(sizeof(struct mi_wf_env));
  e->path = path; return mi_native_env(mi_world_writeFile2, e);
}

static MiVal mi_world_appendFile2(MiVal contents, void *env) {
  struct mi_wf_env *e = (struct mi_wf_env *)env;
  FILE *f = fopen(e->path.as.str.data, "a");
  if (!f) {
    char buf[512]; snprintf(buf, sizeof(buf), "Cannot append file: %s", e->path.as.str.data);
    return mi_err(buf);
  }
  fwrite(contents.as.str.data, 1, contents.as.str.len, f); fclose(f);
  return mi_ok(mi_int(0));
}
static MiVal mi_world_appendFile(MiVal path, void *env) {
  (void)env; struct mi_wf_env *e = mi_alloc(sizeof(struct mi_wf_env));
  e->path = path; return mi_native_env(mi_world_appendFile2, e);
}

static MiVal mi_world_exists(MiVal path, void *env) {
  (void)env; FILE *f = fopen(path.as.str.data, "r");
  if (f) { fclose(f); return mi_int(1); }
  return mi_int(0);
}

static MiVal mi_world_remove(MiVal path, void *env) {
  (void)env;
  if (remove(path.as.str.data) == 0) return mi_ok(mi_int(0));
  char buf[512]; snprintf(buf, sizeof(buf), "Cannot remove: %s", path.as.str.data);
  return mi_err(buf);
}

static MiVal mi_world_readLine(MiVal prompt, void *env) {
  (void)env;
  if (prompt.type == MI_STRING && prompt.as.str.len > 0) {
    printf("%.*s", prompt.as.str.len, prompt.as.str.data); fflush(stdout);
  }
  char buf[4096];
  if (fgets(buf, sizeof(buf), stdin)) {
    size_t len = strlen(buf);
    if (len > 0 && buf[len-1] == '\n') buf[len-1] = '\0';
    return mi_string(buf);
  }
  return mi_string("");
}

static MiVal mi_world_getEnv(MiVal name, void *env) {
  (void)env;
  const char *val = getenv(name.as.str.data);
  if (val) return mi_ok(mi_string(val));
  return mi_none();
}

struct mi_exec_env { MiVal cmd; };
static MiVal mi_world_exec2(MiVal args, void *env) {
  struct mi_exec_env *e = (struct mi_exec_env *)env;
  char cmdline[8192];
  int pos = snprintf(cmdline, sizeof(cmdline), "%s", e->cmd.as.str.data);
  MiVal cur = args;
  while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, "Cons") == 0 && pos < (int)sizeof(cmdline)-1) {
    MiVal a = cur.as.rec.fields[0];
    pos += snprintf(cmdline + pos, sizeof(cmdline) - pos, " %s", a.as.str.data);
    cur = cur.as.rec.fields[1];
  }
  FILE *p = popen(cmdline, "r");
  if (!p) return mi_err("Failed to execute command");
  char *output = mi_alloc(65536); size_t total = 0; size_t n;
  while ((n = fread(output + total, 1, 65536 - total - 1, p)) > 0) total += n;
  output[total] = '\0';
  int status = pclose(p);
  if (status != 0) {
    char buf[512]; snprintf(buf, sizeof(buf), "Command exited with status %d", status);
    return mi_err(buf);
  }
  return mi_ok(mi_stringn(output, (int)total));
}
static MiVal mi_world_exec(MiVal cmd, void *env) {
  (void)env; struct mi_exec_env *e = mi_alloc(sizeof(struct mi_exec_env));
  e->cmd = cmd; return mi_native_env(mi_world_exec2, e);
}

static MiVal mi_world_exit(MiVal code, void *env) {
  (void)env; exit((int)code.as.i);
  return mi_int(0);
}

static MiVal mi_make_record(const char *tag, int n, const char **names, MiVal *fields) {
  MiVal r; r.type = MI_RECORD; r.as.rec.tag = tag;
  r.as.rec.nfields = n; r.as.rec.names = names; r.as.rec.fields = fields;
  return r;
}

static MiVal mi_build_world(int argc, char **argv) {
  MiVal argvList = mi_nil();
  for (int i = argc - 1; i >= 0; i--) argvList = mi_cons(mi_string(argv[i]), argvList);

  const int io_n = 3;
  const char **io_names = mi_alloc(io_n * sizeof(const char*));
  MiVal *io_fields = mi_alloc(io_n * sizeof(MiVal));
  io_names[0] = "println";  io_fields[0] = mi_native(mi_builtin_println);
  io_names[1] = "print";    io_fields[1] = mi_native(mi_builtin_print);
  io_names[2] = "readLine"; io_fields[2] = mi_native(mi_world_readLine);
  MiVal io_rec = mi_make_record("_io_", io_n, io_names, io_fields);

  const int fsr_n = 2;
  const char **fsr_names = mi_alloc(fsr_n * sizeof(const char*));
  MiVal *fsr_fields = mi_alloc(fsr_n * sizeof(MiVal));
  fsr_names[0] = "file";   fsr_fields[0] = mi_native(mi_world_readFile);
  fsr_names[1] = "exists"; fsr_fields[1] = mi_native(mi_world_exists);
  MiVal fsr_rec = mi_make_record("_fs_read_", fsr_n, fsr_names, fsr_fields);

  const int fsw_n = 3;
  const char **fsw_names = mi_alloc(fsw_n * sizeof(const char*));
  MiVal *fsw_fields = mi_alloc(fsw_n * sizeof(MiVal));
  fsw_names[0] = "file";   fsw_fields[0] = mi_native(mi_world_writeFile);
  fsw_names[1] = "append"; fsw_fields[1] = mi_native(mi_world_appendFile);
  fsw_names[2] = "remove"; fsw_fields[2] = mi_native(mi_world_remove);
  MiVal fsw_rec = mi_make_record("_fs_write_", fsw_n, fsw_names, fsw_fields);

  const int fs_n = 2;
  const char **fs_names = mi_alloc(fs_n * sizeof(const char*));
  MiVal *fs_fields = mi_alloc(fs_n * sizeof(MiVal));
  fs_names[0] = "read";  fs_fields[0] = fsr_rec;
  fs_names[1] = "write"; fs_fields[1] = fsw_rec;
  MiVal fs_rec = mi_make_record("_fs_", fs_n, fs_names, fs_fields);

  const int proc_n = 2;
  const char **proc_names = mi_alloc(proc_n * sizeof(const char*));
  MiVal *proc_fields = mi_alloc(proc_n * sizeof(MiVal));
  proc_names[0] = "exec"; proc_fields[0] = mi_native(mi_world_exec);
  proc_names[1] = "exit"; proc_fields[1] = mi_native(mi_world_exit);
  MiVal proc_rec = mi_make_record("_process_", proc_n, proc_names, proc_fields);

  const int world_n = 5;
  const char **world_names = mi_alloc(world_n * sizeof(const char*));
  MiVal *world_fields = mi_alloc(world_n * sizeof(MiVal));
  world_names[0] = "argv";    world_fields[0] = argvList;
  world_names[1] = "getEnv";  world_fields[1] = mi_native(mi_world_getEnv);
  world_names[2] = "io";      world_fields[2] = io_rec;
  world_names[3] = "fs";      world_fields[3] = fs_rec;
  world_names[4] = "process"; world_fields[4] = proc_rec;
  return mi_make_record("_world_", world_n, world_names, world_fields);
}


int main(int argc, char **argv) {
  MiEnv *_env = mi_env_new(NULL);
  mi_env_set(_env, "if", mi_native(mi_builtin_if));
  mi_env_set(_env, "truthy", mi_native(mi_builtin_truthy));
  mi_env_set(_env, "strlen", mi_native(mi_builtin_len));
  mi_env_set(_env, "len", mi_native(mi_builtin_len));
  mi_env_set(_env, "charAt", mi_native(mi_builtin_charAt));
  mi_env_set(_env, "slice", mi_native(mi_builtin_slice));
  mi_env_set(_env, "indexOf", mi_native(mi_builtin_indexOf));
  mi_env_set(_env, "split", mi_native(mi_builtin_split));
  mi_env_set(_env, "trim", mi_native(mi_builtin_trim));
  mi_env_set(_env, "toUpper", mi_native(mi_builtin_toUpper));
  mi_env_set(_env, "toLower", mi_native(mi_builtin_toLower));
  mi_env_set(_env, "replace", mi_native(mi_builtin_replace));
  mi_env_set(_env, "toString", mi_native(mi_builtin_toString));
  mi_env_set(_env, "_toString", mi_native(mi_builtin_toString));
  mi_env_set(_env, "toInt", mi_native(mi_builtin_toInt));
  mi_env_set(_env, "toFloat", mi_native(mi_builtin_toFloat));
  mi_env_set(_env, "float", mi_native(mi_builtin_float));
  mi_env_set(_env, "round", mi_native(mi_builtin_round));
  mi_env_set(_env, "floor", mi_native(mi_builtin_floor));
  mi_env_set(_env, "ceil", mi_native(mi_builtin_ceil));
  mi_env_set(_env, "fields", mi_native(mi_builtin_fields));
  mi_env_set(_env, "fieldNames", mi_native(mi_builtin_fieldNames));
  mi_env_set(_env, "tag", mi_native(mi_builtin_tag));
  mi_env_set(_env, "getField", mi_native(mi_builtin_getField));
  mi_env_set(_env, "setField", mi_native(mi_builtin_setField));

  mi_env_set(_env, "Bool", mi_eval(mi_expr_loc(mi_expr_namespace(2, mi_binding("True", 0, 0, mi_expr_record("True", 0)), mi_binding("False", 0, 0, mi_expr_record("False", 0))), "<prelude>:1:1"), _env));
  mi_env_set(_env, "True", mi_eval(mi_expr_record("True", 0), _env));
  mi_env_set(_env, "False", mi_eval(mi_expr_record("False", 0), _env));
  mi_env_set(_env, "List", mi_eval(mi_expr_loc(mi_expr_namespace(2, mi_binding("Nil", 0, 0, mi_expr_record("Nil", 0)), mi_binding("Cons", 0, 0, mi_expr_lam("head0", mi_expr_lam("tail0", mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("head0")), mi_binding("tail", 0, 0, mi_expr_name("tail0"))))))), "<prelude>:2:1"), _env));
  mi_env_set(_env, "Nil", mi_eval(mi_expr_record("Nil", 0), _env));
  mi_env_set(_env, "Cons", mi_eval(mi_expr_lam("head0", mi_expr_lam("tail0", mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("head0")), mi_binding("tail", 0, 0, mi_expr_name("tail0"))))), _env));
  mi_env_set(_env, "Maybe", mi_eval(mi_expr_loc(mi_expr_namespace(2, mi_binding("Nothing", 0, 0, mi_expr_record("Nothing", 0)), mi_binding("Just", 0, 0, mi_expr_lam("val", mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("val")))))), "<prelude>:3:1"), _env));
  mi_env_set(_env, "Nothing", mi_eval(mi_expr_record("Nothing", 0), _env));
  mi_env_set(_env, "Just", mi_eval(mi_expr_lam("val", mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("val")))), _env));
  mi_env_set(_env, "eq", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_name("a"), 1, mi_alt(mi_pat_wild(), NULL, mi_expr_binop("==", mi_expr_name("a"), mi_expr_name("b")))))), "<prelude>:4:1"), _env));
  mi_env_set(_env, "if", mi_eval(mi_expr_loc(mi_expr_lam("cond", mi_expr_lam("t", mi_expr_lam("e", mi_expr_case(mi_expr_case(mi_expr_name("cond"), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("e")), mi_alt(mi_pat_wild(), NULL, mi_expr_name("t")))))), "<prelude>:5:1"), _env));
  mi_env_set(_env, "truthy", mi_eval(mi_expr_loc(mi_expr_lam("val", mi_expr_case(mi_expr_name("val"), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1)))), "<prelude>:6:1"), _env));
  mi_env_set(_env, "toString", mi_eval(mi_expr_loc(mi_expr_lam("val", mi_expr_case(mi_expr_name("val"), 6, mi_alt(mi_pat_rec("True", 0), NULL, mi_expr_string("True")), mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_string("False")), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_string("[]")), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_string("Nothing")), mi_alt(mi_pat_rec("Just", 1, "_0", mi_pat_var("x")), NULL, mi_expr_binop("+", mi_expr_binop("+", mi_expr_string("Just("), mi_expr_app(mi_expr_name("toString"), mi_expr_name("x"))), mi_expr_string(")"))), mi_alt(mi_pat_wild(), NULL, mi_expr_app(mi_expr_name("_toString"), mi_expr_name("val"))))), "<prelude>:7:1"), _env));
  mi_env_set(_env, "fromMaybe", mi_eval(mi_expr_loc(mi_expr_lam("def", mi_expr_lam("m", mi_expr_case(mi_expr_name("m"), 2, mi_alt(mi_pat_rec("Just", 1, "_0", mi_pat_var("x")), NULL, mi_expr_name("x")), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_name("def"))))), "<prelude>:8:1"), _env));
  mi_env_set(_env, "id", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_name("x")), "<prelude>:9:1"), _env));
  mi_env_set(_env, "const", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_lam("y", mi_expr_name("x"))), "<prelude>:10:1"), _env));
  mi_env_set(_env, "flip", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("a", mi_expr_lam("b", mi_expr_app(mi_expr_app(mi_expr_name("f"), mi_expr_name("b")), mi_expr_name("a"))))), "<prelude>:11:1"), _env));
  mi_env_set(_env, "null", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0)))), "<prelude>:12:1"), _env));
  mi_env_set(_env, "len", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_binop("+", mi_expr_int(1), mi_expr_app(mi_expr_name("len"), mi_expr_name("t")))))), "<prelude>:13:1"), _env));
  mi_env_set(_env, "head", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nothing", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("h")))))), "<prelude>:14:1"), _env));
  mi_env_set(_env, "tail", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nothing", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("t")))))), "<prelude>:15:1"), _env));
  mi_env_set(_env, "fold", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("acc", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_name("acc")), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_name("f")), mi_expr_app(mi_expr_app(mi_expr_name("f"), mi_expr_name("acc")), mi_expr_name("h"))), mi_expr_name("t"))))))), "<prelude>:16:1"), _env));
  mi_env_set(_env, "map", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nil", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_app(mi_expr_name("f"), mi_expr_name("h"))), mi_binding("tail", 0, 0, mi_expr_app(mi_expr_app(mi_expr_name("map"), mi_expr_name("f")), mi_expr_name("t")))))))), "<prelude>:17:1"), _env));
  mi_env_set(_env, "filter", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nil", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_case(mi_expr_case(mi_expr_app(mi_expr_name("f"), mi_expr_name("h")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_app(mi_expr_name("filter"), mi_expr_name("f")), mi_expr_name("t"))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("h")), mi_binding("tail", 0, 0, mi_expr_app(mi_expr_app(mi_expr_name("filter"), mi_expr_name("f")), mi_expr_name("t")))))))))), "<prelude>:18:1"), _env));
  mi_env_set(_env, "concat", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_name("a"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_name("b")), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("h")), mi_binding("tail", 0, 0, mi_expr_app(mi_expr_app(mi_expr_name("concat"), mi_expr_name("t")), mi_expr_name("b")))))))), "<prelude>:19:1"), _env));
  mi_env_set(_env, "push", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_lam("x", mi_expr_app(mi_expr_app(mi_expr_name("concat"), mi_expr_name("lst")), mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("x")), mi_binding("tail", 0, 0, mi_expr_record("Nil", 0)))))), "<prelude>:20:1"), _env));
  mi_env_set(_env, "at", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_lam("i", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nothing", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_case(mi_expr_case(mi_expr_binop("==", mi_expr_name("i"), mi_expr_int(0)), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_app(mi_expr_name("at"), mi_expr_name("t")), mi_expr_binop("-", mi_expr_name("i"), mi_expr_int(1)))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("h"))))))))), "<prelude>:21:1"), _env));
  mi_env_set(_env, "at'", mi_eval(mi_expr_loc(mi_expr_lam("i", mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_name("at"), mi_expr_name("lst")), mi_expr_name("i")))), "<prelude>:22:1"), _env));
  mi_env_set(_env, "sum", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x", mi_expr_binop("+", mi_expr_name("acc"), mi_expr_name("x"))))), mi_expr_int(0)), mi_expr_name("lst"))), "<prelude>:23:1"), _env));
  mi_env_set(_env, "product", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x", mi_expr_binop("*", mi_expr_name("acc"), mi_expr_name("x"))))), mi_expr_int(1)), mi_expr_name("lst"))), "<prelude>:24:1"), _env));
  mi_env_set(_env, "any", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x", mi_expr_case(mi_expr_case(mi_expr_app(mi_expr_name("f"), mi_expr_name("x")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("acc")), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1)))))), mi_expr_int(0)), mi_expr_name("lst")))), "<prelude>:25:1"), _env));
  mi_env_set(_env, "all", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x", mi_expr_case(mi_expr_case(mi_expr_app(mi_expr_name("f"), mi_expr_name("x")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_name("acc")))))), mi_expr_int(1)), mi_expr_name("lst")))), "<prelude>:26:1"), _env));
  mi_env_set(_env, "contains", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_lam("x", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x0", mi_expr_case(mi_expr_case(mi_expr_binop("==", mi_expr_name("x0"), mi_expr_name("x")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("acc")), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1)))))), mi_expr_int(0)), mi_expr_name("lst")))), "<prelude>:27:1"), _env));
  mi_env_set(_env, "range", mi_eval(mi_expr_loc(mi_expr_lam("start", mi_expr_lam("end", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("range_helper"), mi_expr_name("start")), mi_expr_name("end")), mi_expr_record("Nil", 0)))), "<prelude>:28:1"), _env));
  mi_env_set(_env, "range_helper", mi_eval(mi_expr_loc(mi_expr_lam("start", mi_expr_lam("end", mi_expr_lam("acc", mi_expr_case(mi_expr_case(mi_expr_binop(">=", mi_expr_name("start"), mi_expr_name("end")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("range_helper"), mi_expr_name("start")), mi_expr_binop("-", mi_expr_name("end"), mi_expr_int(1))), mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_binop("-", mi_expr_name("end"), mi_expr_int(1))), mi_binding("tail", 0, 0, mi_expr_name("acc"))))), mi_alt(mi_pat_wild(), NULL, mi_expr_name("acc")))))), "<prelude>:29:1"), _env));
  mi_env_set(_env, "zip", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_case(mi_expr_case(mi_expr_name("a"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0))), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_case(mi_expr_case(mi_expr_case(mi_expr_name("b"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0))), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_field(mi_expr_name("a"), "head")), mi_binding("tail", 0, 0, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_field(mi_expr_name("b"), "head")), mi_binding("tail", 0, 0, mi_expr_record("Nil", 0)))))), mi_binding("tail", 0, 0, mi_expr_app(mi_expr_app(mi_expr_name("zip"), mi_expr_field(mi_expr_name("a"), "tail")), mi_expr_field(mi_expr_name("b"), "tail"))))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Nil", 0)))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Nil", 0))))), "<prelude>:30:1"), _env));
  mi_env_set(_env, "last", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nothing", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_case(mi_expr_case(mi_expr_case(mi_expr_name("t"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0))), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_name("last"), mi_expr_name("t"))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_name("h")))))))), "<prelude>:31:1"), _env));
  mi_env_set(_env, "init", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nothing", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_case(mi_expr_case(mi_expr_case(mi_expr_name("t"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0))), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("h")), mi_binding("tail", 0, 0, mi_expr_case(mi_expr_app(mi_expr_name("init"), mi_expr_name("t")), 2, mi_alt(mi_pat_rec("Just", 1, "_0", mi_pat_var("x")), NULL, mi_expr_name("x")), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_record("Nil", 0)))))))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Just", 1, mi_binding("val", 0, 0, mi_expr_record("Nil", 0)))))))), "<prelude>:32:1"), _env));
  mi_env_set(_env, "reverse", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("fold"), mi_expr_lam("acc", mi_expr_lam("x", mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("x")), mi_binding("tail", 0, 0, mi_expr_name("acc")))))), mi_expr_record("Nil", 0)), mi_expr_name("lst"))), "<prelude>:33:1"), _env));
  mi_env_set(_env, "take", mi_eval(mi_expr_loc(mi_expr_lam("n", mi_expr_lam("lst", mi_expr_case(mi_expr_case(mi_expr_binop("==", mi_expr_name("n"), mi_expr_int(0)), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_app(mi_expr_name("take_inner"), mi_expr_name("n")), mi_expr_name("lst"))), mi_alt(mi_pat_wild(), NULL, mi_expr_record("Nil", 0))))), "<prelude>:34:1"), _env));
  mi_env_set(_env, "take_inner", mi_eval(mi_expr_loc(mi_expr_lam("n", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nil", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_name("h")), mi_binding("tail", 0, 0, mi_expr_app(mi_expr_app(mi_expr_name("take"), mi_expr_binop("-", mi_expr_name("n"), mi_expr_int(1))), mi_expr_name("t")))))))), "<prelude>:35:1"), _env));
  mi_env_set(_env, "drop", mi_eval(mi_expr_loc(mi_expr_lam("n", mi_expr_lam("lst", mi_expr_case(mi_expr_case(mi_expr_binop("==", mi_expr_name("n"), mi_expr_int(0)), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_app(mi_expr_app(mi_expr_name("drop_inner"), mi_expr_name("n")), mi_expr_name("lst"))), mi_alt(mi_pat_wild(), NULL, mi_expr_name("lst"))))), "<prelude>:36:1"), _env));
  mi_env_set(_env, "drop_inner", mi_eval(mi_expr_loc(mi_expr_lam("n", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_record("Nil", 0)), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_app(mi_expr_app(mi_expr_name("drop"), mi_expr_binop("-", mi_expr_name("n"), mi_expr_int(1))), mi_expr_name("t")))))), "<prelude>:37:1"), _env));
  mi_env_set(_env, "enumerate", mi_eval(mi_expr_loc(mi_expr_lam("lst", mi_expr_app(mi_expr_app(mi_expr_name("zip"), mi_expr_app(mi_expr_app(mi_expr_app(mi_expr_name("range_helper"), mi_expr_int(0)), mi_expr_app(mi_expr_name("len"), mi_expr_name("lst"))), mi_expr_record("Nil", 0))), mi_expr_name("lst"))), "<prelude>:38:1"), _env));
  mi_env_set(_env, "join", mi_eval(mi_expr_loc(mi_expr_lam("sep", mi_expr_lam("lst", mi_expr_case(mi_expr_name("lst"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_string("")), mi_alt(mi_pat_rec("Cons", 2, "_0", mi_pat_var("h"), "_1", mi_pat_var("t")), NULL, mi_expr_case(mi_expr_case(mi_expr_case(mi_expr_name("t"), 2, mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0))), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_binop("+", mi_expr_binop("+", mi_expr_name("h"), mi_expr_name("sep")), mi_expr_app(mi_expr_app(mi_expr_name("join"), mi_expr_name("sep")), mi_expr_name("t")))), mi_alt(mi_pat_wild(), NULL, mi_expr_name("h"))))))), "<prelude>:39:1"), _env));
  mi_env_set(_env, "abs", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_case(mi_expr_case(mi_expr_binop("<", mi_expr_name("x"), mi_expr_int(0)), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("x")), mi_alt(mi_pat_wild(), NULL, mi_expr_binop("-", mi_expr_int(0), mi_expr_name("x"))))), "<prelude>:40:1"), _env));
  mi_env_set(_env, "neg", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_binop("-", mi_expr_int(0), mi_expr_name("x"))), "<prelude>:41:1"), _env));
  mi_env_set(_env, "min", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_case(mi_expr_binop("<", mi_expr_name("a"), mi_expr_name("b")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("b")), mi_alt(mi_pat_wild(), NULL, mi_expr_name("a"))))), "<prelude>:42:1"), _env));
  mi_env_set(_env, "max", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_case(mi_expr_binop(">", mi_expr_name("a"), mi_expr_name("b")), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("b")), mi_alt(mi_pat_wild(), NULL, mi_expr_name("a"))))), "<prelude>:43:1"), _env));
  mi_env_set(_env, "not", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_case(mi_expr_case(mi_expr_name("x"), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_int(1)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(0)))), "<prelude>:44:1"), _env));
  mi_env_set(_env, "|>", mi_eval(mi_expr_loc(mi_expr_lam("x", mi_expr_lam("f", mi_expr_app(mi_expr_name("f"), mi_expr_name("x")))), "<prelude>:45:1"), _env));
  mi_env_set(_env, ">>", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("g", mi_expr_lam("x", mi_expr_app(mi_expr_name("g"), mi_expr_app(mi_expr_name("f"), mi_expr_name("x")))))), "<prelude>:46:1"), _env));
  mi_env_set(_env, "<<", mi_eval(mi_expr_loc(mi_expr_lam("f", mi_expr_lam("g", mi_expr_lam("x", mi_expr_app(mi_expr_name("f"), mi_expr_app(mi_expr_name("g"), mi_expr_name("x")))))), "<prelude>:47:1"), _env));
  mi_env_set(_env, "&&", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_case(mi_expr_name("a"), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_name("b"))))), "<prelude>:48:1"), _env));
  mi_env_set(_env, "||", mi_eval(mi_expr_loc(mi_expr_lam("a", mi_expr_lam("b", mi_expr_case(mi_expr_case(mi_expr_name("a"), 6, mi_alt(mi_pat_rec("False", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nil", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_rec("Nothing", 0), NULL, mi_expr_int(0)), mi_alt(mi_pat_int(0), NULL, mi_expr_int(0)), mi_alt(mi_pat_string(""), NULL, mi_expr_int(0)), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))), 2, mi_alt(mi_pat_int(0), NULL, mi_expr_name("b")), mi_alt(mi_pat_wild(), NULL, mi_expr_int(1))))), "<prelude>:49:1"), _env));
  mi_env_set(_env, "Collections", mi_eval(mi_expr_loc(mi_expr_string("import not found: https://example.com/milang-stdlib/collections.mi"), "showcase.mi:4:1"), _env));
  mi_env_set(_env, "Shape", mi_eval(mi_expr_loc(mi_expr_namespace(2, mi_binding("Circle", 0, 0, mi_expr_lam("_0", mi_expr_record("Circle", 1, mi_binding("_0", 0, 0, mi_expr_name("_0"))))), mi_binding("Rect", 0, 0, mi_expr_lam("_0", mi_expr_lam("_1", mi_expr_record("Rect", 2, mi_binding("_0", 0, 0, mi_expr_name("_0")), mi_binding("_1", 0, 0, mi_expr_name("_1"))))))), "showcase.mi:7:1"), _env));
  mi_env_set(_env, "Circle", mi_eval(mi_expr_lam("_0", mi_expr_record("Circle", 1, mi_binding("_0", 0, 0, mi_expr_name("_0")))), _env));
  mi_env_set(_env, "Rect", mi_eval(mi_expr_lam("_0", mi_expr_lam("_1", mi_expr_record("Rect", 2, mi_binding("_0", 0, 0, mi_expr_name("_0")), mi_binding("_1", 0, 0, mi_expr_name("_1"))))), _env));
  mi_env_set(_env, "area", mi_eval(mi_expr_loc(mi_expr_lam("shape", mi_expr_case(mi_expr_name("shape"), 2, mi_alt(mi_pat_rec("Circle", 1, "_0", mi_pat_var("_0")), NULL, mi_expr_binop("*", mi_expr_binop("*", mi_expr_float(3.14), mi_expr_name("_0")), mi_expr_name("_0"))), mi_alt(mi_pat_rec("Rect", 2, "_0", mi_pat_var("_0"), "_1", mi_pat_var("_1")), NULL, mi_expr_binop("*", mi_expr_name("_0"), mi_expr_name("_1"))))), "showcase.mi:10:1"), _env));
  mi_env_set(_env, "origin", mi_eval(mi_expr_loc(mi_expr_record("Point", 2, mi_binding("x", 0, 0, mi_expr_loc(mi_expr_int(0), "showcase.mi:15:17")), mi_binding("y", 0, 0, mi_expr_loc(mi_expr_int(0), "showcase.mi:15:24"))), "showcase.mi:15:1"), _env));
  mi_env_set(_env, "moved", mi_eval(mi_expr_loc(mi_expr_record("Point", 2, mi_binding("x", 0, 0, mi_expr_loc(mi_expr_int(5), "showcase.mi:16:21")), mi_binding("y", 0, 0, mi_expr_loc(mi_expr_int(10), "showcase.mi:16:28"))), "showcase.mi:16:1"), _env));
  mi_env_set(_env, "shapes", mi_eval(mi_expr_loc(mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_record("Circle", 1, mi_binding("_0", 0, 0, mi_expr_int(3)))), mi_binding("tail", 0, 0, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_record("Rect", 2, mi_binding("_0", 0, 0, mi_expr_int(4)), mi_binding("_1", 0, 0, mi_expr_int(5)))), mi_binding("tail", 0, 0, mi_expr_record("Nil", 0))))), "showcase.mi:19:1"), _env));
  mi_env_set(_env, "areas", mi_eval(mi_expr_loc(mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_binop("*", mi_expr_binop("*", mi_expr_float(3.14), mi_expr_int(3)), mi_expr_int(3))), mi_binding("tail", 0, 0, mi_expr_record("Cons", 2, mi_binding("head", 0, 0, mi_expr_int(20)), mi_binding("tail", 0, 0, mi_expr_record("Nil", 0))))), "showcase.mi:20:1"), _env));
  mi_env_set(_env, "total", mi_eval(mi_expr_loc(mi_expr_app(mi_expr_field(mi_expr_record("", 0), "sum"), mi_expr_name("areas")), "showcase.mi:21:1"), _env));
  mi_env_set(_env, "main", mi_eval(mi_expr_loc(mi_expr_lam("world", mi_expr_with(mi_expr_int(0), 2, mi_binding("_stmt_709", 0, 0, mi_expr_loc(mi_expr_app(mi_expr_field(mi_expr_field(mi_expr_name("world"), "io"), "println"), mi_expr_binop("+", mi_expr_string("total area: "), mi_expr_app(mi_expr_name("toString"), mi_expr_name("total")))), "showcase.mi:25:3")), mi_binding("_stmt_762", 0, 0, mi_expr_loc(mi_expr_app(mi_expr_field(mi_expr_field(mi_expr_name("world"), "io"), "println"), mi_expr_binop("+", mi_expr_binop("+", mi_expr_binop("+", mi_expr_string("moved to: "), mi_expr_app(mi_expr_name("_toString"), mi_expr_int(5))), mi_expr_string(", ")), mi_expr_app(mi_expr_name("_toString"), mi_expr_int(10)))), "showcase.mi:26:3")))), "showcase.mi:24:1"), _env));

  MiVal _world = mi_build_world(argc, argv);
  MiVal _result = mi_apply(mi_env_get(_env, "main"), _world);
  return (_result.type == MI_INT) ? (int)_result.as.i : 0;
}
