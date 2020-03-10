#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define xstr(s) str(s)
#define str(s) #s

#define ARRAY_ELEMENTS(x) (sizeof(x) / sizeof(x[0]))
#define CALL_ARGUMENTS_LIMIT UINT_MAX

static jmp_buf ex_buf;

enum s_expr_type {
  ATOM,
  CELL,
};

typedef struct s_expr {
  enum s_expr_type type;
  void *expr;
} S_Expr;

typedef struct cell {
  struct s_expr *left;
  struct s_expr *right;
} Cell;

enum atomic_val_types {
  STRING,
  NUMBER,
  BOOLEAN,
  NIL,

  EXPR,
  FEXPR,
  SUBR,
  FSUBR,
};

enum param_types {
  PTYPE_ANY,
  PTYPE_ATOM,
  PTYPE_CELL,
};

typedef struct atom {

  enum atomic_val_types type;
  unsigned int len; /* additional length info for strings, and expr/subr params */
  enum param_types ptype;
  struct s_expr *args_bind_names;

  char *pname;
  void *apval;
  struct s_expr *expr;
  struct s_expr *fexpr;
  void *subr;
  struct s_expr *(*fsubr)(struct s_expr *symbol_table, struct s_expr *e);
} Atom;

// NIL expr & atom
static Atom nil_atom = {
    .len = 0,
    .apval = NULL,
    .type = NIL,
};

static S_Expr nil_s_expr = {
    .type = ATOM,
    .expr = (void *)&nil_atom,
};

// Boolean constants
//
static const bool true_val = true;
static const bool false_val = false;

static Atom true_atom = {
    .apval = (void *)&true_val,
    .type = BOOLEAN,
};
static S_Expr true_s_expr = {
    .type = ATOM,
    .expr = (void *)&true_atom,
};
static Atom false_atom = {
    .apval = (void *)&false_val,
    .type = BOOLEAN,
};
static S_Expr false_s_expr = {
    .type = ATOM,
    .expr = (void *)&false_atom,
};

// system function declarations
S_Expr *cons(S_Expr *left, S_Expr *right);
S_Expr *cons2(S_Expr *left, S_Expr *right);
S_Expr *car(S_Expr *e);
S_Expr *cdr(S_Expr *e);
S_Expr *eq(S_Expr *e1, S_Expr *e2);
S_Expr *equal(S_Expr *e1, S_Expr *e2);
S_Expr *atom(S_Expr *e);
S_Expr *quote(S_Expr *e);
S_Expr *append(S_Expr *list, S_Expr *new_elem);
S_Expr *label(S_Expr **table, S_Expr *lab, S_Expr *e);
S_Expr *lambda(S_Expr *args, S_Expr *e);
S_Expr *flambda(S_Expr *args, S_Expr *e);
S_Expr *cond(S_Expr *symbol_table, S_Expr *args);
S_Expr *assoc(S_Expr *x, S_Expr *a);

// other user-accessible functions
S_Expr *plus(S_Expr *numbers_l);
S_Expr *minus(S_Expr *numbers_l);
S_Expr *multiply(S_Expr *numbers_l);
S_Expr *divide(S_Expr *numbers_l);
S_Expr *lt(S_Expr *numbers_l);
S_Expr *gt(S_Expr *numbers_l);
S_Expr *print(S_Expr *e);

// other function declarations:

S_Expr *setq(S_Expr *expr, S_Expr *val);

void print_s_expr(S_Expr *se);

enum print_parent_t {
  PRINT_PARENT_NONE,
  PRINT_PARENT_CELL_LEFT,
  PRINT_PARENT_CELL_RIGHT
};
void __print_s_expr(S_Expr *se, enum print_parent_t pr_parent);

// evaluation functions:
// S_Expr * apply(S_Expr * fn, S_Expr * args, S_Expr * symbol_table);
S_Expr *apply(S_Expr *fn, S_Expr *args, S_Expr *symbol_table,
              bool maybe_from_list_constructor, bool args_were_list);
S_Expr *eval(S_Expr *e, S_Expr **symbol_table);
// S_Expr * evcon(S_Expr * c, S_Expr * a);
S_Expr *evlis(S_Expr *e, S_Expr *symbol_table);

//
// global symbols

static S_Expr *global_symbols_table;

enum sym_defs {
  /* fundamental functions */
  CAR_SYM_DEF,
  CDR_SYM_DEF,
  CONS_SYM_DEF,
  ATOM_SYM_DEF,
  EQ_SYM_DEF,
  /* other functions */
  QUOTE_SYM_DEF,
  LABEL_SYM_DEF,
  LAMBDA_SYM_DEF,
  FLAMBDA_SYM_DEF,
  COND_SYM_DEF,
  /* */
  EVAL_SYM_DEF,
  /* */
  PLUS_SYM_DEF,
  MINUS_SYM_DEF,
  MULTIPLY_SYM_DEF,
  DIVIDE_SYM_DEF,
  LT_SYM_DEF,
  GT_SYM_DEF,
  PRINT_SYM_DEF,
  /* end */
  MAX_SYM_DEF
};

Atom car_atom_sym_def = {
    .pname = "CAR",
    .subr = car,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
    .ptype = PTYPE_CELL,
};

Atom cdr_atom_sym_def = {
    .pname = "CDR",
    .subr = cdr,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
    .ptype = PTYPE_CELL,
};

Atom cons_atom_sym_def = {
    .pname = "CONS",
    .subr = cons2,
    .type = SUBR,
    .len = 2,
    .ptype = PTYPE_ANY,
};

Atom atom_atom_sym_def = {
    .pname = "ATOM",
    .subr = atom,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
    .ptype = PTYPE_ANY,
};

Atom eq_atom_sym_def = {
    .pname = "EQ",
    .subr = eq,
    .type = SUBR,
    .len = 2,
};

Atom quote_atom_sym_def = {
    .pname = "QUOTE",
    .subr = quote,
    .type = SUBR,
    .len = 1, // CALL_ARGUMENTS_LIMIT,
};

Atom label_atom_sym_def = {
    .pname = "LABEL",
    .subr = atom,
    .type = SUBR,
    .len = 2,
};

Atom lambda_atom_sym_def = {
    .pname = "LAMBDA",
    .subr = lambda,
    .type = SUBR,
    .len = 2,
};

Atom flambda_atom_sym_def = {
    .pname = "FLAMBDA",
    .subr = flambda,
    .type = SUBR,
    .len = 2,
};

Atom cond_atom_sym_def = {
    /**
     * COND here is an FSUBR, as it is a special form.
     * remember: a special form has an arbitrary number of
     * arguments, and its arguments are not evaluated
     * beforehand.
     */
    .pname = "COND",
    .fsubr = cond,
    .type = FSUBR,
};

Atom eval_atom_sym_def = {
    .pname = "EVAL",
    .subr = eval,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
};

// ---------------------------------
//
// ---------------------------------

Atom plus_atom_sym_def = {
    .pname = "+",
    .subr = plus,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
};

Atom minus_atom_sym_def = {
    .pname = "-",
    .subr = minus,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
};

Atom multiply_atom_sym_def = {
    .pname = "*",
    .subr = multiply,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
};

Atom divide_atom_sym_def = {
    .pname = "/",
    .subr = divide,
    .type = SUBR,
    .len = CALL_ARGUMENTS_LIMIT,
};

Atom lt_atom_sym_def = {
    .pname = "<",
    .subr = lt,
    .type = SUBR,
    .len = 2,
};

Atom gt_atom_sym_def = {
    .pname = ">",
    .subr = gt,
    .type = SUBR,
    .len = 2,
};

Atom print_atom_sym_def = {
    .pname = "print",
    .subr = print,
    .type = SUBR,
    .len = 1,
};

S_Expr subr_symbol_defs[MAX_SYM_DEF] = {
    [CAR_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &car_atom_sym_def,
        },
    [CDR_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &cdr_atom_sym_def,
        },
    [CONS_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &cons_atom_sym_def,
        },
    [ATOM_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &atom_atom_sym_def,
        },
    [EQ_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &eq_atom_sym_def,
        },
    /* --- */
    [QUOTE_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &quote_atom_sym_def,
        },
    [LABEL_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &label_atom_sym_def,
        },
    [LAMBDA_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &lambda_atom_sym_def,
        },
    [FLAMBDA_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &flambda_atom_sym_def,
        },
    [COND_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &cond_atom_sym_def,
        },
    [EVAL_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &eval_atom_sym_def,
        },
    [PLUS_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &plus_atom_sym_def,
        },
    [MINUS_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &minus_atom_sym_def,
        },
    [MULTIPLY_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &multiply_atom_sym_def,
        },
    [DIVIDE_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &divide_atom_sym_def,
        },
    [LT_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &lt_atom_sym_def,
        },
    [GT_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &gt_atom_sym_def,
        },
    [PRINT_SYM_DEF] =
        {
            .type = ATOM,
            .expr = &print_atom_sym_def,
        },
};

S_Expr *car_fn_symbol = 0;
S_Expr *cdr_fn_symbol = 0;
S_Expr *cons_fn_symbol = 0;
S_Expr *atom_fn_symbol = 0;
S_Expr *eq_fn_symbol = 0;

S_Expr *quote_fn_symbol = 0;
S_Expr *label_fn_symbol = 0;

S_Expr *lambda_fn_symbol = 0;
S_Expr *flambda_fn_symbol = 0;
S_Expr *cond_fn_symbol = 0;
S_Expr *eval_fn_symbol = 0;

S_Expr *plus_fn_symbol = 0;
S_Expr *minus_fn_symbol = 0;
S_Expr *multiply_fn_symbol = 0;
S_Expr *divide_fn_symbol = 0;
S_Expr *lt_fn_symbol = 0;
S_Expr *gt_fn_symbol = 0;
S_Expr *print_fn_symbol = 0;

// VM memory definitions
//
#define MEM_SEGMENT_SIZE (1024 * 16) // 16k per segment size
#define MEM_NUM_SEGMENTS 64

static uint8_t mem_segments[MEM_NUM_SEGMENTS][MEM_SEGMENT_SIZE];
static unsigned int mem_segments_ixs[MEM_NUM_SEGMENTS] = {0};

static uint8_t mem_segment_i = 0;

//-----------------------------------------------------------------------------
// Exception handling
//-----------------------------------------------------------------------------

void jinni_exception_no_msg(void)
{
  mem_segment_i = 0;
  longjmp(ex_buf, 1);
}

void jinni_exception(const char *fmt, ...)
{
  printf("\nException: ");
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
  fflush(stdout);

  jinni_exception_no_msg();
}

//-----------------------------------------------------------------------------
// VM
//-----------------------------------------------------------------------------

void free_mem_segment(int s)
{
  mem_segments_ixs[s] = 0;
  return;
}

void free_mem_segments(int from, int to)
{
  for (int i = from; i < to; i++)
    mem_segments_ixs[i] = 0;

  return;
}

void *jinni_malloc(size_t size)
{
  if (mem_segment_i >= MEM_NUM_SEGMENTS ||
      (mem_segments_ixs[mem_segment_i] + size) >= MEM_SEGMENT_SIZE) {
    jinni_exception("jinni_malloc -- size: %lu, mem_segment_i: %u. "
                    "mem_segments_ixs[mem_segment_i]: %u\n",
                    size, mem_segment_i, mem_segments_ixs[mem_segment_i]);
  }

  void *p =
      (void *)(&mem_segments[mem_segment_i][mem_segments_ixs[mem_segment_i]]);

  memset(p, 0x0, size);

  mem_segments_ixs[mem_segment_i] += size;
  return p;
}

//void *jinni_malloc_dbg(size_t size, char const *caller_name)
//{
  //printf("jinni_malloc was called from %s\n", caller_name);
  //return jinni_malloc(size);
//}
//#define jinni_malloc(x)	jinni_malloc_dbg(x, __func__)

void jinni_free(void *ptr)
{
  // Not implemented :-P
  return;
}

//-----------------------------------------------------------------------------
// misc. helpers
//-----------------------------------------------------------------------------

//#define PRINT_DBG_INFO

static void print_debug_info(const char *dbg_msg, S_Expr *e)
{
#ifdef PRINT_DBG_INFO
  fprintf(stdout, "\n======\n");
  if (dbg_msg && dbg_msg[0] != 0)
    printf("%s\n", dbg_msg);
  if (e)
    print_s_expr(e);
  fprintf(stdout, "\n======\n");
  fflush(stdout);
#endif // PRINT_DBG_INFO
  return;
}

static unsigned int list_len(S_Expr *args);

bool is_atom(S_Expr *e) { return (e->type == ATOM); }

static S_Expr *copy_list(S_Expr *l)
{
#define COPY_PNAME                                                             \
  if (a2->pname) {                                                             \
    a->pname = jinni_malloc(sizeof(char) * (strlen(a2->pname) + 1));           \
    strcpy(a->pname, a2->pname);                                               \
  }

  if (!l) // <-- why?
    return NULL;

  S_Expr *se = jinni_malloc(sizeof(S_Expr));

  if (l->type == ATOM) {
    Atom *a = jinni_malloc(sizeof(Atom));

    Atom *a2 = (Atom *)(l->expr);
    a->type = a2->type;
    switch (a->type) {
    case STRING:
      a->apval = jinni_malloc(sizeof(char) * (strlen((char *)a2->apval) + 1));
      strcpy((char *)a->apval, (char *)a2->apval);
      break;
    case NUMBER:
      a->apval = jinni_malloc(sizeof(double));
      *((double *)(a->apval)) = *((double *)(a2->apval));
      break;
    case BOOLEAN:
      a->apval = jinni_malloc(sizeof(bool));
      *((bool *)(a->apval)) = *((bool *)(a2->apval));
      break;
    case NIL:
      a->apval = NULL;
      break;
    case EXPR:
      a->expr = copy_list(a2->expr);
      COPY_PNAME
      a->len = a2->len;
      if (a2->args_bind_names)
        a->args_bind_names = copy_list(a2->args_bind_names);
      break;
    case FEXPR:
      a->fexpr = copy_list(a2->fexpr);
      COPY_PNAME
      a->len = a2->len;
      if (a2->args_bind_names)
        a->args_bind_names = copy_list(a2->args_bind_names);
      break;
    case SUBR:
      COPY_PNAME
      a->len = a2->len;
      if (a2->pname) {
        a->pname = jinni_malloc(sizeof(char) * (strlen(a2->pname) + 1));
        strcpy(a->pname, a2->pname);
      }
      a->subr = a2->subr;
      break;
    case FSUBR:
      COPY_PNAME
      a->len = a2->len;
      a->fsubr = a2->fsubr;
      break;
    default:
      jinni_exception("unknown atom type: %d", a->type);
    }
    a->len = a2->len;

    se->expr = (void *)a;
  } else if (l->type == CELL) {
    Cell *c = jinni_malloc(sizeof(Cell));
    if ((Cell *)(l->expr) != NULL) {
      if (((Cell *)(l->expr))->left != NULL)
        c->left = copy_list(((Cell *)(l->expr))->left);
      if (((Cell *)(l->expr))->right != NULL)
        c->right = copy_list(((Cell *)(l->expr))->right);
    } else {
      ; // maybe this is ok for the plist entry edge case
    }

    se->expr = (void *)c;
  } else {
    // this should never happen
    // but ok for plist entry edge case
    ;
  }

  se->type = l->type;

  return se;
}

S_Expr *make_atom()
{
  S_Expr *se = jinni_malloc(sizeof(S_Expr));
  Atom *a = jinni_malloc(sizeof(Atom));

  a->len = 0;
  a->apval = NULL;
  a->type = NIL;

  se->type = ATOM;
  se->expr = (void *)a;
  return se;
}

S_Expr *make_bool_atom(bool v)
{
  S_Expr *se = make_atom();
  ((Atom *)(se->expr))->type = BOOLEAN;
  ((Atom *)(se->expr))->apval = jinni_malloc(sizeof(bool));
  *(bool *)(((Atom *)(se->expr))->apval) = v;
  return se;
}

S_Expr *make_number_atom(double v)
{
  S_Expr *se = make_atom();
  ((Atom *)(se->expr))->type = NUMBER;
  ((Atom *)(se->expr))->apval = jinni_malloc(sizeof(double));
  *(double *)(((Atom *)(se->expr))->apval) = v;
  return se;
}

S_Expr *make_string_atom(const char *s, unsigned int s_len)
{
  S_Expr *se = make_atom();
  ((Atom *)(se->expr))->type = STRING;
  ((Atom *)(se->expr))->apval = jinni_malloc((s_len + 1) * sizeof(char));
  ((Atom *)(se->expr))->len = s_len;
  strcpy(((Atom *)(se->expr))->apval, s);
  return se;
}

bool equal_internal(S_Expr *e1, S_Expr *e2)
{
  if (e1 == NULL && e2 == NULL)
    return true;
  else if (e1 == NULL)
    return false;
  else if (e1->type != e2->type)
    return false;

  switch (e1->type) {
  case ATOM:; // NOP
    Atom *a1 = (Atom *)e1->expr;
    Atom *a2 = e2->expr;
    if (a1->type != a2->type) {
      return false;
    } else if (a1->len != a2->len) {
      return false;
    } else {
      switch (a1->type) {
      case STRING:
      case NUMBER:
      case BOOLEAN:
      case NIL:
        if (sizeof(a1->apval) != sizeof(a2->apval))
          return false;
        else if (a1->apval == NULL && a2->apval == NULL)
          return true;
        else if (a1->apval == NULL)
          return false;
        else if (0 != memcmp(a1->apval, a2->apval, sizeof(*a1->apval)))
          return false;

        return true;

      case EXPR:
        return equal_internal(a1->expr, a2->expr);
      case FEXPR:
        return equal_internal(a1->fexpr, a2->fexpr);
      case SUBR:
        if (a1->subr == a2->subr)
          return true;

        return false;
      case FSUBR:
        if (a1->fsubr == a2->fsubr)
          return true;

        return false;
      }
    }

    break;

  case CELL:; // NOP
    Cell *c1 = e1->expr;
    Cell *c2 = e2->expr;

    return (equal_internal(c1->left, c2->left) &&
            equal_internal(c1->right, c2->right));

  default:
    jinni_exception("inconsistent element type in %s", __FUNCTION__);
  }

  return false;
}

bool is_nil_atom(S_Expr *s_expr_a)
{
  bool res = equal_internal(s_expr_a, &nil_s_expr);
  return res;
}

S_Expr *setq(S_Expr *expr, S_Expr *val)
{
  expr->expr = copy_list(val);
  expr->type = val->type;
  return expr->expr;
}

//-----------------------------------------------------------------------------
// ???
//-----------------------------------------------------------------------------

void __print_s_expr(S_Expr *se, enum print_parent_t pr_parent)
{
  if (!se)
    jinni_exception("unexpected null s-expression");

  if (se->type == ATOM) {
    {
      Atom *a = (Atom *)se->expr;
      switch (a->type) {
      case STRING:
        printf("%s", (char *)a->apval);
        break;
      case NUMBER:;
        double val = *((double *)a->apval);
        if ((val - (int)val) != 0)
          printf("%f", val);
        else
          printf("%d", (int)val);
        break;
      case BOOLEAN:
        printf("%c", *((bool *)a->apval) ? 'T' : 'F');
        break;
      case NIL:
        printf("NIL");
        break;
      case EXPR:
        printf("%s", a->pname);
        break;
      case FEXPR:
        printf("%s", a->pname);
        break;
      case SUBR:
        printf("%s", a->pname);
        break;
      case FSUBR:
        printf("%s", a->pname);
        break;
      default:
        jinni_exception("inconsistent type in atom (%d)", a->type);
      }
    }
  } else if (se->type == CELL) {
    Cell *c = (Cell *)se->expr;

    if (pr_parent == PRINT_PARENT_NONE || pr_parent == PRINT_PARENT_CELL_LEFT)
      printf("(");

    __print_s_expr(c->left, PRINT_PARENT_CELL_LEFT);
    if (is_atom(c->right)) {
      if (!is_nil_atom(c->right)) {
        printf(" . ");
        __print_s_expr(c->right, PRINT_PARENT_CELL_RIGHT);
      }
    } else {
      printf(" ");
      __print_s_expr(c->right, PRINT_PARENT_CELL_RIGHT);
    }

    if (pr_parent == PRINT_PARENT_NONE || pr_parent == PRINT_PARENT_CELL_LEFT)
      printf(")");
  } else {
    jinni_exception("undefined type for s-expression");
  }
  return;
}

void print_s_expr(S_Expr *se)
{
  if (!se) {
    printf("NIL (null s-expr)\n");
    return;
  }

  if (se->type == ATOM) {
    printf("atom: ");
    Atom *a = (Atom *)se->expr;
    if (a->pname)
      printf("(:pname %s) ", a->pname);

    switch (a->type) {
    case STRING:
      printf("%s ", (char *)a->apval);
      break;
    case NUMBER:
      printf("%f ", *((double *)a->apval));
      break;
    case BOOLEAN:
      printf("%c ", *((bool *)a->apval) ? 'T' : 'F');
      break;
    case NIL:
      printf("NIL ");
      break;
    case EXPR:
      printf("EXPR: %s - ", a->pname);
      print_s_expr(a->expr);
      if (a->args_bind_names) {
        printf("bind_names: ");
        print_s_expr(a->args_bind_names);
      }
      break;
    case FEXPR:
      printf("FEXPR: %s - ", a->pname);
      print_s_expr(a->fexpr);
      if (a->args_bind_names) {
        printf("bind_names: ");
        print_s_expr(a->args_bind_names);
      }
      break;
    case SUBR:
      printf("SUBR: %s - %p", a->pname, a->subr);
      break;
    case FSUBR:
      printf("FSUBR: %s - %p", a->pname, (void *)a->fsubr);
      break;
    default:
      printf("err: inconsistent type in atom (%d)\n", a->type);
    }
  } else if (se->type == CELL) {
    printf("[cell: ");
    Cell *c = (Cell *)se->expr;
    print_s_expr(c->left);
    printf(", ");
    print_s_expr(c->right);
    printf("] ");
  } else {
    printf("undefined type for s-expression\n");
  }

  return;
}

//-----------------------------------------------------------------------------
// system functions
//-----------------------------------------------------------------------------

/**
 * `cons` has two arguments and is in fact the function that is used
 * to build S-expressions from smaller S-expressions.
 *
 */
S_Expr *cons(S_Expr *left, S_Expr *right)
{
  S_Expr *se = jinni_malloc(sizeof(S_Expr));
  se->type = CELL;

  Cell *c = jinni_malloc(sizeof(Cell));
  c->left = left;
  c->right = right;

  se->expr = c;
  return se;
}

S_Expr *cons2(S_Expr *left, S_Expr *right)
{
  S_Expr *l = left, *r = right;

  if (!is_atom(left) && list_len(left) == 1 &&
      is_nil_atom(((Cell *)left->expr)->right)) {

    l = ((Cell *)left->expr)->left;
  }

  if (!is_atom(right) && list_len(right) == 1 &&
      is_nil_atom(((Cell *)right->expr)->left) &&
      is_nil_atom(((Cell *)right->expr)->right)) {

    // this means that, differently from CL, given
    // the expresion `(cons nil (cons nil nil))`
    // we will evaluate to `(NIL)` instead of `(NIL NIL)`.
    //
    // I am not totally convinced this is wrong per se,
    // so leaving it like this for now.
    //
    r = &nil_s_expr;
  }

  S_Expr *se = jinni_malloc(sizeof(S_Expr));
  se->type = CELL;

  Cell *c = jinni_malloc(sizeof(Cell));
  c->left = copy_list(l);
  c->right = copy_list(r);

  se->expr = c;
  return se;
}

/**
 * `car` has  one argument. Its value is the first part of its
 * composite argument. `car` of an atomic symbol is undefined.
 *
 */
S_Expr *car(S_Expr *e)
{
  if (is_atom(e)) {
    return e;
  }
  return (((Cell *)(e->expr))->left);
}

/**
 * `cdr` has  one argument. Its value is the second part of its
 * composite argument. `cdr` is also undefined if its argument is atomic.
 *
 */
S_Expr *cdr(S_Expr *e)
{
  if (is_atom(e)) {
    return &nil_s_expr;
  }
  return (((Cell *)(e->expr))->right);
}

/**
 * the predicate `eq` is a test for equality on atomic symbols.
 * It is undefined for non-atomic arguments.
 */
S_Expr *eq(S_Expr *e1, S_Expr *e2)
{
  if (!is_atom(e1) || !is_atom(e2)) {
    jinni_exception("eq called on non-atomic arguments");
  }
  return make_bool_atom(equal_internal(e1, e2));
}

/**
 *
 *
 */
S_Expr *equal(S_Expr *e1, S_Expr *e2)
{
  return make_bool_atom(equal_internal(e1, e2));
}

/**
 * the predicate `atom` is true if its argument is an atomic symbol,
 * and false if its arguments is composite.
 */
S_Expr *atom(S_Expr *e) { return make_bool_atom(is_atom(e)); }

S_Expr *quote(S_Expr *e)
{
  S_Expr *qe = e;
  return qe;
}

S_Expr *quote_internal(S_Expr *e)
{
  S_Expr *qe = make_atom();
  ((Atom *)(qe->expr))->type = EXPR;
  ((Atom *)(qe->expr))->expr = e;
  return qe;
}

S_Expr *append(S_Expr *list, S_Expr *new_elem)
{
  if (is_nil_atom(list))
    return new_elem;

  return (cons(car(list), append(cdr(list), new_elem)));
}

S_Expr *label(S_Expr **table, S_Expr *lab, S_Expr *e)
{
  // `lab` here is expected to be an identifier atom
  if (!is_atom(lab))
    jinni_exception("expected an identifier atom in %s", __FUNCTION__);

  Atom *a = (Atom *)lab->expr;
  if (a->type != STRING)
    jinni_exception("identifier atom is not a label in %s", __FUNCTION__);

  S_Expr *lc = lab;
  S_Expr *ec = e;
  S_Expr *t2 = append((*table), cons(cons(lc, ec), &nil_s_expr));

  *table = t2;

  return lc;
}

static unsigned int list_len(S_Expr *args)
{
  if (is_atom(args)) {
    return 0;
  }

  int len = 0;
  Cell *c = (Cell *)args->expr;

  while (c->right && !is_atom(c->right)) {
    len++;
    c = (Cell *)c->right;
  }

  // for a cell, len is always _at least_ 1
  if (len == 0)
    len = 1;

  // plus if right side is not nil... although this is questionable...
  if (c->right && !is_nil_atom(c->right))
    len++;

  return len;
}

S_Expr *lambda(S_Expr *args, S_Expr *e)
{
  if (is_atom(e))
    // e MUST be a cell (cons'ed atoms)
    jinni_exception("expression MUST be a cell in %s", __FUNCTION__);

  /**
   *
   * an example from the manual:
   *
   *	(LABEL EQUAL (LAMBDA (X Y) (COND
   *		((ATOM X) (COND ((ATOM Y) (EQ X Y)) ((QUOTE T) (QUOTE F))))
   *		((EQUAL (CAR X) (CAR Y)) (EQUAL (CDR X) (CDR Y)))
   *		((QUOTE T) (QUOTE F)))))
   **/

  // meaning lambda returns something quoted (or the current implementation
  // of label in jq is wrong... either way... :P )

  S_Expr *l_ex = make_atom();
  Atom *l_at = (Atom *)l_ex->expr;
  l_at->type = EXPR;
  l_at->expr = e;
  if (is_nil_atom(args))
    return l_ex;
  else if (is_atom(args)) // args MUST be a cell (a list!)
    jinni_exception("arguments expected to be a list in %s", __FUNCTION__);

  l_at->len = list_len(args);

  // args has the name of the arguments that will need to be bound,
  // so make sure to save them somewhere in the l_at atom here...
  l_at->args_bind_names = args;

  // lambda needs to do nothing else, the expression e that uses
  // potentially unbound values is not evaluated here, and when
  // an atom with type EXPR is found by `apply` it will try
  // to find and bind the arguments for the expr.

  return l_ex;
}

S_Expr *flambda(S_Expr *args, S_Expr *e)
{
  if (is_atom(args))
    // args MUST be a cell (a list!)
    jinni_exception("arguments expected to be a list in %s", __FUNCTION__);

  if (is_atom(e))
    // e MUST be a cell (cons'ed atoms)
    jinni_exception("expression MUST be a cell (cons'ed atoms) in %s",
                    __FUNCTION__);

  S_Expr *fl_ex = make_atom();
  Atom *fl_at = (Atom *)fl_ex->expr;
  fl_at->type = FEXPR;
  fl_at->fexpr = e;
  fl_at->len = list_len(args);

  fl_at->args_bind_names = args;

  return fl_ex;
}

S_Expr *cond(S_Expr *symbol_table, S_Expr *args)
{
  if (!args || is_nil_atom(args))
    return &nil_s_expr;
  else if (is_atom(args))
    return eval(args, &symbol_table);

  S_Expr *ev_res = eval(car(car(args)), &symbol_table);

  // NIL translates to `false` for conditionals.
  //
  // Note that `cond` is the only function that really
  // has to know this, for all the rest this shouldn't
  // ever matter.
  if (is_nil_atom(ev_res)) {
    ev_res = &false_s_expr;
  }

  // Everything that's not explicitly `false` (or NIL)
  // is true for cond...
  if (!equal_internal(ev_res, &false_s_expr)) {
    S_Expr *r = eval(cdr(car(args)), &symbol_table);
    return r;
  }

  return cond(symbol_table, cdr(args));
}

//-----------------------------------------------------------------------------
// other user-accessible functions
//-----------------------------------------------------------------------------

double plus_internal(double acc, S_Expr *numbers_l)
{
  if (is_nil_atom(numbers_l))
    return acc;

  Atom *atom_val;

  if (is_atom(numbers_l)) {
    atom_val = numbers_l->expr;
  } else {
    if (is_atom(car(numbers_l))) {
      atom_val = (Atom *)(car(numbers_l)->expr);
    } else {
      jinni_exception("invalid arguments passed to +");
    }
  }

  if (atom_val->type != NUMBER) {
    jinni_exception("non-number passed to plus");
  }
  acc += *((double *)atom_val->apval);
  return plus_internal(acc, cdr(numbers_l));
}

S_Expr *plus(S_Expr *numbers_l)
{
  return make_number_atom(plus_internal(0, numbers_l));
}

double minus_internal(double acc, S_Expr *numbers_l)
{
  if (is_nil_atom(numbers_l))
    return acc;

  Atom *atom_val;

  if (is_atom(numbers_l)) {
    atom_val = numbers_l->expr;
  } else {
    if (is_atom(car(numbers_l))) {
      atom_val = (Atom *)(car(numbers_l)->expr);
    } else {
      jinni_exception("invalid arguments passed to -");
    }
  }

  if (atom_val->type != NUMBER) {
    jinni_exception("non-number passed to minus");
  }
  acc -= *((double *)atom_val->apval);
  return minus_internal(acc, cdr(numbers_l));
}

S_Expr *minus(S_Expr *numbers_l)
{
  S_Expr *n = car(numbers_l);
  if (!is_atom(n)) {
    jinni_exception("invalid arguments passed to minus");
  }
  Atom *a = n->expr;
  if (a->type != NUMBER) {
    jinni_exception("non-number passed to minus");
  }
  return make_number_atom(
      minus_internal(*((double *)a->apval), cdr(numbers_l)));
}

double multiply_internal(double acc, S_Expr *numbers_l)
{
  if (is_nil_atom(numbers_l))
    return acc;

  Atom *atom_val;

  if (is_atom(numbers_l)) {
    atom_val = numbers_l->expr;
  } else {
    if (is_atom(car(numbers_l))) {
      atom_val = (Atom *)(car(numbers_l)->expr);
    } else {
      jinni_exception("invalid arguments passed to *");
    }
  }

  if (atom_val->type != NUMBER) {
    jinni_exception("non-number passed to multiply");
  }
  acc *= *((double *)atom_val->apval);
  return multiply_internal(acc, cdr(numbers_l));
}

S_Expr *multiply(S_Expr *numbers_l)
{
  S_Expr *n = car(numbers_l);
  if (!is_atom(n)) {
    jinni_exception("invalid arguments passed to multiply");
  }
  Atom *a = n->expr;
  if (a->type != NUMBER) {
    jinni_exception("non-number passed to multiply");
  }
  return make_number_atom(
      multiply_internal(*((double *)a->apval), cdr(numbers_l)));
}

double divide_internal(double acc, S_Expr *numbers_l)
{
  if (is_nil_atom(numbers_l))
    return acc;

  Atom *atom_val;

  if (is_atom(numbers_l)) {
    atom_val = numbers_l->expr;
  } else {
    if (is_atom(car(numbers_l))) {
      atom_val = (Atom *)(car(numbers_l)->expr);
    } else {
      jinni_exception("invalid arguments passed to /");
    }
  }

  if (atom_val->type != NUMBER) {
    jinni_exception("non-number passed to divide");
  }
  acc /= *((double *)atom_val->apval);
  return divide_internal(acc, cdr(numbers_l));
}

S_Expr *divide(S_Expr *numbers_l)
{
  S_Expr *n = car(numbers_l);
  if (!is_atom(n)) {
    jinni_exception("invalid arguments passed to divide");
  }
  Atom *a = n->expr;
  if (a->type != NUMBER) {
    jinni_exception("non-number passed to divide");
  }
  return make_number_atom(
      divide_internal(*((double *)a->apval), cdr(numbers_l)));
}

S_Expr *lt(S_Expr *numbers_l)
{
  S_Expr *n1 = car(numbers_l);
  S_Expr *n2 = car(cdr(numbers_l));
  if (!is_atom(n1) || !is_atom(n2)) {
    jinni_exception("invalid arguments passed to lt");
  }
  Atom *a1 = n1->expr;
  Atom *a2 = n2->expr;
  if (a1->type != NUMBER || a2->type != NUMBER) {
    jinni_exception("non-number passed to lt");
  }
  return make_bool_atom(*((double *)a1->apval) < *((double *)a2->apval));
}

S_Expr *gt(S_Expr *numbers_l)
{
  S_Expr *n1 = car(numbers_l);
  S_Expr *n2 = car(cdr(numbers_l));
  if (!is_atom(n1) || !is_atom(n2)) {
    jinni_exception("invalid arguments passed to gt");
  }
  Atom *a1 = n1->expr;
  Atom *a2 = n2->expr;
  if (a1->type != NUMBER || a2->type != NUMBER) {
    jinni_exception("non-number passed to gt");
  }
  return make_bool_atom(*((double *)a1->apval) > *((double *)a2->apval));
}

S_Expr *print(S_Expr *e)
{
  if (!is_atom(e)) {
    jinni_exception("print can only print atomic primitive types!!!");
  }

  Atom *a = (Atom *)e->expr;
  switch (a->type) {
  case STRING:
    printf("%s", (char *)a->apval);
    break;
  case NUMBER:
    printf("%f", *(double *)a->apval);
    break;
  case BOOLEAN:
    printf("%c", (*(bool *)a->apval) ? 'T' : 'F');
    break;
  case NIL:
    printf("NIL");
    break;
  default:
    jinni_exception("print can only print atomic primitive types!!!");
  }
  printf("\n");
  fflush(stdout);

  return e;
}
//-----------------------------------------------------------------------------
// additional supporting system functions
//-----------------------------------------------------------------------------

/**
 * If `a` is an association list such as the one formed by pairlis,
 * then assoc will produce the first pair whose first term is x.
 * Thus it is a table searching function.
 */
S_Expr *assoc(S_Expr *x, S_Expr *a)
{
  if (is_nil_atom(a))
    return a;

  return (equal_internal(car(car(a)), x) ? car(a) : assoc(x, cdr(a)));
}

S_Expr *lookup_symbol_table(char *pname, S_Expr *symbol_table, S_Expr *last)
{
  if (is_nil_atom(symbol_table))
    return last;

  if (is_atom(symbol_table)) {
    // i'm not sure if this should be allowed at all...
    //
    // Atom * a = (Atom *)symbol_table->expr;
    // if (0 == strcmp(a->pname, pname))
    //	return symbol_table;
    // else
    //	return NULL;
    //
    // huh? ...as-if ;-)

    jinni_exception("corrupt symbol table");
  }

  Cell *c = (Cell *)symbol_table->expr;
  // on the cell's left side we expect a cell with two
  // atoms: (left: label - right: expression)
  if (!is_atom(c->left)) {
    // just what we expected...
    Cell *symbol = (Cell *)c->left->expr;
    if (is_atom(symbol->left)) {
      Atom *label = symbol->left->expr;
#ifdef PRINT_DBG_INFO
      printf("\ncomparing %s and %s\n", (char *)label->apval, pname);
#endif
      if (label->type == STRING && 0 == strcmp((char *)label->apval, pname)) {
        last = symbol->right;
        // fallthrough to return ...
      } else {
        if (label->type != STRING)
          printf("warning: label in symbol has invalid type\n");
      }
    } else {
      printf("warning: symbol left-side expression is not a label\n");
      print_s_expr(symbol->left);
    }
  } else {
    printf("warning: left side of symbol table element is not a cell... "
           "ignoring it");
  }

  return lookup_symbol_table(pname, cdr(symbol_table), last);
}

S_Expr *bind(S_Expr *st, S_Expr *names, S_Expr *values, uint16_t len)
{
  print_debug_info("bind - names", names);
  print_debug_info("bind - values", values);

  // here names is supposed to be a list of strings
  // and values a list of expressions
  // to-do: validate all of this?

  if (len == 0)
    return st;

  len--;

  print_debug_info("bind st", st);
  ;

  Atom *a_sym_label = (Atom *)(car(names)->expr);
  S_Expr *new_st =
      append(st, cons(cons(make_string_atom(a_sym_label->pname,
                                            strlen(a_sym_label->pname)),
                           car(values)),
                      &nil_s_expr));

  print_debug_info("bind new_st", new_st);

  return bind(new_st, cdr(names), cdr(values), len);
}

S_Expr *bind_quoting(S_Expr *st, S_Expr *names, S_Expr *values, uint16_t len)
{
  if (len == 0)
    return st;

  len--;

  Atom *a_sym_label = (Atom *)(car(names)->expr);
  S_Expr *new_st = append(
      st, cons(cons(make_string_atom(a_sym_label->pname,
                                     strlen(a_sym_label->pname)),
                    cons(quote_fn_symbol, cons(car(values), &nil_s_expr))),
               &nil_s_expr));

  return bind_quoting(new_st, cdr(names), cdr(values), len);
}

//-----------------------------------------------------------------------------
// evaluation functions
//-----------------------------------------------------------------------------

S_Expr *apply(S_Expr *fn, S_Expr *args, S_Expr *symbol_table,
              bool maybe_from_list_constructor, bool args_were_list)
{
  print_debug_info("apply fn: ", fn);
  print_debug_info("apply args: ", args);

  S_Expr *r = NULL;
  if (is_atom(fn)) {
    Atom *fn_atom = (Atom *)fn->expr;

    // for unnamed literal primitives
    if (!fn_atom->pname) {
      if (fn_atom->type == STRING || fn_atom->type == NUMBER ||
          fn_atom->type == BOOLEAN || fn_atom->type == NIL) {
        if (is_nil_atom(args)) {
          return eval(fn, &symbol_table);
        }

        jinni_exception(
            "trying to eval non-function symbol with args in apply");
      }
    }

    // check that the (f)subr/(f)expr with this pname exists in the environment
    // and retrieve the complete expr atom (as fn_atom here is expected
    // to have only the pname, which is what we can get from the parsing)
    S_Expr *e = lookup_symbol_table(fn_atom->pname, symbol_table, NULL);
    if (!e) {
      // nuke it
      jinni_exception("unbound expression: %s", fn_atom->pname);
    }

    print_debug_info("lookup result:", e);

    if (!is_atom(e))
      return eval(e, &symbol_table);

    Atom *e_atom = (Atom *)e->expr;

    switch (e_atom->ptype) {
    case PTYPE_ATOM:
      if (list_len(args) > 1 || !is_atom(((Cell *)args->expr)->right)) {

        jinni_exception("expected an atom but argument is a cell");
      }
      break;
    case PTYPE_CELL:

      if (is_atom(args)) {
        jinni_exception("expected arity (%d) differs from "
                        "number of arguments (1) \n",
                        e_atom->len);
      }
      if (!maybe_from_list_constructor) {
        jinni_exception("expected a list but got (%d) "
                        "atoms as arguments",
                        list_len(args));
      }
      if (e_atom->len != CALL_ARGUMENTS_LIMIT &&
          list_len(args) != e_atom->len) {

        jinni_exception("expected arity (%d) differs from "
                        "number of arguments (%d) \n",
                        e_atom->len, list_len(args));
      }
      break;
    case PTYPE_ANY:
      if (e_atom->len != CALL_ARGUMENTS_LIMIT) {
        if (e_atom->len == 2) {
          // if args. num are 2 can be either two atoms
          // or a list
          //

          if (is_atom(args)) {
            if (args_were_list) {
              // we are going to allow this because
              // evlis does something crazy at the end
              // which it shouldn't and i don't want to
              // fix it now... but this is wrong!

              ; // ok, fallthrough
            } else {
              jinni_exception("expecting a list but got an atom as argument");
            }
          } else if (is_atom(((Cell *)args->expr)->left) &&
                     is_atom(((Cell *)args->expr)->right)) {
            ; // ok, fallthrough
          } else if (!is_atom(((Cell *)args->expr)->right)) {
            // then this has to follow the list, cannot be
            // two atoms as well...
            S_Expr *look_ahead = ((Cell *)args->expr)->right;
            Cell *look_ahead_c = (Cell *)look_ahead->expr;
            if (is_atom(look_ahead_c->right) &&
                !is_nil_atom(look_ahead_c->right)) {
              // printf("\nargs: ");
              //__print_s_expr(args, PRINT_PARENT_NONE);
              // printf("\n\n");

              jinni_exception("impromer argument list");
            }
          }
        }
      }
      break;
    }

    /*
    if (e_atom->len != CALL_ARGUMENTS_LIMIT) {



            if (list_len(args) == 1 &&
                            is_atom(((Cell *)args->expr)->right)) {
                    ; // Nothing

                    // make sure fn_atom->len matches n of args
            } else if (list_len(args) != e_atom->len) {
                    //#ifdef PRINT_DBG_INFO
                    printf("expected arity (%d) differs from number of arguments
    (%d) \n", e_atom->len, list_len(args)); printf("fn_atom pname: %s\n",
    fn_atom->pname); printf("atom pname: %s\n", e_atom->pname); printf("e: ");
                    __print_s_expr(e, PRINT_PARENT_NONE);
                    printf("\nargs: ");
                    __print_s_expr(args, PRINT_PARENT_NONE);
                    printf("\n\n");
                    //#endif
            } else {
                    printf("\nargs ok 1: ");
                    print_s_expr(args);
                    printf("\nargs ok 2: ");
                    __print_s_expr(args, PRINT_PARENT_NONE);
                    printf("\n\n");
            }



    }
    */

    switch (e_atom->type) {
    case SUBR:
      if (equal_internal(e, car_fn_symbol)) {
        r = car(args);
      } else if (equal_internal(e, cdr_fn_symbol)) {
        r = cdr(args);
      } else if (equal_internal(e, cons_fn_symbol)) {
        r = cons2(car(args), (cdr(args)));
      } else if (equal_internal(e, atom_fn_symbol)) {
        r = atom(args);
      } else if (equal_internal(e, eq_fn_symbol)) {
        r = eq(car(args), car(cdr(args)));
      } else if (equal_internal(e, cond_fn_symbol)) {
        r = cond(symbol_table, args);
      } else if (equal_internal(e, eval_fn_symbol)) {
        r = eval(args, &symbol_table);
      } else {
        if (e_atom->subr) {
          S_Expr *(*fn_subr)(S_Expr * e);
          fn_subr = e_atom->subr;
          print_debug_info("args: ", args);
          r = fn_subr(args);
        } else {
          // nuke it
          jinni_exception("undefined subr: %s", e_atom->pname);
        }
      }

      break;

    case EXPR:

      // at this point args- should be a list of values to bind

      if (!e_atom->args_bind_names) {
        // but we have no names to bind the args
        r = eval(car(e_atom->expr), &symbol_table);
        break;
      }

      // bind each arg from args with each var from name
      // from fn_atom->sowewhere-i-saved-the-var-names

      mem_segment_i++;

      S_Expr *new_symbol_table =
          bind(symbol_table, // not necessary to copy the symbol table! (see
                             // other comment on new_symbol_table)
               e_atom->args_bind_names, args, e_atom->len);

      // values are bound already, the expr defined in the lambda doesn't need
      // any parameters as it takes everything it needs from bound variables,
      // then we call eval on the expr with the new symbol table

      r = eval(car(e_atom->expr), &new_symbol_table);

      mem_segment_i--;

      // copy the result value over to a higher
      // memory segment before freeing
      S_Expr *r2 = copy_list(r);

      free_mem_segment(mem_segment_i + 1);
      r = r2;

      break;

    case FEXPR:
    case FSUBR:

      // this should not happen
      jinni_exception("unexpected case in %s", __FUNCTION__);

      break;

    default:

#ifdef PRINT_DBG_INFO
      printf("evaluating atom primitive at apply()");
      print_s_expr(e);
      printf("\n===\n");
#endif // PRINT_DBG_INFO

      r = eval(e, &symbol_table);

      break;
    }

  } else {
    r = eval(fn, &symbol_table);
  }

  print_debug_info("apply (r): ", r);

  return r;
}

S_Expr *eval(S_Expr *e, S_Expr **symbol_table)
{
  print_debug_info("eval: ", e);

  S_Expr *r = NULL;
  if (is_atom(e)) {
    // check for pname, if it's anonymous
    // must be primitive, otherwise lookup
    // in the symbol table and eval it

    Atom *a = (Atom *)e->expr;
    if (!a->pname || 0 == strcmp(a->pname, "")) {
      if (a->type != STRING && a->type != NUMBER && a->type != BOOLEAN &&
          a->type != NIL) {
        jinni_exception("unbound expression: %s\n", a->pname);
      }

      print_debug_info("non-pr: ", e);

      // all good, primitives eval to themselves,
      // just copy and return
      r = e;
    } else {
      S_Expr *found_e = lookup_symbol_table(a->pname, *symbol_table, NULL);

      // here we check not only that we found the expression, but also
      // that if it is an atom it doesn't have any args, otherwise we would
      // loop forever in `eval` (e.g. if one would enter an invalid expression
      // like `( CDR CONS T F )`

      if (!found_e || (is_atom(found_e) && ((Atom *)found_e->expr)->len)) {
        jinni_exception("unbound expression: %s\n", a->pname);
      }

      r = eval(found_e, symbol_table);
    }
  } else {
    // We need to check for special forms and other rather exceptional
    // stuff here, as in some cases it'll have to be dealt with now
    // before evaluating arguments...
    Atom *maybe_fn = (Atom *)(car(e)->expr);
    if (car(e)->type == ATOM && maybe_fn && maybe_fn->pname) {
      S_Expr *fn = lookup_symbol_table(maybe_fn->pname, *symbol_table, NULL);
      if (fn) {
        Atom *fn_atom = (Atom *)fn->expr;
        if (fn_atom->type == SUBR) {

          if (equal_internal(fn, quote_fn_symbol)) {
            struct s_expr *args = cdr(e);
            if (list_len(args) != 1) {
              jinni_exception(
                  "expected arity (1) differs from number of args %d\n",
                  list_len(args));
            }
            r = quote(car(args));
            goto end_eval;
          } else if (equal_internal(fn, label_fn_symbol)) {
            S_Expr *args = cdr(e);

            print_debug_info("label args:", car(args));
            print_debug_info("", eval(car(cdr(args)), symbol_table));

            r = label(symbol_table, car(args),
                      eval(car(cdr(args)), symbol_table));

            goto end_eval;
          } else if (equal_internal(fn, lambda_fn_symbol)) {
            S_Expr *args = cdr(e);

            print_debug_info("lambda args:", car(args));
            print_debug_info("", cdr(args));

            r = lambda(car(args), cdr(args));

            print_debug_info("", r);

            goto end_eval;
          } else if (equal_internal(fn, flambda_fn_symbol)) {
            S_Expr *args = cdr(e);
            r = flambda(car(args), cdr(args));

            goto end_eval;
          }

        } else if (fn_atom->type == FSUBR) {

          S_Expr *args = cdr(e);

          print_debug_info("fsubr args:", args);
          print_debug_info("", car(args));

          r = fn_atom->fsubr(*symbol_table, args);

          goto end_eval;

        } else if (fn_atom->type == FEXPR) {

          S_Expr *args = cdr(e);

          print_debug_info("fexpr (e):", e);
          print_debug_info("", fn_atom->fexpr);

          S_Expr *new_symbol_table = *symbol_table;
          bool with_new_table = false;
          if (fn_atom->args_bind_names) {
            with_new_table = true;
            // Note: the reason we don't need to copy the symbol
            // table here, is because bind and bind_quoting will
            // call append, which in turn will call cons and always
            // return a NEW s expr, with pointers to the same
            // pre-existing entries in the symbol table, and to the
            // new elements as well...

            mem_segment_i++;

            new_symbol_table = bind_quoting(
                *symbol_table, fn_atom->args_bind_names, args, fn_atom->len);
          }

          r = eval(car(fn_atom->fexpr), &new_symbol_table);

          if (with_new_table) {

            mem_segment_i--;

            // copy the result value over to a higher
            // memory segment before freeing
            S_Expr *r2 = copy_list(r);

            free_mem_segment(mem_segment_i + 1);
            r = r2;
          }
          goto end_eval;
        }
      }
    }

    // Note: maybe we want to validate the num of params for CAR and CDR
    // here, and only allow lists with a something like CONS or QUOTE as
    // first element, but not a literal list, since that may actually be
    // just a bunch of atoms coming from the parser and not a properly
    // constructed list.
    //
    // But it is already implemented in `apply`, so as a quick fix we can
    // check here and pass the info. to it, whether it was created during
    // `evlis`, that is, if the first element was either CONS or QUOTE.

    // printf("\n===\nargs before evlis:\n");
    // print_s_expr(cdr(e));
    // printf("\n===\n");

    bool maybe_from_list_constructor = false;

    if (is_atom(car(car(cdr(e))))) {
      Atom *check_a = (Atom *)car(car(cdr(e)))->expr;
      if (check_a->type == EXPR) {
        maybe_from_list_constructor = true;
        // if (check_a->pname
        //	&& (0 == strcmp(check_a->pname, "CONS") || 0 ==
        // strcmp(check_a->pname, "QUOTE"))) 	maybe_from_list_constructor =
        // true;
      }
    }

    bool args_were_list =
        (false == is_atom(cdr(e)) && false == is_atom(cdr(cdr(e))));

    print_debug_info("eval - cdr(e):", cdr(e));
    S_Expr *args = evlis(cdr(e), *symbol_table);
    print_debug_info("args:", args);

    r = apply(car(e), args, *symbol_table, maybe_from_list_constructor,
              args_were_list);
  }

end_eval:

  print_debug_info("eval (r):", r);

  return r;
}

S_Expr *evlis(S_Expr *e, S_Expr *symbol_table)
{
  print_debug_info("evlis:", e);

  if (!e || is_nil_atom(e))
    return &nil_s_expr;

  S_Expr *evlis_r = evlis(cdr(e), symbol_table);

  print_debug_info("evlis returning:", evlis_r);

  // The following little block takes care of the case
  // when the first element of the list being evaluated
  // is actually a function, so it should not be cons'ed
  // with NIL but rather just evaluated.
  //
  S_Expr *e_cared = car(e);
  print_debug_info("e cared:", e_cared);
  if (!is_atom(e_cared)) {
    S_Expr *head = ((Cell *)e_cared->expr)->left;
    Atom *head_a = (Atom *)head->expr;
    if (head_a->pname) {
      S_Expr *l = lookup_symbol_table(head_a->pname, symbol_table, NULL);
      if (l != NULL) {
        Atom *l_a = (Atom *)l->expr;
        if (l_a->len > 0) {
          S_Expr *e2 = eval(e_cared, &symbol_table);
          {
            // Additional debug info
            // char dbg_msg[256];
            // sprintf(dbg_msg, "evlis -- function %s evaluated to: ",
            // head_a->pname); print_debug_info(dbg_msg, e2);
            // print_debug_info("evlis_r: ", evlis_r);
          }

          if (is_nil_atom(evlis_r))
            return e2;
          else
            return cons(e2, evlis_r);
        }
      }
    }
  }

  S_Expr *c = (false == is_nil_atom(evlis_r))
                  ? cons(eval(car(e), &symbol_table), evlis_r)
                  : eval(car(e), &symbol_table);

  print_debug_info("evlis consed: ", c);

  return c;
}

//-----------------------------------------------------------------------------

void init(void)
{
  memset(mem_segments, 0x00, MEM_NUM_SEGMENTS * MEM_SEGMENT_SIZE);

  car_fn_symbol = &subr_symbol_defs[CAR_SYM_DEF];
  cdr_fn_symbol = &subr_symbol_defs[CDR_SYM_DEF];
  cons_fn_symbol = &subr_symbol_defs[CONS_SYM_DEF];
  atom_fn_symbol = &subr_symbol_defs[ATOM_SYM_DEF];
  eq_fn_symbol = &subr_symbol_defs[EQ_SYM_DEF];

  quote_fn_symbol = &subr_symbol_defs[QUOTE_SYM_DEF];
  label_fn_symbol = &subr_symbol_defs[LABEL_SYM_DEF];

  lambda_fn_symbol = &subr_symbol_defs[LAMBDA_SYM_DEF];
  flambda_fn_symbol = &subr_symbol_defs[FLAMBDA_SYM_DEF];
  cond_fn_symbol = &subr_symbol_defs[COND_SYM_DEF];

  eval_fn_symbol = &subr_symbol_defs[EVAL_SYM_DEF];

  plus_fn_symbol = &subr_symbol_defs[PLUS_SYM_DEF];
  minus_fn_symbol = &subr_symbol_defs[MINUS_SYM_DEF];
  multiply_fn_symbol = &subr_symbol_defs[MULTIPLY_SYM_DEF];
  divide_fn_symbol = &subr_symbol_defs[DIVIDE_SYM_DEF];
  lt_fn_symbol = &subr_symbol_defs[LT_SYM_DEF];
  gt_fn_symbol = &subr_symbol_defs[GT_SYM_DEF];
  print_fn_symbol = &subr_symbol_defs[PRINT_SYM_DEF];

  global_symbols_table =
    cons(cons(make_string_atom("CAR", 3), car_fn_symbol),
    cons(cons(make_string_atom("CDR", 3), cdr_fn_symbol),
    cons(cons(make_string_atom("CONS", 4), cons_fn_symbol),
    cons(cons(make_string_atom("ATOM", 4), atom_fn_symbol),
    cons(cons(make_string_atom("EQ", 2), eq_fn_symbol),
    cons(cons(make_string_atom("QUOTE", 5), quote_fn_symbol),
    cons(cons(make_string_atom("LABEL", 5), label_fn_symbol),
    cons(cons(make_string_atom("LAMBDA", 6), lambda_fn_symbol),
    cons(cons(make_string_atom("COND", 4), cond_fn_symbol),
    cons(cons(make_string_atom("EVAL", 4), eval_fn_symbol),
    cons(cons(make_string_atom("FLAMBDA", 7), flambda_fn_symbol),
    cons(cons(make_string_atom("PRINT", 5), print_fn_symbol),
    cons(cons(make_string_atom("+", 1), plus_fn_symbol),
    cons(cons(make_string_atom("+", 1), plus_fn_symbol),
    cons(cons(make_string_atom("-", 1), minus_fn_symbol),
    cons(cons(make_string_atom("*", 1), multiply_fn_symbol),
    cons(cons(make_string_atom("/", 1), divide_fn_symbol),
    cons(cons(make_string_atom("<", 1), lt_fn_symbol),
    cons(cons(make_string_atom(">", 1), gt_fn_symbol),
    cons(cons(make_string_atom("NIL", 3), &nil_s_expr),
    &nil_s_expr))))))))))))))))))));

  return;
}

//-----------------------------------------------------------------------------
// PARSER
//-----------------------------------------------------------------------------

S_Expr *parse_number(char *number_str, int *consumed);
S_Expr *parse_string(char *string_str, int *consumed);
S_Expr *parse_symbol(char *symbol_str, int *consumed);
S_Expr *parse_bool_atom(char *input_str, int *consumed);
S_Expr *parse_list(char *input_str, int *consumed);
S_Expr *parse_input(char *input_str, int *consumed);

//-----------------------------------------------------------------------------

S_Expr *parse_number(char *number_str, int *consumed)
{
  if (*number_str == '\0')
    return NULL;

#define NUM_MAX_LEN 10 //+1

  int number = 0;
  char *p = number_str;
  bool is_negative = false;

  // for negatives advance the initial '-'
  if (p[0] == '-') {
    p++;
    (*consumed)++;
    is_negative = true;
  }

  while (*p && (*p >= '0' && *p <= '9') && (p - number_str < NUM_MAX_LEN)) {
    number = (number * 10) + ((*p) - '0');
    p++;
  }

  if ((p - number_str) == NUM_MAX_LEN) {
    jinni_exception("integer out of range!");
  }

  if (is_negative)
    number *= -1;

  S_Expr *se = make_number_atom(number);

  *consumed = p - number_str;

  double *d = (double *)(((Atom *)(se->expr))->apval);
  return se;
}

S_Expr *parse_string(char *string_str, int *consumed)
{
  if (*string_str == '\0')
    return NULL;

#define STR_MAX_LEN 1024

  char string[STR_MAX_LEN] = {0};

  char *p = string_str;

  // advance the initial '"'
  p++;
  (*consumed)++;

  while (*p && *p != '"' && strlen(string) < STR_MAX_LEN - 1) {
    sprintf(&string[strlen(string)], "%c", *p);
    p++;
  }

  // make a STRING atom with an apval for this symbol
  S_Expr *se = make_string_atom(string, strlen(string));

  *consumed = p - string_str;

  // advance the '"' char.
  p++;
  (*consumed)++;

  return se;
}

S_Expr *parse_symbol(char *symbol_str, int *consumed)
{
  if (*symbol_str == '\0')
    return NULL;

#define SYMBOL_NAME_MAX_LEN 256

  char symbol_name[SYMBOL_NAME_MAX_LEN] = {0};

  char *p = symbol_str;
  while (*p && *p != ' ' && *p != '(' && *p != ')' &&
         strlen(symbol_name) < SYMBOL_NAME_MAX_LEN - 1) {
    sprintf(&symbol_name[strlen(symbol_name)], "%c", (char)toupper(*p));
    p++;
  }

  *consumed = p - symbol_str;

  if (strcmp(symbol_name, "NIL") == 0) {
    return &nil_s_expr;
  }

  // make an EXPR atom with a pname for this symbol
  S_Expr *se = make_atom();
  ((Atom *)(se->expr))->type = EXPR;
  ((Atom *)(se->expr))->pname =
      jinni_malloc(sizeof(char) * (strlen(symbol_name) + 1));
  strcpy((((Atom *)(se->expr))->pname), symbol_name);

  return se;
}

S_Expr *parse_bool_atom(char *input_str, int *consumed)
{
  if (*input_str == '\0')
    return NULL;

  if (*input_str == 'T') {
    *consumed = 1;
    return &true_s_expr; // make_bool_atom(true);
  } else if (*input_str == 'F') {
    *consumed = 1;
    return &false_s_expr; // make_bool_atom(false);
  } else {
    jinni_exception("in %s", __FUNCTION__);
  }

  return NULL;
}

S_Expr *parse_list(char *input_str, int *consumed)
{
  if (*input_str == '\0')
    return NULL;

  S_Expr *list = NULL;
  S_Expr *new_list = NULL;

  char *p = input_str;
  p++; // advance the '(' character
  (*consumed)++;

  int consumed_loop = 0;
  int consumed_local = 0;

  while (*p && *p != ')') {
    if (*p == ' ') {
      p++;
      consumed_local++;
      continue;
    }

    S_Expr *elem = parse_input(p, &consumed_loop);

    if (elem == NULL) {
      elem = &nil_s_expr;
    }

    if (!list) {
      list = cons(elem, &nil_s_expr);
    } else {
      S_Expr *new_elem = cons(elem, &nil_s_expr);
      new_list = append(list, new_elem);
      list = new_list;
    }

    p += consumed_loop;
    *consumed += (consumed_loop + consumed_local);
    consumed_loop = 0;
    consumed_local = 0;
  }

  if (*p && *p != ')')
    jinni_exception("error: unexpected end of list");

  p++;
  *consumed += consumed_local;
  (*consumed)++;

  print_debug_info("returning list: ", list);

  return list;
}

/**
 * Parse input until it finds a whitespace (not in a string),
 * a newline char or end of input.
 */
S_Expr *parse_input(char *input_str, int *consumed)
{
  if (*input_str == '\0')
    return NULL;

  char *p = input_str;

  if ((*p == 'T' || *p == 'F') &&
        ((*(p + 1) == '\0' || *(p + 1) == '\n') || *(p + 1) == ' ')) {

    return parse_bool_atom(p, consumed);

  } else if (*p == '(') {

    return parse_list(p, consumed);

  } else if (*p == '"') {

    return parse_string(p, consumed);

  } else if (*p >= '0' && *p <= '9') {

    return parse_number(p, consumed);

  } else {

    // catch-all for symbols
    return parse_symbol(p, consumed);
  }

  return NULL;
}

//-----------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------

void exit_print_msg(void) { printf("Bye bye!\n\n"); }

void exit_on_signal(int sig_num)
{
  exit_print_msg();
  exit(0);
}

void init_signals(void)
{
  int sigs[] = {SIGINT, SIGHUP, SIGTERM};
  struct sigaction sact;

  for (int i = 0; i < ARRAY_ELEMENTS(sigs); i++) {
    sact.sa_handler = exit_on_signal;
    sigemptyset(&sact.sa_mask);
    sact.sa_flags = 0;

    if (sigaction(sigs[i], &sact, NULL)) {
      printf("sigaction() failed (%s)", strerror(errno));
      exit(1);
    }
  }
  return;
}

int main(void)
{
  init();
  init_signals();

#define INPUT_MAX_SZ 4096

  char input[INPUT_MAX_SZ] = {0};

  printf("* ");
  while (fgets(input, sizeof input, stdin) && !feof(stdin)) {
    S_Expr *se;
    int consumed = 0;

    if (input[strlen(input) - 1] == '\n')
      input[strlen(input) - 1] = '\0';

    se = parse_input(input, &consumed);

    if (!setjmp(ex_buf) && se != NULL) {

      S_Expr *res = eval(se, &global_symbols_table);

      printf("\n");
      __print_s_expr(res, PRINT_PARENT_NONE);

    } else {
      printf("evaluation ended abnormally\n\n");
    }

    free_mem_segments(1, MEM_NUM_SEGMENTS);
    printf("\n* ");
  }

  exit_print_msg();
  return 0;
}
