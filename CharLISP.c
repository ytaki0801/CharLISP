//
// CharLISP.c: C version of CharLISP
//
// This code is Licensed under CC0.
// https://creativecommons.org/publicdomain/zero/1.0/
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

#define SSTR_MAX 4096

// Basic functions for conscel operations:
// cons, car, cdr, eq, atom

typedef uintptr_t value_t;
enum NODE_TAG { NODE_CHAR, NODE_NUMS, NODE_CONS };

typedef struct _node_t_ {
  value_t value;
  enum NODE_TAG tag;
} _node_t, *node_t;

node_t node(value_t value, enum NODE_TAG tag)
{
  node_t n = malloc(sizeof(node_t));
  n->value = value; n->tag = tag;
  return (n);
}

typedef struct _cons_t_ {
  node_t x, y;
} _cons_t, *cons_t;

#define chr_to_node(s)  (node((value_t)(s), NODE_CHAR))
#define num_to_node(s)  (node((value_t)(s), NODE_NUMS))
#define node_to_chr(s)  ((char)(s->value))
#define node_to_num(s)  ((int)(s->value))

#define n_char(s)  (s->tag == NODE_CHAR)
#define n_cons(s)  (s->tag == NODE_CONS)
#define n_nums(s)  (s->tag == NODE_NUMS)

int eq(node_t s1, node_t s2);
#define atom(s)   (eq(s, NULL) || n_char(s) || n_nums(s))

node_t car(node_t s) {
  if (s == NULL || atom(s)) return NULL;
  else return ((cons_t)(s->value))->x;
}

node_t cdr(node_t s) {
  if (s == NULL || atom(s)) return NULL;
  else return ((cons_t)(s->value))->y;
}

node_t cons(node_t x, node_t y)
{
  cons_t c = malloc(sizeof(cons_t));
  c->x = x; c->y = y;
  node_t n = node((value_t)c, NODE_CONS);
  return (n);
}

int eq(node_t s1, node_t s2)
{
  if (s1 == NULL && s2 == NULL)
    return (1);
  else if (s1 == NULL || s2 == NULL)
    return (0);
  else if (n_cons(s1) || n_cons(s2))
    return (0);
  else if (n_nums(s1) && n_nums(s2))
    return (node_to_num(s1) == node_to_num(s2));
  else if (n_char(s1) && n_char(s2))
    return (node_to_chr(s1) == node_to_chr(s2));
  else return 0;
}

// S-expression syntax analysis: c_syn

node_t c_syn(const char *s, int *pos)
{
  char t = s[*pos];
  *pos = *pos - 1;
  if (t == ')') {
    node_t r = NULL;
    while (s[*pos] != '(') {
      if (s[*pos] == '.') {
        *pos = *pos - 1;
	r = cons(c_syn(s, pos), car(r));
      } else
        r = cons(c_syn(s, pos), r);
    }
    *pos = *pos - 1;
    return (r);
  } else {
    node_t tn;
    if (isdigit(t)) tn = num_to_node(t-'0');
    else            tn = chr_to_node(t);
    return (tn);
  }
}

// S-expression output: c_string

char c_eval_retval[SSTR_MAX];
void c_string(node_t s);

void c_strcons(node_t s)
{
  c_string(car(s));
  node_t sd = cdr(s);
  if (sd == NULL) {
  } else if (n_char(sd) || n_nums(sd)) {
    strcat(c_eval_retval, " . ");
    if (n_char(sd)) {
      char sds[] = { '\0', '\0' };
      sds[0] = node_to_chr(sd);
      strcat(c_eval_retval, sds);
    } else {
      char *nums = malloc(256*sizeof(*nums));
      sprintf(nums, "%d", node_to_num(sd));
      strcat(c_eval_retval, nums);
    }
  } else {
    strcat(c_eval_retval, " ");
    c_strcons(sd);
  }
}

void c_string(node_t s)
{
  if (s == NULL) {
    strcat(c_eval_retval, "nil");
  } else if (n_char(s)) {
    char ss[] = { '\0', '\0' };
    ss[0] = node_to_chr(s);
    strcat(c_eval_retval, ss);
  } else if (n_nums(s)) {
    char *nums = malloc(256*sizeof(*nums));
    sprintf(nums, "%d", node_to_num(s));
    strcat(c_eval_retval, nums);
  } else {
    strcat(c_eval_retval, "(");
    c_strcons(s);
    strcat(c_eval_retval, ")");
  }
}

// The evaluator: c_eval and utility functions

node_t c_eval(node_t e, node_t a);

node_t caar(node_t x) { return car(car(x)); }
node_t cadr(node_t x) { return car(cdr(x)); }
node_t cdar(node_t x) { return cdr(car(x)); }
node_t cadar(node_t x) { return car(cdr(car(x))); }
node_t caddr(node_t x) { return car(cdr(cdr(x))); }
node_t cadddr(node_t x) { return car(cdr(cdr(cdr(x)))); }

node_t c_append(node_t x, node_t y)
{
  if (x == NULL) return y;
  else return cons(car(x), c_append(cdr(x), y));
}

node_t c_pair(node_t x, node_t y)
{
  if (x == NULL || y == NULL) return NULL;
  else if (!atom(x) && !atom(y))
    return cons(cons(car(x), car(y)), c_pair(cdr(x), cdr(y)));
  else if (atom(x)) return cons(cons(x, y), NULL);
  else return NULL;
}

node_t c_assq(node_t k, node_t v)
{
  if (v == NULL) return NULL;
  else if (eq(k, caar(v))) return cdar(v);
  else return c_assq(k, cdr(v));
}

node_t c_reverse(node_t x)
{
  if (x == NULL) return NULL;
  else return c_append(c_reverse(cdr(x)), cons(car(x), NULL));
}

#define C_ADD (chr_to_node('+'))
#define C_SUB (chr_to_node('-'))
#define C_MUL (chr_to_node('*'))
#define C_MOD (chr_to_node('%'))
#define C_EQL (chr_to_node('='))
#define C_LTN (chr_to_node('<'))
#define C_CON (chr_to_node('$'))
#define C_CAR (chr_to_node('['))
#define C_CDR (chr_to_node(']'))

node_t c_lookup(node_t t, node_t a)
{
  if (t == NULL) return NULL;
  else if (n_nums(t)) return t;
  else if (eq(t, C_ADD) || eq(t, C_SUB) || eq(t, C_MUL) || eq(t, C_MOD) ||
           eq(t, C_EQL) || eq(t, C_LTN) ||
	   eq(t, C_CON) || eq(t, C_CAR) || eq(t, C_CDR))
    return t;
  else
    return c_assq(t, a);
}

node_t c_apply(node_t f, node_t v)
{
  if      (eq(f, C_CON)) return cons(car(v), cadr(v));
  else if (eq(f, C_CAR)) return car(car(v));
  else if (eq(f, C_CDR)) return cdr(car(v));
  else if (eq(f, C_EQL)) return num_to_node(eq(car(v), cadr(v)));
  else if (n_nums(car(v)) && n_nums(cadr(v))) {
    int a1 = (node_to_num(car(v)));
    int a2 = (node_to_num(cadr(v)));
    if      (eq(f, C_ADD)) return num_to_node(a1 + a2);
    else if (eq(f, C_SUB)) return num_to_node(a1 - a2);
    else if (eq(f, C_MUL)) return num_to_node(a1 * a2);
    else if (eq(f, C_MOD)) return num_to_node(a1 % a2);
    else if (eq(f, C_LTN)) return num_to_node(a1 < a2);
    else return NULL;
  } else return NULL;
}
 
#define C_QUOTE  (chr_to_node('\''))
#define C_IF     (chr_to_node('?'))
#define C_LAMBDA (chr_to_node(':'))

node_t c_evals(node_t v, node_t a)
{
  if (v == NULL) return NULL;
  else return cons(c_eval(car(v), a), c_evals(cdr(v), a));
}

node_t c_eval(node_t e, node_t a)
{
  while (1) {
    if (atom(e)) return c_lookup(e, a);
    else if (eq(car(e), C_QUOTE)) {
      node_t vals = cadr(e);
      return vals;
    } else if (eq(car(e), C_IF)) {
      if (node_to_num(c_eval(cadr(e), a)))
        return c_eval(caddr(e), a);
      else
        return c_eval(cadddr(e), a);
    } else if (eq(car(e), C_LAMBDA)) {
      node_t r = c_reverse(cdr(e));
      node_t body = car(r);
      node_t vars = c_reverse(cdr(r));
      return cons(vars, cons(body, cons(a, NULL)));
    } else {
      node_t efunc = c_eval(car(e), a);
      node_t fvals = c_evals(cdr(e), a);
      if (atom(efunc)) return c_apply(efunc, fvals);
      else {
        node_t lvars = car(efunc);
        e            = cadr(efunc);
        node_t lenvs = caddr(efunc);
        if (lvars == NULL) a = lenvs;
        else a = c_append(c_pair(lvars, fvals), lenvs);
      }
    }
  }
}

// Interface of eval_string to C

void c_eval_string(const char *s)
{
  int c_len = strlen(s) - 1;
  node_t rs = c_syn(s, &c_len);
  node_t r = c_eval(rs, NULL);
  c_eval_retval[0] = '\0';
  c_string(r);
}

int main(int argc, char *argv[])
{
  char in[SSTR_MAX];

  do {
    printf("CharLISP> ");
    scanf("%s", in);
    c_eval_retval[0] = '\0';
    c_eval_string(in);
    printf("%s\n", c_eval_retval);
  } while (1);

  return (0);
}

/*
Examples:
('(Hello!!)) => (H e l l o ! !)
((:nr($nr))12) => (1 . 2)
((:n(?(=(%n3)0)('Y)('N)))6) => Y
((:x($([(]x))($([x)(](]x)))))('(abc))) => (b a c)
*/

