/* r.c (marv reader and front end) -- [cco]
 *
 * Written in the WCPL C dialect so the result can compile itself: no statics in
 * function scope, no structs passed or returned by value across the dialect's
 * limits, explicit NULL tests, explicit casts.
 */

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "m.h"
#include "r.h"

/* ---------------------------------------------------------------- nodes */

snode_t mksn(void)
{
  snode_t nd;
  sninit(&nd);
  return nd;
}

snode_t *sninit(snode_t *pn)
{
  memset(pn, 0, sizeof(snode_t));
  pn->nt = SN_NULL;
  bufinit(&pn->data, sizeof(char));
  snbinit(&pn->body);
  return pn;
}

void snfini(snode_t *pn)
{
  size_t i;
  for (i = 0; i < snlen(pn); ++i) snfini(snref(pn, i));
  buffini(&pn->body);
  buffini(&pn->data);
}

void snclear(snode_t *pn)
{
  size_t i;
  for (i = 0; i < snlen(pn); ++i) snfini(snref(pn, i));
  bufclear(&pn->body);
  bufclear(&pn->data);
  pn->nt = SN_NULL;
  pn->sym = 0;
  pn->ival = 0;
  pn->dval = 0.0;
}

void snbfini(snbuf_t *pb)
{
  size_t i;
  for (i = 0; i < buflen(pb); ++i) snfini((snode_t*)bufref(pb, i));
  buffini(pb);
}

snode_t *sncpy(snode_t *pdst, const snode_t *psrc)
{
  size_t i;
  snclear(pdst);
  pdst->nt = psrc->nt;
  pdst->fname = psrc->fname;
  pdst->line = psrc->line;
  pdst->col = psrc->col;
  pdst->sym = psrc->sym;
  pdst->ival = psrc->ival;
  pdst->dval = psrc->dval;
  bufcpy(&pdst->data, &((snode_t*)psrc)->data);
  for (i = 0; i < snlen((snode_t*)psrc); ++i)
    sncpy(snnewbk(pdst), snref((snode_t*)psrc, i));
  return pdst;
}

snode_t *snnewbk(snode_t *pn)
{
  return sninit((snode_t*)bufnewbk(&pn->body));
}

/* ---------------------------------------------------------------- errors */

void sneprintf(const snode_t *pn, const char *fmt, ...)
{
  va_list args;
  const char *fn;
  fn = symname(pn->fname);
  if (fn == NULL) fn = "<input>";
  fprintf(stderr, "%s:%d:%d: ", fn, pn->line, pn->col);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, "\n");
  exit(1);
}

void sreprintf(sread_t *pr, const char *fmt, ...)
{
  va_list args;
  const char *fn;
  fn = symname(pr->fname);
  if (fn == NULL) fn = "<input>";
  fprintf(stderr, "%s:%d:%d: ", fn, pr->tline, pr->tcol);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, "\n");
  exit(1);
}

/* ---------------------------------------------------------------- input */

static int srgetc(sread_t *pr)
{
  int c;
  c = pr->ch;
  if (c == EOF) return EOF;
  if (c == '\n') { pr->line += 1; pr->col = 1; }
  else pr->col += 1;
  pr->ch = fgetc(pr->fp);
  return c;
}

bool sropen(sread_t *pr, const char *fname)
{
  FILE *fp;
  memset(pr, 0, sizeof(sread_t));
  bufinit(&pr->tok, sizeof(char));
  fp = fopen(fname, "rb");
  if (fp == NULL) return false;
  pr->fp = fp;
  pr->fname = intern(fname);
  pr->line = 1;
  pr->col = 1;
  pr->tline = 1;
  pr->tcol = 1;
  pr->ch = fgetc(fp);
  return true;
}

void srclose(sread_t *pr)
{
  if (pr->fp != NULL) { fclose(pr->fp); pr->fp = NULL; }
  buffini(&pr->tok);
}

static bool sr_delim(int c)
{
  return c == EOF || c == '(' || c == ')' || c == '"' || c == ';'
      || c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f';
}

static void sr_skipspace(sread_t *pr)
{
  int c;
  for (;;) {
    c = pr->ch;
    if (c == ';') {                          /* line comment */
      while (pr->ch != EOF && pr->ch != '\n') srgetc(pr);
      continue;
    }
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f') {
      srgetc(pr);
      continue;
    }
    return;
  }
}

/* ---------------------------------------------------------------- numbers */

static int sr_hexval(int c)
{
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  return -1;
}

/* a token that starts with #x: integer or SRFI-270 hex float */
static void sr_hexnum(sread_t *pr, const char *s, snode_t *pn)
{
  unsigned long long mant;
  int i, d, frac, neg, sawdot, sawp, pexp, psign, any;
  mant = 0; i = 0; frac = 0; neg = 0; sawdot = 0; sawp = 0; pexp = 0; psign = 1; any = 0;
  if (s[i] == '-') { neg = 1; i += 1; }
  else if (s[i] == '+') i += 1;
  for (; s[i] != '\0'; ++i) {
    if (s[i] == '.') {
      if (sawdot != 0) sreprintf(pr, "malformed hex number: %s", s);
      sawdot = 1;
      continue;
    }
    if (s[i] == 'p' || s[i] == 'P') { sawp = 1; i += 1; break; }
    d = sr_hexval(s[i]);
    if (d < 0) sreprintf(pr, "malformed hex number: %s", s);
    /* keep at most 16 hex digits of significance; the rest only shifts */
    if (mant <= (unsigned long long)0x0FFFFFFFFFFFFFFFULL) {
      mant = (mant << 4) | (unsigned long long)d;
      if (sawdot != 0) frac += 1;
    } else if (sawdot == 0) {
      pexp += 4;
    }
    any = 1;
  }
  if (any == 0) sreprintf(pr, "malformed hex number: %s", s);
  if (sawp != 0) {
    int e;
    e = 0;
    if (s[i] == '-') { psign = -1; i += 1; }
    else if (s[i] == '+') i += 1;
    if (s[i] == '\0') sreprintf(pr, "malformed hex float exponent: %s", s);
    for (; s[i] != '\0'; ++i) {
      if (s[i] < '0' || s[i] > '9') sreprintf(pr, "malformed hex float exponent: %s", s);
      e = e * 10 + (s[i] - '0');
      if (e > 100000) sreprintf(pr, "hex float exponent out of range: %s", s);
    }
    pexp += psign * e;
  }
  if (sawdot != 0 || sawp != 0) {
    double v;
    v = ldexp((double)mant, pexp - 4 * frac);
    pn->nt = SN_FLT;
    pn->dval = (neg != 0) ? -v : v;
  } else {
    pn->nt = SN_INT;
    pn->ival = (neg != 0) ? -(long long)mant : (long long)mant;
  }
}

/* does this token look like a decimal number rather than a symbol? */
/* +inf.0, -inf.0, +nan.0, -nan.0: a float value with no numeral */
static int sr_named_float(const char *s, double *pd)
{
  int neg;
  if (s[0] != '+' && s[0] != '-') return 0;
  neg = (s[0] == '-');
  if (streql(s + 1, "inf.0")) {
    *pd = neg ? -HUGE_VAL : HUGE_VAL;
    return 1;
  }
  if (streql(s + 1, "nan.0")) {
    *pd = HUGE_VAL - HUGE_VAL;
    return 1;
  }
  return 0;
}

static bool sr_isnum(const char *s)
{
  int i;
  double d;
  if (sr_named_float(s, &d)) return true;
  i = 0;
  if (s[i] == '+' || s[i] == '-') i += 1;
  if (s[i] < '0' || s[i] > '9') return false;
  return true;
}

static void sr_decnum(sread_t *pr, const char *s, snode_t *pn)
{
  int i, isflo;
  double named;
  if (sr_named_float(s, &named)) {
    pn->nt = SN_FLT;
    pn->dval = named;
    return;
  }
  i = 0; isflo = 0;
  if (s[i] == '+' || s[i] == '-') i += 1;
  for (; s[i] != '\0'; ++i) {
    if (s[i] == '.' || s[i] == 'e' || s[i] == 'E') { isflo = 1; continue; }
    if (s[i] == '+' || s[i] == '-') continue;      /* exponent sign */
    if (s[i] < '0' || s[i] > '9') sreprintf(pr, "malformed number: %s", s);
  }
  if (isflo != 0) {
    pn->nt = SN_FLT;
    pn->dval = strtod(s, NULL);
  } else {
    pn->nt = SN_INT;
    pn->ival = strtoll(s, NULL, 10);
  }
}

/* ---------------------------------------------------------------- tokens */

static void sr_string(sread_t *pr, snode_t *pn)
{
  int c;
  pn->nt = SN_STR;
  srgetc(pr);                                 /* the opening quote */
  for (;;) {
    c = srgetc(pr);
    if (c == EOF) sreprintf(pr, "unterminated string");
    if (c == '"') break;
    if (c != '\\') { *(char*)bufnewbk(&pn->data) = (char)c; continue; }
    c = srgetc(pr);
    switch (c) {
      case 'n': *(char*)bufnewbk(&pn->data) = '\n'; break;
      case 't': *(char*)bufnewbk(&pn->data) = '\t'; break;
      case 'r': *(char*)bufnewbk(&pn->data) = '\r'; break;
      case 'a': *(char*)bufnewbk(&pn->data) = '\a'; break;
      case 'b': *(char*)bufnewbk(&pn->data) = '\b'; break;
      case '0': *(char*)bufnewbk(&pn->data) = '\0'; break;
      case '\\': *(char*)bufnewbk(&pn->data) = '\\'; break;
      case '"': *(char*)bufnewbk(&pn->data) = '"'; break;
      case 'x': case 'X': {
        unsigned long v; int d;
        v = 0;
        for (;;) {
          d = sr_hexval(pr->ch);
          if (d < 0) break;
          v = v * 16 + (unsigned long)d;
          srgetc(pr);
          if (v > 0x10FFFFUL) sreprintf(pr, "character escape out of range");
        }
        if (pr->ch != ';') sreprintf(pr, "unterminated \\x escape: ';' expected");
        srgetc(pr);
        if (v < 0x80) {
          *(char*)bufnewbk(&pn->data) = (char)v;
        } else {                              /* encode as utf-8, as R7RS says */
          unsigned char ub[8]; unsigned char *ue; unsigned char *up;
          ue = utf8((unsigned long)v, &ub[0]);
          for (up = &ub[0]; up < ue; ++up) *(char*)bufnewbk(&pn->data) = (char)*up;
        }
        break;
      }
      case '\n': {                            /* line continuation: skip space */
        while (pr->ch == ' ' || pr->ch == '\t') srgetc(pr);
        break;
      }
      default: sreprintf(pr, "unsupported string escape: \\%c", c);
    }
  }
}

/* R7RS characters denote integer Unicode scalar values in MARV. Character
 * names are case-sensitive; hex digits and the x prefix are case-insensitive. */
static void sr_character(sread_t *pr, snode_t *pn)
{
  const unsigned char *s;
  size_t n, i, expected;
  unsigned long v, minimum;
  int d;
  if (pr->ch == EOF) sreprintf(pr, "incomplete character literal");
  bufclear(&pr->tok);
  /* A delimiter can itself be the character, e.g. #\\) or a literal space. */
  if (sr_delim(pr->ch)) {
    v = (unsigned long)srgetc(pr);
    if (!sr_delim(pr->ch)) sreprintf(pr, "character literal must be followed by a delimiter");
    pn->nt = SN_INT; pn->ival = (long long)v;
    return;
  }
  while (!sr_delim(pr->ch)) *(char*)bufnewbk(&pr->tok) = (char)srgetc(pr);
  n = buflen(&pr->tok);
  *(char*)bufnewbk(&pr->tok) = '\0';
  s = (const unsigned char *)bufdata(&pr->tok);
  if (n > 1) {
    for (i = 0; i < n; ++i)
      if (s[i] == 0) sreprintf(pr, "invalid NUL in character literal");
  }
  if (n == 1 && s[0] < 0x80) v = s[0];
  else if (n > 1 && (s[0] == 'x' || s[0] == 'X')) {
    v = 0;
    for (i = 1; i < n; ++i) {
      d = sr_hexval(s[i]);
      if (d < 0) sreprintf(pr, "invalid hexadecimal character literal");
      if (v > 0x10FFFFUL / 16UL) sreprintf(pr, "character literal out of Unicode range");
      v = v * 16UL + (unsigned long)d;
      if (v > 0x10FFFFUL) sreprintf(pr, "character literal out of Unicode range");
    }
    if (v >= 0xD800UL && v <= 0xDFFFUL)
      sreprintf(pr, "character literal is not a Unicode scalar value");
  } else if (streql((const char *)s, "alarm")) v = 7;
  else if (streql((const char *)s, "backspace")) v = 8;
  else if (streql((const char *)s, "delete")) v = 127;
  else if (streql((const char *)s, "escape")) v = 27;
  else if (streql((const char *)s, "newline")) v = 10;
  else if (streql((const char *)s, "null")) v = 0;
  else if (streql((const char *)s, "return")) v = 13;
  else if (streql((const char *)s, "space")) v = 32;
  else if (streql((const char *)s, "tab")) v = 9;
  else {
    if (s[0] >= 0xC2 && s[0] <= 0xDF) { expected = 2; v = s[0] & 0x1FUL; minimum = 0x80UL; }
    else if (s[0] >= 0xE0 && s[0] <= 0xEF) { expected = 3; v = s[0] & 0x0FUL; minimum = 0x800UL; }
    else if (s[0] >= 0xF0 && s[0] <= 0xF4) { expected = 4; v = s[0] & 0x07UL; minimum = 0x10000UL; }
    else sreprintf(pr, "unknown or invalid character literal");
    if (n != expected) sreprintf(pr, "character literal must contain exactly one UTF-8 character");
    for (i = 1; i < expected; ++i) {
      if ((s[i] & 0xC0) != 0x80) sreprintf(pr, "invalid UTF-8 in character literal");
      v = (v << 6) | (s[i] & 0x3FUL);
    }
    if (v < minimum || v > 0x10FFFFUL || (v >= 0xD800UL && v <= 0xDFFFUL))
      sreprintf(pr, "invalid UTF-8 Unicode scalar value in character literal");
  }
  pn->nt = SN_INT;
  pn->ival = (long long)v;
}

static void sr_atom(sread_t *pr, snode_t *pn, bool allow_character);

/* #u8(...): each element is a byte, stored as written */
static void sr_bvec(sread_t *pr, snode_t *pn)
{
  snode_t el;
  pn->nt = SN_BVEC;
  srgetc(pr);                                 /* the '(' */
  sninit(&el);
  for (;;) {
    sr_skipspace(pr);
    if (pr->ch == EOF) sreprintf(pr, "unterminated #u8(...)");
    if (pr->ch == ')') { srgetc(pr); break; }
    pr->tline = pr->line; pr->tcol = pr->col;
    snclear(&el);
    if (pr->ch == '(' || pr->ch == '"') sreprintf(pr, "#u8(...) takes byte values only");
    sr_atom(pr, &el, false);
    if (el.nt != SN_INT || el.ival < 0 || el.ival > 255)
      sreprintf(pr, "#u8(...) element is not a byte in 0..255");
    *(char*)bufnewbk(&pn->data) = (char)(unsigned char)el.ival;
  }
  snfini(&el);
}

static void sr_atom(sread_t *pr, snode_t *pn, bool allow_character)
{
  char *s;
  bufclear(&pr->tok);
  if (pr->ch == '#') {
    srgetc(pr);
    if (pr->ch == '\\') {
      if (!allow_character) sreprintf(pr, "character literals are not allowed in #u8(...)");
      srgetc(pr);
      sr_character(pr, pn);
      return;
    }
    *(char*)bufnewbk(&pr->tok) = '#';
  }
  while (!sr_delim(pr->ch)) *(char*)bufnewbk(&pr->tok) = (char)srgetc(pr);
  *(char*)bufnewbk(&pr->tok) = '\0';
  s = (char*)bufdata(&pr->tok);
  if (s[0] == '#') {
    if (s[1] == 'x' || s[1] == 'X') { sr_hexnum(pr, s + 2, pn); return; }
    if (streql(s, "#u8")) {
      if (pr->ch != '(') sreprintf(pr, "'(' expected after #u8");
      sr_bvec(pr, pn);
      return;
    }
    sreprintf(pr, "unsupported # syntax: %s", s);
  }
  if (sr_isnum(s)) { sr_decnum(pr, s, pn); return; }
  pn->nt = SN_SYM;
  pn->sym = intern(s);
}

static void sr_form(sread_t *pr, snode_t *pn)
{
  sr_skipspace(pr);
  pr->tline = pr->line;
  pr->tcol = pr->col;
  pn->fname = pr->fname;
  pn->line = pr->line;
  pn->col = pr->col;
  if (pr->ch == EOF) sreprintf(pr, "unexpected end of file");
  if (pr->ch == ')') sreprintf(pr, "unbalanced ')'");
  if (pr->ch == '"') { sr_string(pr, pn); return; }
  if (pr->ch != '(') { sr_atom(pr, pn, true); return; }
  srgetc(pr);
  pn->nt = SN_LIST;
  for (;;) {
    sr_skipspace(pr);
    if (pr->ch == EOF) {
      pr->tline = pn->line; pr->tcol = pn->col;
      sreprintf(pr, "unterminated list");
    }
    if (pr->ch == ')') { srgetc(pr); return; }
    sr_form(pr, snnewbk(pn));
  }
}

bool srform(sread_t *pr, snode_t *pn)
{
  sr_skipspace(pr);
  if (pr->ch == EOF) return false;
  snclear(pn);
  sr_form(pr, pn);
  return true;
}

bool srlopen(sread_t *pr, snode_t *pn)
{
  sr_skipspace(pr);
  if (pr->ch == EOF) return false;
  pr->tline = pr->line;
  pr->tcol = pr->col;
  if (pr->ch != '(') sreprintf(pr, "'(' expected");
  snclear(pn);
  pn->nt = SN_LIST;
  pn->fname = pr->fname;
  pn->line = pr->line;
  pn->col = pr->col;
  srgetc(pr);
  return true;
}

bool srlnext(sread_t *pr, snode_t *pn)
{
  sr_skipspace(pr);
  if (pr->ch == EOF) sreprintf(pr, "unterminated list");
  if (pr->ch == ')') { srgetc(pr); return false; }
  snclear(pn);
  sr_form(pr, pn);
  return true;
}

/* ================================================================ */
/* instruction table: name, kind, immediates, operands, result, access size.
 * Derived once from the Scheme implementation's *instr-table*; this copy is
 * the source of truth now. 192 instructions. [cco] */

static const instr_t g_instrtab_[] = {
  { "i32.const",            IK_CONST,              1, 0, { VT_VOID },               VT_I32,  4 },
  { "i64.const",            IK_CONST,              1, 0, { VT_VOID },               VT_I64,  8 },
  { "f32.const",            IK_CONST,              1, 0, { VT_VOID },               VT_F32,  4 },
  { "f64.const",            IK_CONST,              1, 0, { VT_VOID },               VT_F64,  8 },
  { "ptr.null",             IK_CONST,              0, 0, { VT_VOID },               VT_PTR,  8 },
  { "ref.data",             IK_REF_DATA,           1, 0, { VT_VOID },               VT_PTR,  8 },
  { "ref.func",             IK_REF_FUNC,           1, 0, { VT_VOID },               VT_PTR,  8 },
  { "frame.addr",           IK_FRAME_ADDR,         1, 0, { VT_VOID },               VT_PTR,  8 },
  { "local.get",            IK_LOCAL_GET,          1, 0, { VT_VOID },               VT_ANY,  0 },
  { "local.set",            IK_LOCAL_SET,          1, 1, { VT_ANY },                VT_VOID, 0 },
  { "local.tee",            IK_LOCAL_TEE,          1, 1, { VT_ANY },                VT_ANY,  0 },
  { "global.get",           IK_GLOBAL_GET,         1, 0, { VT_VOID },               VT_ANY,  0 },
  { "global.set",           IK_GLOBAL_SET,         1, 1, { VT_ANY },                VT_VOID, 0 },
  { "nop",                  IK_NOP,                0, 0, { VT_VOID },               VT_VOID, 0 },
  { "unreachable",          IK_UNREACHABLE,        0, 0, { VT_VOID },               VT_VOID, 0 },
  { "drop",                 IK_DROP,               0, 1, { VT_ANY },                VT_VOID, 0 },
  { "select",               IK_SELECT,             0, 3, { VT_ANY, VT_ANY, VT_I32 }, VT_ANY,  0 },
  { "memory.copy",          IK_MEMORY,             0, 3, { VT_PTR, VT_PTR, VT_I32 }, VT_VOID, 0 },
  { "memory.fill",          IK_MEMORY,             0, 3, { VT_PTR, VT_I32, VT_I32 }, VT_VOID, 0 },
  { "br",                   IK_BR,                 1, 0, { VT_VOID },               VT_VOID, 0 },
  { "br_if",                IK_BR_IF,              1, 1, { VT_I32 },                VT_VOID, 0 },
  { "br_table",             IK_BR_TABLE,           2, 1, { VT_I32 },                VT_VOID, 0 },
  { "return",               IK_RETURN,             0, 0, { VT_VOID },               VT_VOID, 0 },
  { "call",                 IK_CALL,               1, 0, { VT_VOID },               VT_ANY,  0 },
  { "call_indirect",        IK_CALL_INDIRECT,      1, 0, { VT_VOID },               VT_ANY,  0 },
  { "return_call",          IK_RETURN_CALL,        1, 0, { VT_VOID },               VT_VOID, 0 },
  { "trampoline_jump",      IK_RETURN_CALL,        1, 0, { VT_VOID },               VT_VOID, 0 },
  { "trampoline_jump_indirect", IK_RETURN_CALL_INDIRECT, 1, 0, { VT_VOID },         VT_VOID, 0 },
  { "return_call_indirect", IK_RETURN_CALL_INDIRECT, 1, 0, { VT_VOID },               VT_VOID, 0 },
  { "ptr.add",              IK_BINARY,             0, 2, { VT_PTR, VT_I32 },        VT_PTR,  8 },
  { "ptr.add64",            IK_BINARY,             0, 2, { VT_PTR, VT_I64 },        VT_PTR,  8 },
  { "ptr.sub",              IK_BINARY,             0, 2, { VT_PTR, VT_I32 },        VT_PTR,  8 },
  { "ptr.diff",             IK_BINARY,             0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.diff64",           IK_BINARY,             0, 2, { VT_PTR, VT_PTR },        VT_I64,  8 },
  { "ptr.eqz",              IK_UNARY,              0, 1, { VT_PTR },                VT_I32,  4 },
  { "ptr.eq",               IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.ne",               IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.lt_u",             IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.gt_u",             IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.le_u",             IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.ge_u",             IK_COMPARE,            0, 2, { VT_PTR, VT_PTR },        VT_I32,  4 },
  { "ptr.of_i64",           IK_CONVERT,            0, 1, { VT_I64 },                VT_PTR,  8 },
  { "i64.of_ptr",           IK_CONVERT,            0, 1, { VT_PTR },                VT_I64,  8 },
  { "ptr.load",             IK_LOAD,               2, 1, { VT_PTR },                VT_PTR,  8 },
  { "ptr.store",            IK_STORE,              2, 2, { VT_PTR, VT_PTR },        VT_VOID, 8 },
  { "i32.load",             IK_LOAD,               2, 1, { VT_PTR },                VT_I32,  4 },
  { "i32.load8_s",          IK_LOAD,               2, 1, { VT_PTR },                VT_I32,  1 },
  { "i32.load8_u",          IK_LOAD,               2, 1, { VT_PTR },                VT_I32,  1 },
  { "i32.load16_s",         IK_LOAD,               2, 1, { VT_PTR },                VT_I32,  2 },
  { "i32.load16_u",         IK_LOAD,               2, 1, { VT_PTR },                VT_I32,  2 },
  { "i64.load",             IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  8 },
  { "i64.load8_s",          IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  1 },
  { "i64.load8_u",          IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  1 },
  { "i64.load16_s",         IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  2 },
  { "i64.load16_u",         IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  2 },
  { "i64.load32_s",         IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  4 },
  { "i64.load32_u",         IK_LOAD,               2, 1, { VT_PTR },                VT_I64,  4 },
  { "f32.load",             IK_LOAD,               2, 1, { VT_PTR },                VT_F32,  4 },
  { "f64.load",             IK_LOAD,               2, 1, { VT_PTR },                VT_F64,  8 },
  { "i32.store",            IK_STORE,              2, 2, { VT_PTR, VT_I32 },        VT_VOID, 4 },
  { "i32.store8",           IK_STORE,              2, 2, { VT_PTR, VT_I32 },        VT_VOID, 1 },
  { "i32.store16",          IK_STORE,              2, 2, { VT_PTR, VT_I32 },        VT_VOID, 2 },
  { "i64.store",            IK_STORE,              2, 2, { VT_PTR, VT_I64 },        VT_VOID, 8 },
  { "i64.store8",           IK_STORE,              2, 2, { VT_PTR, VT_I64 },        VT_VOID, 1 },
  { "i64.store16",          IK_STORE,              2, 2, { VT_PTR, VT_I64 },        VT_VOID, 2 },
  { "i64.store32",          IK_STORE,              2, 2, { VT_PTR, VT_I64 },        VT_VOID, 4 },
  { "f32.store",            IK_STORE,              2, 2, { VT_PTR, VT_F32 },        VT_VOID, 4 },
  { "f64.store",            IK_STORE,              2, 2, { VT_PTR, VT_F64 },        VT_VOID, 8 },
  { "i32.add",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.sub",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.mul",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.div_s",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.div_u",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.rem_s",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.rem_u",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.and",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.or",               IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.xor",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.shl",              IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.shr_s",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.shr_u",            IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.rotl",             IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.rotr",             IK_BINARY,             0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.clz",              IK_UNARY,              0, 1, { VT_I32 },                VT_I32,  4 },
  { "i32.ctz",              IK_UNARY,              0, 1, { VT_I32 },                VT_I32,  4 },
  { "i32.popcnt",           IK_UNARY,              0, 1, { VT_I32 },                VT_I32,  4 },
  { "i32.eqz",              IK_UNARY,              0, 1, { VT_I32 },                VT_I32,  4 },
  { "i32.eq",               IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.ne",               IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.lt_s",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.lt_u",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.gt_s",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.gt_u",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.le_s",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.le_u",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.ge_s",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i32.ge_u",             IK_COMPARE,            0, 2, { VT_I32, VT_I32 },        VT_I32,  4 },
  { "i64.add",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.sub",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.mul",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.div_s",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.div_u",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.rem_s",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.rem_u",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.and",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.or",               IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.xor",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.shl",              IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.shr_s",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.shr_u",            IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.rotl",             IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.rotr",             IK_BINARY,             0, 2, { VT_I64, VT_I64 },        VT_I64,  8 },
  { "i64.clz",              IK_UNARY,              0, 1, { VT_I64 },                VT_I64,  8 },
  { "i64.ctz",              IK_UNARY,              0, 1, { VT_I64 },                VT_I64,  8 },
  { "i64.popcnt",           IK_UNARY,              0, 1, { VT_I64 },                VT_I64,  8 },
  { "i64.eqz",              IK_UNARY,              0, 1, { VT_I64 },                VT_I32,  4 },
  { "i64.eq",               IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.ne",               IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.lt_s",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.lt_u",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.gt_s",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.gt_u",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.le_s",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.le_u",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.ge_s",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "i64.ge_u",             IK_COMPARE,            0, 2, { VT_I64, VT_I64 },        VT_I32,  4 },
  { "f32.add",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.sub",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.mul",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.div",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.min",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.max",              IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.copysign",         IK_BINARY,             0, 2, { VT_F32, VT_F32 },        VT_F32,  4 },
  { "f32.abs",              IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.neg",              IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.sqrt",             IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.ceil",             IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.floor",            IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.trunc",            IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.nearest",          IK_UNARY,              0, 1, { VT_F32 },                VT_F32,  4 },
  { "f32.eq",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f32.ne",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f32.lt",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f32.gt",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f32.le",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f32.ge",               IK_COMPARE,            0, 2, { VT_F32, VT_F32 },        VT_I32,  4 },
  { "f64.add",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.sub",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.mul",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.div",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.min",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.max",              IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.copysign",         IK_BINARY,             0, 2, { VT_F64, VT_F64 },        VT_F64,  8 },
  { "f64.abs",              IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.neg",              IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.sqrt",             IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.ceil",             IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.floor",            IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.trunc",            IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.nearest",          IK_UNARY,              0, 1, { VT_F64 },                VT_F64,  8 },
  { "f64.eq",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "f64.ne",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "f64.lt",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "f64.gt",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "f64.le",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "f64.ge",               IK_COMPARE,            0, 2, { VT_F64, VT_F64 },        VT_I32,  4 },
  { "i32.wrap_i64",         IK_CONVERT,            0, 1, { VT_I64 },                VT_I32,  4 },
  { "i64.extend_i32_s",     IK_CONVERT,            0, 1, { VT_I32 },                VT_I64,  8 },
  { "i64.extend_i32_u",     IK_CONVERT,            0, 1, { VT_I32 },                VT_I64,  8 },
  { "i32.extend8_s",        IK_CONVERT,            0, 1, { VT_I32 },                VT_I32,  4 },
  { "i32.extend16_s",       IK_CONVERT,            0, 1, { VT_I32 },                VT_I32,  4 },
  { "i64.extend8_s",        IK_CONVERT,            0, 1, { VT_I64 },                VT_I64,  8 },
  { "i64.extend16_s",       IK_CONVERT,            0, 1, { VT_I64 },                VT_I64,  8 },
  { "i64.extend32_s",       IK_CONVERT,            0, 1, { VT_I64 },                VT_I64,  8 },
  { "i32.trunc_f32_s",      IK_CONVERT,            0, 1, { VT_F32 },                VT_I32,  4 },
  { "i32.trunc_f32_u",      IK_CONVERT,            0, 1, { VT_F32 },                VT_I32,  4 },
  { "i32.trunc_f64_s",      IK_CONVERT,            0, 1, { VT_F64 },                VT_I32,  4 },
  { "i32.trunc_f64_u",      IK_CONVERT,            0, 1, { VT_F64 },                VT_I32,  4 },
  { "i64.trunc_f32_s",      IK_CONVERT,            0, 1, { VT_F32 },                VT_I64,  8 },
  { "i64.trunc_f32_u",      IK_CONVERT,            0, 1, { VT_F32 },                VT_I64,  8 },
  { "i64.trunc_f64_s",      IK_CONVERT,            0, 1, { VT_F64 },                VT_I64,  8 },
  { "i64.trunc_f64_u",      IK_CONVERT,            0, 1, { VT_F64 },                VT_I64,  8 },
  { "f32.convert_i32_s",    IK_CONVERT,            0, 1, { VT_I32 },                VT_F32,  4 },
  { "f32.convert_i32_u",    IK_CONVERT,            0, 1, { VT_I32 },                VT_F32,  4 },
  { "f32.convert_i64_s",    IK_CONVERT,            0, 1, { VT_I64 },                VT_F32,  4 },
  { "f32.convert_i64_u",    IK_CONVERT,            0, 1, { VT_I64 },                VT_F32,  4 },
  { "f32.demote_f64",       IK_CONVERT,            0, 1, { VT_F64 },                VT_F32,  4 },
  { "f64.convert_i32_s",    IK_CONVERT,            0, 1, { VT_I32 },                VT_F64,  8 },
  { "f64.convert_i32_u",    IK_CONVERT,            0, 1, { VT_I32 },                VT_F64,  8 },
  { "f64.convert_i64_s",    IK_CONVERT,            0, 1, { VT_I64 },                VT_F64,  8 },
  { "f64.convert_i64_u",    IK_CONVERT,            0, 1, { VT_I64 },                VT_F64,  8 },
  { "f64.promote_f32",      IK_CONVERT,            0, 1, { VT_F32 },                VT_F64,  8 },
  { "i32.reinterpret_f32",  IK_CONVERT,            0, 1, { VT_F32 },                VT_I32,  4 },
  { "i64.reinterpret_f64",  IK_CONVERT,            0, 1, { VT_F64 },                VT_I64,  8 },
  { "f32.reinterpret_i32",  IK_CONVERT,            0, 1, { VT_I32 },                VT_F32,  4 },
  { "f64.reinterpret_i64",  IK_CONVERT,            0, 1, { VT_I64 },                VT_F64,  8 },
};

const instr_t *g_instrs = &g_instrtab_[0];
const int g_ninstrs = (int)(sizeof(g_instrtab_) / sizeof(g_instrtab_[0]));

/* ================================================================ */
/* Instruction lookup: names are interned once, then found by symbol. */

static buf_t g_instrtab;              /* pairs (sym, index), sorted by sym */

typedef struct isym { sym_t sym; int idx; } isym_t;

static int isym_cmp(const void *p1, const void *p2)
{
  const isym_t *a; const isym_t *b;
  a = (const isym_t *)p1; b = (const isym_t *)p2;
  if (a->sym < b->sym) return -1;
  if (a->sym > b->sym) return 1;
  return 0;
}

void init_instrs(void)
{
  int i;
  bufinit(&g_instrtab, sizeof(isym_t));
  for (i = 0; i < g_ninstrs; ++i) {
    isym_t *pe;
    pe = (isym_t *)bufnewbk(&g_instrtab);
    pe->sym = intern(g_instrs[i].name);
    pe->idx = i;
  }
  bufqsort(&g_instrtab, &isym_cmp);
}

const instr_t *instr_lookup(sym_t name)
{
  isym_t key;
  isym_t *pe;
  key.sym = name; key.idx = 0;
  pe = (isym_t *)bufbsearch(&g_instrtab, &key, &isym_cmp);
  if (pe == NULL) return NULL;
  return &g_instrs[pe->idx];
}

const char *vtname(vt_t vt)
{
  switch (vt) {
    case VT_I32: return "i32";
    case VT_I64: return "i64";
    case VT_F32: return "f32";
    case VT_F64: return "f64";
    case VT_PTR: return "ptr";
    case VT_ANY: return "any";
    default: break;
  }
  return "void";
}

vt_t vt_of_sym(sym_t s)
{
  const char *n;
  n = symname(s);
  if (n == NULL) return VT_VOID;
  if (streql(n, "i32")) return VT_I32;
  if (streql(n, "i64")) return VT_I64;
  if (streql(n, "f32")) return VT_F32;
  if (streql(n, "f64")) return VT_F64;
  if (streql(n, "ptr")) return VT_PTR;
  return VT_VOID;
}

/* ================================================================ */
/* What a module and its interfaces declare. */

void mod_init(mod_t *pm)
{
  memset(pm, 0, sizeof(mod_t));
  bufinit(&pm->sigs, sizeof(sig_t));
  bufinit(&pm->globals, sizeof(glb_t));
  bufinit(&pm->consts, sizeof(cst_t));
  bufinit(&pm->records, sizeof(rec_t));
  bufinit(&pm->macros, sizeof(mac_t));
  bufinit(&pm->datas, sizeof(dat_t));
  bufinit(&pm->tramps, sizeof(sym_t));
}

void mod_fini(mod_t *pm)
{
  size_t i;
  for (i = 0; i < buflen(&pm->sigs); ++i)
    buffini(&((sig_t *)bufref(&pm->sigs, i))->ptypes);
  for (i = 0; i < buflen(&pm->consts); ++i)
    snfini(&((cst_t *)bufref(&pm->consts, i))->val);
  buffini(&pm->sigs);
  buffini(&pm->globals);
  buffini(&pm->consts);
  for (i = 0; i < buflen(&pm->records); ++i)
    buffini(&((rec_t *)bufref(&pm->records, i))->fields);
  buffini(&pm->records);
  for (i = 0; i < buflen(&pm->macros); ++i) {
    mac_t *px;
    px = (mac_t *)bufref(&pm->macros, i);
    buffini(&px->params); buffini(&px->locals); buffini(&px->labels);
    buffini(&px->ein); buffini(&px->eout);
    snfini(&px->body);
  }
  buffini(&pm->macros);
  buffini(&pm->datas);
  buffini(&pm->tramps);
}

sig_t *mod_sig(mod_t *pm, sym_t name)
{
  size_t i;
  sig_t *first;
  first = NULL;
  for (i = 0; i < buflen(&pm->sigs); ++i) {
    sig_t *ps;
    ps = (sig_t *)bufref(&pm->sigs, i);
    if (ps->name != name) continue;
    if (ps->local) return ps;         /* ours shadows an interface's */
    if (first == NULL) first = ps;
  }
  return first;
}

static vt_t mod_gtype(mod_t *pm, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *pg;
    pg = (glb_t *)bufref(&pm->globals, i);
    if (pg->name == name) return pg->vt;
  }
  return VT_I32;                      /* as the Scheme version assumes */
}

sym_t mod_globalname(mod_t *pm, sym_t name)
{
  size_t i;
  sym_t first;
  first = 0;
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *pg;
    pg = (glb_t *)bufref(&pm->globals, i);
    if (pg->name != name) continue;
    if (pg->local) return pg->cname;  /* ours shadows an interface's */
    if (first == 0) first = pg->cname;
  }
  return first;
}

/* One letter per type, the result first, so that a shape has one name and
 * two shapes never share one. */
static char sig_letter(vt_t vt)
{
  if (vt == VT_I32) return 'i';
  if (vt == VT_I64) return 'l';
  if (vt == VT_F32) return 'f';
  if (vt == VT_F64) return 'd';
  if (vt == VT_PTR) return 'p';
  return 'v';
}

sym_t sig_letters(sig_t *ps)
{
  cbuf_t cb;
  size_t i;
  sym_t r;
  cbinit(&cb);
  cbputc(sig_letter(ps->res), &cb);
  for (i = 0; i < buflen(&ps->ptypes); ++i)
    cbputc(sig_letter(*(vt_t *)bufref(&ps->ptypes, i)), &cb);
  cbputc(0, &cb);
  r = intern((const char *)cbdata(&cb));
  cbfini(&cb);
  return r;
}

sym_t tramp_area_name(sym_t letters)
{
  return internf("$tc$%s", symname(letters));
}

int sig_nparams(mod_t *pm, sym_t letters)
{
  const char *s;
  (void)pm;
  s = symname(letters);
  if (s == NULL) return 0;
  return (int)strlen(s) - 1;          /* the result letter is not a slot */
}

sym_t mod_globalwoname(mod_t *pm, sym_t name)

{
  size_t i;
  sym_t first;
  first = 0;
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *pg;
    pg = (glb_t *)bufref(&pm->globals, i);
    if (pg->name != name) continue;
    if (pg->local) return pg->woname;  /* ours shadows an interface's */
    if (first == 0) first = pg->woname;
  }
  return first;
}

static cst_t *mod_const(mod_t *pm, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pm->consts); ++i) {
    cst_t *pc;
    pc = (cst_t *)bufref(&pm->consts, i);
    if (pc->name == name) return pc;
  }
  return NULL;
}

static char hexdig(unsigned int v)
{
  v &= 0xFu;
  return (char)((v < 10u) ? ('0' + (int)v) : ('A' + (int)(v - 10u)));
}

/* <owner>_<member>.  A MARV name's structural characters -- the '.' of a
 * nested module, the '$' of a generated one, and '_' itself -- become '_',
 * as they always have, because the C files in lib/ and srt/ spell the result
 * by hand.  Every other character is escaped as _XX, two uppercase hex
 * digits of the byte, so names a Scheme front end produces stay apart:
 * $list->vector and $char-alphabetic? no longer collapse onto $list__vector
 * and $char_alphabetic_.  The escape is not reversible against a name that
 * spells _XX itself (AT_EACCESS does), so an actual clash is reported by
 * cname_check rather than ruled out by the encoding. */
void c_mangle(const char *s, cbuf_t *pcb)
{
  if (s == NULL) return;
  if (s[0] == '$') s += 1;
  for (; *s != '\0'; ++s) {
    unsigned char ch;
    ch = (unsigned char)*s;
    if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')) cbputc((char)ch, pcb);
    else if (ch == '_' || ch == '.' || ch == '$') cbputc('_', pcb);
    else {
      cbputc('_', pcb);
      cbputc(hexdig((unsigned int)ch >> 4), pcb);
      cbputc(hexdig((unsigned int)ch), pcb);
    }
  }
}

static sym_t c_name(sym_t owner, sym_t member)
{
  cbuf_t nb;
  sym_t r;
  cbinit(&nb);
  c_mangle(symname(owner), &nb);
  cbputc('_', &nb);
  c_mangle(symname(member), &nb);
  cbputc(0, &nb);
  r = intern((const char *)cbdata(&nb));
  cbfini(&nb);
  return r;
}

/* Two MARV names can still mangle to one C name: '_', '.' and '$' share an
 * underscore, and a name may spell an escape.  The clash would surface as a
 * redefinition in the generated C, far from its cause, so it is reported
 * here, at the declaration that introduced it. */
/* A module that includes its own interface sees each of its objects twice,
 * once as $mod:name and once as $name, and both are that one object.  Only
 * names that differ after the qualifier is dropped can really clash. [cco] */
static bool same_thing(sym_t a, sym_t b)
{
  const char *x; const char *y; const char *p;
  x = symname(a); y = symname(b);
  if (x == NULL || y == NULL) return false;
  if (x[0] == '$') x += 1;
  if (y[0] == '$') y += 1;
  p = strchr(x, ':'); if (p != NULL) x = p + 1;
  p = strchr(y, ':'); if (p != NULL) y = p + 1;
  return streql(x, y);
}

static void cname_check(mod_t *pm, snode_t *pn, sym_t name, sym_t cname)
{
  size_t i;
  if (cname == 0) return;
  for (i = 0; i < buflen(&pm->sigs); ++i) {
    sig_t *ps;
    ps = (sig_t *)bufref(&pm->sigs, i);
    if (ps->cname == cname && ps->name != name && !same_thing(ps->name, name))
      sneprintf(pn, "%s and %s both name the C function %s",
                symname(ps->name), symname(name), symname(cname));
  }
  for (i = 0; i < buflen(&pm->datas); ++i) {
    dat_t *pd;
    pd = (dat_t *)bufref(&pm->datas, i);
    if (pd->cname == cname && pd->name != name && !same_thing(pd->name, name))
      sneprintf(pn, "%s and %s both name the C object %s",
                symname(pd->name), symname(name), symname(cname));
  }
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *pg;
    pg = (glb_t *)bufref(&pm->globals, i);
    if (pg->cname == cname && pg->name != name && !same_thing(pg->name, name))
      sneprintf(pn, "%s and %s both name the C object %s",
                symname(pg->name), symname(name), symname(cname));
  }
}

/* $<owner>:<member>, the name the wo target uses */
static sym_t w_name(sym_t owner, sym_t member)
{
  const char *o; const char *m;
  o = symname(owner); m = symname(member);
  if (o == NULL) o = "";
  if (m == NULL) m = "";
  if (o[0] == '$') o += 1;
  if (m[0] == '$') m += 1;
  return internf("$%s:%s", o, m);
}

sym_t mod_datawoname(mod_t *pm, sym_t name)
{
  size_t i;
  sym_t first;
  first = 0;
  for (i = 0; i < buflen(&pm->datas); ++i) {
    dat_t *pd;
    pd = (dat_t *)bufref(&pm->datas, i);
    if (pd->name != name) continue;
    if (pd->local) return pd->woname;     /* ours shadows an interface's */
    if (first == 0) first = pd->woname;
  }
  return first;
}

sym_t mod_dataname(mod_t *pm, sym_t name)
{
  size_t i;
  sym_t first;
  first = 0;
  for (i = 0; i < buflen(&pm->datas); ++i) {
    dat_t *pd;
    pd = (dat_t *)bufref(&pm->datas, i);
    if (pd->name != name) continue;
    if (pd->local) return pd->cname;     /* ours shadows an interface's */
    if (first == 0) first = pd->cname;
  }
  return first;
}


/* ------------------------------------------------- (data ...) segments */

/* one little-endian integer of w bytes */
static void dseg_putint(buf_t *pb, unsigned long long v, int w)
{
  int i;
  for (i = 0; i < w; ++i)
    *(char *)bufnewbk(pb) = (char)(unsigned char)(v >> (8 * i));
}

/* the numeric value of one item element, which the reader may have read as
 * either kind: (f64 1) and (i32 1.0) both say one */
static long long dseg_ival(snode_t *pn)
{
  if (pn->nt == SN_INT) return pn->ival;
  if (pn->nt == SN_FLT) return (long long)pn->dval;
  sneprintf(pn, "a data item takes numbers");
  return 0;
}

static double dseg_dval(snode_t *pn)
{
  if (pn->nt == SN_FLT) return pn->dval;
  if (pn->nt == SN_INT) return (double)pn->ival;
  sneprintf(pn, "a data item takes numbers");
  return 0.0;
}

/* (ptr v ...): eight-byte slots, each one zero, a data symbol, (ref.data $s
 * [off]) or (ref.func $s).  A named one leaves its bytes zero and records a
 * relocation, because no target can spell an address in a byte image. */
static void dseg_ptrs(snode_t *pc, dseg_t *pd)
{
  size_t j;
  for (j = 1; j < snlen(pc); ++j) {
    snode_t *pv;
    drel_t rel;
    pv = snref(pc, j);
    rel.off = buflen(&pd->bytes);
    rel.sym = 0;
    rel.addend = 0;
    rel.isfunc = false;
    if (pv->nt == SN_INT && pv->ival == 0) {
      /* a null slot */
    } else if (pv->nt == SN_SYM) {
      rel.sym = pv->sym;
    } else if (pv->nt == SN_LIST && snlen(pv) >= 1
               && snref(pv, 0)->nt == SN_SYM) {
      const char *k;
      k = symname(snref(pv, 0)->sym);
      if (streql(k, "ptr.null")) {
        /* a null slot, spelled as the instruction */
      } else if (streql(k, "ref.data")) {
        if (snlen(pv) < 2 || snref(pv, 1)->nt != SN_SYM)
          sneprintf(pv, "(ref.data $sym [off]) takes a name");
        rel.sym = snref(pv, 1)->sym;
        if (snlen(pv) > 2) rel.addend = dseg_ival(snref(pv, 2));
      } else if (streql(k, "ref.func")) {
        if (snlen(pv) < 2 || snref(pv, 1)->nt != SN_SYM)
          sneprintf(pv, "(ref.func $sym) takes a name");
        rel.sym = snref(pv, 1)->sym;
        rel.isfunc = true;
      } else {
        sneprintf(pv, "a (ptr ...) slot takes 0, a name, (ref.data ...) or (ref.func ...)");
      }
    } else {
      sneprintf(pv, "a (ptr ...) slot takes 0, a name, (ref.data ...) or (ref.func ...)");
    }
    if (rel.sym != 0) memcpy(bufnewbk(&pd->relocs), &rel, sizeof(drel_t));
    dseg_putint(&pd->bytes, 0, 8);
  }
}

/* (iN v ...), (uN v ...), (fN v ...): retain typed values and optionally
 * write their little-endian image for the non-C targets. */
static bool dseg_scalars(snode_t *pc, dseg_t *pd, bool little_endian_image)
{
  const char *k;
  size_t j;
  int w;
  bool isflo;
  k = symname(snref(pc, 0)->sym);
  w = 0;
  isflo = false;
  if (streql(k, "i8") || streql(k, "u8") || streql(k, "bytes")) w = 1;
  else if (streql(k, "i16") || streql(k, "u16")) w = 2;
  else if (streql(k, "i32") || streql(k, "u32")) w = 4;
  else if (streql(k, "i64") || streql(k, "u64")) w = 8;
  else if (streql(k, "f32")) { w = 4; isflo = true; }
  else if (streql(k, "f64")) { w = 8; isflo = true; }
  else return false;
  if (w > 1 && snlen(pc) > 1) {
    dnum_t *run;
    run = (dnum_t *)bufnewbk(&pd->nums);
    run->off = buflen(&pd->bytes);
    run->count = snlen(pc) - 1;
    run->first = buflen(&pd->numvals);
    run->width = w;
    run->kind = isflo ? ((w == 4) ? DNUM_F32 : DNUM_F64) : DNUM_INT;
  }
  for (j = 1; j < snlen(pc); ++j) {
    snode_t *pv;
    pv = snref(pc, j);
    {
      unsigned long long bits;
      if (isflo) {
        double d;
        d = dseg_dval(pv);
        bits = (w == 4) ? (unsigned long long)as_uint32((float)d) : as_uint64(d);
      } else {
        bits = (unsigned long long)dseg_ival(pv);
      }
      if (w < 8) bits &= (((unsigned long long)1 << (w * 8)) - 1);
      if (w > 1) *(unsigned long long *)bufnewbk(&pd->numvals) = bits;
      /* C initializes typed runs separately; its byte image holds only raw
       * bytes and zero placeholders. The other targets need LE data bytes. */
      dseg_putint(&pd->bytes, (w == 1 || little_endian_image) ? bits : 0, w);
    }
  }
  return true;
}

static void dseg_zeros(buf_t *pb, long long n)
{
  for (; n > 0; --n) *(char *)bufnewbk(pb) = 0;
}

/* (data $n [(export "s")] [const|var] [align N|(align N)] item ...)
 *
 * Both attribute spellings occur: stdio.mvh writes `align 4 size 640` as
 * plain adjacent tokens, the examples write `(size 1048576)`.  Anything the
 * spec does not list is an error rather than a silently dropped item, which
 * is how typed items went missing before.  [cco] */
void dseg_read(snode_t *pn, dseg_t *pd, bool little_endian_image)
{
  size_t i, k;
  pd->name = 0;
  pd->expname = NULL;
  pd->align = 8;
  pd->isconst = false;
  pd->allzero = true;
  bufinit(&pd->bytes, sizeof(char));
  bufinit(&pd->nums, sizeof(dnum_t));
  bufinit(&pd->numvals, sizeof(unsigned long long));
  bufinit(&pd->relocs, sizeof(drel_t));
  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  pd->name = snref(pn, 1)->sym;
  for (i = 2; i < snlen(pn); ++i) {
    snode_t *pc;
    pc = snref(pn, i);
    if (pc->nt == SN_SYM && streql(symname(pc->sym), "const")) pd->isconst = true;
    else if (pc->nt == SN_SYM && streql(symname(pc->sym), "var")) pd->isconst = false;
    else if (pc->nt == SN_STR || pc->nt == SN_BVEC) {
      for (k = 0; k < buflen(&pc->data); ++k)
        *(char *)bufnewbk(&pd->bytes) = *(char *)bufref(&pc->data, k);
    } else if (pc->nt == SN_SYM && i + 1 < snlen(pn)
               && snref(pn, i + 1)->nt == SN_INT
               && (streql(symname(pc->sym), "align")
                   || streql(symname(pc->sym), "size"))) {
      long long n;
      n = snref(pn, i + 1)->ival;
      if (streql(symname(pc->sym), "align")) pd->align = n;
      else dseg_zeros(&pd->bytes, n);
      i += 1;
    } else if (pc->nt == SN_LIST && snlen(pc) > 0
               && snref(pc, 0)->nt == SN_SYM) {
      const char *k2;
      k2 = symname(snref(pc, 0)->sym);
      if (streql(k2, "export")) {
        if (snlen(pc) < 2 || snref(pc, 1)->nt != SN_STR)
          sneprintf(pc, "(export \"name\") takes a string");
        else pd->expname = (const char *)cbdata(&snref(pc, 1)->data);
      } else if (streql(k2, "align")) {
        if (snlen(pc) < 2 || snref(pc, 1)->nt != SN_INT)
          sneprintf(pc, "(align N) takes a count");
        else pd->align = snref(pc, 1)->ival;
      } else if (streql(k2, "size") || streql(k2, "zero")) {
        if (snlen(pc) < 2 || snref(pc, 1)->nt != SN_INT)
          sneprintf(pc, "(size N) takes a count");
        else dseg_zeros(&pd->bytes, snref(pc, 1)->ival);
      } else if (streql(k2, "ptr")) {
        dseg_ptrs(pc, pd);
      } else if (!dseg_scalars(pc, pd, little_endian_image)) {
        sneprintf(pc, "(%s ...) is not a data item", k2);
      }
    } else {
      sneprintf(pc, "a data clause takes const, var, align, size, a string, a bytevector or a typed item");
    }
  }
  for (k = 0; k < buflen(&pd->bytes); ++k)
    if (*(char *)bufref(&pd->bytes, k) != 0) { pd->allzero = false; break; }
}

/* (global $n [(export "s")] (mut T)|T [(T.const v)]) -- the initializer is a
 * constant expression, and anything else is refused rather than quietly
 * started at zero */
void glb_read(snode_t *pn, vt_t vt, glb_init_t *pg)
{
  size_t i;
  pg->ismut = false;
  pg->hasval = false;
  pg->isflo = (vt == VT_F32 || vt == VT_F64);
  pg->ival = 0;
  pg->dval = 0.0;
  for (i = 2; i < snlen(pn); ++i) {
    snode_t *pc;
    const char *h;
    pc = snref(pn, i);
    if (pc->nt != SN_LIST || snlen(pc) < 1 || snref(pc, 0)->nt != SN_SYM) continue;
    h = symname(snref(pc, 0)->sym);
    if (streql(h, "export") || streql(h, "mut")) {
      if (streql(h, "mut")) pg->ismut = true;
      continue;
    }
    if (streql(h, "ptr.null")) { pg->hasval = true; continue; }
    if (snlen(pc) > 1 && strstr(h, ".const") != NULL) {
      snode_t *pv;
      pv = snref(pc, 1);
      pg->hasval = true;
      if (pg->isflo) pg->dval = (pv->nt == SN_FLT) ? pv->dval : (double)pv->ival;
      else pg->ival = (pv->nt == SN_FLT) ? (long long)pv->dval : pv->ival;
      continue;
    }
    sneprintf(pc, "a global takes a constant initializer, not (%s ...)", h);
  }
}

void dseg_fini(dseg_t *pd)
{
  buffini(&pd->bytes);
  buffini(&pd->nums);
  buffini(&pd->numvals);
  buffini(&pd->relocs);
}

static mac_t *mod_macro(mod_t *pm, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pm->macros); ++i) {
    mac_t *px;
    px = (mac_t *)bufref(&pm->macros, i);
    if (px->name == name) return px;
  }
  return NULL;
}

/* (macro $n ($p ...) [(locals ($l T) ...)] [(labels $L ...)] (effect ...) body...)
 * The effect clause is required by the language and checked by the Scheme
 * implementation at every expansion; it is parsed and kept here but not yet
 * checked.  [cco] */
static void declare_macro(mod_t *pm, snode_t *pn)
{
  mac_t *px;
  size_t i, body;
  bool haseff;
  if (snlen(pn) < 3 || snref(pn, 1)->nt != SN_SYM || snref(pn, 2)->nt != SN_LIST) return;
  px = (mac_t *)bufnewbk(&pm->macros);
  memset(px, 0, sizeof(mac_t));
  bufinit(&px->params, sizeof(sym_t));
  bufinit(&px->locals, sizeof(var_t));
  bufinit(&px->labels, sizeof(sym_t));
  bufinit(&px->ein, sizeof(vt_t));
  bufinit(&px->eout, sizeof(vt_t));
  sninit(&px->body);
  px->name = snref(pn, 1)->sym;
  for (i = 0; i < snlen(snref(pn, 2)); ++i) {
    snode_t *pp;
    pp = snref(snref(pn, 2), i);
    if (pp->nt == SN_SYM) *(sym_t *)bufnewbk(&px->params) = pp->sym;
  }
  haseff = false;
  body = 3;
  for (i = 3; i < snlen(pn); ++i) {
    snode_t *pc;
    const char *k;
    size_t j;
    pc = snref(pn, i);
    if (pc->nt != SN_LIST || snlen(pc) == 0 || snref(pc, 0)->nt != SN_SYM) break;
    k = symname(snref(pc, 0)->sym);
    if (streql(k, "locals")) {
      for (j = 1; j < snlen(pc); ++j) {
        snode_t *pl;
        var_t *pv;
        pl = snref(pc, j);
        if (pl->nt != SN_LIST || snlen(pl) < 2) continue;
        pv = (var_t *)bufnewbk(&px->locals);
        pv->name = snref(pl, 0)->sym;
        pv->vt = vt_of_sym(snref(pl, 1)->sym);
      }
    } else if (streql(k, "labels")) {
      for (j = 1; j < snlen(pc); ++j)
        if (snref(pc, j)->nt == SN_SYM)
          *(sym_t *)bufnewbk(&px->labels) = snref(pc, j)->sym;
    } else if (streql(k, "effect")) {
      size_t j;
      buf_t *side;
      haseff = true;
      side = &px->ein;
      for (j = 1; j < snlen(pc); ++j) {
        snode_t *pe;
        pe = snref(pc, j);
        if (pe->nt == SN_SYM && streql(symname(pe->sym), "->")) { side = &px->eout; continue; }
        if (pe->nt != SN_LIST) continue;
        for (i = 0; i < snlen(pe); ++i) {       /* a list of value types */
          snode_t *pt;
          pt = snref(pe, i);
          if (pt->nt == SN_SYM)
            *(vt_t *)bufnewbk(side) = streql(symname(pt->sym), "any")
                                      ? VT_ANY : vt_of_sym(pt->sym);
        }
      }
      i = body;                                  /* the loop variable is shared */
    } else {
      break;                          /* the template starts here */
    }
    body = i + 1;
  }
  if (!haseff) sneprintf(pn, "a macro definition needs an (effect ...) clause");
  if (body >= snlen(pn)) sneprintf(pn, "a macro definition needs a template");
  if (body + 1 == snlen(pn)) {
    sncpy(&px->body, snref(pn, body));
  } else {                            /* several forms: a sequence */
    snode_t *ph;
    px->body.nt = SN_LIST;
    px->body.fname = pn->fname; px->body.line = pn->line; px->body.col = pn->col;
    ph = snnewbk(&px->body);
    ph->nt = SN_SYM; ph->sym = intern("seq");
    ph->fname = pn->fname; ph->line = pn->line; ph->col = pn->col;
    for (i = body; i < snlen(pn); ++i) sncpy(snnewbk(&px->body), snref(pn, i));
  }
}

static rec_t *mod_record(mod_t *pm, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pm->records); ++i) {
    rec_t *pr;
    pr = (rec_t *)bufref(&pm->records, i);
    if (pr->name == name) return pr;
  }
  return NULL;
}

static long long align_up(long long v, long long a)
{
  if (a <= 1) return v;
  return ((v + a - 1) / a) * a;
}

/* size, alignment, the type a load yields, and the load/store instructions */
static void field_metrics(mod_t *pm, snode_t *pt, fld_t *pf)
{
  const char *t;
  pf->vt = VT_I32; pf->size = 4; pf->align = 4;
  pf->lop = intern("i32.load"); pf->sop = intern("i32.store");
  if (pt->nt == SN_LIST) {                /* (array T n), (record $R) */
    if (snlen(pt) > 1 && snref(pt, 0)->nt == SN_SYM) {
      const char *k;
      k = symname(snref(pt, 0)->sym);
      if (streql(k, "record") || streql(k, "struct")) {
        rec_t *pr;
        pr = mod_record(pm, snref(pt, 1)->sym);
        pf->vt = VT_PTR; pf->lop = 0; pf->sop = 0;
        pf->size = (pr != NULL) ? pr->size : 8;
        pf->align = (pr != NULL) ? pr->align : 8;
        return;
      }
      if (streql(k, "array")) {
        fld_t el;
        long long cnt;
        cnt = (snlen(pt) > 2 && snref(pt, 2)->nt == SN_INT) ? snref(pt, 2)->ival : 1;
        memset(&el, 0, sizeof(fld_t));
        field_metrics(pm, snref(pt, 1), &el);
        pf->vt = VT_PTR; pf->lop = 0; pf->sop = 0;
        pf->size = el.size * cnt; pf->align = el.align;
        return;
      }
    }
    return;
  }
  if (pt->nt != SN_SYM) return;
  t = symname(pt->sym);
  if (streql(t, "i8")) { pf->size = 1; pf->align = 1;
    pf->lop = intern("i32.load8_s"); pf->sop = intern("i32.store8"); }
  else if (streql(t, "u8")) { pf->size = 1; pf->align = 1;
    pf->lop = intern("i32.load8_u"); pf->sop = intern("i32.store8"); }
  else if (streql(t, "i16")) { pf->size = 2; pf->align = 2;
    pf->lop = intern("i32.load16_s"); pf->sop = intern("i32.store16"); }
  else if (streql(t, "u16")) { pf->size = 2; pf->align = 2;
    pf->lop = intern("i32.load16_u"); pf->sop = intern("i32.store16"); }
  else if (streql(t, "i64") || streql(t, "u64")) { pf->vt = VT_I64;
    pf->size = 8; pf->align = 8;
    pf->lop = intern("i64.load"); pf->sop = intern("i64.store"); }
  else if (streql(t, "f32")) { pf->vt = VT_F32;
    pf->lop = intern("f32.load"); pf->sop = intern("f32.store"); }
  else if (streql(t, "f64")) { pf->vt = VT_F64; pf->size = 8; pf->align = 8;
    pf->lop = intern("f64.load"); pf->sop = intern("f64.store"); }
  else if (streql(t, "ptr")) { pf->vt = VT_PTR; pf->size = 8; pf->align = 8;
    pf->lop = intern("ptr.load"); pf->sop = intern("ptr.store"); }
}

static void declare_record(mod_t *pm, snode_t *pn)
{
  rec_t *pr;
  size_t i;
  long long off, maxal, expsz, expal;
  if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  pr = (rec_t *)bufnewbk(&pm->records);
  memset(pr, 0, sizeof(rec_t));
  bufinit(&pr->fields, sizeof(fld_t));
  pr->name = snref(pn, 1)->sym;
  off = 0; maxal = 1; expsz = -1; expal = -1;
  for (i = 2; i < snlen(pn); ++i) {
    snode_t *pc;
    const char *k;
    pc = snref(pn, i);
    if (pc->nt != SN_LIST || snlen(pc) < 2 || snref(pc, 0)->nt != SN_SYM) continue;
    k = symname(snref(pc, 0)->sym);
    if (streql(k, "size")) { if (snref(pc, 1)->nt == SN_INT) expsz = snref(pc, 1)->ival; }
    else if (streql(k, "align")) { if (snref(pc, 1)->nt == SN_INT) expal = snref(pc, 1)->ival; }
    else if (streql(k, "field") && snlen(pc) > 2) {
      fld_t *pf;
      fld_t m;
      memset(&m, 0, sizeof(fld_t));
      field_metrics(pm, snref(pc, 2), &m);
      pf = (fld_t *)bufnewbk(&pr->fields);
      pr = (rec_t *)bufref(&pm->records, buflen(&pm->records) - 1);   /* may move */
      *pf = m;
      pf->name = snref(pc, 1)->sym;
      pf->off = align_up(off, m.align);
      off = pf->off + m.size;
      if (m.align > maxal) maxal = m.align;
    }
  }
  pr->align = (expal >= 0) ? expal : maxal;
  pr->size = (expsz >= 0) ? expsz : align_up(off, pr->align);
}

/* while an interface is being read, the name it gives its members, and the
 * module name it claims with (module "x"), which is what the C names use [cco] */
static sym_t g_iface;
static sym_t g_ifmod;

/* (func $f (param $x T) ... (result T))  -- in a module or an interface */
static void declare_func(mod_t *pm, snode_t *pn)
{
  size_t i;
  sig_t *ps;
  bool retrec, variadic;
  if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  ps = (sig_t *)bufnewbk(&pm->sigs);
  memset(ps, 0, sizeof(sig_t));
  bufinit(&ps->ptypes, sizeof(vt_t));
  ps->name = snref(pn, 1)->sym;
  ps->res = VT_VOID;
  retrec = false; variadic = false;
  ps->local = (g_iface == 0);
  {
    sym_t owner;
    owner = (g_ifmod != 0) ? g_ifmod : ((g_iface != 0) ? g_iface : pm->name);
    ps->cname = c_name(owner, ps->name);
    ps->woname = w_name(owner, ps->name);
    cname_check(pm, pn, ps->name, ps->cname);
  }
  for (i = 2; i < snlen(pn); ++i) {
    snode_t *pc;
    const char *h;
    pc = snref(pn, i);
    if (pc->nt != SN_LIST || snlen(pc) < 1 || snref(pc, 0)->nt != SN_SYM) continue;
    h = symname(snref(pc, 0)->sym);
    if (streql(h, "param")) {
      size_t j, n, nnames;
      vt_t shared;
      /* a clause carries one or more names sharing a trailing type, a type
       * alone, several types, or the variadic marker: (param $x T),
       * (param $x $y T), (param T), (param T T ...) and (param ...) all
       * occur.  Names say how many parameters share that type, so this has
       * to agree with fn_header; without names, every element that names a
       * type is one parameter. */
      n = snlen(pc);
      nnames = 0;
      for (j = 1; j < n; ++j) {
        snode_t *pt;
        pt = snref(pc, j);
        if (pt->nt == SN_SYM && symname(pt->sym)[0] == '$') nnames += 1;
      }
      shared = (n > 2 && snref(pc, n - 1)->nt == SN_SYM)
                 ? vt_of_sym(snref(pc, n - 1)->sym) : VT_VOID;
      if (nnames > 0 && nnames + 2 == n && shared != VT_VOID) {
        for (j = 0; j < nnames; ++j) *(vt_t *)bufnewbk(&ps->ptypes) = shared;
      } else {
        for (j = 1; j < n; ++j) {
          snode_t *pt;
          pt = snref(pc, j);
          if (pt->nt != SN_SYM) continue;
          if (streql(symname(pt->sym), "...")) { variadic = true; continue; }
          {
            vt_t vt;
            vt = vt_of_sym(pt->sym);
            if (vt != VT_VOID) *(vt_t *)bufnewbk(&ps->ptypes) = vt;
          }
        }
      }
    } else if (streql(h, "result")) {
      snode_t *pt;
      pt = snref(pc, snlen(pc) - 1);
      if (pt->nt == SN_SYM) ps->res = vt_of_sym(pt->sym);
      else retrec = true;                      /* (result (record $r)) */
    } else if (streql(h, "export")) {
      ps->exported = true;
      if (g_iface == 0 && snlen(pc) > 1 && snref(pc, 1)->nt == SN_STR
          && streql((const char *)cbdata(&snref(pc, 1)->data), "main"))
        pm->hasmain = true;
    }
  }
  /* the calling convention: $rp$ in front of a record return, $ap$ behind a
   * variadic parameter list -- call sites pass both */
  if (retrec) {
    size_t n, k;
    n = buflen(&ps->ptypes);
    bufnewbk(&ps->ptypes);
    for (k = n; k > 0; --k)
      *(vt_t *)bufref(&ps->ptypes, k) = *(vt_t *)bufref(&ps->ptypes, k - 1);
    *(vt_t *)bufref(&ps->ptypes, 0) = VT_PTR;
    ps->res = VT_VOID;
  }
  if (variadic) { *(vt_t *)bufnewbk(&ps->ptypes) = VT_PTR; ps->variadic = true; }
}

/* an interface member answers to both $iface:name and $name [cco] */
static int g_mxno;                  /* numbers a macro expansion's private names */

/* A module is lowered more than once -- the use scan lowers it before the
 * emitter does -- and the names the emitter writes have to start where the
 * reference's do, so the pass that writes them says so. */
void mod_reset_fresh(void) { g_mxno = 0; }

static void alias_last_sig(mod_t *pm)
{
  sig_t *ps; sig_t *pa;
  size_t i, n;
  const char *in; const char *fn;
  if (g_iface == 0 || buflen(&pm->sigs) == 0) return;
  ps = (sig_t *)bufref(&pm->sigs, buflen(&pm->sigs) - 1);
  in = symname(g_iface); fn = symname(ps->name);
  if (in[0] == '$') in += 1;
  if (fn[0] == '$') fn += 1;
  pa = (sig_t *)bufnewbk(&pm->sigs);
  ps = (sig_t *)bufref(&pm->sigs, buflen(&pm->sigs) - 2);   /* may have moved */
  memset(pa, 0, sizeof(sig_t));
  bufinit(&pa->ptypes, sizeof(vt_t));
  pa->name = internf("$%s:%s", in, fn);
  pa->res = ps->res;
  pa->cname = ps->cname;
  pa->woname = ps->woname;
  pa->local = ps->local;
  pa->variadic = ps->variadic;
  pa->exported = ps->exported;
  n = buflen(&ps->ptypes);
  for (i = 0; i < n; ++i)
    *(vt_t *)bufnewbk(&pa->ptypes) = *(vt_t *)bufref(&ps->ptypes, i);
}

/* remember a chain shape, so the emitters know which areas to make */
static void mod_note_tramp(mod_t *pm, sym_t letters)
{
  size_t i;
  for (i = 0; i < buflen(&pm->tramps); ++i)
    if (*(sym_t *)bufref(&pm->tramps, i) == letters) return;
  *(sym_t *)bufnewbk(&pm->tramps) = letters;
}

void mod_declare(mod_t *pm, snode_t *pn)
{
  const char *h;
  if (pn->nt != SN_LIST || snlen(pn) < 1 || snref(pn, 0)->nt != SN_SYM) return;
  h = symname(snref(pn, 0)->sym);
  if (streql(h, "module") && g_iface != 0) {
    if (snlen(pn) > 1 && snref(pn, 1)->nt == SN_STR) {
      cbuf_t nb;                        /* (module "stat") names the C prefix */
      cbinit(&nb);
      cbput((const char *)bufdata(&snref(pn, 1)->data),
            buflen(&snref(pn, 1)->data), &nb);
      cbputc(0, &nb);
      g_ifmod = intern((const char *)cbdata(&nb));
      cbfini(&nb);
    }
  } else if (streql(h, "func") || streql(h, "trampoline_func")) {
    declare_func(pm, pn);
    alias_last_sig(pm);
    if (streql(h, "trampoline_func") && buflen(&pm->sigs) >= 1) {
      sig_t *ps;
      sym_t letters;
      /* the alias went on the back when there is one, so the function itself
       * is before it; a module's own function gets no alias and is last */
      ps = (sig_t *)bufref(&pm->sigs,
                            buflen(&pm->sigs) - 1 - ((g_iface != 0) ? 1 : 0));
      if (ps->res != VT_PTR && ps->res != VT_I64)
        sneprintf(pn, "a trampoline_func returns ptr or i64, not %s", vtname(ps->res));
      letters = sig_letters(ps);
      mod_note_tramp(pm, letters);
    }
  } else if (streql(h, "data")) {
    dat_t *pd;
    sym_t q;
    if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
    pd = (dat_t *)bufnewbk(&pm->datas);
    memset(pd, 0, sizeof(dat_t));
    pd->name = snref(pn, 1)->sym;
    pd->local = (g_iface == 0);
    {
      sym_t owner;
      size_t j;
      owner = (g_ifmod != 0) ? g_ifmod : ((g_iface != 0) ? g_iface : pm->name);
      pd->cname = c_name(owner, pd->name);
      pd->woname = w_name(owner, pd->name);
      cname_check(pm, pn, pd->name, pd->cname);
      pd->align = 8;                    /* the shape an import must state */
      pd->size = 0;
      for (j = 2; j < snlen(pn); ++j) {
        snode_t *pc;
        pc = snref(pn, j);
        if (pc->nt == SN_SYM && streql(symname(pc->sym), "const")) pd->isconst = true;
        else if (pc->nt == SN_STR || pc->nt == SN_BVEC)
          pd->size += (long long)buflen(&pc->data);
        else if (pc->nt == SN_LIST && snlen(pc) > 1 && snref(pc, 0)->nt == SN_SYM) {
          const char *k;
          k = symname(snref(pc, 0)->sym);
          if (streql(k, "size") && snref(pc, 1)->nt == SN_INT) pd->size = snref(pc, 1)->ival;
          else if (streql(k, "align") && snref(pc, 1)->nt == SN_INT) pd->align = snref(pc, 1)->ival;
        } else if (pc->nt == SN_SYM && j + 1 < snlen(pn)
                   && snref(pn, j + 1)->nt == SN_INT
                   && (streql(symname(pc->sym), "align")
                       || streql(symname(pc->sym), "size"))) {
          if (streql(symname(pc->sym), "align")) pd->align = snref(pn, j + 1)->ival;
          else pd->size = snref(pn, j + 1)->ival;
          j += 1;
        }
      }
      if (pd->align != 1 && pd->align != 2 && pd->align != 4
          && pd->align != 8 && pd->align != 16) pd->align = 8;
    }
    if (g_iface != 0) {                 /* also known as $iface:name */
      const char *in; const char *dn;
      dat_t *pa;
      in = symname(g_iface); dn = symname(pd->name);
      if (in[0] == '$') in += 1;
      if (dn[0] == '$') dn += 1;
      q = internf("$%s:%s", in, dn);
      pa = (dat_t *)bufnewbk(&pm->datas);
      pd = (dat_t *)bufref(&pm->datas, buflen(&pm->datas) - 2);
      pa->name = q;
      pa->cname = pd->cname;
      pa->woname = pd->woname;
      pa->local = pd->local;
      pa->isconst = pd->isconst;
      pa->align = pd->align;
      pa->size = pd->size;
    }
  } else if (streql(h, "macro")) {
    declare_macro(pm, pn);
    if (g_iface != 0) {                 /* also known as $iface:name */
      mac_t *px; mac_t *pa;
      const char *in; const char *mn;
      size_t k, n;
      if (buflen(&pm->macros) == 0) return;
      px = (mac_t *)bufref(&pm->macros, buflen(&pm->macros) - 1);
      in = symname(g_iface); mn = symname(px->name);
      if (in[0] == '$') in += 1;
      if (mn[0] == '$') mn += 1;
      pa = (mac_t *)bufnewbk(&pm->macros);
      px = (mac_t *)bufref(&pm->macros, buflen(&pm->macros) - 2);
      memset(pa, 0, sizeof(mac_t));
      bufinit(&pa->params, sizeof(sym_t));
      bufinit(&pa->locals, sizeof(var_t));
      bufinit(&pa->labels, sizeof(sym_t));
      bufinit(&pa->ein, sizeof(vt_t));
      bufinit(&pa->eout, sizeof(vt_t));
      sninit(&pa->body);
      pa->name = internf("$%s:%s", in, mn);
      n = buflen(&px->params);
      for (k = 0; k < n; ++k) *(sym_t *)bufnewbk(&pa->params) = *(sym_t *)bufref(&px->params, k);
      n = buflen(&px->locals);
      for (k = 0; k < n; ++k) *(var_t *)bufnewbk(&pa->locals) = *(var_t *)bufref(&px->locals, k);
      n = buflen(&px->labels);
      for (k = 0; k < n; ++k) *(sym_t *)bufnewbk(&pa->labels) = *(sym_t *)bufref(&px->labels, k);
      n = buflen(&px->ein);
      for (k = 0; k < n; ++k) *(vt_t *)bufnewbk(&pa->ein) = *(vt_t *)bufref(&px->ein, k);
      n = buflen(&px->eout);
      for (k = 0; k < n; ++k) *(vt_t *)bufnewbk(&pa->eout) = *(vt_t *)bufref(&px->eout, k);
      sncpy(&pa->body, &px->body);
    }
  } else if (streql(h, "record")) {
    declare_record(pm, pn);
    if (g_iface != 0) {                 /* also known as $iface:name */
      rec_t *pr; rec_t *pa;
      const char *in; const char *rn;
      size_t k, n;
      pr = (rec_t *)bufref(&pm->records, buflen(&pm->records) - 1);
      in = symname(g_iface); rn = symname(pr->name);
      if (in[0] == '$') in += 1;
      if (rn[0] == '$') rn += 1;
      pa = (rec_t *)bufnewbk(&pm->records);
      pr = (rec_t *)bufref(&pm->records, buflen(&pm->records) - 2);
      memset(pa, 0, sizeof(rec_t));
      bufinit(&pa->fields, sizeof(fld_t));
      pa->name = internf("$%s:%s", in, rn);
      pa->size = pr->size; pa->align = pr->align;
      n = buflen(&pr->fields);
      for (k = 0; k < n; ++k)
        *(fld_t *)bufnewbk(&pa->fields) = *(fld_t *)bufref(&pr->fields, k);
    }
  } else if (streql(h, "global")) {
    glb_t *pg;
    snode_t *pt;
    size_t j;
    if (snlen(pn) < 3 || snref(pn, 1)->nt != SN_SYM) return;
    pg = (glb_t *)bufnewbk(&pm->globals);
    memset(pg, 0, sizeof(glb_t));
    pg->name = snref(pn, 1)->sym;
    pg->local = (g_iface == 0);
    {
      sym_t owner;
      owner = (g_ifmod != 0) ? g_ifmod : ((g_iface != 0) ? g_iface : pm->name);
      pg->cname = c_name(owner, pg->name);
      pg->woname = w_name(owner, pg->name);
      cname_check(pm, pn, pg->name, pg->cname);
    }
    pg->vt = VT_I32;
    for (j = 2; j < snlen(pn); ++j) {
      pt = snref(pn, j);
      if (pt->nt == SN_LIST && snlen(pt) == 2 && snref(pt, 0)->nt == SN_SYM
          && streql(symname(snref(pt, 0)->sym), "mut")) pt = snref(pt, 1);
      if (pt->nt == SN_SYM && vt_of_sym(pt->sym) != VT_VOID) {
        pg->vt = vt_of_sym(pt->sym);
        break;
      }
    }
    if (g_iface != 0) {                 /* also known as $iface:name */
      const char *in; const char *gn;
      glb_t *pa;
      in = symname(g_iface); gn = symname(pg->name);
      if (in[0] == '$') in += 1;
      if (gn[0] == '$') gn += 1;
      pa = (glb_t *)bufnewbk(&pm->globals);
      pg = (glb_t *)bufref(&pm->globals, buflen(&pm->globals) - 2);
      memset(pa, 0, sizeof(glb_t));
      pa->name = internf("$%s:%s", in, gn);
      pa->cname = pg->cname; pa->woname = pg->woname;
      pa->vt = pg->vt; pa->local = pg->local;
    }
  } else if (streql(h, "const")) {
    cst_t *pc;
    snode_t *pv;
    /* (const $n T v) and (const $n v), where v may be an expression */
    if (snlen(pn) < 3 || snref(pn, 1)->nt != SN_SYM) return;
    pc = (cst_t *)bufnewbk(&pm->consts);
    memset(pc, 0, sizeof(cst_t));
    sninit(&pc->val);
    pc->name = snref(pn, 1)->sym;
    pc->vt = VT_I32;
    pv = snref(pn, 2);
    if (snlen(pn) > 3 && pv->nt == SN_SYM && vt_of_sym(pv->sym) != VT_VOID) {
      pc->vt = vt_of_sym(pv->sym);
      pv = snref(pn, 3);
    }
    if (pv->nt == SN_INT) { pc->isnum = true; pc->ival = pv->ival; }
    else if (pv->nt == SN_FLT) { pc->isnum = true; pc->dval = pv->dval; }
    else sncpy(&pc->val, pv);        /* a constant expression */
    if (g_iface != 0) {              /* also answers to $iface:name */
      cst_t *pa;
      const char *in; const char *fn;
      in = symname(g_iface); fn = symname(pc->name);
      if (in[0] == '$') in += 1;
      if (fn[0] == '$') fn += 1;
      pa = (cst_t *)bufnewbk(&pm->consts);
      pc = (cst_t *)bufref(&pm->consts, buflen(&pm->consts) - 2);
      memset(pa, 0, sizeof(cst_t));
      sninit(&pa->val);
      pa->name = internf("$%s:%s", in, fn);
      pa->vt = pc->vt; pa->isnum = pc->isnum; pa->ival = pc->ival; pa->dval = pc->dval;
      sncpy(&pa->val, &pc->val);
    }
  }
}

/* ================================================================ */
/* Where a file comes from: the three search path lists.            */

/* A build with -D PREFIX=<dir>/ looks in that installed tree as well as the
 * local one.  WCPL takes #ifdef only at top level, so this sits here. [cco] */
#ifndef __WCPL__
#ifdef PREFIX
#define MVSTR2(x) #x
#define MVSTR(x)  MVSTR2(x)
static const char *g_prefix_target = MVSTR(PREFIX) "target";
static const char *g_prefix_lib    = MVSTR(PREFIX) "lib";
#endif
#ifndef PREFIX
static const char *g_prefix_target = "targets/";
static const char *g_prefix_lib    = "libs/";
#endif
#endif
#ifdef __WCPL__
static const char *g_prefix_target = "targets/";
static const char *g_prefix_lib    = "libs/";
#endif

static buf_t g_seen;
static buf_t g_sysmvh;            /* resolved paths */
static buf_t g_sysmod;            /* the names as written */
buf_t g_tdirs;             /* -T, where a .mvt is looked up */
buf_t g_ldirs;             /* -L, system .mvh and its .c adapter */
buf_t g_idirs;             /* -I, a local .mvh */

/* every directory is held with its separator, so joining is concatenation */
void dirs_add(buf_t *pb, const char *d)
{
  size_t n;
  cbuf_t cb;
  n = strlen(d);
  if (n == 0) return;
  if (d[n - 1] == '/' || d[n - 1] == '\\') {
    *(sym_t *)bufnewbk(pb) = intern(d);
    return;
  }
  cbinit(&cb);
  cbputs(d, &cb);
  cbputc('/', &cb);
  cbputc(0, &cb);
  *(sym_t *)bufnewbk(pb) = intern((const char *)cbdata(&cb));
  cbfini(&cb);
}

void dirs_init(void)
{
  bufinit(&g_tdirs, sizeof(sym_t));
  bufinit(&g_ldirs, sizeof(sym_t));
  bufinit(&g_idirs, sizeof(sym_t));
  bufinit(&g_sysmvh, sizeof(sym_t));
  bufinit(&g_sysmod, sizeof(sym_t));
  bufinit(&g_seen, sizeof(sym_t));
  dirs_add(&g_tdirs, "target/");
  dirs_add(&g_tdirs, g_prefix_target);
  dirs_add(&g_ldirs, "lib/");
  dirs_add(&g_ldirs, g_prefix_lib);
  dirs_add(&g_idirs, "./");
}

void dirs_fini(void)
{
  buffini(&g_tdirs);
  buffini(&g_ldirs);
  buffini(&g_idirs);
  buffini(&g_sysmvh);
  buffini(&g_sysmod);
  buffini(&g_seen);
}

bool file_readable(const char *path)
{
  FILE *fp;
  fp = fopen(path, "r");
  if (fp == NULL) return false;
  fclose(fp);
  return true;
}

/* the directory part of a path, separator included, or "" */
void path_dir(const char *path, cbuf_t *pcb)
{
  size_t n;
  n = strlen(path);
  while (n > 0 && path[n - 1] != '/' && path[n - 1] != '\\') n -= 1;
  cbclear(pcb);
  cbput(path, n, pcb);
}

/* A dotted name is tried as written and with its dots turned into
 * separators, so $sys.stat finds sys.stat.mvh or sys/stat.mvh. [cco] */
bool find_in_dirs(buf_t *pb, const char *first, const char *name,
                         const char *ext, cbuf_t *pcb)
{
  size_t d, k, n;
  bool dotted;
  if (name[0] == '$') name += 1;
  dotted = false;
  for (k = 0; name[k] != '\0'; ++k) if (name[k] == '.') dotted = true;
  n = (first != NULL) ? buflen(pb) + 1 : buflen(pb);
  for (d = 0; d < n; ++d) {
    const char *dir;
    size_t pass;
    if (first != NULL) dir = (d == 0) ? first : symname(*(sym_t *)bufref(pb, d - 1));
    else dir = symname(*(sym_t *)bufref(pb, d));
    if (dir == NULL) continue;
    for (pass = 0; pass < (dotted ? 2u : 1u); ++pass) {
      cbclear(pcb);
      cbputs(dir, pcb);
      for (k = 0; name[k] != '\0'; ++k)
        cbputc((pass == 1 && name[k] == '.') ? '/' : name[k], pcb);
      cbputs(ext, pcb);
      cbputc(0, pcb);
      if (file_readable((const char *)cbdata(pcb))) return true;
    }
  }
  return false;
}

/* The system headers this module pulled in, in the order they were seen.
 * A link needs them, because each one names a .c adapter to compile in; a
 * -c compile needs them too, to write the marv-sys-deps line an object
 * carries so a later link can read its dependencies back out.  [cco] */

void sys_reset(void)
{
  bufclear(&g_sysmvh);
  bufclear(&g_sysmod);
  bufclear(&g_seen);
}

size_t sys_count(void) { return buflen(&g_sysmod); }

const char *sys_name(size_t i)
{
  if (i >= buflen(&g_sysmod)) return "";
  return symname(*(sym_t *)bufref(&g_sysmod, i));
}

static void sys_note(sym_t path, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&g_sysmvh); ++i)
    if (*(sym_t *)bufref(&g_sysmvh, i) == path) return;
  *(sym_t *)bufnewbk(&g_sysmvh) = path;
  *(sym_t *)bufnewbk(&g_sysmod) = name;
}

/* A system .mvh is paired with the .c adapter beside it, by its own name
 * first and then by the slashed spelling of a dotted one. */
bool adapter_of(const char *mvh, cbuf_t *pcb)
{
  size_t n, k, base;
  n = strlen(mvh);
  if (n < 4) return false;
  cbclear(pcb);
  cbput(mvh, n - 4, pcb);
  cbputs(".c", pcb);
  cbputc(0, pcb);
  if (file_readable((const char *)cbdata(pcb))) return true;
  base = n - 4;
  while (base > 0 && mvh[base - 1] != '/' && mvh[base - 1] != '\\') base -= 1;
  cbclear(pcb);
  cbput(mvh, base, pcb);
  for (k = base; k < n - 4; ++k) cbputc((mvh[k] == '.') ? '/' : mvh[k], pcb);
  cbputs(".c", pcb);
  cbputc(0, pcb);
  return file_readable((const char *)cbdata(pcb));
}

static bool seen_before(sym_t path)
{
  size_t i;
  for (i = 0; i < buflen(&g_seen); ++i)
    if (*(sym_t *)bufref(&g_seen, i) == path) return true;
  *(sym_t *)bufnewbk(&g_seen) = path;
  return false;
}

/* an (include [system] $name) form: -L for a system header, the including
 * file's own directory and then -I for a local one.  A header is read once;
 * a system one is recorded after its own includes, so a module's dependency
 * list names what it needs before what needs it.  [cco] */
void mod_include(mod_t *pm, const char *src, snode_t *pn)
{
  size_t i;
  bool sys;
  cbuf_t path;
  cbuf_t here;
  sys = false;
  for (i = 1; i < snlen(pn); ++i) {
    snode_t *pc;
    pc = snref(pn, i);
    if (pc->nt == SN_SYM && streql(symname(pc->sym), "system")) sys = true;
  }
  cbinit(&path);
  cbinit(&here);
  path_dir(src, &here);
  cbputc(0, &here);
  for (i = 1; i < snlen(pn); ++i) {
    const char *nm;
    snode_t *pc;
    sym_t rp;
    pc = snref(pn, i);
    if (pc->nt != SN_SYM) continue;
    nm = symname(pc->sym);
    if (streql(nm, "system")) continue;
    if (nm[0] == '$') nm += 1;
    if (sys) {
      if (!find_in_dirs(&g_ldirs, NULL, nm, ".mvh", &path))
        sneprintf(pn, "cannot find system interface %s on the -L path", nm);
    } else {
      if (!find_in_dirs(&g_idirs, (const char *)cbdata(&here), nm, ".mvh", &path))
        sneprintf(pn, "cannot find interface %s beside %s or on the -I path", nm, src);
    }
    rp = intern((const char *)cbdata(&path));
    if (seen_before(rp)) continue;
    if (!mod_iface(pm, symname(rp)))
      sneprintf(pn, "cannot read interface %s", symname(rp));
    if (sys) sys_note(rp, intern(nm));
  }
  cbfini(&here);
  cbfini(&path);
}

const char *sys_path(size_t i)
{
  if (i >= buflen(&g_sysmvh)) return "";
  return symname(*(sym_t *)bufref(&g_sysmvh, i));
}

bool mod_iface(mod_t *pm, const char *path)
{
  sread_t rd;
  snode_t nd;
  if (!sropen(&rd, path)) return false;
  sninit(&nd);
  while (srlopen(&rd, &nd)) {                  /* (interface $n ...) members */
    int k;
    bool inc;
    k = 0;
    inc = false;
    g_iface = 0;
    g_ifmod = 0;
    while (srlnext(&rd, &nd)) {
      if (k == 0 && nd.nt == SN_SYM) {
        k = 1;
        /* A header may include another; nearly every one in lib/ does, and
         * what it pulls in is a dependency of anything that includes it. */
        if (streql(symname(nd.sym), "include")) {
          snode_t f;
          sninit(&f);
          f.nt = SN_LIST;
          f.fname = nd.fname; f.line = nd.line; f.col = nd.col;
          sncpy(snnewbk(&f), &nd);
          while (srlnext(&rd, &nd)) sncpy(snnewbk(&f), &nd);
          mod_include(pm, path, &f);
          snfini(&f);
          inc = true;
          break;
        }
        continue;                                                /* 'interface' */
      }
      if (k == 1) {
        k = 2;
        if (nd.nt == SN_SYM) { g_iface = nd.sym; continue; }     /* its name */
      }
      mod_declare(pm, &nd);
    }
    g_iface = 0;
    g_ifmod = 0;
    if (inc) continue;
  }
  snfini(&nd);
  srclose(&rd);
  return true;
}

/* ================================================================ */
/* The unstacker: a (func ...) form becomes three-address code.
 *
 * The abstract stack holds operand descriptors. A local, a constant, a frame
 * address or a data reference sits on it without being copied; a copy appears
 * only when a value would otherwise be read after the thing it names has
 * changed (preserve_var), or when a loop would make the stack live across a
 * back edge (materialize). Follows the Scheme implementation's design. [cco] */

void fn_init(fn_t *pf)
{
  memset(pf, 0, sizeof(fn_t));
  bufinit(&pf->vars, sizeof(var_t));
  bufinit(&pf->code, sizeof(ir_t));
}

void fn_fini(fn_t *pf)
{
  size_t i;
  for (i = 0; i < buflen(&pf->code); ++i)
    buffini(&((ir_t *)bufref(&pf->code, i))->args);
  buffini(&pf->code);
  buffini(&pf->vars);
}

/* one function's worth of state while flattening */
typedef struct unst {
  mod_t *mod;
  fn_t *fn;
  buf_t stk;          /* opnd_t: the abstract operand stack, top at the back */
  buf_t ctrl;         /* ctrl_t: enclosing blocks and loops */
  snode_t *at;        /* the form being flattened, for diagnostics */
  int fuel;           /* guards against a macro that expands into itself */
  long long vabase;   /* where this function's variadic slot array starts */
  long long vaneed;   /* and how much of it the widest call needs */
} unst_t;

typedef struct ctrl {
  sym_t label;
  bool isloop;
  sym_t target;       /* label to jump to for a branch out */
  bool hasres;
  opnd_t res;
} ctrl_t;

static void u_push(unst_t *pu, const opnd_t *po)
{
  memcpy(bufnewbk(&pu->stk), po, sizeof(opnd_t));
}

static void u_pop(unst_t *pu, opnd_t *po)
{
  void *p;
  if (buflen(&pu->stk) == 0)
    sneprintf(pu->at, "operand stack is empty");
  p = bufpopbk(&pu->stk);
  memcpy(po, p, sizeof(opnd_t));
}

static ir_t *u_emit(unst_t *pu, iop_t op)
{
  ir_t *pi;
  pi = (ir_t *)bufnewbk(&pu->fn->code);
  memset(pi, 0, sizeof(ir_t));
  bufinit(&pi->args, sizeof(opnd_t));
  pi->op = op;
  pi->line = pu->at->line;
  pi->col = pu->at->col;
  return pi;
}

static void ir_arg(ir_t *pi, const opnd_t *po)
{
  memcpy(bufnewbk(&pi->args), po, sizeof(opnd_t));
}

static void u_temp(unst_t *pu, vt_t vt, opnd_t *po)
{
  var_t *pv;
  memset(po, 0, sizeof(opnd_t));
  po->kind = OK_VREG;
  po->vt = vt;
  po->sym = internf("$_t%d", pu->fn->ntemp);
  pu->fn->ntemp += 1;
  pv = (var_t *)bufnewbk(&pu->fn->vars);
  pv->name = po->sym;
  pv->vt = vt;
}

static sym_t u_label(unst_t *pu, const char *prefix)
{
  sym_t s;
  s = internf("L_%s_%s_%d", symname(pu->fn->name) + 1, prefix, pu->fn->nlabel);
  pu->fn->nlabel += 1;
  return s;
}

static void u_mov(unst_t *pu, const opnd_t *dst, const opnd_t *src)
{
  ir_t *pi;
  pi = u_emit(pu, IR_MOV);
  pi->hasdst = true;
  pi->dst = *dst;
  pi->vt = dst->vt;
  ir_arg(pi, src);
}

/* A macro use: substitute the arguments into the template, after renaming the
 * template's own locals and labels apart, and flatten what comes out. An
 * argument used twice is substituted twice, which the language documents.
 * Follows the Scheme implementation's instantiate/expand-form.   [cco] */
typedef struct subs { sym_t from; snode_t *node; sym_t to; } subs_t;

static void u_subst(snode_t *dst, snode_t *src, buf_t *psubs)
{
  size_t i;
  if (src->nt == SN_SYM) {
    for (i = 0; i < buflen(psubs); ++i) {
      subs_t *ps;
      ps = (subs_t *)bufref(psubs, i);
      if (ps->from != src->sym) continue;
      if (ps->node != NULL) {           /* a parameter: the argument form */
        sncpy(dst, ps->node);
        dst->line = src->line; dst->col = src->col; dst->fname = src->fname;
      } else {                          /* a private local or label */
        snclear(dst);
        dst->nt = SN_SYM; dst->sym = ps->to;
        dst->fname = src->fname; dst->line = src->line; dst->col = src->col;
      }
      return;
    }
  }
  sncpy(dst, src);
  if (src->nt != SN_LIST) return;
  snclear(dst);
  dst->nt = SN_LIST;
  dst->fname = src->fname; dst->line = src->line; dst->col = src->col;
  for (i = 0; i < snlen(src); ++i) u_subst(snnewbk(dst), snref(src, i), psubs);
}

static void u_form(unst_t *pu, snode_t *pn);

static void u_macro(unst_t *pu, snode_t *pn, mac_t *px)
{
  buf_t subs;
  snode_t out;
  size_t i, nargs, nparams;

  pu->fuel -= 1;
  if (pu->fuel <= 0) sneprintf(pn, "macro expansion too deep (recursive macro?)");
  nparams = buflen(&px->params);
  nargs = snlen(pn) - 1;
  if (nargs != nparams)
    sneprintf(pn, "macro %s takes %d arguments, given %d",
              symname(px->name), (int)nparams, (int)nargs);

  bufinit(&subs, sizeof(subs_t));
  for (i = 0; i < nparams; ++i) {       /* parameters stand for argument forms */
    subs_t *ps;
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = *(sym_t *)bufref(&px->params, i);
    ps->node = snref(pn, i + 1);
    ps->to = 0;
  }
  for (i = 0; i < buflen(&px->locals); ++i) {   /* private locals, renamed */
    var_t *pv;
    var_t *pnew;
    subs_t *ps;
    pv = (var_t *)bufref(&px->locals, i);
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = pv->name;
    ps->node = NULL;
    ps->to = internf("%s$m%d", symname(pv->name), ++g_mxno);
    pnew = (var_t *)bufnewbk(&pu->fn->vars);    /* allocated in the caller */
    pnew->name = ps->to;
    pnew->vt = pv->vt;
  }
  for (i = 0; i < buflen(&px->labels); ++i) {   /* private labels, renamed */
    subs_t *ps;
    sym_t l;
    l = *(sym_t *)bufref(&px->labels, i);
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = l;
    ps->node = NULL;
    ps->to = internf("%s$m%d", symname(l), ++g_mxno);
  }

  /* The declared effect, checked against what the expansion really does to
   * the abstract stack: the inputs must be there with the declared types, and
   * the outputs must be what is left. A body that branches away or returns
   * clears the stack, and is accepted as the Scheme implementation accepts a
   * 'dead' simulation.  [cco] */
  {
    size_t nin, nout, depth, k;
    nin = buflen(&px->ein);
    nout = buflen(&px->eout);
    depth = buflen(&pu->stk);
    if (depth < nin)
      sneprintf(pn, "macro %s wants %d operands, %d on the stack",
                symname(px->name), (int)nin, (int)depth);
    for (k = 0; k < nin; ++k) {        /* the last declared input is on top */
      vt_t want;
      opnd_t *po;
      want = *(vt_t *)bufref(&px->ein, nin - 1 - k);
      po = (opnd_t *)bufref(&pu->stk, depth - 1 - k);
      if (want != VT_ANY && po->vt != VT_ANY && want != po->vt)
        sneprintf(pn, "macro %s takes %s where the stack has %s",
                  symname(px->name), vtname(want), vtname(po->vt));
    }

    sninit(&out);
    u_subst(&out, &px->body, &subs);
    buffini(&subs);
    u_form(pu, &out);
    snfini(&out);

    {
      size_t now;
      bool dead;
      now = buflen(&pu->stk);
      dead = false;
      if (buflen(&pu->fn->code) > 0) {       /* did the body leave for good? */
        iop_t last;
        last = ((ir_t *)bufref(&pu->fn->code, buflen(&pu->fn->code) - 1))->op;
        dead = (last == IR_RETURN || last == IR_JUMP || last == IR_RETCALL
                || last == IR_TJUMP || last == IR_TJUMPI
                || last == IR_RETCALLI || last == IR_UNREACHABLE || last == IR_BRTABLE);
      }
      if (!dead && now + nin >= depth) {
        if (now != depth - nin + nout)
          sneprintf(pn, "macro %s declares %d result%s, its body leaves %d",
                    symname(px->name), (int)nout, (nout == 1) ? "" : "s",
                    (int)(now - (depth - nin)));
        for (k = 0; k < nout; ++k) {
          vt_t want;
          opnd_t *po;
          want = *(vt_t *)bufref(&px->eout, nout - 1 - k);
          po = (opnd_t *)bufref(&pu->stk, now - 1 - k);
          if (want != VT_ANY && po->vt != VT_ANY && want != po->vt)
            sneprintf(pn, "macro %s declares %s, its body leaves %s",
                      symname(px->name), vtname(want), vtname(po->vt));
        }
      }
    }
  }
  pu->fuel += 1;
}

static fld_t *u_field(unst_t *pu, snode_t *pn, sym_t rname, sym_t fname)
{
  rec_t *pr;
  size_t i;
  pr = mod_record(pu->mod, rname);
  if (pr == NULL) sneprintf(pn, "unknown record %s", symname(rname));
  for (i = 0; i < buflen(&pr->fields); ++i) {
    fld_t *pf;
    pf = (fld_t *)bufref(&pr->fields, i);
    if (pf->name == fname) return pf;
  }
  sneprintf(pn, "record %s has no field %s", symname(rname), symname(fname));
  return NULL;
}

static bool u_isvar(unst_t *pu, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pu->fn->vars); ++i)
    if (((var_t *)bufref(&pu->fn->vars, i))->name == name) return true;
  return false;
}

static vt_t u_vartype(unst_t *pu, sym_t name)
{
  size_t i;
  for (i = 0; i < buflen(&pu->fn->vars); ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pu->fn->vars, i);
    if (pv->name == name) return pv->vt;
  }
  sneprintf(pu->at, "undeclared local or parameter %s", symname(name));
  return VT_VOID;
}

/* before $x is written, copy any pending reference to $x out of the stack */
static void u_preserve(unst_t *pu, sym_t vname)
{
  size_t i;
  for (i = 0; i < buflen(&pu->stk); ++i) {
    opnd_t *po;
    po = (opnd_t *)bufref(&pu->stk, i);
    if (po->kind == OK_VREG && po->sym == vname) {
      opnd_t tmp;
      u_temp(pu, po->vt, &tmp);
      u_mov(pu, &tmp, po);
      *po = tmp;
    }
  }
}

/* a loop makes the stack live across a back edge: put it all in temporaries */
static void u_materialize(unst_t *pu)
{
  size_t i;
  for (i = 0; i < buflen(&pu->stk); ++i) {
    opnd_t *po;
    po = (opnd_t *)bufref(&pu->stk, i);
    if (po->kind != OK_VREG) {
      opnd_t tmp;
      u_temp(pu, po->vt, &tmp);
      u_mov(pu, &tmp, po);
      *po = tmp;
    }
  }
}

static ctrl_t *u_ctrl(unst_t *pu, sym_t label)
{
  size_t n;
  n = buflen(&pu->ctrl);
  while (n > 0) {
    ctrl_t *pc;
    n -= 1;
    pc = (ctrl_t *)bufref(&pu->ctrl, n);
    if (pc->label == label) return pc;
  }
  sneprintf(pu->at, "unknown branch label %s", symname(label));
  return NULL;
}

static bool u_terminates(snode_t *pn);
static void u_seq(unst_t *pu, snode_t *pn, size_t from);

/* does this instruction leave the block for good? an (if ...) does
 * when both of its arms do, which is how a tail-recursive body ends */
static bool u_terminates(snode_t *pn)
{
  const char *h;
  size_t i;
  bool sawthen, sawelse, thenok, elseok;
  if (pn->nt != SN_LIST || snlen(pn) == 0 || snref(pn, 0)->nt != SN_SYM) return false;
  h = symname(snref(pn, 0)->sym);
  if (streql(h, "return") || streql(h, "br") || streql(h, "unreachable")
      || streql(h, "return_call") || streql(h, "return_call_indirect")
      || streql(h, "trampoline_jump") || streql(h, "trampoline_jump_indirect"))
    return true;
  if (streql(h, "seq") || streql(h, "then") || streql(h, "else")) {
    if (snlen(pn) < 2) return false;
    return u_terminates(snref(pn, snlen(pn) - 1));
  }
  if (streql(h, "if")) {
    sawthen = false; sawelse = false; thenok = false; elseok = false;
    for (i = 1; i < snlen(pn); ++i) {
      snode_t *pc;
      const char *k;
      pc = snref(pn, i);
      if (pc->nt != SN_LIST || snlen(pc) == 0 || snref(pc, 0)->nt != SN_SYM) continue;
      k = symname(snref(pc, 0)->sym);
      if (streql(k, "then")) { sawthen = true; thenok = u_terminates(pc); }
      else if (streql(k, "else")) { sawelse = true; elseok = u_terminates(pc); }
    }
    return sawthen && sawelse && thenok && elseok;
  }
  return false;
}

static bool u_seq_terminates(snode_t *pn, size_t from)
{
  size_t n;
  n = snlen(pn);
  if (n <= from) return false;
  return u_terminates(snref(pn, n - 1));
}

/* control forms: block, loop, if */
static void u_block(unst_t *pu, snode_t *pn)
{
  ctrl_t *pc;
  sym_t endl;
  vt_t rt;
  size_t first;
  opnd_t res;
  bool isloop;
  sym_t startl;
  const char *h;

  h = symname(snref(pn, 0)->sym);
  isloop = streql(h, "loop");
  first = 1;
  rt = VT_VOID;
  memset(&res, 0, sizeof(opnd_t));
  if (first < snlen(pn) && snref(pn, first)->nt == SN_SYM) first += 1;   /* $label */
  if (first < snlen(pn) && snref(pn, first)->nt == SN_LIST
      && snlen(snref(pn, first)) == 2
      && streql(symname(snref(snref(pn, first), 0)->sym), "result")) {
    rt = vt_of_sym(snref(snref(pn, first), 1)->sym);
    first += 1;
  }
  if (isloop) u_materialize(pu);
  if (rt != VT_VOID) u_temp(pu, rt, &res);

  startl = u_label(pu, isloop ? "loop" : "blk");
  endl = isloop ? u_label(pu, "loop_end") : startl;
  pc = (ctrl_t *)bufnewbk(&pu->ctrl);
  memset(pc, 0, sizeof(ctrl_t));
  pc->label = (snlen(pn) > 1 && snref(pn, 1)->nt == SN_SYM) ? snref(pn, 1)->sym : 0;
  pc->isloop = isloop;
  pc->target = isloop ? startl : endl;
  pc->hasres = (rt != VT_VOID);
  pc->res = res;

  if (isloop) {
    ir_t *pi;
    pi = u_emit(pu, IR_LOOPBEG);
    pi->name = startl;
  }
  u_seq(pu, pn, first);
  if (rt != VT_VOID && !u_seq_terminates(pn, first) && buflen(&pu->stk) > 0) {
    opnd_t top;
    u_pop(pu, &top);
    u_mov(pu, &res, &top);
  }
  {
    ir_t *pi;
    pi = u_emit(pu, isloop ? IR_LOOPEND : IR_LABEL);
    pi->name = endl;
  }
  bufpopbk(&pu->ctrl);
  if (rt != VT_VOID) u_push(pu, &res);
}

static void u_if(unst_t *pu, snode_t *pn)
{
  size_t i, first;
  vt_t rt;
  opnd_t cnd, res;
  snode_t *pthen; snode_t *pelse;
  sym_t elsel, endl;
  ir_t *pi;

  first = 1;
  rt = VT_VOID;
  if (first < snlen(pn) && snref(pn, first)->nt == SN_LIST
      && snlen(snref(pn, first)) == 2
      && streql(symname(snref(snref(pn, first), 0)->sym), "result")) {
    rt = vt_of_sym(snref(snref(pn, first), 1)->sym);
    first += 1;
  }
  pthen = NULL; pelse = NULL;
  for (i = first; i < snlen(pn); ++i) {
    snode_t *pc;
    pc = snref(pn, i);
    if (pc->nt == SN_LIST && snlen(pc) > 0 && snref(pc, 0)->nt == SN_SYM) {
      const char *h;
      h = symname(snref(pc, 0)->sym);
      if (streql(h, "then")) { pthen = pc; continue; }
      if (streql(h, "else")) { pelse = pc; continue; }
    }
    u_form(pu, pc);                  /* the condition, in folded position */
  }
  if (pthen == NULL) sneprintf(pn, "(if ...) without a (then ...) clause");
  u_pop(pu, &cnd);
  memset(&res, 0, sizeof(opnd_t));
  if (rt != VT_VOID) u_temp(pu, rt, &res);

  elsel = u_label(pu, "else");
  endl = u_label(pu, "endif");
  pi = u_emit(pu, IR_BRFALSE);
  pi->name = (pelse != NULL) ? elsel : endl;
  ir_arg(pi, &cnd);

  u_seq(pu, pthen, 1);
  if (rt != VT_VOID && !u_seq_terminates(pthen, 1) && buflen(&pu->stk) > 0) {
    opnd_t top;
    u_pop(pu, &top);
    u_mov(pu, &res, &top);
  }
  if (pelse != NULL) {
    if (!u_seq_terminates(pthen, 1)) { pi = u_emit(pu, IR_JUMP); pi->name = endl; }
    pi = u_emit(pu, IR_LABEL); pi->name = elsel;
    u_seq(pu, pelse, 1);
    if (rt != VT_VOID && !u_seq_terminates(pelse, 1) && buflen(&pu->stk) > 0) {
      opnd_t top;
      u_pop(pu, &top);
      u_mov(pu, &res, &top);
    }
  }
  pi = u_emit(pu, IR_LABEL);
  pi->name = endl;
  if (rt != VT_VOID) u_push(pu, &res);
}

/* one instruction in folded form: operands first, then the operation */
static void u_form(unst_t *pu, snode_t *pn)
{
  const instr_t *pin;
  const char *h;
  sym_t head;
  size_t i, first, ciargs;
  vt_t cires;
  vt_t citps[16];              /* an indirect form's stated parameter types */
  int ncitps;
  opnd_t o, a, b, c;
  ir_t *pi;

  ciargs = 0;
  cires = VT_VOID;
  ncitps = 0;
  pu->at = pn;
  if (pn->nt == SN_SYM) {             /* $x as an operand is (local.get $x) */
    const char *vn;
    vn = symname(pn->sym);
    if (vn != NULL && vn[0] == '$') {
      /* a constant is reached only through (const $x), as in the Scheme
       * implementation, so that a local never silently shadows one */
      if (!u_isvar(pu, pn->sym) && mod_const(pu->mod, pn->sym) != NULL)
        sneprintf(pn, "%s is a constant here; write (const %s)", vn, vn);
      memset(&o, 0, sizeof(opnd_t));
      o.kind = OK_VREG;
      o.sym = pn->sym;
      o.vt = u_vartype(pu, o.sym);
      u_push(pu, &o);
      return;
    }
  }
  if (pn->nt != SN_LIST || snlen(pn) == 0 || snref(pn, 0)->nt != SN_SYM)
    sneprintf(pn, "an instruction must be a list starting with a name");
  head = snref(pn, 0)->sym;
  h = symname(head);

  {                                   /* a macro use, by its $-name */
    mac_t *px;
    px = mod_macro(pu->mod, head);
    if (px != NULL) { u_macro(pu, pn, px); return; }
  }
  if (streql(h, "seq")) {             /* a macro template's body */
    u_seq(pu, pn, 1);
    return;
  }
  if (streql(h, "block") || streql(h, "loop")) { u_block(pu, pn); return; }
  if (streql(h, "if")) { u_if(pu, pn); return; }
  if (streql(h, "const")) {                       /* a named constant */
    cst_t *pc;
    if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM)
      sneprintf(pn, "(const $name) expected");
    pc = mod_const(pu->mod, snref(pn, 1)->sym);
    if (pc == NULL) sneprintf(pn, "unknown constant %s", symname(snref(pn, 1)->sym));
    if (!pc->isnum) { u_form(pu, &pc->val); return; }
    memset(&o, 0, sizeof(opnd_t));
    o.kind = OK_IMM; o.vt = pc->vt; o.ival = pc->ival; o.dval = pc->dval;
    u_push(pu, &o);
    return;
  }

  /* Record and variadic forms, rewritten the way the Scheme implementation
   * rewrites them: a layout query becomes a constant, a field access becomes
   * a load or store at the field's offset, and a va.* intrinsic becomes work
   * on the $ap$ cursor.  [cco] */
  if (streql(h, "sizeof") || streql(h, "sizeof64") || streql(h, "alignof")) {
    rec_t *pr;
    if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) sneprintf(pn, "%s needs a record", h);
    pr = mod_record(pu->mod, snref(pn, 1)->sym);
    if (pr == NULL) sneprintf(pn, "unknown record %s", symname(snref(pn, 1)->sym));
    memset(&o, 0, sizeof(opnd_t));
    o.kind = OK_IMM;
    o.vt = streql(h, "sizeof64") ? VT_I64 : VT_I32;
    o.ival = streql(h, "alignof") ? pr->align : pr->size;
    u_push(pu, &o);
    return;
  }
  if (streql(h, "offsetof")) {
    fld_t *pf;
    if (snlen(pn) < 3) sneprintf(pn, "offsetof needs a record and a field");
    pf = u_field(pu, pn, snref(pn, 1)->sym, snref(pn, 2)->sym);
    memset(&o, 0, sizeof(opnd_t));
    o.kind = OK_IMM; o.vt = VT_I32; o.ival = pf->off;
    u_push(pu, &o);
    return;
  }
  if (streql(h, "field.addr") || streql(h, "field.get") || streql(h, "field.set")) {
    fld_t *pf;
    if (snlen(pn) < 4) sneprintf(pn, "%s needs a record, a field and a pointer", h);
    pf = u_field(pu, pn, snref(pn, 1)->sym, snref(pn, 2)->sym);
    u_form(pu, snref(pn, 3));             /* the pointer */
    if (streql(h, "field.addr")) {
      u_pop(pu, &a);
      if (pf->off == 0) { u_push(pu, &a); return; }
      if (a.kind == OK_FRAME) { a.ival += pf->off; u_push(pu, &a); return; }
      memset(&b, 0, sizeof(opnd_t));
      b.kind = OK_IMM; b.vt = VT_I32; b.ival = pf->off;
      u_temp(pu, VT_PTR, &o);
      pi = u_emit(pu, IR_BINARY);
      pi->hasdst = true; pi->dst = o; pi->vt = VT_PTR; pi->name = intern("ptr.add");
      ir_arg(pi, &a); ir_arg(pi, &b);
      u_push(pu, &o);
      return;
    }
    if (pf->lop == 0 || pf->sop == 0)
      sneprintf(pn, "%s on an aggregate field; use field.addr", h);
    if (streql(h, "field.get")) {
      long long off;
      u_pop(pu, &a);
      off = pf->off;
      if (a.kind == OK_FRAME) { off += a.ival; a.ival = 0; }
      u_temp(pu, pf->vt, &o);
      pi = u_emit(pu, IR_LOAD);
      pi->hasdst = true; pi->dst = o; pi->vt = pf->vt; pi->name = pf->lop;
      pi->off = off;
      ir_arg(pi, &a);
      u_push(pu, &o);
    } else {
      long long off;
      if (snlen(pn) < 5) sneprintf(pn, "field.set needs a value");
      u_form(pu, snref(pn, 4));           /* the value */
      u_pop(pu, &b);
      u_pop(pu, &a);
      off = pf->off;
      if (a.kind == OK_FRAME) { off += a.ival; a.ival = 0; }
      pi = u_emit(pu, IR_STORE);
      pi->name = pf->sop; pi->off = off; pi->vt = pf->vt;
      ir_arg(pi, &a); ir_arg(pi, &b);
    }
    return;
  }
  if (streql(h, "va.start")) {            /* the cursor itself */
    memset(&o, 0, sizeof(opnd_t));
    o.kind = OK_VREG; o.sym = intern("$ap$"); o.vt = VT_PTR;
    u_push(pu, &o);
    return;
  }
  if (streql(h, "va.count")) {            /* the count sits one slot below it */
    opnd_t ap;
    memset(&ap, 0, sizeof(opnd_t));
    ap.kind = OK_VREG; ap.sym = intern("$ap$"); ap.vt = VT_PTR;
    u_temp(pu, VT_I32, &o);
    pi = u_emit(pu, IR_LOAD);
    pi->hasdst = true; pi->dst = o; pi->vt = VT_I32; pi->name = intern("i32.load");
    pi->off = -16;
    ir_arg(pi, &ap);
    u_push(pu, &o);
    return;
  }
  if (streql(h, "va.arg")) {              /* advance the cursor, read the slot */
    opnd_t ap, sixteen;
    vt_t vt;
    const char *tn;
    if (snlen(pn) < 3 || snref(pn, 1)->nt != SN_SYM || snref(pn, 2)->nt != SN_SYM)
      sneprintf(pn, "va.arg needs a cursor and a type");
    memset(&ap, 0, sizeof(opnd_t));
    ap.kind = OK_VREG; ap.sym = snref(pn, 1)->sym; ap.vt = VT_PTR;
    memset(&sixteen, 0, sizeof(opnd_t));
    sixteen.kind = OK_IMM; sixteen.vt = VT_I32; sixteen.ival = 16;
    u_preserve(pu, ap.sym);
    u_temp(pu, VT_PTR, &o);
    pi = u_emit(pu, IR_BINARY);
    pi->hasdst = true; pi->dst = o; pi->vt = VT_PTR; pi->name = intern("ptr.add");
    ir_arg(pi, &ap); ir_arg(pi, &sixteen);
    u_mov(pu, &ap, &o);
    tn = symname(snref(pn, 2)->sym);
    vt = vt_of_sym(snref(pn, 2)->sym);
    if (vt == VT_VOID) sneprintf(pn, "va.arg cannot read a %s", tn);
    u_temp(pu, (vt == VT_F32) ? VT_F64 : vt, &b);
    pi = u_emit(pu, IR_LOAD);
    pi->hasdst = true; pi->dst = b; pi->vt = b.vt;
    pi->name = intern((vt == VT_I32) ? "i32.load" :
                      (vt == VT_I64) ? "i64.load" :
                      (vt == VT_PTR) ? "ptr.load" : "f64.load");
    pi->off = -16;
    ir_arg(pi, &ap);
    if (vt == VT_F32) {                   /* the slot holds it widened */
      u_temp(pu, VT_F32, &o);
      pi = u_emit(pu, IR_UNARY);
      pi->hasdst = true; pi->dst = o; pi->vt = VT_F32;
      pi->name = intern("f32.demote_f64");
      ir_arg(pi, &b);
      u_push(pu, &o);
    } else {
      u_push(pu, &b);
    }
    return;
  }

  pin = instr_lookup(head);
  if (pin == NULL) sneprintf(pn, "unknown instruction %s", h);
  /* nimm is how many immediates the form may carry, not how many it must:
   * a load's offset and align are optional keywords.  What is certain is that
   * an instruction taking any immediate needs something after the head, and
   * the readers below index that position without looking. */
  if (pin->nimm > 0 && snlen(pn) < 2)
    sneprintf(pn, "%s takes an immediate operand", h);

  /* immediates come first in the form, then the folded operands */
  first = 1 + (size_t)pin->nimm;
  if (pin->kind == IK_CALL_INDIRECT || pin->kind == IK_RETURN_CALL_INDIRECT) {
    first = 1;
    while (first < snlen(pn) && snref(pn, first)->nt == SN_LIST
           && snlen(snref(pn, first)) > 0 && snref(snref(pn, first), 0)->nt == SN_SYM) {
      const char *k;
      k = symname(snref(snref(pn, first), 0)->sym);
      if (streql(k, "type")) { first += 1; continue; }          /* (type ...) */
      if (streql(k, "param")) {          /* (param T ...): every type counts */
        size_t ci;
        for (ci = 1; ci < snlen(snref(pn, first)); ++ci) {
          snode_t *pt;
          pt = snref(snref(pn, first), ci);
          if (pt->nt == SN_SYM) {
            if (ncitps < 16) citps[ncitps] = vt_of_sym(pt->sym);
            ncitps += 1;
          }
        }
        ciargs = (size_t)ncitps;
        first += 1; continue;
      }
      if (streql(k, "result")) {                                /* (result T) */
        snode_t *pt;
        pt = snref(snref(pn, first), snlen(snref(pn, first)) - 1);
        cires = (pt->nt == SN_SYM) ? vt_of_sym(pt->sym) : VT_PTR;
        first += 1;
        continue;
      }
      break;                          /* the operands start here */
    }
  }
  if (pin->kind == IK_BR_TABLE) {     /* a run of labels, then the index */
    first = 1;
    while (first < snlen(pn) && snref(pn, first)->nt == SN_SYM) first += 1;
  }
  if (pin->kind == IK_LOAD || pin->kind == IK_STORE) {
    first = 1;
    while (first + 1 < snlen(pn) && snref(pn, first)->nt == SN_SYM) {
      const char *k;
      k = symname(snref(pn, first)->sym);
      if (!streql(k, "offset") && !streql(k, "align")) break;
      first += 2;                          /* the keyword and its value */
    }
  }
  for (i = first; i < snlen(pn); ++i) u_form(pu, snref(pn, i));

  switch (pin->kind) {
    case IK_NOP: return;
    case IK_UNREACHABLE: u_emit(pu, IR_UNREACHABLE); return;
    case IK_DROP: u_pop(pu, &a); return;
    case IK_CONST: {
      snode_t *pv;
      memset(&o, 0, sizeof(opnd_t));
      o.kind = OK_IMM; o.vt = pin->res;
      if (pin->nimm > 0) {
        if (snlen(pn) < 2) sneprintf(pn, "%s needs a value", h);
        pv = snref(pn, 1);
        if (pv->nt == SN_INT) o.ival = pv->ival;
        else if (pv->nt == SN_FLT) o.dval = pv->dval;
        else if (pv->nt == SN_SYM) {       /* (i32.const $X): a named constant */
          cst_t *pc;
          pc = mod_const(pu->mod, pv->sym);
          if (pc == NULL) sneprintf(pn, "unknown constant %s", symname(pv->sym));
          if (!pc->isnum) { u_form(pu, &pc->val); return; }
          o.ival = pc->ival; o.dval = pc->dval;
        }
        else sneprintf(pn, "%s needs a number", h);
      }
      u_push(pu, &o);
      return;
    }
    case IK_FRAME_ADDR:
      memset(&o, 0, sizeof(opnd_t));
      o.kind = OK_FRAME; o.vt = VT_PTR;
      o.ival = (snlen(pn) > 1 && snref(pn, 1)->nt == SN_INT) ? snref(pn, 1)->ival : 0;
      u_push(pu, &o);
      return;
    case IK_REF_DATA: case IK_REF_FUNC:
      memset(&o, 0, sizeof(opnd_t));
      o.kind = (pin->kind == IK_REF_DATA) ? OK_DATA : OK_FUNC;
      o.vt = VT_PTR;
      if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) sneprintf(pn, "%s needs a name", h);
      o.sym = snref(pn, 1)->sym;
      if (snlen(pn) > 2 && snref(pn, 2)->nt == SN_INT) o.ival = snref(pn, 2)->ival;
      u_push(pu, &o);
      return;
    case IK_LOCAL_GET:
      memset(&o, 0, sizeof(opnd_t));
      o.kind = OK_VREG;
      o.sym = snref(pn, 1)->sym;
      o.vt = u_vartype(pu, o.sym);
      u_push(pu, &o);
      return;
    case IK_LOCAL_SET: case IK_LOCAL_TEE: {
      sym_t vn;
      vn = snref(pn, 1)->sym;
      u_pop(pu, &a);
      u_preserve(pu, vn);
      memset(&o, 0, sizeof(opnd_t));
      o.kind = OK_VREG; o.sym = vn; o.vt = u_vartype(pu, vn);
      u_mov(pu, &o, &a);
      if (pin->kind == IK_LOCAL_TEE) u_push(pu, &o);
      return;
    }
    case IK_GLOBAL_GET:
      u_temp(pu, mod_gtype(pu->mod, snref(pn, 1)->sym), &o);
      pi = u_emit(pu, IR_GGET);
      pi->hasdst = true; pi->dst = o; pi->vt = o.vt; pi->name = snref(pn, 1)->sym;
      u_push(pu, &o);
      return;
    case IK_GLOBAL_SET:
      u_pop(pu, &a);
      pi = u_emit(pu, IR_GSET);
      pi->name = snref(pn, 1)->sym;
      pi->vt = mod_gtype(pu->mod, pi->name);
      ir_arg(pi, &a);
      return;
    case IK_MEMORY:
      /* the run is given as it is in WebAssembly: where, what, how many */
      u_pop(pu, &c); u_pop(pu, &b); u_pop(pu, &a);
      pi = u_emit(pu, IR_MEMORY);
      pi->name = head;
      ir_arg(pi, &a); ir_arg(pi, &b); ir_arg(pi, &c);
      return;
    case IK_SELECT:
      u_pop(pu, &c); u_pop(pu, &b); u_pop(pu, &a);
      u_temp(pu, a.vt, &o);
      pi = u_emit(pu, IR_SELECT);
      pi->hasdst = true; pi->dst = o; pi->vt = o.vt;
      ir_arg(pi, &a); ir_arg(pi, &b); ir_arg(pi, &c);
      u_push(pu, &o);
      return;
    case IK_UNARY: case IK_CONVERT:
      u_pop(pu, &a);
      u_temp(pu, pin->res, &o);
      pi = u_emit(pu, IR_UNARY);
      pi->hasdst = true; pi->dst = o; pi->vt = o.vt; pi->name = head;
      ir_arg(pi, &a);
      u_push(pu, &o);
      return;
    case IK_BINARY: case IK_COMPARE:
      u_pop(pu, &b); u_pop(pu, &a);
      /* (frame.addr K) plus or minus a constant folds into the address */
      if (a.kind == OK_FRAME && b.kind == OK_IMM
          && (streql(h, "ptr.add") || streql(h, "ptr.sub"))) {
        o = a;
        o.ival = streql(h, "ptr.add") ? (a.ival + b.ival) : (a.ival - b.ival);
        u_push(pu, &o);
        return;
      }
      u_temp(pu, pin->res, &o);
      pi = u_emit(pu, IR_BINARY);
      pi->hasdst = true; pi->dst = o; pi->vt = o.vt; pi->name = head;
      ir_arg(pi, &a); ir_arg(pi, &b);
      u_push(pu, &o);
      return;
    case IK_LOAD: case IK_STORE: {
      long long off;
      int aln;
      size_t k;
      off = 0; aln = 0;
      for (k = 1; k + 1 < first; k += 2) {
        const char *kw;
        snode_t *pv;
        long long v;
        kw = symname(snref(pn, k)->sym);
        pv = snref(pn, k + 1);
        if (pv->nt == SN_INT) {
          v = pv->ival;
        } else if (pv->nt == SN_SYM) {     /* offset $CONST / align $CONST */
          cst_t *pc;
          pc = mod_const(pu->mod, pv->sym);
          if (pc == NULL || !pc->isnum)
            sneprintf(pn, "%s takes a number or a scalar constant", kw);
          v = pc->ival;
        } else {
          sneprintf(pn, "%s takes a number or a scalar constant", kw);
          v = 0;
        }
        if (streql(kw, "offset")) off = v;
        else if (streql(kw, "align")) aln = (int)v;
      }
      if (pin->kind == IK_LOAD) {
        u_pop(pu, &a);
        if (a.kind == OK_FRAME) { off += a.ival; a.ival = 0; }
        u_temp(pu, pin->res, &o);
        pi = u_emit(pu, IR_LOAD);
        pi->hasdst = true; pi->dst = o; pi->vt = o.vt; pi->name = head;
        pi->off = off; pi->aln = aln;
        ir_arg(pi, &a);
        u_push(pu, &o);
      } else {
        u_pop(pu, &b); u_pop(pu, &a);
        if (a.kind == OK_FRAME) { off += a.ival; a.ival = 0; }
        pi = u_emit(pu, IR_STORE);
        pi->name = head; pi->off = off; pi->aln = aln;
        pi->vt = pin->ops[1];
        ir_arg(pi, &a); ir_arg(pi, &b);
      }
      return;
    }
    case IK_RETURN:
      pi = u_emit(pu, IR_RETURN);
      pi->vt = pu->fn->res;
      if (pu->fn->res != VT_VOID && buflen(&pu->stk) > 0) {
        u_pop(pu, &a);
        ir_arg(pi, &a);
      }
      bufclear(&pu->stk);
      return;
    case IK_BR_TABLE: {
      size_t k;
      u_pop(pu, &a);                  /* the index */
      pi = u_emit(pu, IR_BRTABLE);
      ir_arg(pi, &a);
      for (k = 1; k + 1 < first + 1 && k < snlen(pn) && snref(pn, k)->nt == SN_SYM; ++k) {
        ctrl_t *pc;
        opnd_t lo;
        pc = u_ctrl(pu, snref(pn, k)->sym);
        memset(&lo, 0, sizeof(opnd_t));
        lo.kind = OK_FUNC;            /* a label, carried as a name */
        lo.sym = pc->target;
        ir_arg(pi, &lo);
      }
      bufclear(&pu->stk);
      return;
    }
    case IK_BR: case IK_BR_IF: {
      ctrl_t *pc;
      pc = u_ctrl(pu, snref(pn, 1)->sym);
      if (pin->kind == IK_BR_IF) {
        u_pop(pu, &a);                   /* the condition */
        if (pc->hasres && buflen(&pu->stk) > 0) {
          opnd_t top;
          top = *(opnd_t *)bufref(&pu->stk, buflen(&pu->stk) - 1);
          u_mov(pu, &pc->res, &top);
        }
        pi = u_emit(pu, IR_BRTRUE);      /* br_if jumps when it holds */
        pi->name = pc->target;
        pi->vt = VT_I32;
        ir_arg(pi, &a);
      } else {
        if (pc->hasres && buflen(&pu->stk) > 0) {
          u_pop(pu, &a);                 /* the value the block yields */
          u_mov(pu, &pc->res, &a);
        }
        pi = u_emit(pu, IR_JUMP);
        pi->name = pc->target;
        bufclear(&pu->stk);
      }
      return;
    }
    case IK_CALL: case IK_RETURN_CALL:
    case IK_CALL_INDIRECT: case IK_RETURN_CALL_INDIRECT: {
      bool indirect, tail;
      size_t nargs, have, k;
      sig_t *ps;
      vt_t res;
      sym_t tjump_letters;
      indirect = (pin->kind == IK_CALL_INDIRECT || pin->kind == IK_RETURN_CALL_INDIRECT);
      tjump_letters = 0;
      if (streql(h, "trampoline_jump") || streql(h, "trampoline_jump_indirect")) {
        size_t ti;
        sym_t letters;
        bool known;
        if (pu->fn->res != VT_PTR && pu->fn->res != VT_I64)
          sneprintf(pn, "a trampoline_jump needs its function to return ptr or i64, not %s",
                    vtname(pu->fn->res));
        if (indirect) {
          /* no callee to look up: the shape is the one the form states */
          cbuf_t lb;
          int ci;
          cbinit(&lb);
          cbputc(sig_letter(cires), &lb);
          for (ci = 0; ci < ncitps; ++ci) cbputc(sig_letter(citps[ci]), &lb);
          cbputc(0, &lb);
          letters = intern((const char *)cbdata(&lb));
          cbfini(&lb);
        } else {
          ps = mod_sig(pu->mod, snref(pn, 1)->sym);
          if (ps == NULL) sneprintf(pn, "unknown function %s", symname(snref(pn, 1)->sym));
          letters = sig_letters(ps);
        }
        known = false;
        for (ti = 0; ti < buflen(&pu->mod->tramps); ++ti)
          if (*(sym_t *)bufref(&pu->mod->tramps, ti) == letters) { known = true; break; }
        if (!known)
          sneprintf(pn, "no trampoline_func takes the shape %s that this jump needs",
                    symname(letters));
        tjump_letters = letters;
      }
      tail = (pin->kind == IK_RETURN_CALL || pin->kind == IK_RETURN_CALL_INDIRECT);
      res = VT_VOID;
      ps = NULL;
      if (!indirect) {
        ps = mod_sig(pu->mod, snref(pn, 1)->sym);
        if (ps == NULL) sneprintf(pn, "unknown function %s", symname(snref(pn, 1)->sym));
        res = ps->res;
        nargs = buflen(&ps->ptypes);
      } else {
        nargs = ciargs + 1;              /* the arguments, then the pointer */
        res = cires;
      }
      pi = u_emit(pu, streql(h, "trampoline_jump") ? IR_TJUMP
                      : streql(h, "trampoline_jump_indirect") ? IR_TJUMPI
                      : (tail ? (indirect ? IR_RETCALLI : IR_RETCALL)
                              : (indirect ? IR_CALLI : IR_CALL)));
      if (tjump_letters != 0) pi->sig = tjump_letters;
      if (!indirect) pi->name = (ps != NULL && ps->cname != 0) ? ps->cname
                                                              : snref(pn, 1)->sym;
      have = buflen(&pu->stk);
      if (have < nargs)
        sneprintf(pn, "%s wants %d arguments, %d on the stack", h, (int)nargs, (int)have);
      for (k = have - nargs; k < have; ++k)
        ir_arg(pi, (opnd_t *)bufref(&pu->stk, k));
      for (k = 0; k < nargs; ++k) bufpopbk(&pu->stk);
      pi->vt = res;
      if (!tail && res != VT_VOID) {
        u_temp(pu, res, &o);
        pi = (ir_t *)bufref(&pu->fn->code, buflen(&pu->fn->code) - 1);
        pi->hasdst = true; pi->dst = o;
        u_push(pu, &o);
      }
      if (tail) bufclear(&pu->stk);
      return;
    }
    default:
      sneprintf(pn, "instruction kind not handled yet: %s", h);
  }
}

static void u_seq(unst_t *pu, snode_t *pn, size_t from)
{
  size_t i;
  for (i = from; i < snlen(pn); ++i) u_form(pu, snref(pn, i));
}

/* ================================================================ */
/* Lowering: macros, named constants, record access and the variadic
 * intrinsics become core instructions, in a tree. The unstacker and the .wo
 * emitter both work on what comes out, so the rewrites live in one place.
 * Follows the Scheme implementation's lower-expr.   [cco] */

typedef struct lwr {
  mod_t *mod;
  fn_t *fn;
  int fuel;
  long long vabase;   /* where the variadic slot array starts in the frame */
  long long vaneed;   /* and how much of it the widest call needs */
} lwr_t;

static void lw_form(lwr_t *pl, snode_t *src, snode_t *dst);

static snode_t *lw_sym(snode_t *dst, const char *name, snode_t *at)
{
  snclear(dst);
  dst->nt = SN_SYM;
  dst->sym = intern(name);
  dst->fname = at->fname; dst->line = at->line; dst->col = at->col;
  return dst;
}

static snode_t *lw_int(snode_t *dst, long long v, snode_t *at)
{
  snclear(dst);
  dst->nt = SN_INT;
  dst->ival = v;
  dst->fname = at->fname; dst->line = at->line; dst->col = at->col;
  return dst;
}

static snode_t *lw_list(snode_t *dst, snode_t *at)
{
  snclear(dst);
  dst->nt = SN_LIST;
  dst->fname = at->fname; dst->line = at->line; dst->col = at->col;
  return dst;
}

/* (op imm... children...) built from pieces */
static snode_t *lw_op(snode_t *dst, const char *op, snode_t *at)
{
  lw_list(dst, at);
  lw_sym(snnewbk(dst), op, at);
  return dst;
}

/* a pointer expression offset by a constant, folded when it is zero */
static void lw_ptradd(snode_t *dst, snode_t *base, long long off, snode_t *at)
{
  if (off == 0) { sncpy(dst, base); return; }
  lw_op(dst, (off > 0) ? "ptr.add" : "ptr.sub", at);
  sncpy(snnewbk(dst), base);
  { snode_t *pc; pc = lw_op(snnewbk(dst), "i32.const", at);
    lw_int(snnewbk(pc), (off > 0) ? off : -off, at); }
}

static fld_t *lw_field(lwr_t *pl, snode_t *pn, sym_t rname, sym_t fname)
{
  rec_t *pr;
  size_t i;
  pr = mod_record(pl->mod, rname);
  if (pr == NULL) sneprintf(pn, "unknown record %s", symname(rname));
  for (i = 0; i < buflen(&pr->fields); ++i) {
    fld_t *pf;
    pf = (fld_t *)bufref(&pr->fields, i);
    if (pf->name == fname) return pf;
  }
  sneprintf(pn, "record %s has no field %s", symname(rname), symname(fname));
  return NULL;
}

/* substitute a macro's arguments and rename its private names apart */
static void lw_macro(lwr_t *pl, snode_t *src, snode_t *dst, mac_t *px)
{
  buf_t subs;
  snode_t out;
  size_t i, nargs, nparams;

  pl->fuel -= 1;
  if (pl->fuel <= 0) sneprintf(src, "macro expansion too deep (recursive macro?)");
  nparams = buflen(&px->params);
  nargs = snlen(src) - 1;
  if (nargs != nparams)
    sneprintf(src, "macro %s takes %d arguments, given %d",
              symname(px->name), (int)nparams, (int)nargs);

  bufinit(&subs, sizeof(subs_t));
  for (i = 0; i < nparams; ++i) {
    subs_t *ps;
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = *(sym_t *)bufref(&px->params, i);
    ps->node = snref(src, i + 1);
    ps->to = 0;
  }
  for (i = 0; i < buflen(&px->locals); ++i) {
    var_t *pv; var_t *pnew; subs_t *ps;
    pv = (var_t *)bufref(&px->locals, i);
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = pv->name; ps->node = NULL;
    ps->to = internf("%s$m%d", symname(pv->name), ++g_mxno);
    pnew = (var_t *)bufnewbk(&pl->fn->vars);
    pnew->name = ps->to; pnew->vt = pv->vt;
  }
  for (i = 0; i < buflen(&px->labels); ++i) {
    subs_t *ps; sym_t l;
    l = *(sym_t *)bufref(&px->labels, i);
    ps = (subs_t *)bufnewbk(&subs);
    ps->from = l; ps->node = NULL;
    ps->to = internf("%s$m%d", symname(l), ++g_mxno);
  }

  sninit(&out);
  u_subst(&out, &px->body, &subs);
  buffini(&subs);
  lw_form(pl, &out, dst);
  snfini(&out);
  pl->fuel += 1;
}

/* what a lowered expression leaves on the stack, as far as the slot array
 * needs to know: enough for local.get, the const forms and the table */
static vt_t lw_type_of(lwr_t *pl, snode_t *pn)
{
  const instr_t *pin;
  const char *h;
  if (pn->nt != SN_LIST || snlen(pn) == 0 || snref(pn, 0)->nt != SN_SYM) return VT_I32;
  h = symname(snref(pn, 0)->sym);
  if (streql(h, "local.get") || streql(h, "local.tee")) {
    size_t i;
    if (snlen(pn) < 2) return VT_I32;
    for (i = 0; i < buflen(&pl->fn->vars); ++i) {
      var_t *pv;
      pv = (var_t *)bufref(&pl->fn->vars, i);
      if (pv->name == snref(pn, 1)->sym) return pv->vt;
    }
    return VT_I32;
  }
  if (streql(h, "global.get") && snlen(pn) > 1 && snref(pn, 1)->nt == SN_SYM)
    return mod_gtype(pl->mod, snref(pn, 1)->sym);
  if (streql(h, "seq")) {
    if (snlen(pn) < 2) return VT_VOID;
    return lw_type_of(pl, snref(pn, snlen(pn) - 1));
  }
  if (streql(h, "call") || streql(h, "return_call")) {
    sig_t *ps;
    if (snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return VT_I32;
    ps = mod_sig(pl->mod, snref(pn, 1)->sym);
    return (ps != NULL) ? ps->res : VT_I32;
  }
  pin = instr_lookup(snref(pn, 0)->sym);
  if (pin != NULL && pin->res != VT_ANY) return pin->res;
  return VT_I32;
}

/* a call whose callee takes a trailing $ap$: build the slot array first */
static void lw_vacall(lwr_t *pl, snode_t *src, snode_t *dst, sig_t *ps)
{
  size_t fixed, nargs, nva, k;
  snode_t *pseq;
  snode_t *pcall;
  fixed = buflen(&ps->ptypes) - 1;
  nargs = snlen(src) - 2;               /* without the head and the callee */
  if (nargs < fixed) sneprintf(src, "%s wants at least %d arguments",
                               symname(snref(src, 0)->sym), (int)fixed);
  nva = nargs - fixed;

  pseq = lw_op(dst, "seq", src);
  {                                     /* the count, below the cursor */
    snode_t *pst; snode_t *pc;
    pst = lw_op(snnewbk(pseq), "i32.store", src);
    pc = lw_op(snnewbk(pst), "frame.addr", src);
    lw_int(snnewbk(pc), pl->vabase, src);
    pc = lw_op(snnewbk(pst), "i32.const", src);
    lw_int(snnewbk(pc), (long long)nva, src);
  }
  for (k = 0; k < nva; ++k) {           /* then one 16-byte slot each */
    snode_t *pst; snode_t *pc; snode_t low;
    const char *op;
    vt_t vt;
    sninit(&low);
    lw_form(pl, snref(src, 2 + fixed + k), &low);
    vt = lw_type_of(pl, &low);
    op = (vt == VT_I64) ? "i64.store" : (vt == VT_F64 || vt == VT_F32) ? "f64.store"
       : (vt == VT_PTR) ? "ptr.store" : "i32.store";
    pst = lw_op(snnewbk(pseq), op, src);
    pc = lw_op(snnewbk(pst), "frame.addr", src);
    lw_int(snnewbk(pc), pl->vabase + 16 * (long long)(k + 1), src);
    if (vt == VT_F32) {                 /* widened into its slot */
      pc = lw_op(snnewbk(pst), "f64.promote_f32", src);
      sncpy(snnewbk(pc), &low);
    } else {
      sncpy(snnewbk(pst), &low);
    }
    snfini(&low);
  }
  pcall = lw_list(snnewbk(pseq), src);  /* the call, with the cursor last */
  sncpy(snnewbk(pcall), snref(src, 0));
  sncpy(snnewbk(pcall), snref(src, 1));
  for (k = 0; k < fixed; ++k) lw_form(pl, snref(src, 2 + k), snnewbk(pcall));
  {
    snode_t *pc;
    pc = lw_op(snnewbk(pcall), "frame.addr", src);
    lw_int(snnewbk(pc), pl->vabase + 16, src);
  }
  if (16 * (long long)(nva + 1) > pl->vaneed)
    pl->vaneed = 16 * (long long)(nva + 1);
}

static void lw_form(lwr_t *pl, snode_t *src, snode_t *dst)
{
  const char *h;
  sym_t head;
  size_t i;

  if (src->nt != SN_LIST || snlen(src) == 0 || snref(src, 0)->nt != SN_SYM) {
    sncpy(dst, src);
    return;
  }
  head = snref(src, 0)->sym;
  h = symname(head);

  {                                     /* a macro use */
    mac_t *px;
    px = mod_macro(pl->mod, head);
    if (px != NULL) { lw_macro(pl, src, dst, px); return; }
  }

  if (streql(h, "const")) {             /* a named constant */
    cst_t *pc;
    if (snlen(src) < 2 || snref(src, 1)->nt != SN_SYM)
      sneprintf(src, "(const $name) expected");
    pc = mod_const(pl->mod, snref(src, 1)->sym);
    if (pc == NULL) sneprintf(src, "unknown constant %s", symname(snref(src, 1)->sym));
    if (!pc->isnum) { lw_form(pl, &pc->val, dst); return; }
    if (pc->vt == VT_F64 || pc->vt == VT_F32) {
      snode_t *pv;
      lw_op(dst, (pc->vt == VT_F32) ? "f32.const" : "f64.const", src);
      pv = snnewbk(dst);
      snclear(pv); pv->nt = SN_FLT; pv->dval = pc->dval;
      pv->fname = src->fname; pv->line = src->line; pv->col = src->col;
    } else if (pc->vt == VT_PTR && pc->ival == 0) {
      lw_op(dst, "ptr.null", src);
    } else {
      lw_op(dst, (pc->vt == VT_I64) ? "i64.const" : "i32.const", src);
      lw_int(snnewbk(dst), pc->ival, src);
    }
    return;
  }

  if (streql(h, "sizeof") || streql(h, "sizeof64") || streql(h, "alignof")) {
    rec_t *pr;
    if (snlen(src) < 2 || snref(src, 1)->nt != SN_SYM)
      sneprintf(src, "%s needs a record", h);
    pr = mod_record(pl->mod, snref(src, 1)->sym);
    if (pr == NULL) sneprintf(src, "unknown record %s", symname(snref(src, 1)->sym));
    lw_op(dst, streql(h, "sizeof64") ? "i64.const" : "i32.const", src);
    lw_int(snnewbk(dst), streql(h, "alignof") ? pr->align : pr->size, src);
    return;
  }
  if (streql(h, "offsetof")) {
    fld_t *pf;
    if (snlen(src) < 3) sneprintf(src, "offsetof needs a record and a field");
    pf = lw_field(pl, src, snref(src, 1)->sym, snref(src, 2)->sym);
    lw_op(dst, "i32.const", src);
    lw_int(snnewbk(dst), pf->off, src);
    return;
  }
  if (streql(h, "field.addr") || streql(h, "field.get") || streql(h, "field.set")) {
    fld_t *pf;
    snode_t ptr;
    if (snlen(src) < 4) sneprintf(src, "%s needs a record, a field and a pointer", h);
    pf = lw_field(pl, src, snref(src, 1)->sym, snref(src, 2)->sym);
    sninit(&ptr);
    lw_form(pl, snref(src, 3), &ptr);
    if (streql(h, "field.addr")) {
      lw_ptradd(dst, &ptr, pf->off, src);
    } else if (pf->lop == 0 || pf->sop == 0) {
      sneprintf(src, "%s on an aggregate field; use field.addr", h);
    } else if (streql(h, "field.get")) {
      lw_op(dst, symname(pf->lop), src);
      if (pf->off != 0) {
        lw_sym(snnewbk(dst), "offset", src);
        lw_int(snnewbk(dst), pf->off, src);
      }
      sncpy(snnewbk(dst), &ptr);
    } else {
      if (snlen(src) < 5) sneprintf(src, "field.set needs a value");
      lw_op(dst, symname(pf->sop), src);
      if (pf->off != 0) {
        lw_sym(snnewbk(dst), "offset", src);
        lw_int(snnewbk(dst), pf->off, src);
      }
      sncpy(snnewbk(dst), &ptr);
      lw_form(pl, snref(src, 4), snnewbk(dst));
    }
    snfini(&ptr);
    return;
  }

  if (streql(h, "va.start")) {
    lw_op(dst, "local.get", src);
    lw_sym(snnewbk(dst), "$ap$", src);
    return;
  }
  if (streql(h, "va.count")) {          /* the count sits below the cursor */
    snode_t *pl1; snode_t *psub; snode_t *pc;
    pl1 = lw_op(dst, "i32.load", src);
    psub = lw_op(snnewbk(pl1), "ptr.sub", src);
    pc = lw_op(snnewbk(psub), "local.get", src);
    lw_sym(snnewbk(pc), "$ap$", src);
    pc = lw_op(snnewbk(psub), "i32.const", src);
    lw_int(snnewbk(pc), 16, src);
    return;
  }
  if (streql(h, "va.arg")) {            /* advance the cursor, read behind it */
    snode_t *pseq; snode_t *pset; snode_t *padd; snode_t *pc; snode_t *pld; snode_t *psub;
    const char *ap;
    vt_t vt;
    if (snlen(src) < 3 || snref(src, 1)->nt != SN_SYM || snref(src, 2)->nt != SN_SYM)
      sneprintf(src, "va.arg needs a cursor and a type");
    ap = symname(snref(src, 1)->sym);
    vt = vt_of_sym(snref(src, 2)->sym);
    pseq = lw_op(dst, "seq", src);
    pset = lw_op(snnewbk(pseq), "local.set", src);
    lw_sym(snnewbk(pset), ap, src);
    padd = lw_op(snnewbk(pset), "ptr.add", src);
    pc = lw_op(snnewbk(padd), "local.get", src);
    lw_sym(snnewbk(pc), ap, src);
    pc = lw_op(snnewbk(padd), "i32.const", src);
    lw_int(snnewbk(pc), 16, src);
    if (vt == VT_F32) {
      snode_t *pdem;
      pdem = lw_op(snnewbk(pseq), "f32.demote_f64", src);
      pld = lw_op(snnewbk(pdem), "f64.load", src);
    } else {
      pld = lw_op(snnewbk(pseq),
                  (vt == VT_I64) ? "i64.load" : (vt == VT_F64) ? "f64.load"
                  : (vt == VT_PTR) ? "ptr.load" : "i32.load", src);
    }
    psub = lw_op(snnewbk(pld), "ptr.sub", src);
    pc = lw_op(snnewbk(psub), "local.get", src);
    lw_sym(snnewbk(pc), ap, src);
    pc = lw_op(snnewbk(psub), "i32.const", src);
    lw_int(snnewbk(pc), 16, src);
    return;
  }

  if (streql(h, "call") || streql(h, "return_call")) {
    sig_t *ps;
    if (snlen(src) > 1 && snref(src, 1)->nt == SN_SYM) {
      ps = mod_sig(pl->mod, snref(src, 1)->sym);
      if (ps != NULL && ps->variadic) { lw_vacall(pl, src, dst, ps); return; }
    }
  }

  lw_list(dst, src);                    /* anything else: lower the children */
  for (i = 0; i < snlen(src); ++i) lw_form(pl, snref(src, i), snnewbk(dst));
}

static int g_useno;

int mod_usemax(void) { return g_useno; }

/* A module's tables hold an entry per spelling, so a qualified name and the bare
 * name are two entries for one import.  Numbering only the entry that was
 * looked up left the other at nought, and an emitter walking the use order
 * dropped the import.  [cco] */
static void use_spread(mod_t *pm, sym_t woname, int no)
{
  size_t i;
  if (woname == 0) return;
  for (i = 0; i < buflen(&pm->sigs); ++i) {
    sig_t *p;
    p = (sig_t *)bufref(&pm->sigs, i);
    if (p->woname == woname && p->useno == 0) { p->used = true; p->useno = no; }
  }
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *p;
    p = (glb_t *)bufref(&pm->globals, i);
    if (p->woname == woname && p->useno == 0) { p->used = true; p->useno = no; }
  }
  for (i = 0; i < buflen(&pm->datas); ++i) {
    dat_t *p;
    p = (dat_t *)bufref(&pm->datas, i);
    if (p->woname == woname && p->useno == 0) { p->used = true; p->useno = no; }
  }
}

/* walk a lowered body and mark the imports it uses */
void mod_scan_uses(mod_t *pm, snode_t *pn)
{
  size_t i;
  const char *h;
  if (pn->nt != SN_LIST || snlen(pn) == 0) return;
  if (snref(pn, 0)->nt == SN_SYM && snlen(pn) > 1 && snref(pn, 1)->nt == SN_SYM) {
    h = symname(snref(pn, 0)->sym);
    if (streql(h, "call") || streql(h, "return_call")) {
      sig_t *ps;
      ps = mod_sig(pm, snref(pn, 1)->sym);
      if (ps != NULL && !ps->used) { ps->used = true; ps->useno = ++g_useno; use_spread(pm, ps->woname, ps->useno); }
    } else if (streql(h, "global.get") || streql(h, "global.set")) {
      for (i = 0; i < buflen(&pm->globals); ++i) {
        glb_t *pg;
        pg = (glb_t *)bufref(&pm->globals, i);
        if (pg->name == snref(pn, 1)->sym) {
          if (!pg->used) { pg->used = true; pg->useno = ++g_useno; use_spread(pm, pg->woname, pg->useno); }
          break;
        }
      }
    } else if (streql(h, "ref.func")) {
      sig_t *ps;
      ps = mod_sig(pm, snref(pn, 1)->sym);
      if (ps != NULL && !ps->used) { ps->used = true; ps->useno = ++g_useno; use_spread(pm, ps->woname, ps->useno); }
    } else if (streql(h, "ref.data")) {
      for (i = 0; i < buflen(&pm->datas); ++i) {
        dat_t *pd;
        pd = (dat_t *)bufref(&pm->datas, i);
        if (pd->name == snref(pn, 1)->sym) {
          if (!pd->used) { pd->used = true; pd->useno = ++g_useno; use_spread(pm, pd->woname, pd->useno); }
          break;
        }
      }
    }
  }
  for (i = 0; i < snlen(pn); ++i) mod_scan_uses(pm, snref(pn, i));
}

/* lower the body of a (func ...) whose header ends at FIRST into a (seq ...) */
void fn_lower(mod_t *pm, snode_t *pn, size_t first, fn_t *pf, snode_t *out)
{
  lwr_t lw;
  size_t i;
  memset(&lw, 0, sizeof(lwr_t));
  lw.mod = pm;
  lw.fn = pf;
  lw.fuel = 10000;
  lw.vabase = ((pf->frame + 15) / 16) * 16;
  lw.vaneed = 0;
  lw_op(out, "seq", pn);
  for (i = first; i < snlen(pn); ++i) lw_form(&lw, snref(pn, i), snnewbk(out));
  if (lw.vaneed > 0) pf->frame = lw.vabase + lw.vaneed;
}


/* parse a (func ...) header into PF; returns where its body starts */
size_t fn_header(mod_t *pm, snode_t *pn, fn_t *pf)
{
  size_t i, first;
  (void)pm;
  pf->name = snref(pn, 1)->sym;
  pf->res = VT_VOID;

  /* header: export, params, result, frame, locals; then the body */
  first = 2;
  for (i = 2; i < snlen(pn); ++i) {
    snode_t *pc;
    const char *k;
    pc = snref(pn, i);
    if (pc->nt != SN_LIST || snlen(pc) == 0 || snref(pc, 0)->nt != SN_SYM) break;
    k = symname(snref(pc, 0)->sym);
    if (streql(k, "export")) {
      if (snlen(pc) > 1 && snref(pc, 1)->nt == SN_STR) {
        cbuf_t nb;                      /* the bytes are not 0-terminated */
        cbinit(&nb);
        cbput((const char *)bufdata(&snref(pc, 1)->data),
              buflen(&snref(pc, 1)->data), &nb);
        cbputc(0, &nb);
        pf->expname = intern((const char *)cbdata(&nb));
        cbfini(&nb);
      }
    } else if (streql(k, "param")) {
      var_t *pv;
      if (snlen(pc) == 2 && snref(pc, 1)->nt == SN_SYM
          && streql(symname(snref(pc, 1)->sym), "...")) {
        pv = (var_t *)bufnewbk(&pf->vars);   /* (param ...) brings in $ap$ */
        pv->name = intern("$ap$"); pv->vt = VT_PTR;
        pf->nparams += 1;
        pf->variadic = true;
        first = i + 1;
        continue;
      }
      {
        size_t j, n;
        vt_t vt;
        snode_t *type;
        n = snlen(pc);
        if (n < 3) sneprintf(pc, "(param $name ... type) needs a name and type");
        type = snref(pc, n - 1);
        if (type->nt != SN_SYM || (vt = vt_of_sym(type->sym)) == VT_VOID)
          sneprintf(pc, "(param $name ... type) needs a scalar type");
        for (j = 1; j + 1 < n; ++j) {
          snode_t *name;
          name = snref(pc, j);
          if (name->nt != SN_SYM || symname(name->sym)[0] != '$')
            sneprintf(pc, "(param $name ... type) needs parameter names");
          pv = (var_t *)bufnewbk(&pf->vars);
          pv->name = name->sym;
          pv->vt = vt;
          pf->nparams += 1;
        }
      }
    } else if (streql(k, "result")) {
      if (snref(pc, 1)->nt == SN_SYM) {
        pf->res = vt_of_sym(snref(pc, 1)->sym);
      } else {                            /* (result (record $R)): writes through $rp$ */
        var_t *pv;
        size_t n, j;
        pf->res = VT_VOID;
        n = buflen(&pf->vars);
        pv = (var_t *)bufnewbk(&pf->vars);
        for (j = n; j > 0; --j)
          *(var_t *)bufref(&pf->vars, j) = *(var_t *)bufref(&pf->vars, j - 1);
        pv = (var_t *)bufref(&pf->vars, 0);
        pv->name = intern("$rp$"); pv->vt = VT_PTR;
        pf->nparams += 1;
      }
    } else if (streql(k, "frame")) {
      if (snlen(pc) > 1 && snref(pc, 1)->nt == SN_INT) pf->frame = snref(pc, 1)->ival;
    } else if (streql(k, "local")) {
      size_t j, n;
      vt_t vt;
      snode_t *type;
      n = snlen(pc);
      if (n < 3) sneprintf(pc, "(local $name ... type) needs a name and type");
      type = snref(pc, n - 1);
      if (type->nt != SN_SYM || (vt = vt_of_sym(type->sym)) == VT_VOID)
        sneprintf(pc, "(local $name ... type) needs a scalar type");
      for (j = 1; j + 1 < n; ++j) {
        snode_t *name;
        var_t *pv;
        name = snref(pc, j);
        if (name->nt != SN_SYM || symname(name->sym)[0] != '$')
          sneprintf(pc, "(local $name ... type) needs local names");
        pv = (var_t *)bufnewbk(&pf->vars);
        pv->name = name->sym;
        pv->vt = vt;
      }
    } else {
      break;                              /* the body starts here */
    }
    first = i + 1;
  }

  return first;
}

bool fn_unstack(mod_t *pm, snode_t *pn, fn_t *pf)
{
  unst_t u;
  snode_t lowered;
  size_t first;
  const char *h;

  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 0)->nt != SN_SYM) return false;
  h = symname(snref(pn, 0)->sym);
  if (!streql(h, "func") && !streql(h, "trampoline_func")) return false;
  if (snref(pn, 1)->nt != SN_SYM) sneprintf(pn, "(func $name ...) expected");

  fn_init(pf);
  first = fn_header(pm, pn, pf);
  sninit(&lowered);
  fn_lower(pm, pn, first, pf, &lowered);       /* macros, records, variadics */

  memset(&u, 0, sizeof(unst_t));
  u.mod = pm;
  u.fn = pf;
  u.at = pn;
  u.fuel = 10000;
  bufinit(&u.stk, sizeof(opnd_t));
  bufinit(&u.ctrl, sizeof(ctrl_t));
  u_seq(&u, &lowered, 1);                      /* the lowered body is a (seq ...) */
  /* A body that falls off the end returns what it left on the stack.
   * A body whose last form leaves for good (return, tail call,
   * trampoline jump, ...) must not grow a dead return behind it:
   * the source-level check sees through the IR labels and jumps a
   * trailing block/if leaves in the instruction stream. */
  if (buflen(&u.fn->code) == 0 || !u_seq_terminates(&lowered, 1)) {
    ir_t *pi;
    pi = u_emit(&u, IR_RETURN);
    pi->vt = pf->res;
    if (pf->res != VT_VOID && buflen(&u.stk) > 0) {
      opnd_t top;
      u_pop(&u, &top);
      ir_arg(pi, &top);
    }
  }
  buffini(&u.ctrl);
  buffini(&u.stk);
  snfini(&lowered);
  return true;
}
