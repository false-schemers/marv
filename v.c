/* v.c (marv emitters) -- [cco]
 *
 * The three target writers: the WCPL .wo writer, the restricted C99 writer, and
 * the RV64G register allocator and assembler. The first two read the lowered
 * tree, because WebAssembly and C both have structured control flow; the third
 * reads the unstacker's three-address code.
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
#include "v.h"
#include "a.h"

/* ---------------------------------------------------------------- output */

static void sn_wstring(const snode_t *pn, FILE *fp)
{
  size_t i;
  unsigned char c;
  fputc('"', fp);
  for (i = 0; i < buflen(&((snode_t*)pn)->data); ++i) {
    c = (unsigned char)*(char*)bufref(&((snode_t*)pn)->data, i);
    if (c == '"') fputs("\\\"", fp);
    else if (c == '\\') fputs("\\\\", fp);
    else if (c == '\n') fputs("\\n", fp);
    else if (c == '\t') fputs("\\t", fp);
    else if (c < 0x20 || c >= 0x7f) fprintf(fp, "\\x%x;", (unsigned)c);
    else fputc((int)c, fp);
  }
  fputc('"', fp);
}

void snwrite(const snode_t *pn, FILE *fp)
{
  size_t i;
  char buf[40];
  switch (pn->nt) {
    case SN_LIST:
      fputc('(', fp);
      for (i = 0; i < snlen((snode_t*)pn); ++i) {
        if (i > 0) fputc(' ', fp);
        snwrite(snref((snode_t*)pn, i), fp);
      }
      fputc(')', fp);
      break;
    case SN_SYM: fputs(symname(pn->sym), fp); break;
    case SN_INT: fprintf(fp, "%lld", pn->ival); break;
    case SN_FLT:
      /* exact and readable: the hex form round-trips bit for bit */
      udtohex(as_uint64(pn->dval), &buf[0]);
      fprintf(fp, "%s", &buf[0]);
      break;
    case SN_STR: sn_wstring(pn, fp); break;
    case SN_BVEC:
      fputs("#u8(", fp);
      for (i = 0; i < buflen(&((snode_t*)pn)->data); ++i) {
        if (i > 0) fputc(' ', fp);
        fprintf(fp, "%u", (unsigned)(unsigned char)*(char*)bufref(&((snode_t*)pn)->data, i));
      }
      fputc(')', fp);
      break;
    default: fputs("#<null>", fp); break;
  }
}

/* ================================================================ */
/* Printing three-address code. Not a target: a way to read what the
 * unstacker produced, and the shape the real emitters will walk. [cco] */

static void op_write(const opnd_t *po, FILE *fp)
{
  char buf[40];
  switch (po->kind) {
    case OK_IMM:
      if (po->vt == VT_F32 || po->vt == VT_F64) {
        udtohex(as_uint64(po->dval), &buf[0]);
        fprintf(fp, "%s", &buf[0]);
      } else {
        fprintf(fp, "%lld", po->ival);
      }
      break;
    case OK_FRAME: fprintf(fp, "frame[%lld]", po->ival); break;
    case OK_DATA:
      fprintf(fp, "&%s", symname(po->sym));
      if (po->ival != 0) fprintf(fp, "+%lld", po->ival);
      break;
    case OK_FUNC: fprintf(fp, "&%s", symname(po->sym)); break;
    case OK_VREG: fprintf(fp, "%s", symname(po->sym)); break;
    default: fputs("?", fp); break;
  }
}

static void args_write(const ir_t *pi, FILE *fp)
{
  size_t i;
  for (i = 0; i < buflen(&((ir_t *)pi)->args); ++i) {
    if (i > 0) fputs(", ", fp);
    op_write((opnd_t *)bufref(&((ir_t *)pi)->args, i), fp);
  }
}

void ir_write(const fn_t *pf, FILE *fp)
{
  size_t i;
  fprintf(fp, "func %s", symname(pf->name));
  if (pf->expname != 0) fprintf(fp, " export \"%s\"", symname(pf->expname));
  fprintf(fp, " -> %s", vtname(pf->res));
  if (pf->frame != 0) fprintf(fp, " frame %lld", pf->frame);
  fprintf(fp, "  (%d params, %d vars, %d temps)\n",
          pf->nparams, (int)buflen(&((fn_t *)pf)->vars), pf->ntemp);
  for (i = 0; i < buflen(&((fn_t *)pf)->code); ++i) {
    ir_t *pi;
    pi = (ir_t *)bufref(&((fn_t *)pf)->code, i);
    if (pi->op == IR_LABEL) { fprintf(fp, "%s:\n", symname(pi->name)); continue; }
    fputs("    ", fp);
    if (pi->hasdst) { op_write(&pi->dst, fp); fputs(" = ", fp); }
    switch (pi->op) {
      case IR_MOV: args_write(pi, fp); break;
      case IR_UNARY: case IR_BINARY:
        fprintf(fp, "%s ", symname(pi->name));
        args_write(pi, fp);
        break;
      case IR_SELECT: fputs("select ", fp); args_write(pi, fp); break;
      case IR_LOAD:
        fprintf(fp, "%s [", symname(pi->name));
        args_write(pi, fp);
        fprintf(fp, " + %lld]", pi->off);
        break;
      case IR_MEMORY: {
        size_t k;
        fprintf(fp, "%s", symname(pi->name));
        for (k = 0; k < buflen(&pi->args); ++k) {
          fputs(" ", fp);
          op_write((opnd_t *)bufref(&pi->args, k), fp);
        }
        break;
      }
      case IR_STORE: {
        size_t n;
        n = buflen(&pi->args);
        fprintf(fp, "%s [", symname(pi->name));
        if (n > 0) op_write((opnd_t *)bufref(&pi->args, 0), fp);
        fprintf(fp, " + %lld] = ", pi->off);
        if (n > 1) op_write((opnd_t *)bufref(&pi->args, 1), fp);
        break;
      }
      case IR_GGET: fprintf(fp, "global %s", symname(pi->name)); break;
      case IR_GSET:
        fprintf(fp, "global %s = ", symname(pi->name));
        args_write(pi, fp);
        break;
      case IR_JUMP: fprintf(fp, "jump %s", symname(pi->name)); break;
      case IR_BRFALSE:
        fputs("if not ", fp);
        args_write(pi, fp);
        fprintf(fp, " jump %s", symname(pi->name));
        break;
      case IR_BRTRUE:
        fputs("if ", fp);
        args_write(pi, fp);
        fprintf(fp, " jump %s", symname(pi->name));
        break;
      case IR_BRTABLE: {
        size_t k;
        fputs("switch ", fp);
        if (buflen(&pi->args) > 0) op_write((opnd_t *)bufref(&pi->args, 0), fp);
        fputs(" [", fp);
        for (k = 1; k < buflen(&pi->args); ++k) {
          if (k > 1) fputs(", ", fp);
          fputs(symname(((opnd_t *)bufref(&pi->args, k))->sym), fp);
        }
        fputs("]", fp);
        break;
      }
      case IR_LOOPBEG: fprintf(fp, "loop %s", symname(pi->name)); break;
      case IR_LOOPEND: fprintf(fp, "endloop %s", symname(pi->name)); break;
      case IR_UNREACHABLE: fputs("unreachable", fp); break;
      case IR_RETURN:
        fputs("return", fp);
        if (buflen(&pi->args) > 0) { fputs(" ", fp); args_write(pi, fp); }
        break;
      case IR_CALL: case IR_RETCALL:
        fprintf(fp, "%scall %s(", (pi->op == IR_RETCALL) ? "tail " : "", symname(pi->name));
        args_write(pi, fp);
        fputs(")", fp);
        break;
      case IR_CALLI: case IR_RETCALLI:
        fprintf(fp, "%scall_indirect(", (pi->op == IR_RETCALLI) ? "tail " : "");
        args_write(pi, fp);
        fputs(")", fp);
        break;
      default: fputs("???", fp); break;
    }
    fputs("\n", fp);
  }
}

/* ================================================================ */
/* The cc target: restricted C. Named cc, not after any one C standard,
 * since the dialect it must fit is a moving target.   [cco]
 *
 * Instructions become calls to marv_<op> helpers, which the prologue defines
 * as macros; a helper that a module never uses is simply never defined. */

static const char *cc_prologue =
"/* A MARV tail call should be a jump rather than a call that happens to be\n"
" * last.  Where the compiler can promise that, say so and\n"
" * let it refuse what it cannot keep.  Where it cannot, the transfer is an\n"
" * ordinary call and the stack grows by a frame each time: a chain stays\n"
" * bounded because it resets, by returning the whole way out to a trampoline\n"
" * every so often, and that is the generator's business rather than this\n"
" * macro's.  So there is nothing to refuse here. */\n"
"#if defined(__has_attribute)\n"
"#  if __has_attribute(musttail)\n"
"#    define MARV_TAILCALL_GUARANTEE 1\n"
"#    define MARV_MUSTTAIL __attribute__((musttail))\n"
"#  endif\n"
"#endif\n"
"#ifndef MARV_MUSTTAIL\n"
"#  define MARV_MUSTTAIL\n"
"#endif\n"
"\n"
"#include <stdint.h>\n"
"#include <string.h>\n"
"#include <math.h>\n"
"\n"
"typedef int32_t  marv_i32;\n"
"typedef int64_t  marv_i64;\n"
"typedef float    marv_f32;\n"
"typedef double   marv_f64;\n"
"typedef void*    marv_ptr;\n"
"typedef uint32_t marv_u32;\n"
"typedef uint64_t marv_u64;\n"
"\n"
"#if defined(__GNUC__) || defined(__clang__)\n"
"#define MARV_ALIGNED(n) __attribute__((aligned(n)))\n"
"#elif defined(_MSC_VER)\n"
"#define MARV_ALIGNED(n) __declspec(align(n))\n"
"#else\n"
"/* no way to ask for alignment: the frame and the data segments get\n"
" * whatever the compiler gives them, which may not be enough */\n"
"#define MARV_ALIGNED(n)\n"
"#endif\n"
"\n"
"#define marv_i32_add(a, b) ((marv_i32)((marv_u32)(a) + (marv_u32)(b)))\n"
"#define marv_i32_sub(a, b) ((marv_i32)((marv_u32)(a) - (marv_u32)(b)))\n"
"#define marv_i32_mul(a, b) ((marv_i32)((marv_u32)(a) * (marv_u32)(b)))\n"
"#define marv_i32_div_s(a, b) ((a) / (b))\n"
"#define marv_i32_div_u(a, b) ((marv_i32)((marv_u32)(a) / (marv_u32)(b)))\n"
"#define marv_i32_rem_s(a, b) ((a) % (b))\n"
"#define marv_i32_rem_u(a, b) ((marv_i32)((marv_u32)(a) % (marv_u32)(b)))\n"
"#define marv_i32_and(a, b) ((marv_i32)((marv_u32)(a) & (marv_u32)(b)))\n"
"#define marv_i32_or(a, b) ((marv_i32)((marv_u32)(a) | (marv_u32)(b)))\n"
"#define marv_i32_xor(a, b) ((marv_i32)((marv_u32)(a) ^ (marv_u32)(b)))\n"
"#define marv_i32_shl(a, b) ((marv_i32)((marv_u32)(a) << ((marv_u32)(b) & 31)))\n"
"#define marv_i32_shr_s(a, b) ((marv_i32)((a) >> ((marv_u32)(b) & 31)))\n"
"#define marv_i32_shr_u(a, b) ((marv_i32)((marv_u32)(a) >> ((marv_u32)(b) & 31)))\n"
"#define marv_i32_rotl(a, b) ((marv_i32)(((marv_u32)(a) << ((marv_u32)(b) & 31)) |\\\n"
"                             ((marv_u32)(a) >> ((32 - ((marv_u32)(b) & 31)) & 31))))\n"
"#define marv_i32_rotr(a, b) ((marv_i32)(((marv_u32)(a) >> ((marv_u32)(b) & 31)) |\\\n"
"                             ((marv_u32)(a) << ((32 - ((marv_u32)(b) & 31)) & 31))))\n"
"#define marv_i32_eqz(a) ((marv_i32)((a) == 0))\n"
"#define marv_i32_eq(a, b) ((marv_i32)((a) == (b)))\n"
"#define marv_i32_ne(a, b) ((marv_i32)((a) != (b)))\n"
"#define marv_i32_lt_s(a, b) ((marv_i32)((a) < (b)))\n"
"#define marv_i32_lt_u(a, b) ((marv_i32)((marv_u32)(a) < (marv_u32)(b)))\n"
"#define marv_i32_gt_s(a, b) ((marv_i32)((a) > (b)))\n"
"#define marv_i32_gt_u(a, b) ((marv_i32)((marv_u32)(a) > (marv_u32)(b)))\n"
"#define marv_i32_le_s(a, b) ((marv_i32)((a) <= (b)))\n"
"#define marv_i32_le_u(a, b) ((marv_i32)((marv_u32)(a) <= (marv_u32)(b)))\n"
"#define marv_i32_ge_s(a, b) ((marv_i32)((a) >= (b)))\n"
"#define marv_i32_ge_u(a, b) ((marv_i32)((marv_u32)(a) >= (marv_u32)(b)))\n"
"#define marv_i32_extend8_s(a) ((marv_i32)(int8_t)(a))\n"
"#define marv_i32_extend16_s(a) ((marv_i32)(int16_t)(a))\n"
"#define marv_i32_wrap_i64(a) ((marv_i32)(marv_u32)(marv_u64)(a))\n"
"#define marv_i32_clz(a) marv_clz32((marv_u32)(a))\n"
"#define marv_i32_ctz(a) marv_ctz32((marv_u32)(a))\n"
"#define marv_i32_popcnt(a) marv_pop32((marv_u32)(a))\n"
"\n"
"#define marv_i64_clz(a) marv_clz64((marv_u64)(a))\n"
"#define marv_i64_ctz(a) marv_ctz64((marv_u64)(a))\n"
"#define marv_i64_popcnt(a) marv_pop64((marv_u64)(a))\n"
"#define marv_i64_rotl(a, b) ((marv_i64)(((marv_u64)(a) << ((marv_u64)(b) & 63)) |\\\n"
"                             ((marv_u64)(a) >> ((64 - ((marv_u64)(b) & 63)) & 63))))\n"
"#define marv_i64_rotr(a, b) ((marv_i64)(((marv_u64)(a) >> ((marv_u64)(b) & 63)) |\\\n"
"                             ((marv_u64)(a) << ((64 - ((marv_u64)(b) & 63)) & 63))))\n"
"\n"
"#define marv_i64_add(a, b) ((marv_i64)((marv_u64)(a) + (marv_u64)(b)))\n"
"#define marv_i64_sub(a, b) ((marv_i64)((marv_u64)(a) - (marv_u64)(b)))\n"
"#define marv_i64_mul(a, b) ((marv_i64)((marv_u64)(a) * (marv_u64)(b)))\n"
"#define marv_i64_div_s(a, b) ((a) / (b))\n"
"#define marv_i64_div_u(a, b) ((marv_i64)((marv_u64)(a) / (marv_u64)(b)))\n"
"#define marv_i64_rem_s(a, b) ((a) % (b))\n"
"#define marv_i64_rem_u(a, b) ((marv_i64)((marv_u64)(a) % (marv_u64)(b)))\n"
"#define marv_i64_and(a, b) ((marv_i64)((marv_u64)(a) & (marv_u64)(b)))\n"
"#define marv_i64_or(a, b) ((marv_i64)((marv_u64)(a) | (marv_u64)(b)))\n"
"#define marv_i64_xor(a, b) ((marv_i64)((marv_u64)(a) ^ (marv_u64)(b)))\n"
"#define marv_i64_shl(a, b) ((marv_i64)((marv_u64)(a) << ((marv_u64)(b) & 63)))\n"
"#define marv_i64_shr_s(a, b) ((marv_i64)((a) >> ((marv_u64)(b) & 63)))\n"
"#define marv_i64_shr_u(a, b) ((marv_i64)((marv_u64)(a) >> ((marv_u64)(b) & 63)))\n"
"#define marv_i64_eqz(a) ((marv_i32)((a) == 0))\n"
"#define marv_i64_eq(a, b) ((marv_i32)((a) == (b)))\n"
"#define marv_i64_ne(a, b) ((marv_i32)((a) != (b)))\n"
"#define marv_i64_lt_s(a, b) ((marv_i32)((a) < (b)))\n"
"#define marv_i64_lt_u(a, b) ((marv_i32)((marv_u64)(a) < (marv_u64)(b)))\n"
"#define marv_i64_gt_s(a, b) ((marv_i32)((a) > (b)))\n"
"#define marv_i64_gt_u(a, b) ((marv_i32)((marv_u64)(a) > (marv_u64)(b)))\n"
"#define marv_i64_le_s(a, b) ((marv_i32)((a) <= (b)))\n"
"#define marv_i64_ge_s(a, b) ((marv_i32)((a) >= (b)))\n"
"#define marv_i64_ge_u(a, b) ((marv_i32)((marv_u64)(a) >= (marv_u64)(b)))\n"
"#define marv_i64_le_u(a, b) ((marv_i32)((marv_u64)(a) <= (marv_u64)(b)))\n"
"#define marv_i64_extend_i32_s(a) ((marv_i64)(a))\n"
"#define marv_i64_extend_i32_u(a) ((marv_i64)(marv_u64)(marv_u32)(a))\n"
"#define marv_i64_extend8_s(a) ((marv_i64)(int8_t)(a))\n"
"#define marv_i64_extend16_s(a) ((marv_i64)(int16_t)(a))\n"
"#define marv_i64_extend32_s(a) ((marv_i64)(int32_t)(a))\n"
"\n"
"#define marv_f64_add(a, b) ((a) + (b))\n"
"#define marv_f64_sub(a, b) ((a) - (b))\n"
"#define marv_f64_mul(a, b) ((a) * (b))\n"
"#define marv_f64_div(a, b) ((a) / (b))\n"
"#define marv_f64_min(a, b) (fmin((a), (b)))\n"
"#define marv_f64_max(a, b) (fmax((a), (b)))\n"
"#define marv_f64_copysign(a, b) (copysign((a), (b)))\n"
"#define marv_f64_abs(a) (fabs(a))\n"
"#define marv_f64_neg(a) (-(a))\n"
"#define marv_f64_sqrt(a) (sqrt(a))\n"
"#define marv_f64_floor(a) (floor(a))\n"
"#define marv_f64_ceil(a) (ceil(a))\n"
"#define marv_f64_trunc(a) (trunc(a))\n"
"#define marv_f64_nearest(a) (nearbyint(a))\n"
"#define marv_f64_eq(a, b) ((marv_i32)((a) == (b)))\n"
"#define marv_f64_ne(a, b) ((marv_i32)((a) != (b)))\n"
"#define marv_f64_lt(a, b) ((marv_i32)((a) < (b)))\n"
"#define marv_f64_gt(a, b) ((marv_i32)((a) > (b)))\n"
"#define marv_f64_le(a, b) ((marv_i32)((a) <= (b)))\n"
"#define marv_f64_ge(a, b) ((marv_i32)((a) >= (b)))\n"
"#define marv_f32_add(a, b) ((a) + (b))\n"
"#define marv_f32_sub(a, b) ((a) - (b))\n"
"#define marv_f32_mul(a, b) ((a) * (b))\n"
"#define marv_f32_div(a, b) ((a) / (b))\n"
"#define marv_f32_eq(a, b) ((marv_i32)((a) == (b)))\n"
"#define marv_f32_demote_f64(a) ((marv_f32)(a))\n"
"#define marv_f64_promote_f32(a) ((marv_f64)(a))\n"
"#define marv_i32_trunc_f64_s(a) ((marv_i32)(a))\n"
"#define marv_i32_trunc_f64_u(a) ((marv_i32)(marv_u32)(a))\n"
"#define marv_i32_trunc_f32_s(a) ((marv_i32)(a))\n"
"#define marv_i64_trunc_f64_s(a) ((marv_i64)(a))\n"
"#define marv_f64_convert_i32_s(a) ((marv_f64)(a))\n"
"#define marv_f64_convert_i32_u(a) ((marv_f64)(marv_u32)(a))\n"
"#define marv_f64_convert_i64_s(a) ((marv_f64)(a))\n"
"#define marv_f32_convert_i32_s(a) ((marv_f32)(a))\n"
"#define marv_f32_convert_i32_u(a) ((marv_f32)(marv_u32)(a))\n"
"#define marv_f32_convert_i64_s(a) ((marv_f32)(a))\n"
"#define marv_f32_convert_i64_u(a) ((marv_f32)(marv_u64)(a))\n"
"#define marv_f32_ne(a, b) ((marv_i32)((a) != (b)))\n"
"#define marv_f32_lt(a, b) ((marv_i32)((a) < (b)))\n"
"#define marv_f32_gt(a, b) ((marv_i32)((a) > (b)))\n"
"#define marv_f32_le(a, b) ((marv_i32)((a) <= (b)))\n"
"#define marv_f32_ge(a, b) ((marv_i32)((a) >= (b)))\n"
"#define marv_f32_abs(a) (fabsf(a))\n"
"#define marv_f32_neg(a) (-(a))\n"
"#define marv_f32_sqrt(a) (sqrtf(a))\n"
"#define marv_f32_floor(a) (floorf(a))\n"
"#define marv_f32_ceil(a) (ceilf(a))\n"
"#define marv_f32_trunc(a) (truncf(a))\n"
"#define marv_f32_nearest(a) (nearbyintf(a))\n"
"#define marv_f32_min(a, b) (fminf((a), (b)))\n"
"#define marv_f32_max(a, b) (fmaxf((a), (b)))\n"
"#define marv_f32_copysign(a, b) (copysignf((a), (b)))\n"
"#define marv_i32_trunc_f32_u(a) ((marv_i32)(marv_u32)(a))\n"
"#define marv_i64_trunc_f32_s(a) ((marv_i64)(a))\n"
"#define marv_i64_trunc_f32_u(a) ((marv_i64)(marv_u64)(a))\n"
"#define marv_i64_trunc_f64_u(a) ((marv_i64)(marv_u64)(a))\n"
"#define marv_f64_convert_i64_u(a) ((marv_f64)(marv_u64)(a))\n"
"\n"
"#define marv_ptr_null() ((marv_ptr)0)\n"
"#define marv_ptr_add(p, i) ((marv_ptr)((unsigned char*)(p) + (marv_i32)(i)))\n"
"#define marv_ptr_add64(p, i) ((marv_ptr)((unsigned char*)(p) + (marv_i64)(i)))\n"
"#define marv_ptr_sub(p, i) ((marv_ptr)((unsigned char*)(p) - (marv_i32)(i)))\n"
"#define marv_ptr_diff(p, q) ((marv_i32)((unsigned char*)(p) - (unsigned char*)(q)))\n"
"#define marv_ptr_diff64(p, q) ((marv_i64)((unsigned char*)(p) - (unsigned char*)(q)))\n"
"#define marv_ptr_eqz(p) ((marv_i32)((p) == (marv_ptr)0))\n"
"#define marv_ptr_eq(p, q) ((marv_i32)((p) == (q)))\n"
"#define marv_ptr_ne(p, q) ((marv_i32)((p) != (q)))\n"
"#define marv_ptr_lt_u(p, q) ((marv_i32)((unsigned char*)(p) < (unsigned char*)(q)))\n"
"#define marv_ptr_gt_u(p, q) ((marv_i32)((unsigned char*)(p) > (unsigned char*)(q)))\n"
"#define marv_ptr_le_u(p, q) ((marv_i32)((unsigned char*)(p) <= (unsigned char*)(q)))\n"
"#define marv_ptr_ge_u(p, q) ((marv_i32)((unsigned char*)(p) >= (unsigned char*)(q)))\n"
"#define marv_ptr_of_i64(a) ((marv_ptr)(uintptr_t)(marv_u64)(a))\n"
"#define marv_i64_of_ptr(p) ((marv_i64)(marv_u64)(uintptr_t)(p))\n"
"\n"
"static marv_i32 marv_i32_reinterpret_f32(marv_f32 f)\n"
"{ marv_i32 v; memcpy(&v, &f, 4); return v; }\n"
"static marv_f32 marv_f32_reinterpret_i32(marv_i32 v)\n"
"{ marv_f32 f; memcpy(&f, &v, 4); return f; }\n"
"static marv_i64 marv_i64_reinterpret_f64(marv_f64 f)\n"
"{ marv_i64 v; memcpy(&v, &f, 8); return v; }\n"
"static marv_f64 marv_f64_reinterpret_i64(marv_i64 v)\n"
"{ marv_f64 f; memcpy(&f, &v, 8); return f; }\n"
"static void marv_unreachable(void) { *(volatile int *)0 = 0; }\n"
"\n"
"static marv_i32 marv_clz32(marv_u32 v)\n"
"{ marv_i32 n; if (v == 0) return 32; n = 0;\n"
"  while ((v & 0x80000000u) == 0) { v <<= 1; n += 1; } return n; }\n"
"static marv_i32 marv_ctz32(marv_u32 v)\n"
"{ marv_i32 n; if (v == 0) return 32; n = 0;\n"
"  while ((v & 1u) == 0) { v >>= 1; n += 1; } return n; }\n"
"static marv_i32 marv_pop32(marv_u32 v)\n"
"{ marv_i32 n; n = 0; while (v != 0) { n += (marv_i32)(v & 1u); v >>= 1; } return n; }\n"
"static marv_i32 marv_clz64(marv_u64 v)\n"
"{ marv_i32 n; if (v == 0) return 64; n = 0;\n"
"  while ((v & 0x8000000000000000ull) == 0) { v <<= 1; n += 1; } return n; }\n"
"static marv_i32 marv_ctz64(marv_u64 v)\n"
"{ marv_i32 n; if (v == 0) return 64; n = 0;\n"
"  while ((v & 1ull) == 0) { v >>= 1; n += 1; } return n; }\n"
"static marv_i32 marv_pop64(marv_u64 v)\n"
"{ marv_i32 n; n = 0; while (v != 0) { n += (marv_i32)(v & 1ull); v >>= 1; } return n; }\n"
"\n"
"#define MARV_LOAD(T, p, off) marv_load_##T((unsigned char*)(p) + (off))\n"
"#define MARV_STORE(T, p, off, v) marv_store_##T((unsigned char*)(p) + (off), (v))\n"
"#define MARV_LDST(T, CT)\\\n"
"  static CT marv_load_##T(void *p) { CT v; memcpy(&v, p, sizeof(v)); return v; }\\\n"
"  static void marv_store_##T(void *p, CT v) { memcpy(p, &v, sizeof(v)); }\n"
"MARV_LDST(i8, int8_t) MARV_LDST(u8, uint8_t)\n"
"MARV_LDST(i16, int16_t) MARV_LDST(u16, uint16_t)\n"
"MARV_LDST(i32, int32_t) MARV_LDST(u32, uint32_t)\n"
"MARV_LDST(i64, int64_t) MARV_LDST(f32, float)\n"
"MARV_LDST(f64, double) MARV_LDST(ptr, marv_ptr)\n"
"\n"
"/* Pre-main initialization of native-typed data runs and pointer slots. */\n"
"#if defined(__GNUC__) || defined(__clang__)\n"
"#define MARV_CTOR(f) static void f(void) __attribute__((constructor));\\\n"
"                     static void f(void)\n"
"#elif defined(_MSC_VER)\n"
"#pragma section(\".CRT$XCU\", read)\n"
"#define MARV_CTOR(f) static void f(void);\\\n"
"                     __declspec(allocate(\".CRT$XCU\"))\\\n"
"                     static void (*f##_entry_)(void) = f;\\\n"
"                     static void f(void)\n"
"#else\n"
"#define MARV_CTOR(f) static void f(void)\n"
"#define MARV_NO_CTOR 1\n"
"#endif\n";

/* a MARV name becomes a C identifier, by the same rule the declarations
 * were named with: a second copy of it would let a definition and its
 * forward declaration disagree */
static void cc_ident(const char *s, cbuf_t *pcb)
{
  c_mangle(s, pcb);
}

static const char *cc_type(vt_t vt)
{
  switch (vt) {
    case VT_I32: return "marv_i32";
    case VT_I64: return "marv_i64";
    case VT_F32: return "marv_f32";
    case VT_F64: return "marv_f64";
    case VT_PTR: return "marv_ptr";
    default: break;
  }
  return "void";
}

/* the helper name for an instruction: i32.add -> marv_i32_add */
static void cc_helper(sym_t op, cbuf_t *pcb)
{
  cbputs("marv_", pcb);
  cc_ident(symname(op), pcb);
}

static mod_t *g_cc_mod;               /* the module being emitted [cco] */

/* 1 for an infinity, 2 for a NaN, 0 for a value with a hex literal */
static int nonfinite(double d)
{
  if (d != d) return 2;
  if (d > 1.7976931348623157e308 || d < -1.7976931348623157e308) return 1;
  return 0;
}

static void cc_opnd(const opnd_t *po, const char *mod, FILE *fp)
{
  cbuf_t nb;
  char buf[48];
  switch (po->kind) {
    case OK_IMM:
      if (po->vt == VT_F32 || po->vt == VT_F64) {
        int nf;
        const char *cast;
        cast = (po->vt == VT_F32) ? "(marv_f32)" : "";
        nf = nonfinite(po->dval);
        if (nf == 2) fprintf(fp, "(%s(HUGE_VAL - HUGE_VAL))", cast);
        else if (nf == 1)
          fprintf(fp, "(%s%sHUGE_VAL)", cast, (po->dval < 0.0) ? "-" : "");
        else {
          udtohex(as_uint64(po->dval), &buf[0]);
          fprintf(fp, (po->vt == VT_F32) ? "((marv_f32)%s)" : "%s", &buf[0]);
        }
      } else if (po->vt == VT_I64) {
        if (po->ival == (-9223372036854775807LL - 1LL))
          fputs("(-9223372036854775807LL - 1LL)", fp);
        else fprintf(fp, "INT64_C(%lld)", po->ival);
      } else if (po->vt == VT_PTR) {
        if (po->ival == 0) fputs("marv_ptr_null()", fp);
        else if (po->ival == (-9223372036854775807LL - 1LL))
          fputs("marv_ptr_of_i64(-9223372036854775807LL - 1LL)", fp);
        else fprintf(fp, "marv_ptr_of_i64(INT64_C(%lld))", po->ival);
      } else {
        if (po->ival == -2147483647LL - 1LL) fputs("(-2147483647 - 1)", fp);
        else fprintf(fp, "INT32_C(%lld)", po->ival);
      }
      break;
    case OK_FRAME:
      fprintf(fp, "(marv_ptr)(fr$ + %lld)", po->ival);
      break;
    case OK_DATA:
      if (g_cc_mod != NULL && mod_dataname(g_cc_mod, po->sym) != 0) {
        fprintf(fp, "(marv_ptr)(%s", symname(mod_dataname(g_cc_mod, po->sym)));
      } else {
        cbinit(&nb); cbputs(mod, &nb); cbputc('_', &nb);
        cc_ident(symname(po->sym), &nb); cbputc(0, &nb);
        fprintf(fp, "(marv_ptr)(%s", (const char *)cbdata(&nb));
        cbfini(&nb);
      }
      if (po->ival != 0) fprintf(fp, " + %lld", po->ival);
      fputs(")", fp);
      break;
    case OK_FUNC: {
      sig_t *ps;                        /* a reference names its owner's symbol */
      ps = (g_cc_mod != NULL) ? mod_sig(g_cc_mod, po->sym) : NULL;
      if (ps != NULL && ps->cname != 0) {
        fprintf(fp, "(marv_ptr)%s", symname(ps->cname));
      } else {
        cbinit(&nb); cbputs(mod, &nb); cbputc('_', &nb);
        cc_ident(symname(po->sym), &nb); cbputc(0, &nb);
        fprintf(fp, "(marv_ptr)%s", (const char *)cbdata(&nb));
        cbfini(&nb);
      }
      break;
    }
    case OK_VREG:
      cbinit(&nb); cbputs("v_", &nb);
      cc_ident(symname(po->sym), &nb); cbputc(0, &nb);
      fputs((const char *)cbdata(&nb), fp);
      cbfini(&nb);
      break;
    default: fputs("0", fp); break;
  }
}

static void cc_args(const ir_t *pi, size_t from, const char *mod, FILE *fp)
{
  size_t i;
  for (i = from; i < buflen(&((ir_t *)pi)->args); ++i) {
    if (i > from) fputs(", ", fp);
    cc_opnd((opnd_t *)bufref(&((ir_t *)pi)->args, i), mod, fp);
  }
}

/* the memory type suffix a load or store instruction works in */
/* a slot holds one value of its own type, at its own width */
static const char *cc_slotname(vt_t vt)
{
  if (vt == VT_I32) return "i32";
  if (vt == VT_I64) return "i64";
  if (vt == VT_F32) return "f32";
  if (vt == VT_F64) return "f64";
  return "ptr";
}

static const char *cc_memtype(const char *op)

{
  if (strstr(op, "load8_s") != NULL || strstr(op, "store8") != NULL) {
    return (strstr(op, "load8_u") != NULL) ? "u8" : "i8";
  }
  if (strstr(op, "load8_u") != NULL) return "u8";
  if (strstr(op, "load16_s") != NULL) return "i16";
  if (strstr(op, "load16_u") != NULL) return "u16";
  if (strstr(op, "store16") != NULL) return "i16";
  if (strstr(op, "load32_u") != NULL) return "u32";
  if (strstr(op, "load32_s") != NULL || strstr(op, "store32") != NULL) return "i32";
  if (strstr(op, "i32.") == op) return "i32";
  if (strstr(op, "i64.") == op) return "i64";
  if (strstr(op, "f32.") == op) return "f32";
  if (strstr(op, "f64.") == op) return "f64";
  if (strstr(op, "ptr.") == op) return "ptr";
  return "i32";
}

/* A function whose name its module's interface declares is the module's
 * contract with other modules: its definition must carry external linkage
 * even without an (export "..."), whose business is the extra unqualified
 * alias.  Otherwise the extern declaration cc_decls_write took from the
 * interface meets a static definition and the C compiler refuses both. */
static bool cc_iface_visible(const mod_t *pm, const fn_t *pf)
{
  sig_t *pl;
  size_t i;
  pl = mod_sig((mod_t *)pm, pf->name);
  if (pl == NULL || pl->cname == 0) return false;
  for (i = 0; i < buflen(&((mod_t *)pm)->sigs); ++i) {
    sig_t *ps;
    ps = (sig_t *)bufref(&((mod_t *)pm)->sigs, i);
    if (ps->cname == pl->cname && !ps->local) return true;
  }
  return false;
}

/* the same for an interface-declared global or data segment */
static bool cc_glb_iface_visible(const mod_t *pm, sym_t name)
{
  sym_t cn;
  size_t i;
  cn = mod_globalname((mod_t *)pm, name);
  if (cn == 0) return false;
  for (i = 0; i < buflen(&((mod_t *)pm)->globals); ++i) {
    glb_t *pg;
    pg = (glb_t *)bufref(&((mod_t *)pm)->globals, i);
    if (pg->cname == cn && !pg->local) return true;
  }
  return false;
}

static bool cc_dat_iface_visible(const mod_t *pm, sym_t name)
{
  sym_t cn;
  size_t i;
  cn = mod_dataname((mod_t *)pm, name);
  if (cn == 0) return false;
  for (i = 0; i < buflen(&((mod_t *)pm)->datas); ++i) {
    dat_t *pd;
    pd = (dat_t *)bufref(&((mod_t *)pm)->datas, i);
    if (pd->cname == cn && !pd->local) return true;
  }
  return false;
}

static void cc_alias(const fn_t *pf, const char *mod, FILE *fp);

void cc_func(const mod_t *pm, const fn_t *pf, FILE *fp)
{
  size_t i;
  g_cc_mod = (mod_t *)pm;
  cbuf_t mn;
  const char *mod;
  cbinit(&mn);
  cc_ident(symname(pm->name), &mn);
  cbputc(0, &mn);
  mod = (const char *)cbdata(&mn);

  fputs((pf->expname != 0 || cc_iface_visible(pm, pf)) ? "" : "static ", fp);
  fprintf(fp, "%s %s_", cc_type(pf->res), mod);
  { cbuf_t nb; cbinit(&nb); cc_ident(symname(pf->name), &nb); cbputc(0, &nb);
    fputs((const char *)cbdata(&nb), fp); cbfini(&nb); }
  fputs("(", fp);
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    cbuf_t nb;
    pv = (var_t *)bufref(&((fn_t *)pf)->vars, i);
    if (i > 0) fputs(", ", fp);
    cbinit(&nb); cbputs("v_", &nb); cc_ident(symname(pv->name), &nb); cbputc(0, &nb);
    fprintf(fp, "%s %s", cc_type(pv->vt), (const char *)cbdata(&nb));
    cbfini(&nb);
  }
  if (pf->nparams == 0) fputs("void", fp);
  fputs(")\n{\n", fp);

  for (i = (size_t)pf->nparams; i < buflen(&((fn_t *)pf)->vars); ++i) {
    var_t *pv;
    cbuf_t nb;
    pv = (var_t *)bufref(&((fn_t *)pf)->vars, i);
    cbinit(&nb); cbputs("v_", &nb); cc_ident(symname(pv->name), &nb); cbputc(0, &nb);
    fprintf(fp, "  %s %s = 0;\n", cc_type(pv->vt), (const char *)cbdata(&nb));
    cbfini(&nb);
  }
  if (pf->frame != 0) {
    fprintf(fp, "  MARV_ALIGNED(16) unsigned char fr$[%lld];\n", pf->frame);
    /* a module may declare a frame and not use it, which is its business;
     * cl warns about the unreferenced array, so the reference is spelled out */
    fputs("  (void)fr$;\n", fp);
  }

  for (i = 0; i < buflen(&((fn_t *)pf)->code); ++i) {
    ir_t *pi;
    cbuf_t hb;
    pi = (ir_t *)bufref(&((fn_t *)pf)->code, i);
    switch (pi->op) {
      case IR_LABEL: case IR_LOOPEND:
        fprintf(fp, " L%s: ;\n", symname(pi->name) + 1);
        break;
      case IR_LOOPBEG:
        fprintf(fp, " L%s: ;\n", symname(pi->name) + 1);
        break;
      case IR_JUMP:
        fprintf(fp, "  goto L%s;\n", symname(pi->name) + 1);
        break;
      case IR_BRTRUE:
        fputs("  if (", fp); cc_args(pi, 0, mod, fp);
        fprintf(fp, " != 0) goto L%s;\n", symname(pi->name) + 1);
        break;
      case IR_BRFALSE:
        fputs("  if (", fp); cc_args(pi, 0, mod, fp);
        fprintf(fp, " == 0) goto L%s;\n", symname(pi->name) + 1);
        break;
      case IR_BRTABLE: {
        size_t k;
        fputs("  switch (", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 0), mod, fp);
        fputs(") {\n", fp);
        for (k = 1; k + 1 < buflen(&pi->args); ++k)
          fprintf(fp, "    case %d: goto L%s;\n", (int)(k - 1),
                  symname(((opnd_t *)bufref(&pi->args, k))->sym) + 1);
        if (buflen(&pi->args) > 1)
          fprintf(fp, "    default: goto L%s;\n",
                  symname(((opnd_t *)bufref(&pi->args, buflen(&pi->args) - 1))->sym) + 1);
        fputs("  }\n", fp);
        break;
      }
      case IR_MOV:
        fputs("  ", fp); cc_opnd(&pi->dst, mod, fp); fputs(" = ", fp);
        cc_args(pi, 0, mod, fp); fputs(";\n", fp);
        break;
      case IR_UNARY: case IR_BINARY:
        fputs("  ", fp); cc_opnd(&pi->dst, mod, fp); fputs(" = ", fp);
        cbinit(&hb); cc_helper(pi->name, &hb); cbputc(0, &hb);
        fprintf(fp, "%s(", (const char *)cbdata(&hb));
        cbfini(&hb);
        cc_args(pi, 0, mod, fp); fputs(");\n", fp);
        break;
      case IR_SELECT:
        fputs("  ", fp); cc_opnd(&pi->dst, mod, fp); fputs(" = (", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 2), mod, fp);
        fputs(" != 0) ? ", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 0), mod, fp);
        fputs(" : ", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 1), mod, fp);
        fputs(";\n", fp);
        break;
      case IR_LOAD:
        fputs("  ", fp); cc_opnd(&pi->dst, mod, fp);
        fprintf(fp, " = (%s)MARV_LOAD(%s, ", cc_type(pi->vt), cc_memtype(symname(pi->name)));
        cc_args(pi, 0, mod, fp);
        fprintf(fp, ", %lld);\n", pi->off);
        break;
      case IR_MEMORY: {
        /* the C library's routines are the tuned ones on this target, and
         * memmove rather than memcpy because a run may overlap.  The count is
         * read unsigned, as WebAssembly reads it. */
        bool isfill;
        isfill = streql(symname(pi->name), "memory.fill");
        fputs(isfill ? "  memset(" : "  memmove(", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 0), mod, fp);
        fputs(", ", fp);
        if (isfill) fputs("(int)(", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 1), mod, fp);
        if (isfill) fputs(")", fp);
        fputs(", (size_t)(marv_u32)(", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, 2), mod, fp);
        fputs("));\n", fp);
        break;
      }
      case IR_STORE:
        fprintf(fp, "  MARV_STORE(%s, ", cc_memtype(symname(pi->name)));
        cc_opnd((opnd_t *)bufref(&pi->args, 0), mod, fp);
        fprintf(fp, ", %lld, ", pi->off);
        cc_opnd((opnd_t *)bufref(&pi->args, 1), mod, fp);
        fputs(");\n", fp);
        break;
      case IR_GGET: {
        sym_t gn;
        gn = mod_globalname((mod_t *)pm, pi->name);
        fputs("  ", fp); cc_opnd(&pi->dst, mod, fp);
        if (gn != 0) fprintf(fp, " = %s;\n", symname(gn));
        else {
          cbinit(&hb); cc_ident(symname(pi->name), &hb); cbputc(0, &hb);
          fprintf(fp, " = %s_%s;\n", mod, (const char *)cbdata(&hb));
          cbfini(&hb);
        }
        break;
      }
      case IR_GSET: {
        sym_t gn;
        gn = mod_globalname((mod_t *)pm, pi->name);
        if (gn != 0) fprintf(fp, "  %s = ", symname(gn));
        else {
          cbinit(&hb); cc_ident(symname(pi->name), &hb); cbputc(0, &hb);
          fprintf(fp, "  %s_%s = ", mod, (const char *)cbdata(&hb));
          cbfini(&hb);
        }
        cc_args(pi, 0, mod, fp); fputs(";\n", fp);
        break;
      }
      case IR_TJUMP: {
        sym_t letters;
        size_t k;
        int off;
        letters = pi->sig;
        if (letters == 0) {
          fprintf(stderr, "marv: a trampoline_jump with no chain shape\n");
          exit(1);
        }
        fputs("#ifdef MARV_TAILCALL_GUARANTEE\n", fp);
        fprintf(fp, "  MARV_MUSTTAIL return %s(", symname(pi->name));
        cc_args(pi, 0, mod, fp);
        fputs(");\n", fp);
        fputs("#else\n", fp);
        fprintf(fp, "  MARV_STORE(i32, tc_%s, 0, %lu);\n", symname(letters),
                (unsigned long)buflen(&pi->args));
        fprintf(fp, "  MARV_STORE(ptr, tc_%s, 8, (marv_ptr)&%s);\n",
                symname(letters), symname(pi->name));
        off = 16;
        for (k = 0; k < buflen(&pi->args); ++k) {
          opnd_t *pa;
          pa = (opnd_t *)bufref(&pi->args, k);
          fprintf(fp, "  MARV_STORE(%s, tc_%s, %d, ",
                  cc_slotname(pa->vt), symname(letters), off);
          cc_opnd(pa, mod, fp);
          fputs(");\n", fp);
          off += 16;
        }
        fprintf(fp, "  return (%s)tc_%s;\n", cc_type(pf->res), symname(letters));
        fputs("#endif\n", fp);
        break;
      }
      case IR_TJUMPI: {
        sym_t letters;
        size_t k, n;
        int off;
        letters = pi->sig;
        if (letters == 0) {
          fprintf(stderr, "marv: a trampoline_jump_indirect with no chain shape\n");
          exit(1);
        }
        n = buflen(&pi->args);          /* the arguments, then the callee */
        if (n == 0) {
          fprintf(stderr, "marv: a trampoline_jump_indirect with no callee\n");
          exit(1);
        }
        fputs("#ifdef MARV_TAILCALL_GUARANTEE\n", fp);
        fprintf(fp, "  MARV_MUSTTAIL return ((%s (*)(", cc_type(pi->vt));
        if (n == 1) fputs("void", fp);
        else for (k = 0; k + 1 < n; ++k) {
          if (k > 0) fputs(", ", fp);
          fputs(cc_type(((opnd_t *)bufref(&pi->args, k))->vt), fp);
        }
        fputs("))(", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, n - 1), mod, fp);
        /* the cast closes before the call, or the call is of the cast */
        fputs("))(", fp);
        for (k = 0; k + 1 < n; ++k) {
          if (k > 0) fputs(", ", fp);
          cc_opnd((opnd_t *)bufref(&pi->args, k), mod, fp);
        }
        fputs(");\n", fp);
        fputs("#else\n", fp);
        fprintf(fp, "  MARV_STORE(i32, tc_%s, 0, %lu);\n", symname(letters),
                (unsigned long)(n - 1));
        fputs("  MARV_STORE(ptr, tc_", fp);
        fputs(symname(letters), fp);
        fputs(", 8, ", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, n - 1), mod, fp);
        fputs(");\n", fp);
        off = 16;
        for (k = 0; k + 1 < n; ++k) {
          opnd_t *pa;
          pa = (opnd_t *)bufref(&pi->args, k);
          fprintf(fp, "  MARV_STORE(%s, tc_%s, %d, ",
                  cc_slotname(pa->vt), symname(letters), off);
          cc_opnd(pa, mod, fp);
          fputs(");\n", fp);
          off += 16;
        }
        fprintf(fp, "  return (%s)tc_%s;\n", cc_type(pf->res), symname(letters));
        fputs("#endif\n", fp);
        break;
      }
      case IR_CALL: case IR_RETCALL:
        fputs("  ", fp);
        if (pi->op == IR_RETCALL) fputs("MARV_MUSTTAIL return ", fp);
        else if (pi->hasdst) { cc_opnd(&pi->dst, mod, fp); fputs(" = ", fp); }
        fprintf(fp, "%s(", symname(pi->name));   /* already the C name */
        cc_args(pi, 0, mod, fp); fputs(");\n", fp);
        break;
      case IR_RETURN:
        if (buflen(&pi->args) > 0) {
          fputs("  return ", fp); cc_args(pi, 0, mod, fp); fputs(";\n", fp);
        } else if (pf->res != VT_VOID) {
          fprintf(fp, "  return (%s)0;\n", cc_type(pf->res));   /* not reached */
        } else {
          fputs("  return;\n", fp);
        }
        break;
      case IR_UNREACHABLE:
        fputs("  marv_unreachable();\n", fp);
        break;
      case IR_CALLI: case IR_RETCALLI: {
        size_t n, j;
        n = buflen(&pi->args);
        if (n == 0) {
          fprintf(stderr, "marv: an indirect call with no callee\n");
          exit(1);
        }
        fputs("  ", fp);
        if (pi->op == IR_RETCALLI) fputs("MARV_MUSTTAIL return ", fp);
        else if (pi->hasdst) { cc_opnd(&pi->dst, mod, fp); fputs(" = ", fp); }
        fprintf(fp, "((%s (*)(", cc_type(pi->vt));
        if (n == 1) fputs("void", fp);
        else for (j = 0; j + 1 < n; ++j) {
          if (j > 0) fputs(", ", fp);
          fputs(cc_type(((opnd_t *)bufref(&pi->args, j))->vt), fp);
        }
        fputs("))(", fp);
        cc_opnd((opnd_t *)bufref(&pi->args, n - 1), mod, fp);
        fputs("))(", fp);
        for (j = 0; j + 1 < n; ++j) {
          if (j > 0) fputs(", ", fp);
          cc_opnd((opnd_t *)bufref(&pi->args, j), mod, fp);
        }
        fputs(");\n", fp);
        break;
      }
      default:
        fprintf(stderr, "marv: the cc target cannot emit %s yet\n",
                (pi->name != 0) ? symname(pi->name) : "this instruction");
        exit(1);
    }
  }
  fputs("}\n\n", fp);
  if (pf->expname != 0 && streql(symname(pf->expname), "main")) {
    cbuf_t nb;                          /* the C entry point calls it */
    cbinit(&nb); cc_ident(symname(pf->name), &nb); cbputc(0, &nb);
    /* a module's main takes the two arguments, one of them, or neither, and
     * gets what it declared; the entry point still has the C signature */
    fputs("int main(int argc, char **argv)\n{\n", fp);
    if (pf->nparams < 1) fputs("  (void)argc;\n", fp);
    if (pf->nparams < 2) fputs("  (void)argv;\n", fp);
    fprintf(fp, "  return (int)%s_%s(", mod, (const char *)cbdata(&nb));
    if (pf->nparams >= 1) fputs("(marv_i32)argc", fp);
    if (pf->nparams >= 2) fputs(", (marv_ptr)argv", fp);
    fputs(");\n}\n\n", fp);
    cbfini(&nb);
  } else if (pf->expname != 0) {
    cc_alias(pf, mod, fp);
  }
  cbfini(&mn);
}

/* An exported function is reachable under its export name as well as its
 * qualified one, because a C caller outside MARV spells the name the module
 * exported and knows nothing of the qualification.  On the wo target the
 * export clause says this and on rv the two names are two labels on one
 * function; C has no portable aliasing, so it is a forwarding definition.
 * `main` is not one of these: it already has the entry wrapper above.  [cco] */
static void cc_alias(const fn_t *pf, const char *mod, FILE *fp)
{
  size_t i;
  cbuf_t eb;
  cbuf_t qb;
  cbinit(&eb); c_mangle(symname(pf->expname), &eb); cbputc(0, &eb);
  cbinit(&qb); cbputs(mod, &qb); cbputc('_', &qb);
  cc_ident(symname(pf->name), &qb); cbputc(0, &qb);
  if (!streql((const char *)cbdata(&eb), (const char *)cbdata(&qb))) {
    fprintf(fp, "%s %s(", cc_type(pf->res), (const char *)cbdata(&eb));
    for (i = 0; i < (size_t)pf->nparams; ++i) {
      var_t *pv;
      pv = (var_t *)bufref(&((fn_t *)pf)->vars, i);
      if (i > 0) fputs(", ", fp);
      fprintf(fp, "%s a%lu", cc_type(pv->vt), (unsigned long)i);
    }
    if (pf->nparams == 0) fputs("void", fp);
    fputs(")\n{\n", fp);
    fprintf(fp, "  %s%s(", (pf->res == VT_VOID) ? "" : "return ",
            (const char *)cbdata(&qb));
    for (i = 0; i < (size_t)pf->nparams; ++i)
      fprintf(fp, "%sa%lu", (i > 0) ? ", " : "", (unsigned long)i);
    fputs(");\n}\n\n", fp);
  }
  cbfini(&qb);
  cbfini(&eb);
}

/* the system modules this object needs, for a later link to read */
static void deps_line(tt_t t, FILE *fp)
{
  size_t d;
  if (sys_count() == 0) return;
  fprintf(fp, "%smarv-sys-deps:", tt_comment(t));
  for (d = 0; d < sys_count(); ++d) fprintf(fp, " %s", sys_name(d));
  if (t == TT_CC) fputs(" */", fp);
  fputs("\n", fp);
}

void cc_prologue_write(FILE *fp)
{
  deps_line(TT_CC, fp);
  fputs(cc_prologue, fp);
  fputs("\n", fp);
  /* where the fixed runtime ends and this module begins: the driver cuts a
   * large generated file into translation units here */
  fputs("/* marv-runtime-end */\n\n", fp);
}

/* The areas a trampoline_jump writes when the compiler cannot make a jump of
 * it: one per chain shape, module scoped, sixteen-byte slots with the count
 * at nought and the function to enter at eight, so the geometry is the one
 * the variadic slots already use.  On a compiler that can jump there is no
 * area and no loop.  [cco] */
void cc_tramp_areas(const mod_t *pm, FILE *fp)
{
  size_t i;
  if (buflen(&((mod_t *)pm)->tramps) == 0) return;
  fputs("#ifndef MARV_TAILCALL_GUARANTEE\n", fp);
  for (i = 0; i < buflen(&((mod_t *)pm)->tramps); ++i) {
    sym_t letters;
    int n;
    letters = *(sym_t *)bufref(&((mod_t *)pm)->tramps, i);
    n = sig_nparams((mod_t *)pm, letters);
    fprintf(fp, "MARV_ALIGNED(16) static unsigned char tc_%s[%d];\n",
            symname(letters), 16 + 16 * n);
  }
  fputs("#endif\n\n", fp);
}

/* every function the module might call, declared once */

void cc_decls_write(const mod_t *pm, FILE *fp)
{
  size_t i, k;
  buf_t seen;
  bufinit(&seen, sizeof(sym_t));
  for (i = 0; i < buflen(&((mod_t *)pm)->sigs); ++i) {
    sig_t *ps;
    bool dup;
    ps = (sig_t *)bufref(&((mod_t *)pm)->sigs, i);
    if (ps->cname == 0) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == ps->cname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = ps->cname;
    fprintf(fp, "%s%s %s(",
            (!ps->local) ? "extern " : (ps->exported ? "" : "static "),
            cc_type(ps->res), symname(ps->cname));
    if (buflen(&ps->ptypes) == 0) fputs("void", fp);
    for (k = 0; k < buflen(&ps->ptypes); ++k) {
      if (k > 0) fputs(", ", fp);
      fputs(cc_type(*(vt_t *)bufref(&ps->ptypes, k)), fp);
    }
    fputs(");\n", fp);
  }
  for (i = 0; i < buflen(&((mod_t *)pm)->datas); ++i) {     /* imported data */
    dat_t *pd;
    bool dup;
    pd = (dat_t *)bufref(&((mod_t *)pm)->datas, i);
    if (pd->local || pd->cname == 0) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == pd->cname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = pd->cname;
    fprintf(fp, "extern unsigned char %s[];\n", symname(pd->cname));
  }
  for (i = 0; i < buflen(&((mod_t *)pm)->globals); ++i) {   /* imported globals */
    glb_t *pg;
    bool dup;
    pg = (glb_t *)bufref(&((mod_t *)pm)->globals, i);
    if (pg->local || pg->cname == 0) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == pg->cname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = pg->cname;
    fprintf(fp, "extern %s %s;\n", cc_type(pg->vt), symname(pg->cname));
  }
  fputs("\n", fp);
  buffini(&seen);
}

/* a (data ...) or (global ...) member, as a C definition */
/* MARV_ALIGNED wants a power of two, and nothing here needs more than the
 * widest slot */
static long long cc_align(long long a)
{
  if (a > 8) return 16;
  if (a > 4) return 8;
  if (a > 2) return 4;
  if (a > 1) return 2;
  return 1;
}

/* the C name of what a data segment's pointer slot points at */
static void cc_relname(const mod_t *pm, drel_t *pr, const char *mod, FILE *fp)
{
  cbuf_t nb;
  if (pr->isfunc) {
    sig_t *ps;
    ps = mod_sig((mod_t *)pm, pr->sym);
    if (ps != NULL && ps->cname != 0) { fputs(symname(ps->cname), fp); return; }
  } else {
    sym_t dn;
    dn = mod_dataname((mod_t *)pm, pr->sym);
    if (dn != 0) { fputs(symname(dn), fp); return; }
  }
  cbinit(&nb); cbputs(mod, &nb); cbputc('_', &nb);
  cc_ident(symname(pr->sym), &nb); cbputc(0, &nb);
  fputs((const char *)cbdata(&nb), fp);
  cbfini(&nb);
}

/* Runs whose representation is all zero need no copy from a C initializer. */
static bool cc_data_nonzero(dseg_t *pd, dnum_t *run)
{
  size_t i;
  for (i = 0; i < run->count; ++i)
    if (*(unsigned long long *)bufref(&pd->numvals, run->first + i) != 0) return true;
  return false;
}

/* All storage declarations are emitted before these constructors, so both
 * their relocations and the driver's C splitter can see every segment. A run's
 * temporary array has the C target's native representation even when its
 * destination in the byte image is unaligned. */
void cc_data_init(const mod_t *pm, snode_t *pn, FILE *fp)
{
  dseg_t ds;
  size_t i, j;
  bool needs;
  sym_t dn;
  cbuf_t mn, nb;
  const char *mod, *dname;
  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  dseg_read(pn, &ds, false);
  needs = buflen(&ds.relocs) != 0;
  for (i = 0; i < buflen(&ds.nums); ++i)
    if (cc_data_nonzero(&ds, (dnum_t *)bufref(&ds.nums, i))) needs = true;
  if (!needs) { dseg_fini(&ds); return; }
  cbinit(&mn); cc_ident(symname(pm->name), &mn); cbputc(0, &mn);
  mod = (const char *)cbdata(&mn);
  cbinit(&nb);
  dn = mod_dataname((mod_t *)pm, snref(pn, 1)->sym);
  if (dn != 0) cbputs(symname(dn), &nb);
  else { cbputs(mod, &nb); cbputc('_', &nb); cc_ident(symname(snref(pn, 1)->sym), &nb); }
  cbputc(0, &nb);
  dname = (const char *)cbdata(&nb);
  fputs("#ifdef MARV_NO_CTOR\n", fp);
  fputs("#error \"typed data or pointer relocations require a startup constructor\"\n", fp);
  fputs("#endif\n", fp);
  fprintf(fp, "MARV_CTOR(marv_reloc_%s)\n{\n", dname);
  for (i = 0; i < buflen(&ds.nums); ++i) {
    dnum_t *run;
    const char *type;
    run = (dnum_t *)bufref(&ds.nums, i);
    if (!cc_data_nonzero(&ds, run)) continue;
    type = (run->kind == DNUM_F32) ? "float" : (run->kind == DNUM_F64) ? "double"
         : (run->width == 2) ? "uint16_t" : (run->width == 4) ? "uint32_t" : "uint64_t";
    fprintf(fp, "  static const %s marv_run_%lu[%lu] = {", type,
            (unsigned long)i, (unsigned long)run->count);
    for (j = 0; j < run->count; ++j) {
      unsigned long long bits;
      bits = *(unsigned long long *)bufref(&ds.numvals, run->first + j);
      if (j > 0) fputs(",", fp);
      if ((j % 6) == 0) fputs("\n    ", fp);
      if (run->kind == DNUM_INT) {
        fprintf(fp, "(uint%d_t)UINT64_C(0x%llx)", run->width * 8, bits);
      } else {
        double d;
        int nf;
        const char *cast;
        /* HUGE_VAL and a hex literal are both double: an f32 run needs the
         * cast its element type asks for, as an f32 immediate does. */
        cast = (run->kind == DNUM_F32) ? "(marv_f32)" : "";
        d = (run->kind == DNUM_F32) ? (double)as_float((unsigned)bits) : as_double(bits);
        nf = nonfinite(d);
        if (nf == 2) fprintf(fp, "(%s(HUGE_VAL - HUGE_VAL))", cast);
        else if (nf == 1)
          fprintf(fp, "(%s%sHUGE_VAL)", cast, (d < 0.0) ? "-" : "");
        else {
          char num[48];
          udtohex(as_uint64(d), &num[0]);
          fprintf(fp, "(%s%s)", cast, &num[0]);
        }
      }
    }
    fputs("\n  };\n", fp);
  }
  for (i = 0; i < buflen(&ds.nums); ++i) {
    dnum_t *run;
    run = (dnum_t *)bufref(&ds.nums, i);
    if (!cc_data_nonzero(&ds, run)) continue;
    fprintf(fp, "  memcpy(%s + %lu, marv_run_%lu, sizeof(marv_run_%lu));\n",
            dname, (unsigned long)run->off, (unsigned long)i, (unsigned long)i);
  }
  for (i = 0; i < buflen(&ds.relocs); ++i) {
    drel_t *pr;
    pr = (drel_t *)bufref(&ds.relocs, i);
    fprintf(fp, "  MARV_STORE(ptr, %s, %lu, (marv_ptr)", dname,
            (unsigned long)pr->off);
    if (pr->isfunc) cc_relname(pm, pr, mod, fp);
    else {
      fputs("(", fp);
      cc_relname(pm, pr, mod, fp);
      if (pr->addend != 0) fprintf(fp, " + %lld", pr->addend);
      fputs(")", fp);
    }
    fputs(");\n", fp);
  }
  fputs("}\n\n", fp);
  cbfini(&nb); cbfini(&mn);
  dseg_fini(&ds);
}

void cc_static(const mod_t *pm, snode_t *pn, FILE *fp)
{
  cbuf_t mn, nb;
  const char *mod;
  const char *h;
  bool isdata, exported;
  size_t i;
  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  h = symname(snref(pn, 0)->sym);
  isdata = streql(h, "data");
  cbinit(&mn); cc_ident(symname(pm->name), &mn); cbputc(0, &mn);
  mod = (const char *)cbdata(&mn);
  cbinit(&nb); cc_ident(symname(snref(pn, 1)->sym), &nb); cbputc(0, &nb);

  exported = false;
  for (i = 2; i < snlen(pn); ++i)
    if (snref(pn, i)->nt == SN_LIST && snlen(snref(pn, i)) > 1
        && snref(snref(pn, i), 0)->nt == SN_SYM
        && streql(symname(snref(snref(pn, i), 0)->sym), "export")) exported = true;

  if (isdata) {
    dseg_t ds;
    size_t k;
    sym_t dn;
    cbuf_t db;
    const char *dname;
    dseg_read(pn, &ds, false);
    dn = mod_dataname((mod_t *)pm, snref(pn, 1)->sym);
    cbinit(&db);
    if (dn != 0) cbputs(symname(dn), &db);
    else { cbputs(mod, &db); cbputc('_', &db); cbputs((const char *)cbdata(&nb), &db); }
    cbputc(0, &db);
    dname = (const char *)cbdata(&db);
    /* a zeroed buffer gets no initializer: one of these is a megabyte */
    fprintf(fp, "%sMARV_ALIGNED(%lld) unsigned char %s[%lu]",
            (exported || cc_dat_iface_visible(pm, snref(pn, 1)->sym))
              ? "" : "static ", cc_align(ds.align), dname,
            (unsigned long)buflen(&ds.bytes));
    if (!ds.allzero) {
      fputs(" = {", fp);
      for (k = 0; k < buflen(&ds.bytes); ++k) {
        if (k > 0) fputs(",", fp);
        if ((k % 20) == 0) fputs("\n  ", fp);
        fprintf(fp, "%u", (unsigned)(unsigned char)*(char *)bufref(&ds.bytes, k));
      }
      fputs("}", fp);
    }
    fputs(";\n", fp);
    fputs("\n", fp);
    cbfini(&db);
    dseg_fini(&ds);
  } else {
    vt_t vt;
    snode_t *pt;
    vt = VT_I32;
    for (i = 2; i < snlen(pn); ++i) {
      pt = snref(pn, i);
      if (pt->nt == SN_LIST && snlen(pt) == 2 && snref(pt, 0)->nt == SN_SYM
          && streql(symname(snref(pt, 0)->sym), "mut")) pt = snref(pt, 1);
      if (pt->nt == SN_SYM) {
        const char *t;
        t = symname(pt->sym);
        if (streql(t, "i64")) vt = VT_I64;
        else if (streql(t, "f32")) vt = VT_F32;
        else if (streql(t, "f64")) vt = VT_F64;
        else if (streql(t, "ptr")) vt = VT_PTR;
        else if (streql(t, "i32")) vt = VT_I32;
        break;
      }
    }
    {
      sym_t gn;
      glb_init_t gi;
      glb_read(pn, vt, &gi);
      gn = mod_globalname((mod_t *)pm, snref(pn, 1)->sym);
      fprintf(fp, "%s%s ",
              (exported || cc_glb_iface_visible(pm, snref(pn, 1)->sym))
                ? "" : "static ", cc_type(vt));
      if (gn != 0) fprintf(fp, "%s", symname(gn));
      else fprintf(fp, "%s_%s", mod, (const char *)cbdata(&nb));
      /* the declared initial value, which is not always zero */
      if (gi.hasval) {
        opnd_t o;
        memset(&o, 0, sizeof(opnd_t));
        o.kind = OK_IMM; o.vt = vt; o.ival = gi.ival; o.dval = gi.dval;
        fputs(" = ", fp);
        cc_opnd(&o, mod, fp);
      }
      fputs(";\n\n", fp);
    }
  }
  cbfini(&nb);
  cbfini(&mn);
}

/* ================================================================ */
/* The wo target: WCPL's extended WebAssembly text. Written from the lowered
 * tree rather than from the three-address code, because WebAssembly has
 * structured control flow and the IR has already thrown that away.  [cco]
 *
 * Pointers are i32 here, but a pointer slot in memory is eight bytes, so a
 * ptr load widens and a ptr store narrows. */

static mod_t *g_wo_mod;
static fn_t *g_wo_fn;

static void wo_name(sym_t owner, sym_t member, FILE *fp)
{
  const char *o; const char *m;
  o = symname(owner); m = symname(member);
  if (o == NULL) o = "";
  if (o[0] == '$') o += 1;
  if (m != NULL && m[0] == '$') m += 1;
  fprintf(fp, "$%s:%s", o, m);
}

/* a name that may already be qualified, as $mod:name */
/* true when this wo name belongs to the module being emitted */
static bool wo_is_ours(mod_t *pm, sym_t woname)
{
  const char *w; const char *m; const char *colon;
  w = symname(woname); m = symname(pm->name);
  if (w == NULL || m == NULL) return false;
  if (w[0] == '$') w += 1;
  if (m[0] == '$') m += 1;
  colon = strchr(w, ':');
  if (colon == NULL) return false;
  return (size_t)(colon - w) == strlen(m) && strncmp(w, m, strlen(m)) == 0;
}

static void wo_qname(mod_t *pm, sym_t name, FILE *fp)
{
  const char *s;
  s = symname(name);
  if (s == NULL) { fputs("$?", fp); return; }
  if (s[0] == '$') s += 1;
  if (strchr(s, ':') != NULL) { fprintf(fp, "$%s", s); return; }
  wo_name(pm->name, name, fp);
}

static const char *wo_type(vt_t vt)
{
  switch (vt) {
    case VT_I64: return "i64";
    case VT_F32: return "f32";
    case VT_F64: return "f64";
    default: break;
  }
  return "i32";                       /* i32 and ptr alike */
}

/* the wasm spelling of an operation, where it differs */
/* what a ptr operation needs emitted before it, or NULL */
static const char *wo_op_pre(const char *op)
{
  if (strstr(op, "ptr.") != op) return NULL;
  if (streql(op + 4, "add64")) return "i32.wrap_i64";
  return NULL;
}

/* and after it */
static const char *wo_op_post(const char *op)
{
  if (strstr(op, "ptr.") != op) return NULL;
  if (streql(op + 4, "diff64")) return "i64.extend_i32_s";
  return NULL;
}

static const char *wo_op(const char *op)
{
  const char *p;
  if (streql(op, "i64.of_ptr")) return "i64.extend_i32_u";
  if (strstr(op, "ptr.") != op) return op;
  p = op + 4;
  if (streql(p, "add") || streql(p, "add64")) return "i32.add";
  if (streql(p, "sub")) return "i32.sub";
  if (streql(p, "diff") || streql(p, "diff64")) return "i32.sub";
  if (streql(p, "eqz")) return "i32.eqz";
  if (streql(p, "eq")) return "i32.eq";
  if (streql(p, "ne")) return "i32.ne";
  if (streql(p, "lt_u")) return "i32.lt_u";
  if (streql(p, "gt_u")) return "i32.gt_u";
  if (streql(p, "le_u")) return "i32.le_u";
  if (streql(p, "ge_u")) return "i32.ge_u";
  if (streql(p, "of_i64")) return "i32.wrap_i64";
  if (streql(p, "null")) return "i32.const 0";
  fprintf(stderr, "marv: %s has no wasm spelling\n", op);
  exit(1);
  return op;
}

static void wo_body(snode_t *pn, int ind, FILE *fp);
static int nonfinite(double d);

static void wo_ind(int ind, FILE *fp)
{
  int i;
  for (i = 0; i < ind; ++i) fputs("  ", fp);
}

/* the optional offset and align attributes of a load or store */
static size_t wo_memattrs(snode_t *pn, long long *poff, long long *paln)
{
  size_t i;
  *poff = 0; *paln = 0;
  i = 1;
  while (i + 1 < snlen(pn) && snref(pn, i)->nt == SN_SYM) {
    const char *k;
    k = symname(snref(pn, i)->sym);
    if (streql(k, "offset") && snref(pn, i + 1)->nt == SN_INT) *poff = snref(pn, i + 1)->ival;
    else if (streql(k, "align") && snref(pn, i + 1)->nt == SN_INT) *paln = snref(pn, i + 1)->ival;
    else break;
    i += 2;
  }
  return i;
}

/* The shortest decimal that reads back as this double, which is what the
 * reference's Scheme writer produces, with the point R7RS requires. [cco] */
static void mv_shortest(double d, cbuf_t *pcb)
{
  int p, k, i, nd;
  char buf[64];
  char dig[32];
  const char *e;
  cbclear(pcb);
  if (d != d) { cbputs("+nan.0", pcb); cbputc(0, pcb); return; }
  if (d > 1.7976931348623157e308) { cbputs("+inf.0", pcb); cbputc(0, pcb); return; }
  if (d < -1.7976931348623157e308) { cbputs("-inf.0", pcb); cbputc(0, pcb); return; }
  if (d == 0.0) {
    if (1.0 / d < 0.0) cbputc('-', pcb);
    cbputs("0.0", pcb); cbputc(0, pcb); return;
  }
  /* Sixteen significant digits, and seventeen when sixteen do not read back
   * as the same double.  Seventeen always suffice; asking for them every time
   * spells a short value long.  [cco] */
  p = 16;
  snprintf(&buf[0], sizeof(buf), "%.*e", p - 1, d);
  if (strtod(&buf[0], NULL) != d) {
    p = 17;
    snprintf(&buf[0], sizeof(buf), "%.*e", p - 1, d);
  }
  /* split %e into its digits and its exponent */
  nd = 0;
  i = 0;
  if (buf[i] == '-') { cbputc('-', pcb); i += 1; }
  for (; buf[i] != '\0' && buf[i] != 'e' && buf[i] != 'E'; ++i)
    if (buf[i] >= '0' && buf[i] <= '9' && nd < 31) dig[nd++] = buf[i];
  dig[nd] = '\0';
  e = &buf[i];
  k = (e[0] == 'e' || e[0] == 'E') ? (int)strtol(e + 1, NULL, 10) : 0;
  while (nd > 1 && dig[nd - 1] == '0') dig[--nd] = '\0';
  /* plain decimal in the range the reference writes plainly, else an
   * exponent, with the point R7RS asks for either way */
  if (k >= -4 && k <= 15) {
    if (k >= 0) {
      for (i = 0; i <= k; ++i) cbputc((i < nd) ? dig[i] : '0', pcb);
      cbputc('.', pcb);
      if (nd > k + 1) { for (i = k + 1; i < nd; ++i) cbputc(dig[i], pcb); }
      else cbputc('0', pcb);
    } else {
      cbputs("0.", pcb);
      for (i = 0; i < -k - 1; ++i) cbputc('0', pcb);
      for (i = 0; i < nd; ++i) cbputc(dig[i], pcb);
    }
  } else {
    cbputc(dig[0], pcb);
    if (nd > 1) {
      cbputc('.', pcb);
      for (i = 1; i < nd; ++i) cbputc(dig[i], pcb);
    }
    cbputc('e', pcb);
    snprintf(&buf[0], sizeof(buf), "%d", k);
    cbputs(&buf[0], pcb);
  }
  cbputc(0, pcb);
}

static void wo_float(double d, bool single, FILE *fp)
{
  char buf[48];
  int nf;
  nf = nonfinite(d);
  if (nf == 2) { fputs("+nan", fp); return; }          /* the words WAT keeps */
  if (nf == 1) { fputs((d < 0.0) ? "-inf" : "+inf", fp); return; }
  if (single) uftohex(as_uint32((float)d), &buf[0]);   /* f32 has its own shape */
  else udtohex(as_uint64(d), &buf[0]);
  fputs(&buf[0], fp);
}

/* restore the stack pointer, where this function moved it */
static void wo_unframe(int ind, FILE *fp)
{
  if (g_wo_fn == NULL || g_wo_fn->frame == 0) return;
  wo_ind(ind, fp); fputs("local.get $bp$\n", fp);
  wo_ind(ind, fp); fputs("global.set $crt:sp$\n", fp);
}

static void wo_form(snode_t *pn, int ind, FILE *fp)
{
  const char *h;
  const instr_t *pin;
  size_t i, first;
  long long off, aln;

  if (pn->nt != SN_LIST || snlen(pn) == 0 || snref(pn, 0)->nt != SN_SYM) {
    if (pn->nt == SN_SYM) {           /* a bare local */
      wo_ind(ind, fp);
      fprintf(fp, "local.get %s\n", symname(pn->sym));
      return;
    }
    sneprintf(pn, "cannot emit this form");
  }
  h = symname(snref(pn, 0)->sym);

  if (streql(h, "seq")) { wo_body(pn, ind, fp); return; }

  if (streql(h, "block") || streql(h, "loop")) {
    size_t k;
    const char *lbl;
    k = 1;
    lbl = NULL;
    if (k < snlen(pn) && snref(pn, k)->nt == SN_SYM) { lbl = symname(snref(pn, k)->sym); k += 1; }
    wo_ind(ind, fp);
    fprintf(fp, "%s%s%s", h, (lbl != NULL) ? " " : "", (lbl != NULL) ? lbl : "");
    if (k < snlen(pn) && snref(pn, k)->nt == SN_LIST && snlen(snref(pn, k)) == 2
        && streql(symname(snref(snref(pn, k), 0)->sym), "result")) {
      fprintf(fp, " (result %s)", wo_type(vt_of_sym(snref(snref(pn, k), 1)->sym)));
      k += 1;
    }
    fputs("\n", fp);
    for (; k < snlen(pn); ++k) wo_form(snref(pn, k), ind, fp);
    wo_ind(ind, fp);
    fprintf(fp, "end%s%s\n", (lbl != NULL) ? " " : "", (lbl != NULL) ? lbl : "");
    return;
  }

  if (streql(h, "if")) {
    size_t k;
    snode_t *pthen; snode_t *pelse;
    const char *rt;
    k = 1;
    rt = NULL;
    if (k < snlen(pn) && snref(pn, k)->nt == SN_LIST && snlen(snref(pn, k)) == 2
        && snref(snref(pn, k), 0)->nt == SN_SYM
        && streql(symname(snref(snref(pn, k), 0)->sym), "result")) {
      rt = wo_type(vt_of_sym(snref(snref(pn, k), 1)->sym));
      k += 1;
    }
    pthen = NULL; pelse = NULL;
    for (i = k; i < snlen(pn); ++i) {
      snode_t *pc;
      pc = snref(pn, i);
      if (pc->nt == SN_LIST && snlen(pc) > 0 && snref(pc, 0)->nt == SN_SYM) {
        const char *k2;
        k2 = symname(snref(pc, 0)->sym);
        if (streql(k2, "then")) { pthen = pc; continue; }
        if (streql(k2, "else")) { pelse = pc; continue; }
      }
      wo_form(pc, ind, fp);           /* the condition */
    }
    wo_ind(ind, fp);
    if (rt != NULL) fprintf(fp, "if (result %s)\n", rt); else fputs("if\n", fp);
    if (pthen != NULL) wo_body(pthen, ind, fp);
    if (pelse != NULL && snlen(pelse) > 1) {
      wo_ind(ind, fp); fputs("else\n", fp); wo_body(pelse, ind, fp);
    }
    wo_ind(ind, fp); fputs("end\n", fp);
    return;
  }

  pin = instr_lookup(snref(pn, 0)->sym);
  if (pin == NULL) sneprintf(pn, "unknown instruction %s", h);

  switch (pin->kind) {
    case IK_CONST:
      wo_ind(ind, fp);
      if (streql(h, "ptr.null")) { fputs("i32.const 0\n", fp); return; }
      fprintf(fp, "%s ", h);
      if (snlen(pn) > 1 && snref(pn, 1)->nt == SN_FLT)
        {
          cbuf_t db;
          wo_float(snref(pn, 1)->dval, strstr(h, "f32.") == h, fp);
          cbinit(&db);
          mv_shortest(snref(pn, 1)->dval, &db);
          fprintf(fp, " (; %s ;)", (const char *)cbdata(&db));
          cbfini(&db);
        }
      else if (snlen(pn) > 1 && snref(pn, 1)->nt == SN_INT) fprintf(fp, "%lld", snref(pn, 1)->ival);
      fputs("\n", fp);
      return;
    case IK_FRAME_ADDR: {
      long long k;
      k = (snlen(pn) > 1 && snref(pn, 1)->nt == SN_INT) ? snref(pn, 1)->ival : 0;
      wo_ind(ind, fp); fputs("local.get $fp$\n", fp);
      if (k != 0) {
        wo_ind(ind, fp); fprintf(fp, "i32.const %lld\n", k);
        wo_ind(ind, fp); fputs("i32.add\n", fp);
      }
      return;
    }
    case IK_REF_DATA: case IK_REF_FUNC: {
      long long k;
      wo_ind(ind, fp);
      fprintf(fp, "%s ", (pin->kind == IK_REF_DATA) ? "ref.data" : "ref.func");
      if (pin->kind == IK_REF_FUNC) {     /* its owner's symbol, not ours */
        sig_t *ps;
        ps = mod_sig(g_wo_mod, snref(pn, 1)->sym);
        if (ps != NULL && ps->woname != 0) fputs(symname(ps->woname), fp);
        else wo_qname(g_wo_mod, snref(pn, 1)->sym, fp);
      } else {
        sym_t dn;
        dn = mod_datawoname(g_wo_mod, snref(pn, 1)->sym);
        if (dn != 0) fputs(symname(dn), fp);
        else wo_qname(g_wo_mod, snref(pn, 1)->sym, fp);
      }
      fputs("\n", fp);
      k = (snlen(pn) > 2 && snref(pn, 2)->nt == SN_INT) ? snref(pn, 2)->ival : 0;
      if (k != 0) {
        wo_ind(ind, fp); fprintf(fp, "i32.const %lld\n", k);
        wo_ind(ind, fp); fputs("i32.add\n", fp);
      }
      return;
    }
    case IK_LOCAL_GET:
      wo_ind(ind, fp); fprintf(fp, "local.get %s\n", symname(snref(pn, 1)->sym));
      return;
    case IK_LOCAL_SET: case IK_LOCAL_TEE:
      for (i = 2; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      wo_ind(ind, fp);
      fprintf(fp, "%s %s\n", streql(h, "local.tee") ? "local.tee" : "local.set",
              symname(snref(pn, 1)->sym));
      return;
    case IK_GLOBAL_GET: {
      sym_t gn;
      gn = mod_globalwoname(g_wo_mod, snref(pn, 1)->sym);
      wo_ind(ind, fp); fputs("global.get ", fp);
      if (gn != 0) fputs(symname(gn), fp);
      else wo_qname(g_wo_mod, snref(pn, 1)->sym, fp);
      fputs("\n", fp);
    }
      return;
    case IK_GLOBAL_SET: {
      sym_t gn;
      for (i = 2; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      gn = mod_globalwoname(g_wo_mod, snref(pn, 1)->sym);
      wo_ind(ind, fp); fputs("global.set ", fp);
      if (gn != 0) fputs(symname(gn), fp);
      else wo_qname(g_wo_mod, snref(pn, 1)->sym, fp);
      fputs("\n", fp);
    }
      return;
    case IK_MEMORY: {
      /* WebAssembly has these, so they go through as themselves: a pointer is
       * an i32 here, which is what memory.copy and memory.fill take */
      for (i = 1; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      wo_ind(ind, fp);
      fprintf(fp, "%s\n", h);
      return;
    }
    case IK_LOAD: case IK_STORE: {
      const char *op;
      bool isptr;
      first = wo_memattrs(pn, &off, &aln);
      long long sz;
      isptr = (strstr(h, "ptr.") == h);
      if (pin->kind == IK_LOAD) {
        op = isptr ? "i32.load" : h;
        sz = isptr ? 4 : (long long)pin->size;
        for (i = first; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
        wo_ind(ind, fp);
        fprintf(fp, "%s offset=%lld align=%lld\n", op, off, (aln > 0) ? aln : sz);
      } else {
        op = isptr ? "i64.store" : h;
        sz = isptr ? 8 : (long long)pin->size;
        if (first < snlen(pn)) wo_form(snref(pn, first), ind, fp);        /* address */
        if (first + 1 < snlen(pn)) wo_form(snref(pn, first + 1), ind, fp); /* value */
        if (isptr) { wo_ind(ind, fp); fputs("i64.extend_i32_u\n", fp); }
        wo_ind(ind, fp);
        fprintf(fp, "%s offset=%lld align=%lld\n", op, off, (aln > 0) ? aln : sz);
      }
      return;
    }
    case IK_RETURN:
      for (i = 1; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      wo_unframe(ind, fp);
      wo_ind(ind, fp); fputs("return\n", fp);
      return;
    case IK_BR: case IK_BR_IF:
      for (i = 2; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      wo_ind(ind, fp);
      fprintf(fp, "%s %s\n", h, symname(snref(pn, 1)->sym));
      return;
    case IK_BR_TABLE: {
      size_t k;
      k = 1;
      while (k < snlen(pn) && snref(pn, k)->nt == SN_SYM) k += 1;
      for (i = k; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      wo_ind(ind, fp); fputs("br_table", fp);
      for (i = 1; i < k; ++i) fprintf(fp, " %s", symname(snref(pn, i)->sym));
      fputs("\n", fp);
      return;
    }
    case IK_CALL: case IK_RETURN_CALL: {
      sig_t *ps;
      for (i = 2; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      ps = mod_sig(g_wo_mod, snref(pn, 1)->sym);
      if (streql(h, "return_call")) wo_unframe(ind, fp);
      wo_ind(ind, fp);
      fputs((streql(h, "return_call") || streql(h, "trampoline_jump"))
            ? "return_call " : "call ", fp);
      if (ps != NULL && ps->woname != 0) fprintf(fp, "%s", symname(ps->woname));
      else wo_qname(g_wo_mod, snref(pn, 1)->sym, fp);
      fputs("\n", fp);
      return;
    }
    case IK_CALL_INDIRECT: case IK_RETURN_CALL_INDIRECT: {
      size_t k;
      int nparam;
      cbuf_t sigb;
      cbinit(&sigb);
      nparam = 0;
      k = 1;
      while (k < snlen(pn) && snref(pn, k)->nt == SN_LIST
             && snlen(snref(pn, k)) > 0 && snref(snref(pn, k), 0)->nt == SN_SYM) {
        const char *c2;
        c2 = symname(snref(snref(pn, k), 0)->sym);
        if (streql(c2, "param")) {
          size_t t;
          for (t = 1; t < snlen(snref(pn, k)); ++t) {
            snode_t *pt;
            pt = snref(snref(pn, k), t);
            if (pt->nt != SN_SYM || vt_of_sym(pt->sym) == VT_VOID) continue;
            if (nparam == 0) cbputs(" (param", &sigb);
            cbputc(' ', &sigb);
            cbputs(wo_type(vt_of_sym(pt->sym)), &sigb);
            nparam += 1;
          }
        } else if (streql(c2, "result")) {
          if (nparam > 0) { cbputs(")", &sigb); nparam = 0; }
          cbputs(" (result ", &sigb);
          cbputs(wo_type(vt_of_sym(snref(snref(pn, k), snlen(snref(pn, k)) - 1)->sym)), &sigb);
          cbputs(")", &sigb);
        } else break;
        k += 1;
      }
      if (nparam > 0) cbputs(")", &sigb);
      cbputc(0, &sigb);
      for (i = k; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      if (streql(h, "return_call_indirect")
          || streql(h, "trampoline_jump_indirect")) wo_unframe(ind, fp);
      wo_ind(ind, fp);
      fprintf(fp, "%s%s\n", (streql(h, "return_call_indirect")
              || streql(h, "trampoline_jump_indirect"))
              ? "return_call_indirect" : "call_indirect", (const char *)cbdata(&sigb));
      cbfini(&sigb);
      return;
    }
    default: {
      const char *pre; const char *post;
      for (i = 1 + (size_t)pin->nimm; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
      pre = wo_op_pre(h);
      if (pre != NULL) { wo_ind(ind, fp); fprintf(fp, "%s\n", pre); }
      wo_ind(ind, fp);
      fprintf(fp, "%s\n", wo_op(h));
      post = wo_op_post(h);
      if (post != NULL) { wo_ind(ind, fp); fprintf(fp, "%s\n", post); }
      return;
    }
  }
}

static void wo_body(snode_t *pn, int ind, FILE *fp)
{
  size_t i;
  for (i = 1; i < snlen(pn); ++i) wo_form(snref(pn, i), ind, fp);
}

/* does this form leave the function or its block for good? an (if ...) does
 * when both of its arms do, which is how a tail-recursive body ends  [cco] */
static bool wo_terminates(snode_t *pn)
{
  const char *h;
  size_t i;
  bool sawthen, sawelse, thenok, elseok;
  if (pn == NULL || pn->nt != SN_LIST || snlen(pn) == 0
      || snref(pn, 0)->nt != SN_SYM) return false;
  h = symname(snref(pn, 0)->sym);
  if (streql(h, "return") || streql(h, "br") || streql(h, "br_table")
      || streql(h, "unreachable") || streql(h, "return_call")
      || streql(h, "return_call_indirect") || streql(h, "trampoline_jump")
      || streql(h, "trampoline_jump_indirect"))
    return true;
  if (streql(h, "seq") || streql(h, "then") || streql(h, "else")) {
    if (snlen(pn) < 2) return false;
    return wo_terminates(snref(pn, snlen(pn) - 1));
  }
  if (streql(h, "if")) {
    sawthen = false; sawelse = false; thenok = false; elseok = false;
    for (i = 1; i < snlen(pn); ++i) {
      snode_t *pc;
      const char *k;
      pc = snref(pn, i);
      if (pc->nt != SN_LIST || snlen(pc) == 0 || snref(pc, 0)->nt != SN_SYM) continue;
      k = symname(snref(pc, 0)->sym);
      if (streql(k, "then")) { sawthen = true; thenok = wo_terminates(pc); }
      else if (streql(k, "else")) { sawelse = true; elseok = wo_terminates(pc); }
    }
    return sawthen && sawelse && thenok && elseok;
  }
  return false;
}

void wo_func(mod_t *pm, fn_t *pf, snode_t *body, FILE *fp)
{
  size_t i;
  g_wo_mod = pm;
  g_wo_fn = pf;
  fputs("  (func ", fp);
  wo_name(pm->name, pf->name, fp);
  if (pf->expname != 0) fprintf(fp, " (export \"%s\")", symname(pf->expname));
  fputs("\n    ", fp);
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pf->vars, i);
    fprintf(fp, "(param %s %s) ", symname(pv->name), wo_type(pv->vt));
  }
  if (pf->res != VT_VOID) fprintf(fp, "(result %s) ", wo_type(pf->res));
  fputs("\n", fp);
  if (buflen(&pf->vars) > (size_t)pf->nparams || pf->frame != 0) {
    fputs("    ", fp);
    for (i = (size_t)pf->nparams; i < buflen(&pf->vars); ++i) {
      var_t *pv;
      pv = (var_t *)bufref(&pf->vars, i);
      fprintf(fp, "(local %s %s) ", symname(pv->name), wo_type(pv->vt));
    }
    if (pf->frame != 0) fputs("(local $bp$ i32) (local $fp$ i32) ", fp);
    fputs("\n", fp);
  }
  if (pf->frame != 0) {               /* carve the frame out of the stack */
    fputs("    global.get $crt:sp$\n    local.set $bp$\n", fp);
    fputs("    global.get $crt:sp$\n", fp);
    fprintf(fp, "    i32.const %lld\n    i32.sub\n", pf->frame);
    fputs("    local.tee $fp$\n    global.set $crt:sp$\n", fp);
  }
  wo_body(body, 2, fp);
  {
    bool dead, lastif;
    snode_t *plast;
    plast = (snlen(body) > 1) ? snref(body, snlen(body) - 1) : NULL;
    dead = (plast != NULL) && wo_terminates(plast);
    lastif = (plast != NULL) && plast->nt == SN_LIST && snlen(plast) > 0
             && snref(plast, 0)->nt == SN_SYM
             && streql(symname(snref(plast, 0)->sym), "if");
    if (!dead) wo_unframe(2, fp);
    else if (pf->res != VT_VOID && lastif) fputs("    unreachable\n", fp);
  }
  fputs("  )\n", fp);
  if (pf->expname != 0 && streql(symname(pf->expname), "main")) {
    fputs("  (func ", fp);              /* the entry point WASI starts at */
    wo_name(pm->name, intern("$_start"), fp);
    fputs(" (export \"_start\")\n", fp);
    fputs("    call $crt:initialize\n    i32.const 0\n    i32.const 0\n    call ", fp);
    wo_name(pm->name, pf->name, fp);
    fputs("\n    call $crt:terminate\n  )\n", fp);
  }
  g_wo_fn = NULL;
}

/* the module header: imports first, as WebAssembly wants them */
void wo_prologue(mod_t *pm, FILE *fp)
{
  deps_line(TT_WO, fp);
  size_t i, k;
  int want;
  buf_t seen;
  g_wo_mod = pm;
  fputs("(module ", fp);
  fprintf(fp, "$%s\n", symname(pm->name) + 1);
  fputs("  (import \"crt\" \"sp$\" (global $crt:sp$ (mut i32)))\n", fp);
  fputs("  (import \"crt\" \"memory\" (memory $crt:memory 0))\n", fp);
  if (pm->hasmain) {
    fputs("  (import \"crt\" \"initialize\" (func $crt:initialize))\n", fp);
    fputs("  (import \"crt\" \"terminate\" (func $crt:terminate (param i32)))\n", fp);
  }
  bufinit(&seen, sizeof(sym_t));
  for (want = 1; want <= mod_usemax(); ++want) {
  for (i = 0; i < buflen(&pm->sigs); ++i) {
    sig_t *ps;
    bool dup;
    const char *w; const char *colon;
    ps = (sig_t *)bufref(&pm->sigs, i);
    if (ps->local || ps->woname == 0 || ps->useno != want) continue;
    if (wo_is_ours(pm, ps->woname)) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == ps->woname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = ps->woname;
    w = symname(ps->woname) + 1;
    colon = strchr(w, ':');
    if (colon == NULL) continue;
    fprintf(fp, "  (import \"%.*s\" \"%s\" (func %s", (int)(colon - w), w, colon + 1,
            symname(ps->woname));
    for (k = 0; k < buflen(&ps->ptypes); ++k)
      fprintf(fp, " (param %s)", wo_type(*(vt_t *)bufref(&ps->ptypes, k)));
    if (ps->res != VT_VOID) fprintf(fp, " (result %s)", wo_type(ps->res));
    fputs("))\n", fp);
  }
  for (i = 0; i < buflen(&pm->globals); ++i) {
    glb_t *pg;
    bool dup;
    const char *w; const char *colon;
    pg = (glb_t *)bufref(&pm->globals, i);
    if (pg->local || pg->woname == 0 || pg->useno != want) continue;
    if (wo_is_ours(pm, pg->woname)) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == pg->woname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = pg->woname;
    w = symname(pg->woname) + 1;
    colon = strchr(w, ':');
    if (colon == NULL) continue;
    fprintf(fp, "  (import \"%.*s\" \"%s\" (global %s (mut %s)))\n",
            (int)(colon - w), w, colon + 1, symname(pg->woname), wo_type(pg->vt));
  }
  for (i = 0; i < buflen(&pm->datas); ++i) {
    dat_t *pd;
    bool dup;
    const char *w; const char *colon;
    pd = (dat_t *)bufref(&pm->datas, i);
    if (pd->local || pd->woname == 0 || pd->useno != want) continue;
    if (wo_is_ours(pm, pd->woname)) continue;
    dup = false;
    for (k = 0; k < buflen(&seen); ++k)
      if (*(sym_t *)bufref(&seen, k) == pd->woname) { dup = true; break; }
    if (dup) continue;
    *(sym_t *)bufnewbk(&seen) = pd->woname;
    w = symname(pd->woname) + 1;
    colon = strchr(w, ':');
    if (colon == NULL) continue;
    fprintf(fp, "  (import \"%.*s\" \"%s\" (data %s %s align=%lld size=%lld))\n",
            (int)(colon - w), w, colon + 1, symname(pd->woname),
            pd->isconst ? "const" : "var", pd->align, pd->size);
  }
  }
  buffini(&seen);
}

void wo_epilogue(FILE *fp)
{
  fputs(")\n", fp);
}

/* a (data ...) or (global ...) member */
/* the wo name of what a data segment's pointer slot points at */
static void wo_relname(mod_t *pm, drel_t *pr, FILE *fp)
{
  if (pr->isfunc) {
    sig_t *ps;
    ps = mod_sig(pm, pr->sym);
    if (ps != NULL && ps->woname != 0) { fputs(symname(ps->woname), fp); return; }
  } else {
    sym_t dn;
    dn = mod_datawoname(pm, pr->sym);
    if (dn != 0) { fputs(symname(dn), fp); return; }
  }
  wo_qname(pm, pr->sym, fp);
}

void wo_static(mod_t *pm, snode_t *pn, FILE *fp)
{
  const char *h;
  size_t i, k;
  bool isdata, isconst;
  g_wo_mod = pm;
  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  h = symname(snref(pn, 0)->sym);
  isdata = streql(h, "data");
  if (isdata) {
    dseg_t ds;
    long long align;
    const char *exp;
    bool allzero;
    dseg_read(pn, &ds, true);
    align = ds.align;
    exp = ds.expname;
    isconst = ds.isconst;
    allzero = ds.allzero && buflen(&ds.relocs) == 0;
    if (align > 16) align = 16;
    fputs("  (data ", fp);
    wo_name(pm->name, snref(pn, 1)->sym, fp);
    if (exp != NULL) fprintf(fp, " (export \"%s\")", exp);
    fprintf(fp, " %s align=%lld ", isconst ? "const" : "var", align);
    if (allzero && buflen(&ds.bytes) > 0) {
      fprintf(fp, "size=%lu", (unsigned long)buflen(&ds.bytes));
    } else {
      fputs("\"", fp);
      for (k = 0; k < buflen(&ds.bytes); ++k) {
        unsigned char ch;
        ch = (unsigned char)*(char *)bufref(&ds.bytes, k);
        if (ch == 0x09) fputs("\\t", fp);
        else if (ch == 0x0a) fputs("\\n", fp);
        else if (ch == 0x0d) fputs("\\r", fp);
        else if (ch == 0x22) fputs("\\\"", fp);
        else if (ch == 0x27) fputs("\\'", fp);
        else if (ch == 0x5c) fputs("\\\\", fp);
        else if (ch >= 0x20 && ch < 0x7f) fputc((int)ch, fp);
        else fprintf(fp, "\\%x%x", (unsigned)(ch >> 4), (unsigned)(ch & 0xF));
      }
      fputs("\"", fp);
    }
    /* the pointer slots, as the sequence that fills them: the reference
     * writes the whole run as one list after the bytes, and so does this */
    for (i = 0; i < buflen(&ds.relocs); ++i) {
      drel_t *pr;
      pr = (drel_t *)bufref(&ds.relocs, i);
      fputs((i == 0) ? "\n    (" : "\n     ", fp);
      fputs(pr->isfunc ? "ref.func " : "ref.data ", fp);
      wo_relname(pm, pr, fp);
      if (pr->addend != 0) fprintf(fp, " offset=%lld", pr->addend);
      fprintf(fp, "\n     data.put_ref offset=%lu", (unsigned long)pr->off);
      if (i + 1 == buflen(&ds.relocs)) fputs(")", fp);
    }
    fputs(")\n", fp);
    dseg_fini(&ds);
  } else {
    vt_t vt;
    snode_t *pt;
    vt = VT_I32;
    for (i = 2; i < snlen(pn); ++i) {
      pt = snref(pn, i);
      if (pt->nt == SN_LIST && snlen(pt) == 2 && snref(pt, 0)->nt == SN_SYM
          && streql(symname(snref(pt, 0)->sym), "mut")) pt = snref(pt, 1);
      if (pt->nt == SN_SYM && vt_of_sym(pt->sym) != VT_VOID) { vt = vt_of_sym(pt->sym); break; }
    }
    {
      const char *gexp;
      size_t j;
      gexp = NULL;
      for (j = 2; j < snlen(pn); ++j) {
        snode_t *pe;
        pe = snref(pn, j);
        if (pe->nt == SN_LIST && snlen(pe) > 1 && snref(pe, 0)->nt == SN_SYM
            && streql(symname(snref(pe, 0)->sym), "export")
            && snref(pe, 1)->nt == SN_STR)
          gexp = (const char *)cbdata(&snref(pe, 1)->data);
      }
      glb_init_t gi;
      glb_read(pn, vt, &gi);
      fputs("  (global ", fp);
      wo_name(pm->name, snref(pn, 1)->sym, fp);
      if (gexp != NULL) fprintf(fp, " (export \"%s\")", gexp);
      /* the declaration's own mutability, and its own initial value */
      if (gi.ismut) fprintf(fp, " (mut %s)", wo_type(vt));
      else fprintf(fp, " %s", wo_type(vt));
      fprintf(fp, " (%s.const ", wo_type(vt));
      if (gi.isflo) wo_float(gi.dval, vt == VT_F32, fp);
      else fprintf(fp, "%lld", gi.ival);
      fputs("))\n", fp);
    }
  }
}

/* ================================================================ */
/* The rv target: RV64G, LP64D.
 *
 * The unstacker's three-address code is what this reads, and the one thing it
 * has to decide is where each vreg lives. That is a linear scan over live
 * intervals with two pools: an interval that spans a call needs a register the
 * callee must preserve, anything else prefers one it need not. t5 and t6 are
 * kept back as scratch for a reload or a materialised immediate, and ft10 and
 * ft11 likewise.
 *
 * The frame, from sp upwards:
 *
 *     0                 outgoing stack arguments
 *     frame_base        the module's own (frame N)
 *     spill_base        one slot per spilled vreg
 *     save_base         ra, then the callee-saved registers actually used
 *     total             rounded up to sixteen
 *
 * [cco] */

#define RV_GPSCRATCH  "t5"
#define RV_GPSCRATCH2 "t6"
#define RV_FPSCRATCH  "ft10"
#define RV_FPSCRATCH2 "ft11"

static const char *g_rv_callee_gp[12] = {
  "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11"
};
static const char *g_rv_callee_fp[12] = {
  "fs0", "fs1", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7", "fs8", "fs9",
  "fs10", "fs11"
};
static const char *g_rv_caller_fp[10] = {
  "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7", "ft8", "ft9"
};
static const char *g_rv_gpargs[8] = {
  "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"
};
static const char *g_rv_fpargs[8] = {
  "fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"
};

typedef struct iv {
  sym_t sym;
  vt_t vt;
  int start, end;             /* instruction indices, inclusive */
  bool crosses;               /* spans a call, so it needs a preserved register */
  const char *reg;            /* where it lives, or NULL when spilled */
  int slot;                   /* which spill slot, when reg is NULL */
  struct iv *same;            /* coalesced into this one, which owns the register */
  int ndef, nuse;             /* how often the vreg is written and read */
} iv_t;

typedef struct ra {
  buf_t ivs;                  /* iv_t, one per vreg */
  buf_t useds;                /* const char *: callee-saved GP actually used */
  buf_t usedfs;               /* const char *: callee-saved FP actually used */
  int nspills;
  bool hascalls;
} ra_t;

static bool rv_isfp(vt_t vt) { return vt == VT_F32 || vt == VT_F64; }

/* the interval a vreg's own entry was coalesced into, which is the one that
 * owns the register or the slot */
static iv_t *rv_iv(ra_t *pr, sym_t sym)
{
  size_t i;
  for (i = 0; i < buflen(&pr->ivs); ++i) {
    iv_t *p;
    p = (iv_t *)bufref(&pr->ivs, i);
    if (p->sym != sym) continue;
    while (p->same != NULL) p = p->same;
    return p;
  }
  return NULL;
}

/* a vreg's own entry, whatever it was coalesced into: what says whether the
 * vreg itself is read before it is written */
static iv_t *rv_iv_own(ra_t *pr, sym_t sym)
{
  size_t i;
  for (i = 0; i < buflen(&pr->ivs); ++i) {
    iv_t *p;
    p = (iv_t *)bufref(&pr->ivs, i);
    if (p->sym == sym) return p;
  }
  return NULL;
}

/* the vregs an instruction writes and reads */
static void rv_defs_uses(ir_t *pi, buf_t *pdefs, buf_t *puses)
{
  size_t i;
  bufclear(pdefs);
  bufclear(puses);
  if (pi->hasdst && pi->dst.kind == OK_VREG)
    *(sym_t *)bufnewbk(pdefs) = pi->dst.sym;
  for (i = 0; i < buflen(&pi->args); ++i) {
    opnd_t *po;
    po = (opnd_t *)bufref(&pi->args, i);
    if (po->kind == OK_VREG) *(sym_t *)bufnewbk(puses) = po->sym;
  }
}

/* the largest number of GP arguments this function passes or takes, which is
 * what says how many of a2..a6 are free for the scan to use */
static int rv_max_gp_arity(fn_t *pf)
{
  size_t i, k;
  int mx, n;
  mx = 0;
  for (i = 0; i < (size_t)pf->nparams; ++i)
    if (!rv_isfp(((var_t *)bufref(&pf->vars, i))->vt)) mx += 1;
  for (i = 0; i < buflen(&pf->code); ++i) {
    ir_t *pi;
    pi = (ir_t *)bufref(&pf->code, i);
    if (pi->op == IR_MEMORY) { if (mx < 3) mx = 3; continue; }
    if (pi->op != IR_CALL && pi->op != IR_CALLI
        && pi->op != IR_RETCALL && pi->op != IR_RETCALLI) continue;
    n = 0;
    for (k = 0; k < buflen(&pi->args); ++k)
      if (!rv_isfp(((opnd_t *)bufref(&pi->args, k))->vt)) n += 1;
    if (pi->op == IR_CALLI || pi->op == IR_RETCALLI) n -= 1;   /* the callee */
    if (n > mx) mx = n;
  }
  return mx;
}

static void rv_note_callee(ra_t *pr, const char *reg, bool isfp)
{
  buf_t *pb;
  size_t i;
  pb = isfp ? &pr->usedfs : &pr->useds;
  for (i = 0; i < buflen(pb); ++i)
    if (streql(*(const char **)bufref(pb, i), reg)) return;
  *(const char **)bufnewbk(pb) = reg;
}

/* one bank's scan: the intervals come in sorted by start */
static void rv_scan(ra_t *pr, buf_t *pivs, const char **caller, int ncaller,
                    const char **callee, int ncallee, bool isfp)
{
  buf_t cfree;                /* const char *: free caller-saved */
  buf_t sfree;                /* const char *: free callee-saved */
  buf_t act;                  /* iv_t *: live, with where it sits */
  buf_t actcallee;            /* bool: whether that register was callee-saved */
  size_t i, k;
  int j;
  bufinit(&cfree, sizeof(const char *));
  bufinit(&sfree, sizeof(const char *));
  bufinit(&act, sizeof(iv_t *));
  bufinit(&actcallee, sizeof(bool));
  for (j = ncaller - 1; j >= 0; --j) *(const char **)bufnewbk(&cfree) = caller[j];
  for (j = ncallee - 1; j >= 0; --j) *(const char **)bufnewbk(&sfree) = callee[j];

  for (i = 0; i < buflen(pivs); ++i) {
    iv_t *pv;
    bool got;
    pv = *(iv_t **)bufref(pivs, i);
    /* expire what has ended, returning each register to its own pool */
    k = 0;
    while (k < buflen(&act)) {
      iv_t *pa;
      pa = *(iv_t **)bufref(&act, k);
      if (pa->end < pv->start) {
        if (*(bool *)bufref(&actcallee, k)) *(const char **)bufnewbk(&sfree) = pa->reg;
        else *(const char **)bufnewbk(&cfree) = pa->reg;
        bufrem(&act, k);
        bufrem(&actcallee, k);
        continue;
      }
      k += 1;
    }
    got = false;
    if (pv->crosses) {
      if (buflen(&sfree) > 0) {
        pv->reg = *(const char **)bufref(&sfree, buflen(&sfree) - 1);
        bufpopbk(&sfree);
        *(iv_t **)bufnewbk(&act) = pv;
        *(bool *)bufnewbk(&actcallee) = true;
        rv_note_callee(pr, pv->reg, isfp);
        got = true;
      }
    } else {
      if (buflen(&cfree) > 0) {
        pv->reg = *(const char **)bufref(&cfree, buflen(&cfree) - 1);
        bufpopbk(&cfree);
        *(iv_t **)bufnewbk(&act) = pv;
        *(bool *)bufnewbk(&actcallee) = false;
        got = true;
      } else if (buflen(&sfree) > 0) {
        pv->reg = *(const char **)bufref(&sfree, buflen(&sfree) - 1);
        bufpopbk(&sfree);
        *(iv_t **)bufnewbk(&act) = pv;
        *(bool *)bufnewbk(&actcallee) = true;
        rv_note_callee(pr, pv->reg, isfp);
        got = true;
      }
    }
    if (!got) {
      /* nothing free: take the register of whatever lives longest, if that
       * outlives this interval, and spill the loser */
      size_t best;
      bool any;
      any = false; best = 0;
      for (k = 0; k < buflen(&act); ++k) {
        iv_t *pa;
        pa = *(iv_t **)bufref(&act, k);
        if (pv->crosses && !*(bool *)bufref(&actcallee, k)) continue;
        if (!any || pa->end > (*(iv_t **)bufref(&act, best))->end) { best = k; any = true; }
      }
      if (any && (*(iv_t **)bufref(&act, best))->end > pv->end) {
        iv_t *pa;
        pa = *(iv_t **)bufref(&act, best);
        pv->reg = pa->reg;
        pa->reg = NULL;
        pa->slot = pr->nspills; pr->nspills += 1;
        *(iv_t **)bufref(&act, best) = pv;
      } else {
        pv->reg = NULL;
        pv->slot = pr->nspills; pr->nspills += 1;
      }
    }
  }
  buffini(&cfree); buffini(&sfree); buffini(&act); buffini(&actcallee);
}

static void rv_sort_by_start(buf_t *pb)
{
  size_t i, j;
  for (i = 1; i < buflen(pb); ++i) {
    iv_t *pv;
    pv = *(iv_t **)bufref(pb, i);
    j = i;
    while (j > 0 && (*(iv_t **)bufref(pb, j - 1))->start > pv->start) {
      *(iv_t **)bufref(pb, j) = *(iv_t **)bufref(pb, j - 1);
      j -= 1;
    }
    *(iv_t **)bufref(pb, j) = pv;
  }
}

static void rv_alloc(fn_t *pf, ra_t *pr)
{
  buf_t defs, uses, lstart, lrange, calls, gp, fp;
  buf_t extra;                /* const char *: the spare argument registers */
  size_t i, k;
  int idx, pass, mx;
  bufinit(&pr->ivs, sizeof(iv_t));
  bufinit(&pr->useds, sizeof(const char *));
  bufinit(&pr->usedfs, sizeof(const char *));
  pr->nspills = 0;
  pr->hascalls = false;

  for (i = 0; i < buflen(&pf->vars); ++i) {
    var_t *pv;
    iv_t *piv;
    pv = (var_t *)bufref(&pf->vars, i);
    piv = (iv_t *)bufnewbk(&pr->ivs);
    piv->sym = pv->name;
    piv->vt = pv->vt;
    piv->crosses = false;
    piv->reg = NULL;
    piv->slot = -1;
    piv->same = NULL;
    piv->nuse = 0;
    /* a parameter, or a local read before it is written, is written once
     * by the entry sequence as well */
    piv->ndef = (i < (size_t)pf->nparams) ? 1 : 0;
    if (i < (size_t)pf->nparams) { piv->start = 0; piv->end = 0; }
    else { piv->start = (int)buflen(&pf->code) + 1; piv->end = -1; }
  }

  bufinit(&defs, sizeof(sym_t));
  bufinit(&uses, sizeof(sym_t));
  bufinit(&lstart, sizeof(int));
  bufinit(&lrange, sizeof(int) * 2);
  bufinit(&calls, sizeof(int));

  idx = 1;
  for (i = 0; i < buflen(&pf->code); ++i) {
    ir_t *pi;
    pi = (ir_t *)bufref(&pf->code, i);
    if (pi->op == IR_LOOPBEG) {
      *(int *)bufnewbk(&lstart) = idx;
    } else if (pi->op == IR_LOOPEND) {
      if (buflen(&lstart) > 0) {
        int *pe;
        pe = (int *)bufnewbk(&lrange);
        pe[0] = *(int *)bufref(&lstart, buflen(&lstart) - 1);
        pe[1] = idx;
        bufpopbk(&lstart);
      }
    } else if (pi->op == IR_CALL || pi->op == IR_CALLI) {
      *(int *)bufnewbk(&calls) = idx;
      pr->hascalls = true;
    } else if (pi->op == IR_RETCALL || pi->op == IR_RETCALLI) {
      pr->hascalls = true;
    } else if (pi->op == IR_MEMORY) {
      /* it becomes a call, so ra is live across it like any other */
      *(int *)bufnewbk(&calls) = idx;
      pr->hascalls = true;
    }
    rv_defs_uses(pi, &defs, &uses);
    for (k = 0; k < buflen(&defs); ++k) {
      iv_t *piv;
      piv = rv_iv_own(pr, *(sym_t *)bufref(&defs, k));
      if (piv != NULL) piv->ndef += 1;
    }
    for (k = 0; k < buflen(&uses); ++k) {
      iv_t *piv;
      piv = rv_iv_own(pr, *(sym_t *)bufref(&uses, k));
      if (piv != NULL) piv->nuse += 1;
    }
    /* a use before any definition means the value is live from entry */
    for (k = 0; k < buflen(&uses); ++k) {
      iv_t *piv;
      piv = rv_iv(pr, *(sym_t *)bufref(&uses, k));
      if (piv == NULL) continue;
      if (piv->start > (int)buflen(&pf->code)) {
        piv->start = 0;
        if (idx > piv->end) piv->end = idx;
      } else {
        if (idx < piv->start) piv->start = idx;
        if (idx > piv->end) piv->end = idx;
      }
    }
    for (k = 0; k < buflen(&defs); ++k) {
      iv_t *piv;
      piv = rv_iv(pr, *(sym_t *)bufref(&defs, k));
      if (piv == NULL) continue;
      if (idx < piv->start) piv->start = idx;
      if (idx > piv->end) piv->end = idx;
    }
    idx += 1;
  }

  /* a value live into a loop stays live to its end; three passes carry it
   * out of nested loops */
  for (pass = 0; pass < 3; ++pass) {
    for (k = 0; k < buflen(&lrange); ++k) {
      int *pe;
      pe = (int *)bufref(&lrange, k);
      for (i = 0; i < buflen(&pr->ivs); ++i) {
        iv_t *piv;
        piv = (iv_t *)bufref(&pr->ivs, i);
        if (piv->start < pe[0] && piv->end >= pe[0] && piv->end < pe[1])
          piv->end = pe[1];
      }
    }
  }

  /* A copy whose source dies at it can share the source's register, and then
   * the copy writes the register it just read and the commit leaves it out.
   *
   * The test is that the source's interval ends no later than the destination's
   * begins.  Since the copy is itself a use of the one and a definition of the
   * other, that can only hold when both happen at the copy, so a source still
   * live afterwards -- across a back edge, say, where the extension above has
   * already lengthened it -- does not qualify.  The types have to match,
   * because a coalesced pair that spills shares one slot and one width.  [cco] */
  for (i = 0; i < buflen(&pf->code); ++i) {
    ir_t *pi;
    iv_t *pa;
    iv_t *pb;
    opnd_t *ps;
    pi = (ir_t *)bufref(&pf->code, i);
    if (pi->op != IR_MOV || !pi->hasdst || pi->dst.kind != OK_VREG) continue;
    if (buflen(&pi->args) != 1) continue;
    ps = (opnd_t *)bufref(&pi->args, 0);
    if (ps->kind != OK_VREG) continue;
    pa = rv_iv(pr, ps->sym);
    pb = rv_iv(pr, pi->dst.sym);
    if (pa == NULL || pb == NULL || pa == pb) continue;
    if (pa->vt != pb->vt) continue;
    if (pa->end > pb->start) continue;
    if (pb->end > pa->end) pa->end = pb->end;
    if (pb->start < pa->start) pa->start = pb->start;
    pb->same = pa;
  }

  for (i = 0; i < buflen(&pr->ivs); ++i) {
    iv_t *piv;
    piv = (iv_t *)bufref(&pr->ivs, i);
    if (piv->same != NULL) continue;
    for (k = 0; k < buflen(&calls); ++k) {
      int at;
      at = *(int *)bufref(&calls, k);
      if (piv->start < at && piv->end > at) { piv->crosses = true; break; }
    }
  }

  bufinit(&gp, sizeof(iv_t *));
  bufinit(&fp, sizeof(iv_t *));
  for (i = 0; i < buflen(&pr->ivs); ++i) {
    iv_t *piv;
    piv = (iv_t *)bufref(&pr->ivs, i);
    if (piv->same != NULL) continue;                 /* not its own register */
    if (piv->end < piv->start) continue;             /* never live */
    if (rv_isfp(piv->vt)) *(iv_t **)bufnewbk(&fp) = piv;
    else *(iv_t **)bufnewbk(&gp) = piv;
  }
  rv_sort_by_start(&gp);
  rv_sort_by_start(&fp);

  /* t0..t4, and whatever of a2..a6 sits above this function's argument use */
  bufinit(&extra, sizeof(const char *));
  *(const char **)bufnewbk(&extra) = "t0";
  *(const char **)bufnewbk(&extra) = "t1";
  *(const char **)bufnewbk(&extra) = "t2";
  *(const char **)bufnewbk(&extra) = "t3";
  *(const char **)bufnewbk(&extra) = "t4";
  mx = rv_max_gp_arity(pf);
  if (mx < 2) mx = 2;
  for (idx = mx; idx <= 6; ++idx)
    *(const char **)bufnewbk(&extra) = g_rv_gpargs[idx];
  rv_scan(pr, &gp, (const char **)bufdata(&extra), (int)buflen(&extra),
          &g_rv_callee_gp[0], 12, false);
  rv_scan(pr, &fp, &g_rv_caller_fp[0], 10, &g_rv_callee_fp[0], 12, true);

  /* a vreg that is never live still needs somewhere to be written */
  for (i = 0; i < buflen(&pr->ivs); ++i) {
    iv_t *piv;
    piv = (iv_t *)bufref(&pr->ivs, i);
    if (piv->same != NULL) continue;
    if (piv->reg == NULL && piv->slot < 0)
      piv->reg = rv_isfp(piv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH;
  }

  buffini(&defs); buffini(&uses); buffini(&lstart); buffini(&lrange);
  buffini(&calls); buffini(&gp); buffini(&fp); buffini(&extra);
}

static void rv_fini(ra_t *pr)
{
  buffini(&pr->ivs); buffini(&pr->useds); buffini(&pr->usedfs);
}

/* Base RV64G has no bit-counting instruction, so each is a short loop.
 * a7 holds the value being consumed and t6 a scratch bit. */
static int g_rv_lbl;

static void rv_bitcount(const char *op, const char *d, const char *s, FILE *fp)
{
  int n;
  bool is32;
  n = g_rv_lbl; g_rv_lbl += 1;
  is32 = (op[1] == '3');
  if (streql(op + 4, "popcnt")) {
    if (is32) {
      fprintf(fp, "  slli a7, %s, 32\n", s);
      fprintf(fp, "  srli a7, a7, 32\n");
    } else fprintf(fp, "  mv a7, %s\n", s);
    fprintf(fp, "  li %s, 0\n", d);
    fprintf(fp, "  beqz a7, .Lpop_dn%d\n", n);
    fprintf(fp, ".Lpop_lp%d:\n", n);
    fprintf(fp, "  addi t6, a7, -1\n");
    fprintf(fp, "  and a7, a7, t6\n");
    fprintf(fp, "  addiw %s, %s, 1\n", d, d);
    fprintf(fp, "  bnez a7, .Lpop_lp%d\n", n);
    fprintf(fp, ".Lpop_dn%d:\n", n);
    return;
  }
  if (streql(op + 4, "clz")) {
    if (is32) fprintf(fp, "  slli a7, %s, 32\n", s);
    else fprintf(fp, "  mv a7, %s\n", s);
    fprintf(fp, "  li %s, %d\n", d, is32 ? 32 : 64);
    fprintf(fp, "  beqz a7, .Lclz_dn%d\n", n);
    fprintf(fp, "  li %s, 0\n", d);
    fprintf(fp, ".Lclz_lp%d:\n", n);
    fprintf(fp, "  bltz a7, .Lclz_dn%d\n", n);
    fprintf(fp, "  slli a7, a7, 1\n");
    fprintf(fp, "  addiw %s, %s, 1\n", d, d);
    fprintf(fp, "  j .Lclz_lp%d\n", n);
    fprintf(fp, ".Lclz_dn%d:\n", n);
    return;
  }
  if (is32) {
    fprintf(fp, "  slli a7, %s, 32\n", s);
    fprintf(fp, "  srli a7, a7, 32\n");
  } else fprintf(fp, "  mv a7, %s\n", s);
  fprintf(fp, "  li %s, %d\n", d, is32 ? 32 : 64);
  fprintf(fp, "  beqz a7, .Lctz_dn%d\n", n);
  fprintf(fp, "  li %s, 0\n", d);
  fprintf(fp, ".Lctz_lp%d:\n", n);
  fprintf(fp, "  andi t6, a7, 1\n");
  fprintf(fp, "  bnez t6, .Lctz_dn%d\n", n);
  fprintf(fp, "  srli a7, a7, 1\n");
  fprintf(fp, "  addiw %s, %s, 1\n", d, d);
  fprintf(fp, "  j .Lctz_lp%d\n", n);
  fprintf(fp, ".Lctz_dn%d:\n", n);
}

/* Rounding goes out to an integer in the asked-for mode and back, and the
 * sign is put back from the source so that a negative value rounding to zero
 * keeps its sign. */
static void rv_round(const char *op, const char *d, const char *s, FILE *fp)
{
  const char *mode;
  bool is32;
  is32 = (op[1] == '3');
  mode = "rtz";
  if (streql(op + 4, "floor")) mode = "rdn";
  else if (streql(op + 4, "ceil")) mode = "rup";
  else if (streql(op + 4, "nearest")) mode = "rne";
  if (is32) {
    fprintf(fp, "  fcvt.w.s a7, %s, %s\n", s, mode);
    fprintf(fp, "  fcvt.s.w %s, a7\n", d);
    fprintf(fp, "  fsgnj.s %s, %s, %s\n", d, d, s);
  } else {
    fprintf(fp, "  fcvt.l.d a7, %s, %s\n", s, mode);
    fprintf(fp, "  fcvt.d.l %s, a7\n", d);
    fprintf(fp, "  fsgnj.d %s, %s, %s\n", d, d, s);
  }
}

/* ---------------------------------------------------------------- */
/* Where a function's values sit, and the code that moves them.     */

static mod_t *g_rv_mod;
static fn_t *g_rv_fn;
static ra_t *g_rv_ra;
static long long g_rv_framebase;
static long long g_rv_spillbase;
static long long g_rv_savebase;
static long long g_rv_total;

static bool rv_simm12(long long n) { return n >= -2048 && n <= 2047; }

static long long rv_spill_off(int slot)
{
  return g_rv_spillbase + (long long)slot * 8;
}

/* the load and store that carry one value type to and from a stack slot */
static const char *rv_ldmnem(vt_t vt)
{
  if (vt == VT_I32) return "lw";
  if (vt == VT_F32) return "flw";
  if (vt == VT_F64) return "fld";
  return "ld";
}

static const char *rv_stmnem(vt_t vt)
{
  if (vt == VT_I32) return "sw";
  if (vt == VT_F32) return "fsw";
  if (vt == VT_F64) return "fsd";
  return "sd";
}

/* an address that is sp plus a constant */
static void rv_sp_addr(const char *dst, long long off, FILE *fp)
{
  if (rv_simm12(off)) {
    fprintf(fp, "  addi %s, sp, %lld\n", dst, off);
  } else {
    fprintf(fp, "  li %s, %lld\n", dst, off);
    fprintf(fp, "  add %s, sp, %s\n", dst, dst);
  }
}

/* put an operand where it can be used, and say which register that is */
static const char *rv_opnd(opnd_t *po, const char *scratch, FILE *fp)
{
  switch (po->kind) {
    case OK_VREG: {
      iv_t *piv;
      piv = rv_iv(g_rv_ra, po->sym);
      if (piv == NULL) return scratch;
      if (piv->reg != NULL) return piv->reg;
      fprintf(fp, "  %s %s, %lld(sp)\n", rv_ldmnem(piv->vt), scratch,
              rv_spill_off(piv->slot));
      return scratch;
    }
    case OK_IMM:
      if (po->vt == VT_F32) {
        unsigned int b;
        b = as_uint32((float)po->dval);
        if (b == 0) fprintf(fp, "  fmv.w.x %s, zero\n", scratch);
        else {
          fprintf(fp, "  li %s, %ld\n", RV_GPSCRATCH2, (long)(int)b);
          fprintf(fp, "  fmv.w.x %s, %s\n", scratch, RV_GPSCRATCH2);
        }
        return scratch;
      }
      if (po->vt == VT_F64) {
        unsigned long long b;
        b = as_uint64(po->dval);
        if (b == 0) fprintf(fp, "  fmv.d.x %s, zero\n", scratch);
        else {
          fprintf(fp, "  li %s, %lld\n", RV_GPSCRATCH2, (long long)b);
          fprintf(fp, "  fmv.d.x %s, %s\n", scratch, RV_GPSCRATCH2);
        }
        return scratch;
      }
      if (po->ival == 0) return "zero";
      fprintf(fp, "  li %s, %lld\n", scratch, po->ival);
      return scratch;
    case OK_FRAME: {
      long long off;
      off = g_rv_framebase + po->ival;
      if (off == 0) return "sp";
      rv_sp_addr(scratch, off, fp);
      return scratch;
    }
    case OK_DATA: {
      sym_t dn;
      dn = mod_dataname(g_rv_mod, po->sym);
      fprintf(fp, "  la %s, %s", scratch,
              (dn != 0) ? symname(dn) : symname(po->sym));
      if (po->ival != 0) fprintf(fp, " + %lld", po->ival);
      fputs("\n", fp);
      return scratch;
    }
    case OK_FUNC: {
      sig_t *ps;
      ps = mod_sig(g_rv_mod, po->sym);
      fprintf(fp, "  la %s, %s\n", scratch,
              (ps != NULL && ps->cname != 0) ? symname(ps->cname) : symname(po->sym));
      return scratch;
    }
    default: break;
  }
  return scratch;
}

/* write a computed value to where its vreg lives */
static void rv_commit(opnd_t *pd, const char *from, FILE *fp)
{
  iv_t *piv;
  if (pd->kind != OK_VREG) return;
  piv = rv_iv(g_rv_ra, pd->sym);
  if (piv == NULL) return;
  if (piv->reg != NULL) {
    if (streql(piv->reg, from)) return;
    if (piv->vt == VT_F32) fprintf(fp, "  fmv.s %s, %s\n", piv->reg, from);
    else if (piv->vt == VT_F64) fprintf(fp, "  fmv.d %s, %s\n", piv->reg, from);
    else fprintf(fp, "  mv %s, %s\n", piv->reg, from);
    return;
  }
  fprintf(fp, "  %s %s, %lld(sp)\n", rv_stmnem(piv->vt), from,
          rv_spill_off(piv->slot));
}

/* the register a destination wants its value in before the commit */
static const char *rv_dstreg(opnd_t *pd)
{
  iv_t *piv;
  if (pd->kind == OK_VREG) {
    piv = rv_iv(g_rv_ra, pd->sym);
    if (piv != NULL && piv->reg != NULL) return piv->reg;
    if (piv != NULL) return rv_isfp(piv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH;
  }
  return rv_isfp(pd->vt) ? RV_FPSCRATCH : RV_GPSCRATCH;
}

/* ---------------------------------------------------------------- */
/* Instruction selection.                                           */

static bool rv_is(const char *op, const char *name) { return streql(op, name); }

/* a two-operand operation; some need more than one instruction */
static void rv_bin(const char *op, const char *d, const char *l,
                   const char *r, FILE *fp)
{
  const char *m;
  m = NULL;
  /* 32-bit arithmetic keeps its result sign extended, which is what the
   * w-suffixed forms do */
  if (rv_is(op, "i32.add")) m = "addw";
  else if (rv_is(op, "i32.sub")) m = "subw";
  else if (rv_is(op, "i32.mul")) m = "mulw";
  else if (rv_is(op, "i32.div_s")) m = "divw";
  else if (rv_is(op, "i32.div_u")) m = "divuw";
  else if (rv_is(op, "i32.rem_s")) m = "remw";
  else if (rv_is(op, "i32.rem_u")) m = "remuw";
  else if (rv_is(op, "i32.shl")) m = "sllw";
  else if (rv_is(op, "i32.shr_s")) m = "sraw";
  else if (rv_is(op, "i32.shr_u")) m = "srlw";
  else if (rv_is(op, "i32.and") || rv_is(op, "i64.and")) m = "and";
  else if (rv_is(op, "i32.or") || rv_is(op, "i64.or")) m = "or";
  else if (rv_is(op, "i32.xor") || rv_is(op, "i64.xor")) m = "xor";
  else if (rv_is(op, "i64.add") || rv_is(op, "ptr.add")
           || rv_is(op, "ptr.add64")) m = "add";
  else if (rv_is(op, "i64.sub") || rv_is(op, "ptr.sub")
           || rv_is(op, "ptr.diff64")) m = "sub";
  else if (rv_is(op, "ptr.diff")) m = "subw";
  else if (rv_is(op, "i64.mul")) m = "mul";
  else if (rv_is(op, "i64.div_s")) m = "div";
  else if (rv_is(op, "i64.div_u")) m = "divu";
  else if (rv_is(op, "i64.rem_s")) m = "rem";
  else if (rv_is(op, "i64.rem_u")) m = "remu";
  else if (rv_is(op, "i64.shl")) m = "sll";
  else if (rv_is(op, "i64.shr_s")) m = "sra";
  else if (rv_is(op, "i64.shr_u")) m = "srl";
  else if (rv_is(op, "f32.add")) m = "fadd.s";
  else if (rv_is(op, "f32.sub")) m = "fsub.s";
  else if (rv_is(op, "f32.mul")) m = "fmul.s";
  else if (rv_is(op, "f32.div")) m = "fdiv.s";
  else if (rv_is(op, "f32.min")) m = "fmin.s";
  else if (rv_is(op, "f32.max")) m = "fmax.s";
  else if (rv_is(op, "f32.copysign")) m = "fsgnj.s";
  else if (rv_is(op, "f32.eq")) m = "feq.s";
  else if (rv_is(op, "f64.add")) m = "fadd.d";
  else if (rv_is(op, "f64.sub")) m = "fsub.d";
  else if (rv_is(op, "f64.mul")) m = "fmul.d";
  else if (rv_is(op, "f64.div")) m = "fdiv.d";
  else if (rv_is(op, "f64.min")) m = "fmin.d";
  else if (rv_is(op, "f64.max")) m = "fmax.d";
  else if (rv_is(op, "f64.copysign")) m = "fsgnj.d";
  else if (rv_is(op, "f64.eq")) m = "feq.d";
  if (m != NULL) {
    fprintf(fp, "  %s %s, %s, %s\n", m, d, l, r);
    return;
  }
  /* the ones that take a sequence */
  if (rv_is(op, "i32.rotl") || rv_is(op, "i32.rotr")
      || rv_is(op, "i64.rotl") || rv_is(op, "i64.rotr")) {
    bool is32, isleft;
    const char *sl; const char *sr;
    is32 = (op[1] == '3');
    isleft = streql(op + 4, "rotl");
    sl = is32 ? "sllw" : "sll";
    sr = is32 ? "srlw" : "srl";
    fprintf(fp, "  %s a7, zero, %s\n", is32 ? "subw" : "sub", r);
    fprintf(fp, "  %s a7, %s, a7\n", isleft ? sr : sl, l);
    fprintf(fp, "  %s %s, %s, %s\n", isleft ? sl : sr, d, l, r);
    fprintf(fp, "  or %s, %s, a7\n", d, d);
    return;
  }
  if (rv_is(op, "i32.eq") || rv_is(op, "i64.eq") || rv_is(op, "ptr.eq")) {
    fprintf(fp, "  sub %s, %s, %s\n", d, l, r);
    fprintf(fp, "  seqz %s, %s\n", d, d);
    return;
  }
  if (rv_is(op, "i32.ne") || rv_is(op, "i64.ne") || rv_is(op, "ptr.ne")) {
    fprintf(fp, "  sub %s, %s, %s\n", d, l, r);
    fprintf(fp, "  snez %s, %s\n", d, d);
    return;
  }
  if (rv_is(op, "i32.lt_s") || rv_is(op, "i64.lt_s")) {
    fprintf(fp, "  slt %s, %s, %s\n", d, l, r); return;
  }
  if (rv_is(op, "i32.lt_u") || rv_is(op, "i64.lt_u") || rv_is(op, "ptr.lt_u")) {
    fprintf(fp, "  sltu %s, %s, %s\n", d, l, r); return;
  }
  if (rv_is(op, "i32.gt_s") || rv_is(op, "i64.gt_s")) {
    fprintf(fp, "  slt %s, %s, %s\n", d, r, l); return;
  }
  if (rv_is(op, "i32.gt_u") || rv_is(op, "i64.gt_u") || rv_is(op, "ptr.gt_u")) {
    fprintf(fp, "  sltu %s, %s, %s\n", d, r, l); return;
  }
  if (rv_is(op, "i32.le_s") || rv_is(op, "i64.le_s")) {
    fprintf(fp, "  slt %s, %s, %s\n", d, r, l);
    fprintf(fp, "  xori %s, %s, 1\n", d, d); return;
  }
  if (rv_is(op, "i32.le_u") || rv_is(op, "i64.le_u") || rv_is(op, "ptr.le_u")) {
    fprintf(fp, "  sltu %s, %s, %s\n", d, r, l);
    fprintf(fp, "  xori %s, %s, 1\n", d, d); return;
  }
  if (rv_is(op, "i32.ge_s") || rv_is(op, "i64.ge_s")) {
    fprintf(fp, "  slt %s, %s, %s\n", d, l, r);
    fprintf(fp, "  xori %s, %s, 1\n", d, d); return;
  }
  if (rv_is(op, "i32.ge_u") || rv_is(op, "i64.ge_u") || rv_is(op, "ptr.ge_u")) {
    fprintf(fp, "  sltu %s, %s, %s\n", d, l, r);
    fprintf(fp, "  xori %s, %s, 1\n", d, d); return;
  }
  if (rv_is(op, "f32.ne") || rv_is(op, "f64.ne")) {
    fprintf(fp, "  feq.%c %s, %s, %s\n", (op[1] == '3') ? 's' : 'd', d, l, r);
    fprintf(fp, "  xori %s, %s, 1\n", d, d); return;
  }
  if (rv_is(op, "f32.lt") || rv_is(op, "f64.lt")) {
    fprintf(fp, "  flt.%c %s, %s, %s\n", (op[1] == '3') ? 's' : 'd', d, l, r); return;
  }
  if (rv_is(op, "f32.gt") || rv_is(op, "f64.gt")) {
    fprintf(fp, "  flt.%c %s, %s, %s\n", (op[1] == '3') ? 's' : 'd', d, r, l); return;
  }
  if (rv_is(op, "f32.le") || rv_is(op, "f64.le")) {
    fprintf(fp, "  fle.%c %s, %s, %s\n", (op[1] == '3') ? 's' : 'd', d, l, r); return;
  }
  if (rv_is(op, "f32.ge") || rv_is(op, "f64.ge")) {
    fprintf(fp, "  fle.%c %s, %s, %s\n", (op[1] == '3') ? 's' : 'd', d, r, l); return;
  }
  fprintf(stderr, "marv: the rv target cannot emit %s yet\n", op);
  exit(1);
}

/* a one-operand operation */
/* A constant right operand, in the instruction rather than in a register.
 * RV64I has addi, andi, ori, xori, the shift-by-constant forms and slti and
 * sltiu; a comparison against a constant is a subtraction of it.  Answers
 * false where there is no immediate form or the constant does not fit, and
 * the caller then materialises it as before.  [cco] */
static bool rv_binimm(const char *op, const char *d, const char *l,
                      opnd_t *pb, FILE *fp)
{
  long long v;
  const char *m;
  if (pb->kind != OK_IMM) return false;
  if (pb->vt == VT_F32 || pb->vt == VT_F64) return false;
  v = pb->ival;

  /* the shifts take a count, not a signed immediate, and the register forms
   * read only the low bits of it: the constant has to be reduced the same way */
  m = NULL;
  if (rv_is(op, "i64.shl")) m = "slli";
  else if (rv_is(op, "i64.shr_s")) m = "srai";
  else if (rv_is(op, "i64.shr_u")) m = "srli";
  if (m != NULL) {
    fprintf(fp, "  %s %s, %s, %lu\n", m, d, l,
            (unsigned long)((unsigned long long)v & 63));
    return true;
  }
  if (rv_is(op, "i32.shl")) m = "slliw";
  else if (rv_is(op, "i32.shr_s")) m = "sraiw";
  else if (rv_is(op, "i32.shr_u")) m = "srliw";
  if (m != NULL) {
    fprintf(fp, "  %s %s, %s, %lu\n", m, d, l,
            (unsigned long)((unsigned long long)v & 31));
    return true;
  }

  if (rv_simm12(v)) {
    m = NULL;
    if (rv_is(op, "i64.add") || rv_is(op, "ptr.add") || rv_is(op, "ptr.add64"))
      m = "addi";
    else if (rv_is(op, "i32.add")) m = "addiw";
    else if (rv_is(op, "i32.and") || rv_is(op, "i64.and")) m = "andi";
    else if (rv_is(op, "i32.or") || rv_is(op, "i64.or")) m = "ori";
    else if (rv_is(op, "i32.xor") || rv_is(op, "i64.xor")) m = "xori";
    if (m != NULL) {
      fprintf(fp, "  %s %s, %s, %lld\n", m, d, l, v);
      return true;
    }
    if (rv_is(op, "i32.lt_s") || rv_is(op, "i64.lt_s")) {
      fprintf(fp, "  slti %s, %s, %lld\n", d, l, v);
      return true;
    }
    if (rv_is(op, "i32.lt_u") || rv_is(op, "i64.lt_u")
        || rv_is(op, "ptr.lt_u")) {
      fprintf(fp, "  sltiu %s, %s, %lld\n", d, l, v);
      return true;
    }
    if (rv_is(op, "i32.ge_s") || rv_is(op, "i64.ge_s")) {
      fprintf(fp, "  slti %s, %s, %lld\n", d, l, v);
      fprintf(fp, "  xori %s, %s, 1\n", d, d);
      return true;
    }
    if (rv_is(op, "i32.ge_u") || rv_is(op, "i64.ge_u")
        || rv_is(op, "ptr.ge_u")) {
      fprintf(fp, "  sltiu %s, %s, %lld\n", d, l, v);
      fprintf(fp, "  xori %s, %s, 1\n", d, d);
      return true;
    }
  }

  /* a subtraction of v is an addition of -v, which fits over its own range */
  if (v >= -2047 && v <= 2048) {
    m = NULL;
    if (rv_is(op, "i64.sub") || rv_is(op, "ptr.sub") || rv_is(op, "ptr.diff64"))
      m = "addi";
    else if (rv_is(op, "i32.sub") || rv_is(op, "ptr.diff")) m = "addiw";
    if (m != NULL) {
      fprintf(fp, "  %s %s, %s, %lld\n", m, d, l, -v);
      return true;
    }
    /* equality against a constant: subtract it and ask about the result */
    if (rv_is(op, "i32.eq") || rv_is(op, "i64.eq") || rv_is(op, "ptr.eq")) {
      fprintf(fp, "  addi %s, %s, %lld\n", d, l, -v);
      fprintf(fp, "  seqz %s, %s\n", d, d);
      return true;
    }
    if (rv_is(op, "i32.ne") || rv_is(op, "i64.ne") || rv_is(op, "ptr.ne")) {
      fprintf(fp, "  addi %s, %s, %lld\n", d, l, -v);
      fprintf(fp, "  snez %s, %s\n", d, d);
      return true;
    }
  }
  return false;
}

static void rv_un(const char *op, const char *d, const char *s, FILE *fp)
{
  const char *m;
  m = NULL;
  if (rv_is(op, "i32.eqz") || rv_is(op, "i64.eqz") || rv_is(op, "ptr.eqz")) m = "seqz";
  else if (rv_is(op, "i32.extend8_s") || rv_is(op, "i64.extend8_s")) m = NULL;
  else if (rv_is(op, "f32.abs")) m = "fsgnjx.s";
  else if (rv_is(op, "f32.neg")) m = "fsgnjn.s";
  else if (rv_is(op, "f32.sqrt")) m = "fsqrt.s";
  else if (rv_is(op, "f64.abs")) m = "fsgnjx.d";
  else if (rv_is(op, "f64.neg")) m = "fsgnjn.d";
  else if (rv_is(op, "f64.sqrt")) m = "fsqrt.d";
  if (m != NULL) {
    if (m[0] == 'f' && m[1] == 's' && m[2] == 'g')
      fprintf(fp, "  %s %s, %s, %s\n", m, d, s, s);   /* sign from itself */
    else fprintf(fp, "  %s %s, %s\n", m, d, s);
    return;
  }
  if (rv_is(op, "i32.extend8_s") || rv_is(op, "i64.extend8_s")) {
    fprintf(fp, "  slli %s, %s, 56\n", d, s);
    fprintf(fp, "  srai %s, %s, 56\n", d, d);
    return;
  }
  if (rv_is(op, "i32.extend16_s") || rv_is(op, "i64.extend16_s")) {
    fprintf(fp, "  slli %s, %s, 48\n", d, s);
    fprintf(fp, "  srai %s, %s, 48\n", d, d);
    return;
  }
  if (rv_is(op, "i64.extend32_s") || rv_is(op, "i64.extend_i32_s")
      || rv_is(op, "i32.wrap_i64")) {
    fprintf(fp, "  addiw %s, %s, 0\n", d, s);
    return;
  }
  if (rv_is(op, "i64.extend_i32_u")) {
    fprintf(fp, "  slli %s, %s, 32\n", d, s);
    fprintf(fp, "  srli %s, %s, 32\n", d, d);
    return;
  }
  if (rv_is(op, "ptr.of_i64") || rv_is(op, "i64.of_ptr")) {
    if (!streql(d, s)) fprintf(fp, "  mv %s, %s\n", d, s);
    return;
  }
  if (rv_is(op, "i32.clz") || rv_is(op, "i64.clz")
      || rv_is(op, "i32.ctz") || rv_is(op, "i64.ctz")
      || rv_is(op, "i32.popcnt") || rv_is(op, "i64.popcnt")) {
    rv_bitcount(op, d, s, fp);
    return;
  }
  if (rv_is(op, "f32.demote_f64")) { fprintf(fp, "  fcvt.s.d %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.promote_f32")) { fprintf(fp, "  fcvt.d.s %s, %s\n", d, s); return; }
  if (rv_is(op, "i32.trunc_f32_s")) { fprintf(fp, "  fcvt.w.s %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i32.trunc_f32_u")) { fprintf(fp, "  fcvt.wu.s %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i32.trunc_f64_s")) { fprintf(fp, "  fcvt.w.d %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i32.trunc_f64_u")) { fprintf(fp, "  fcvt.wu.d %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i64.trunc_f32_s")) { fprintf(fp, "  fcvt.l.s %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i64.trunc_f32_u")) { fprintf(fp, "  fcvt.lu.s %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i64.trunc_f64_s")) { fprintf(fp, "  fcvt.l.d %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "i64.trunc_f64_u")) { fprintf(fp, "  fcvt.lu.d %s, %s, rtz\n", d, s); return; }
  if (rv_is(op, "f32.convert_i32_s")) { fprintf(fp, "  fcvt.s.w %s, %s\n", d, s); return; }
  if (rv_is(op, "f32.convert_i32_u")) { fprintf(fp, "  fcvt.s.wu %s, %s\n", d, s); return; }
  if (rv_is(op, "f32.convert_i64_s")) { fprintf(fp, "  fcvt.s.l %s, %s\n", d, s); return; }
  if (rv_is(op, "f32.convert_i64_u")) { fprintf(fp, "  fcvt.s.lu %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.convert_i32_s")) { fprintf(fp, "  fcvt.d.w %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.convert_i32_u")) { fprintf(fp, "  fcvt.d.wu %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.convert_i64_s")) { fprintf(fp, "  fcvt.d.l %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.convert_i64_u")) { fprintf(fp, "  fcvt.d.lu %s, %s\n", d, s); return; }
  if (rv_is(op, "i32.reinterpret_f32")) { fprintf(fp, "  fmv.x.w %s, %s\n", d, s); return; }
  if (rv_is(op, "f32.reinterpret_i32")) { fprintf(fp, "  fmv.w.x %s, %s\n", d, s); return; }
  if (rv_is(op, "i64.reinterpret_f64")) { fprintf(fp, "  fmv.x.d %s, %s\n", d, s); return; }
  if (rv_is(op, "f64.reinterpret_i64")) { fprintf(fp, "  fmv.d.x %s, %s\n", d, s); return; }
  if (rv_is(op, "f32.floor") || rv_is(op, "f32.ceil") || rv_is(op, "f32.trunc")
      || rv_is(op, "f32.nearest") || rv_is(op, "f64.floor")
      || rv_is(op, "f64.ceil") || rv_is(op, "f64.trunc")
      || rv_is(op, "f64.nearest")) {
    rv_round(op, d, s, fp);
    return;
  }
  fprintf(stderr, "marv: the rv target cannot emit %s yet\n", op);
  exit(1);
}

static void rv_args(ir_t *pi, size_t first, size_t last, FILE *fp);
static void rv_teardown(FILE *fp);
static void rv_ins(ir_t *pi, FILE *fp);

/* A temporary whose only use is the copy right after it does not need a
 * register of its own: the instruction can write where the copy would have
 * put the value, and the copy goes away.  g_rv_fuse is that copy's
 * destination while the instruction before it is emitted, and
 * g_rv_fused says whether the emitter took it -- it declines where the
 * destination's register is one of its own operands, because a few of the
 * sequences read an operand after writing the result.  [cco] */
static opnd_t *g_rv_fuse;
static bool g_rv_fused;

static opnd_t *rv_fusable(fn_t *pf, size_t i)
{
  ir_t *pi;
  ir_t *pn;
  iv_t *piv;
  opnd_t *ps;
  pi = (ir_t *)bufref(&pf->code, i);
  if (!pi->hasdst || pi->dst.kind != OK_VREG) return NULL;
  if (i + 1 >= buflen(&pf->code)) return NULL;
  pn = (ir_t *)bufref(&pf->code, i + 1);
  if (pn->op != IR_MOV || !pn->hasdst || pn->dst.kind != OK_VREG) return NULL;
  if (buflen(&pn->args) != 1) return NULL;
  ps = (opnd_t *)bufref(&pn->args, 0);
  if (ps->kind != OK_VREG || ps->sym != pi->dst.sym) return NULL;
  if (pn->dst.sym == pi->dst.sym) return NULL;
  piv = rv_iv_own(g_rv_ra, pi->dst.sym);
  if (piv == NULL || piv->ndef != 1 || piv->nuse != 1) return NULL;
  if (piv->vt != pn->dst.vt) return NULL;
  return &pn->dst;
}

static const char *rv_ldop(const char *op)
{
  if (streql(op, "i32.load") || streql(op, "i64.load32_s")) return "lw";
  if (streql(op, "i64.load32_u")) return "lwu";
  if (streql(op, "i32.load8_s") || streql(op, "i64.load8_s")) return "lb";
  if (streql(op, "i32.load8_u") || streql(op, "i64.load8_u")) return "lbu";
  if (streql(op, "i32.load16_s") || streql(op, "i64.load16_s")) return "lh";
  if (streql(op, "i32.load16_u") || streql(op, "i64.load16_u")) return "lhu";
  if (streql(op, "f32.load")) return "flw";
  if (streql(op, "f64.load")) return "fld";
  return "ld";
}

static const char *rv_stop(const char *op)
{
  if (streql(op, "i32.store") || streql(op, "i64.store32")) return "sw";
  if (streql(op, "i32.store8") || streql(op, "i64.store8")) return "sb";
  if (streql(op, "i32.store16") || streql(op, "i64.store16")) return "sh";
  if (streql(op, "f32.store")) return "fsw";
  if (streql(op, "f64.store")) return "fsd";
  return "sd";
}

/* a label, as this function spells it */
static void rv_label(sym_t name, FILE *fp)
{
  const char *s;
  s = symname(name);
  if (s == NULL) s = "?";
  if (s[0] == '$') s += 1;
  fprintf(fp, ".L%s", s);
}

static void rv_ins(ir_t *pi, FILE *fp)
{
  const char *op;
  const char *d;
  const char *l;
  const char *r;
  op = (pi->name != 0) ? symname(pi->name) : "";
  switch (pi->op) {
    case IR_LABEL: case IR_LOOPBEG: case IR_LOOPEND:
      rv_label(pi->name, fp); fputs(":\n", fp);
      return;
    case IR_JUMP:
      fputs("  j ", fp); rv_label(pi->name, fp); fputs("\n", fp);
      return;
    case IR_BRFALSE: case IR_BRTRUE:
      l = rv_opnd((opnd_t *)bufref(&pi->args, 0), RV_GPSCRATCH, fp);
      fprintf(fp, "  %s %s, ", (pi->op == IR_BRFALSE) ? "beqz" : "bnez", l);
      rv_label(pi->name, fp); fputs("\n", fp);
      return;
    case IR_MOV: {
      opnd_t *ps;
      ps = (opnd_t *)bufref(&pi->args, 0);
      l = rv_opnd(ps, rv_isfp(pi->dst.vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      rv_commit(&pi->dst, l, fp);
      return;
    }
    case IR_UNARY: {
      opnd_t *ps;
      opnd_t *pt;
      bool srcfp;
      ps = (opnd_t *)bufref(&pi->args, 0);
      srcfp = rv_isfp(ps->vt);
      l = rv_opnd(ps, srcfp ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      pt = &pi->dst;
      if (g_rv_fuse != NULL && !streql(rv_dstreg(g_rv_fuse), l)) {
        pt = g_rv_fuse; g_rv_fused = true;
      }
      d = rv_dstreg(pt);
      rv_un(op, d, l, fp);
      rv_commit(pt, d, fp);
      return;
    }
    case IR_BINARY: {
      opnd_t *pa;
      opnd_t *pb;
      opnd_t *pt;
      pa = (opnd_t *)bufref(&pi->args, 0);
      pb = (opnd_t *)bufref(&pi->args, 1);
      l = rv_opnd(pa, rv_isfp(pa->vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      pt = &pi->dst;
      r = NULL;
      if (pb->kind != OK_IMM || pb->vt == VT_F32 || pb->vt == VT_F64)
        r = rv_opnd(pb, rv_isfp(pb->vt) ? RV_FPSCRATCH2 : RV_GPSCRATCH2, fp);
      if (g_rv_fuse != NULL) {
        const char *fd;
        fd = rv_dstreg(g_rv_fuse);
        if (!streql(fd, l) && (r == NULL || !streql(fd, r))) {
          pt = g_rv_fuse; g_rv_fused = true;
        }
      }
      d = rv_dstreg(pt);
      if (r != NULL) rv_bin(op, d, l, r, fp);
      else if (!rv_binimm(op, d, l, pb, fp)) {
        r = rv_opnd(pb, rv_isfp(pb->vt) ? RV_FPSCRATCH2 : RV_GPSCRATCH2, fp);
        rv_bin(op, d, l, r, fp);
      }
      rv_commit(pt, d, fp);
      return;
    }
    case IR_SELECT: {
      opnd_t *pc;
      opnd_t *pa;
      opnd_t *pb;
      int n;
      n = g_rv_lbl; g_rv_lbl += 1;
      pa = (opnd_t *)bufref(&pi->args, 0);
      pb = (opnd_t *)bufref(&pi->args, 1);
      pc = (opnd_t *)bufref(&pi->args, 2);
      d = rv_dstreg(&pi->dst);
      r = rv_opnd(pc, RV_GPSCRATCH2, fp);
      fprintf(fp, "  beqz %s, .Lsel_e%d\n", r, n);
      l = rv_opnd(pa, rv_isfp(pa->vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      if (!streql(l, d)) {
        if (pi->dst.vt == VT_F32) fprintf(fp, "  fmv.s %s, %s\n", d, l);
        else if (pi->dst.vt == VT_F64) fprintf(fp, "  fmv.d %s, %s\n", d, l);
        else fprintf(fp, "  mv %s, %s\n", d, l);
      }
      fprintf(fp, "  j .Lsel_d%d\n", n);
      fprintf(fp, ".Lsel_e%d:\n", n);
      l = rv_opnd(pb, rv_isfp(pb->vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      if (!streql(l, d)) {
        if (pi->dst.vt == VT_F32) fprintf(fp, "  fmv.s %s, %s\n", d, l);
        else if (pi->dst.vt == VT_F64) fprintf(fp, "  fmv.d %s, %s\n", d, l);
        else fprintf(fp, "  mv %s, %s\n", d, l);
      }
      fprintf(fp, ".Lsel_d%d:\n", n);
      rv_commit(&pi->dst, d, fp);
      return;
    }
    case IR_LOAD: {
      opnd_t *pa;
      opnd_t *pt;
      pa = (opnd_t *)bufref(&pi->args, 0);
      l = rv_opnd(pa, RV_GPSCRATCH, fp);
      if (!rv_simm12(pi->off)) {
        fprintf(fp, "  li %s, %lld\n", RV_GPSCRATCH2, pi->off);
        fprintf(fp, "  add %s, %s, %s\n", RV_GPSCRATCH, l, RV_GPSCRATCH2);
        l = RV_GPSCRATCH;
      }
      pt = &pi->dst;
      /* the address is read by the load itself, so a destination that is the
       * same register is fine here */
      if (g_rv_fuse != NULL) { pt = g_rv_fuse; g_rv_fused = true; }
      d = rv_dstreg(pt);
      fprintf(fp, "  %s %s, %lld(%s)\n", rv_ldop(op), d,
              rv_simm12(pi->off) ? pi->off : 0, l);
      rv_commit(pt, d, fp);
      return;
    }
    case IR_MEMORY: {
      /* RV64G has no bulk instruction, and the platform's memmove and memset
       * are tuned for it: the three operands are already in call order */
      rv_args(pi, 0, buflen(&pi->args), fp);
      fprintf(fp, "  call %s\n",
              streql(op, "memory.fill") ? "memset" : "memmove");
      return;
    }
    case IR_STORE: {
      opnd_t *pa;
      opnd_t *pv;
      pa = (opnd_t *)bufref(&pi->args, 0);
      pv = (opnd_t *)bufref(&pi->args, 1);
      l = rv_opnd(pa, RV_GPSCRATCH, fp);
      if (!rv_simm12(pi->off)) {
        fprintf(fp, "  li a7, %lld\n", pi->off);
        fprintf(fp, "  add a7, %s, a7\n", l);
        l = "a7";
        r = rv_opnd(pv, rv_isfp(pv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH2, fp);
        fprintf(fp, "  %s %s, 0(%s)\n", rv_stop(op), r, l);
      } else {
        r = rv_opnd(pv, rv_isfp(pv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH2, fp);
        fprintf(fp, "  %s %s, %lld(%s)\n", rv_stop(op), r, pi->off, l);
      }
      return;
    }
    case IR_GGET: {
      sym_t gn;
      gn = mod_globalname(g_rv_mod, pi->name);
      d = rv_dstreg(&pi->dst);
      fprintf(fp, "  la %s, %s\n", RV_GPSCRATCH2,
              (gn != 0) ? symname(gn) : symname(pi->name));
      fprintf(fp, "  %s %s, 0(%s)\n", rv_ldmnem(pi->dst.vt), d, RV_GPSCRATCH2);
      rv_commit(&pi->dst, d, fp);
      return;
    }
    case IR_GSET: {
      sym_t gn;
      opnd_t *pv;
      pv = (opnd_t *)bufref(&pi->args, 0);
      gn = mod_globalname(g_rv_mod, pi->name);
      r = rv_opnd(pv, rv_isfp(pv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
      fprintf(fp, "  la a7, %s\n", (gn != 0) ? symname(gn) : symname(pi->name));
      fprintf(fp, "  %s %s, 0(a7)\n", rv_stmnem(pv->vt), r);
      return;
    }
    case IR_BRTABLE: {
      size_t k, nt;
      opnd_t *ps;
      int n;
      n = g_rv_lbl; g_rv_lbl += 1;
      nt = buflen(&pi->args);
      if (nt < 2) { fputs("  # br_table with no target\n", fp); return; }
      ps = (opnd_t *)bufref(&pi->args, 0);
      l = rv_opnd(ps, RV_GPSCRATCH, fp);
      for (k = 1; k + 1 < nt; ++k) {
        opnd_t *pt;
        pt = (opnd_t *)bufref(&pi->args, k);
        fprintf(fp, "  li a7, %lu\n", (unsigned long)(k - 1));
        fprintf(fp, "  bne %s, a7, .Lbt%d_%lu\n", l, n, (unsigned long)k);
        fputs("  j ", fp); rv_label(pt->sym, fp); fputs("\n", fp);
        fprintf(fp, ".Lbt%d_%lu:\n", n, (unsigned long)k);
      }
      fputs("  j ", fp);
      rv_label(((opnd_t *)bufref(&pi->args, nt - 1))->sym, fp);
      fputs("\n", fp);
      return;
    }
    case IR_CALL: case IR_CALLI: {
      sig_t *ps;
      rv_args(pi, 0, buflen(&pi->args) - ((pi->op == IR_CALLI) ? 1 : 0), fp);
      if (pi->op == IR_CALLI) {
        opnd_t *pt;
        pt = (opnd_t *)bufref(&pi->args, 0);
        l = rv_opnd(pt, "a7", fp);
        fprintf(fp, "  jalr ra, 0(%s)\n", l);
      } else {
        ps = mod_sig(g_rv_mod, pi->name);
        fprintf(fp, "  call %s\n",
                (ps != NULL && ps->cname != 0) ? symname(ps->cname) : symname(pi->name));
      }
      if (pi->hasdst) {
        opnd_t *pt;
        pt = &pi->dst;
        if (g_rv_fuse != NULL) { pt = g_rv_fuse; g_rv_fused = true; }
        rv_commit(pt, rv_isfp(pi->dst.vt) ? "fa0" : "a0", fp);
      }
      return;
    }
    case IR_TJUMP: case IR_TJUMPI: case IR_RETCALL: case IR_RETCALLI: {
      sig_t *ps;
      bool ind;
      ind = (pi->op == IR_RETCALLI || pi->op == IR_TJUMPI);
      rv_args(pi, 0, buflen(&pi->args) - (ind ? 1 : 0), fp);
      if (ind) {
        opnd_t *pt;
        pt = (opnd_t *)bufref(&pi->args, buflen(&pi->args) - 1);
        l = rv_opnd(pt, "a7", fp);
        if (!streql(l, "a7")) fprintf(fp, "  mv a7, %s\n", l);
        rv_teardown(fp);
        fputs("  jr a7\n", fp);
      } else {
        ps = mod_sig(g_rv_mod, pi->name);
        rv_teardown(fp);
        fprintf(fp, "  tail %s\n",
                (ps != NULL && ps->cname != 0) ? symname(ps->cname) : symname(pi->name));
      }
      return;
    }
    case IR_RETURN:
      if (buflen(&pi->args) > 0) {
        opnd_t *pv;
        pv = (opnd_t *)bufref(&pi->args, 0);
        l = rv_opnd(pv, rv_isfp(pv->vt) ? RV_FPSCRATCH : RV_GPSCRATCH, fp);
        if (rv_isfp(pv->vt)) {
          if (!streql(l, "fa0"))
            fprintf(fp, "  fmv.%c fa0, %s\n", (pv->vt == VT_F32) ? 's' : 'd', l);
        } else {
          if (!streql(l, "a0")) fprintf(fp, "  mv a0, %s\n", l);
        }
      }
      rv_teardown(fp);
      fputs("  ret\n", fp);
      return;
    case IR_UNREACHABLE:
      fputs("  unimp\n", fp);
      return;
    default:
      fprintf(stderr, "marv: the rv target cannot emit this instruction yet\n");
      exit(1);
  }
}

/* ---------------------------------------------------------------- */
/* A module, and a function at a time.                              */

void rv_prologue(mod_t *pm, FILE *fp)
{
  g_rv_mod = pm;
  deps_line(TT_RV, fp);
  fputs("# Generated by MARV Compiler (RV64G LP64D Target)\n", fp);
  fputs("  .option pic\n\n", fp);
}

/* the alignment an assembler wants, as a power of two */
static int rv_p2(long long align)
{
  if (align >= 16) return 4;
  if (align >= 8) return 3;
  if (align >= 4) return 2;
  if (align >= 2) return 1;
  return 0;
}

/* the assembler name of what a data segment's pointer slot points at */
static void rv_relname(mod_t *pm, drel_t *pr, FILE *fp)
{
  if (pr->isfunc) {
    sig_t *ps;
    ps = mod_sig(pm, pr->sym);
    fputs((ps != NULL && ps->cname != 0) ? symname(ps->cname)
                                         : symname(pr->sym), fp);
  } else {
    sym_t dn;
    dn = mod_dataname(pm, pr->sym);
    fputs((dn != 0) ? symname(dn) : symname(pr->sym), fp);
  }
}

void rv_static(mod_t *pm, snode_t *pn, FILE *fp)
{
  const char *h;
  size_t i, k;
  g_rv_mod = pm;
  if (pn->nt != SN_LIST || snlen(pn) < 2 || snref(pn, 1)->nt != SN_SYM) return;
  h = symname(snref(pn, 0)->sym);
  if (streql(h, "global")) {
    vt_t vt;
    snode_t *pt;
    const char *gexp;
    sym_t gn;
    vt = VT_I32;
    gexp = NULL;
    for (i = 2; i < snlen(pn); ++i) {
      pt = snref(pn, i);
      if (pt->nt == SN_LIST && snlen(pt) > 1 && snref(pt, 0)->nt == SN_SYM
          && streql(symname(snref(pt, 0)->sym), "export")
          && snref(pt, 1)->nt == SN_STR)
        gexp = (const char *)cbdata(&snref(pt, 1)->data);
      if (pt->nt == SN_LIST && snlen(pt) == 2 && snref(pt, 0)->nt == SN_SYM
          && streql(symname(snref(pt, 0)->sym), "mut")) pt = snref(pt, 1);
      if (pt->nt == SN_SYM && vt_of_sym(pt->sym) != VT_VOID) vt = vt_of_sym(pt->sym);
    }
    gn = mod_globalname(pm, snref(pn, 1)->sym);
    fputs("  .data\n", fp);
    if (gexp != NULL) fprintf(fp, "  .globl %s\n", symname(gn));
    fputs("  .p2align 3\n", fp);
    fprintf(fp, "%s:\n", symname(gn));
    {   /* the declared initial value, as the bytes of its own width */
      glb_init_t gi;
      unsigned long long bits;
      glb_read(pn, vt, &gi);
      if (vt == VT_F32) bits = (unsigned long long)as_uint32((float)gi.dval);
      else if (vt == VT_F64) bits = as_uint64(gi.dval);
      else bits = (unsigned long long)gi.ival;
      if (vt == VT_I32 || vt == VT_F32)
        fprintf(fp, "  .word %lu\n", (unsigned long)(bits & 0xFFFFFFFFu));
      else fprintf(fp, "  .dword %llu\n", bits);
    }
    fputs("\n", fp);
    return;
  }
  if (streql(h, "data")) {
    dseg_t ds;
    sym_t dn;
    size_t nrel;
    dseg_read(pn, &ds, true);
    nrel = buflen(&ds.relocs);
    dn = mod_dataname(pm, snref(pn, 1)->sym);
    fputs(ds.isconst ? "  .section .rodata\n" : "  .data\n", fp);
    if (ds.expname != NULL) fprintf(fp, "  .globl %s\n", symname(dn));
    fprintf(fp, "  .p2align %d\n", rv_p2(ds.align));
    fprintf(fp, "%s:\n", symname(dn));
    if (ds.allzero && nrel == 0 && buflen(&ds.bytes) > 0) {
      fprintf(fp, "  .zero %lu\n", (unsigned long)buflen(&ds.bytes));
    } else {
      /* the bytes, except where a pointer slot stands: there the assembler
       * writes the address for us, and the eight zero bytes are skipped */
      size_t r;
      r = 0;
      for (k = 0; k < buflen(&ds.bytes); ) {
        drel_t *pr;
        pr = (r < nrel) ? (drel_t *)bufref(&ds.relocs, r) : NULL;
        if (pr != NULL && pr->off == k) {
          fputs("  .dword ", fp);
          rv_relname(pm, pr, fp);
          if (pr->addend != 0) fprintf(fp, " + %lld", pr->addend);
          fputs("\n", fp);
          k += 8;
          r += 1;
        } else {
          fprintf(fp, "  .byte %u\n",
                  (unsigned)(unsigned char)*(char *)bufref(&ds.bytes, k));
          k += 1;
        }
      }
    }
    fputs("\n", fp);
    dseg_fini(&ds);
    return;
  }
}

void rv_text(FILE *fp) { fputs("  .text\n\n", fp); }

/* give back the stack and return */
static void rv_teardown(FILE *fp)
{
  long long off;
  size_t i;
  if (g_rv_total <= 0) return;
  off = g_rv_savebase;
  if (g_rv_ra->hascalls) { fprintf(fp, "  ld ra, %lld(sp)\n", off); off += 8; }
  for (i = 0; i < buflen(&g_rv_ra->useds); ++i) {
    fprintf(fp, "  ld %s, %lld(sp)\n",
            *(const char **)bufref(&g_rv_ra->useds, i), off);
    off += 8;
  }
  for (i = 0; i < buflen(&g_rv_ra->usedfs); ++i) {
    fprintf(fp, "  fld %s, %lld(sp)\n",
            *(const char **)bufref(&g_rv_ra->usedfs, i), off);
    off += 8;
  }
  fprintf(fp, "  addi sp, sp, %lld\n", g_rv_total);
}

/* the arguments of a call, into a0.. / fa0.. and the stack beyond that
 *
 * An argument that has to be materialised -- a constant, an address, a spilled
 * value -- is materialised into the argument register itself rather than into
 * the scratch and then moved.  That is safe because the allocator hands out
 * only the argument registers above every call's own arity in this function,
 * so no argument register a call writes is any value's home.  [cco] */
static void rv_args(ir_t *pi, size_t first, size_t last, FILE *fp)
{
  size_t i;
  int gi, fi, st;
  gi = 0; fi = 0; st = 0;
  for (i = first; i < last; ++i) {
    opnd_t *pa;
    const char *sreg;
    pa = (opnd_t *)bufref(&pi->args, i);
    if (rv_isfp(pa->vt)) {
      if (fi < 8) {
        sreg = rv_opnd(pa, g_rv_fpargs[fi], fp);
        if (!streql(sreg, g_rv_fpargs[fi]))
          fprintf(fp, "  fmv.%c %s, %s\n", (pa->vt == VT_F32) ? 's' : 'd',
                  g_rv_fpargs[fi], sreg);
        fi += 1;
      } else {
        sreg = rv_opnd(pa, RV_FPSCRATCH, fp);
        fprintf(fp, "  %s %s, %d(sp)\n", (pa->vt == VT_F32) ? "fsw" : "fsd",
                sreg, st * 8);
        fi += 1; st += 1;
      }
    } else {
      if (gi < 8) {
        sreg = rv_opnd(pa, g_rv_gpargs[gi], fp);
        if (!streql(sreg, g_rv_gpargs[gi]))
          fprintf(fp, "  mv %s, %s\n", g_rv_gpargs[gi], sreg);
        gi += 1;
      } else {
        sreg = rv_opnd(pa, RV_GPSCRATCH, fp);
        fprintf(fp, "  sd %s, %d(sp)\n", sreg, st * 8);
        gi += 1; st += 1;
      }
    }
  }
}

/* A comparison whose only use is the branch after it is the branch: RV64I
 * compares and branches in one instruction, where the three-address code says
 * compare into a temporary and then test it.  Answers true when it emitted
 * both, and the caller then steps over the branch.
 *
 * The floating-point comparisons stay as they are, since there is no
 * floating-point branch to fuse them into.  An unsigned 32-bit comparison
 * branches on the 64-bit registers for the same reason the sltu form works:
 * sign extension carries the unsigned 32-bit order into the unsigned 64-bit
 * one.  [cco] */
static bool rv_brfuse(fn_t *pf, size_t i, FILE *fp)
{
  ir_t *pi;
  ir_t *pn;
  iv_t *piv;
  opnd_t *pt;
  const char *op;
  const char *m;
  bool whentrue, swap;
  pi = (ir_t *)bufref(&pf->code, i);
  if (pi->op != IR_UNARY && pi->op != IR_BINARY) return false;
  if (!pi->hasdst || pi->dst.kind != OK_VREG) return false;
  if (i + 1 >= buflen(&pf->code)) return false;
  pn = (ir_t *)bufref(&pf->code, i + 1);
  if (pn->op != IR_BRFALSE && pn->op != IR_BRTRUE) return false;
  if (buflen(&pn->args) != 1) return false;
  pt = (opnd_t *)bufref(&pn->args, 0);
  if (pt->kind != OK_VREG || pt->sym != pi->dst.sym) return false;
  piv = rv_iv_own(g_rv_ra, pi->dst.sym);
  if (piv == NULL || piv->ndef != 1 || piv->nuse != 1) return false;
  whentrue = (pn->op == IR_BRTRUE);
  op = (pi->name != 0) ? symname(pi->name) : "";

  if (pi->op == IR_UNARY) {
    const char *l;
    if (!rv_is(op, "i32.eqz") && !rv_is(op, "i64.eqz")
        && !rv_is(op, "ptr.eqz")) return false;
    l = rv_opnd((opnd_t *)bufref(&pi->args, 0), RV_GPSCRATCH, fp);
    fprintf(fp, "  %s %s, ", whentrue ? "beqz" : "bnez", l);
    rv_label(pn->name, fp);
    fputs("\n", fp);
    return true;
  }

  m = NULL;
  swap = false;
  if (rv_is(op, "i32.eq") || rv_is(op, "i64.eq") || rv_is(op, "ptr.eq"))
    m = whentrue ? "beq" : "bne";
  else if (rv_is(op, "i32.ne") || rv_is(op, "i64.ne") || rv_is(op, "ptr.ne"))
    m = whentrue ? "bne" : "beq";
  else if (rv_is(op, "i32.lt_s") || rv_is(op, "i64.lt_s"))
    m = whentrue ? "blt" : "bge";
  else if (rv_is(op, "i32.lt_u") || rv_is(op, "i64.lt_u")
           || rv_is(op, "ptr.lt_u"))
    m = whentrue ? "bltu" : "bgeu";
  else if (rv_is(op, "i32.ge_s") || rv_is(op, "i64.ge_s"))
    m = whentrue ? "bge" : "blt";
  else if (rv_is(op, "i32.ge_u") || rv_is(op, "i64.ge_u")
           || rv_is(op, "ptr.ge_u"))
    m = whentrue ? "bgeu" : "bltu";
  else if (rv_is(op, "i32.gt_s") || rv_is(op, "i64.gt_s")) {
    m = whentrue ? "blt" : "bge"; swap = true;
  } else if (rv_is(op, "i32.gt_u") || rv_is(op, "i64.gt_u")
             || rv_is(op, "ptr.gt_u")) {
    m = whentrue ? "bltu" : "bgeu"; swap = true;
  } else if (rv_is(op, "i32.le_s") || rv_is(op, "i64.le_s")) {
    m = whentrue ? "bge" : "blt"; swap = true;
  } else if (rv_is(op, "i32.le_u") || rv_is(op, "i64.le_u")
             || rv_is(op, "ptr.le_u")) {
    m = whentrue ? "bgeu" : "bltu"; swap = true;
  }
  if (m == NULL) return false;
  {
    opnd_t *pa;
    opnd_t *pb;
    const char *l;
    const char *r;
    pa = (opnd_t *)bufref(&pi->args, 0);
    pb = (opnd_t *)bufref(&pi->args, 1);
    if (rv_isfp(pa->vt) || rv_isfp(pb->vt)) return false;
    l = rv_opnd(pa, RV_GPSCRATCH, fp);
    r = rv_opnd(pb, RV_GPSCRATCH2, fp);
    fprintf(fp, "  %s %s, %s, ", m, swap ? r : l, swap ? l : r);
    rv_label(pn->name, fp);
    fputs("\n", fp);
  }
  return true;
}

void rv_func(mod_t *pm, fn_t *pf, FILE *fp)
{
  ra_t ra;
  size_t i, k;
  int mxst, gi, fi, st;
  long long nsaved;
  sym_t asym;
  sym_t qsym;
  g_rv_mod = pm;
  g_rv_fn = pf;
  g_rv_ra = &ra;
  rv_alloc(pf, &ra);

  /* the widest outgoing argument list says how much stack a call needs */
  mxst = 0;
  for (i = 0; i < buflen(&pf->code); ++i) {
    ir_t *pi;
    int g2, f2, s2;
    pi = (ir_t *)bufref(&pf->code, i);
    if (pi->op != IR_CALL && pi->op != IR_CALLI) continue;
    g2 = 0; f2 = 0; s2 = 0;
    for (k = (pi->op == IR_CALLI) ? 1 : 0; k < buflen(&pi->args); ++k) {
      opnd_t *pa;
      pa = (opnd_t *)bufref(&pi->args, k);
      if (rv_isfp(pa->vt)) { if (f2 >= 8) s2 += 1; f2 += 1; }
      else { if (g2 >= 8) s2 += 1; g2 += 1; }
    }
    if (s2 > mxst) mxst = s2;
  }
  g_rv_framebase = ((long long)mxst * 8 + 15) / 16 * 16;
  g_rv_spillbase = g_rv_framebase + pf->frame;
  g_rv_savebase = g_rv_spillbase + (long long)ra.nspills * 8;
  nsaved = (ra.hascalls ? 1 : 0) + (long long)buflen(&ra.useds)
           + (long long)buflen(&ra.usedfs);
  g_rv_total = (g_rv_savebase + nsaved * 8 + 15) / 16 * 16;

  {
    sig_t *ps;
    ps = mod_sig(pm, pf->name);
    qsym = (ps != NULL && ps->cname != 0) ? ps->cname : pf->name;
    if (pf->expname != 0) {
      cbuf_t nb;
      cbinit(&nb);
      c_mangle(symname(pf->expname), &nb);
      cbputc(0, &nb);
      asym = intern((const char *)cbdata(&nb));
      cbfini(&nb);
    } else {
      asym = qsym;
    }
  }
  fprintf(fp, "# -- Function %s (frame=%lld, spills=%d, total_sp=%lld) --\n",
          symname(asym), pf->frame, ra.nspills, g_rv_total);
  if (pf->expname != 0) {
    fprintf(fp, "  .globl %s\n", symname(asym));
    if (qsym != asym) fprintf(fp, "  .globl %s\n", symname(qsym));
  }
  fputs("  .p2align 2\n", fp);
  fprintf(fp, "%s:\n", symname(asym));
  if (qsym != asym) fprintf(fp, "%s:\n", symname(qsym));

  if (g_rv_total > 0) {
    long long off;
    fprintf(fp, "  addi sp, sp, -%lld\n", g_rv_total);
    off = g_rv_savebase;
    if (ra.hascalls) { fprintf(fp, "  sd ra, %lld(sp)\n", off); off += 8; }
    for (i = 0; i < buflen(&ra.useds); ++i) {
      fprintf(fp, "  sd %s, %lld(sp)\n", *(const char **)bufref(&ra.useds, i), off);
      off += 8;
    }
    for (i = 0; i < buflen(&ra.usedfs); ++i) {
      fprintf(fp, "  fsd %s, %lld(sp)\n", *(const char **)bufref(&ra.usedfs, i), off);
      off += 8;
    }
  }

  /* the incoming arguments into wherever the scan put them */
  gi = 0; fi = 0; st = 0;
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    opnd_t d;
    pv = (var_t *)bufref(&pf->vars, i);
    memset(&d, 0, sizeof(opnd_t));
    d.kind = OK_VREG; d.sym = pv->name; d.vt = pv->vt;
    if (rv_isfp(pv->vt)) {
      if (fi < 8) { rv_commit(&d, g_rv_fpargs[fi], fp); fi += 1; }
      else {
        fprintf(fp, "  %s %s, %lld(sp)\n", (pv->vt == VT_F32) ? "flw" : "fld",
                RV_FPSCRATCH2, g_rv_total + (long long)st * 8);
        rv_commit(&d, RV_FPSCRATCH2, fp);
        fi += 1; st += 1;
      }
    } else {
      if (gi < 8) { rv_commit(&d, g_rv_gpargs[gi], fp); gi += 1; }
      else {
        fprintf(fp, "  %s %s, %lld(sp)\n", (pv->vt == VT_I32) ? "lw" : "ld",
                RV_GPSCRATCH2, g_rv_total + (long long)st * 8);
        rv_commit(&d, RV_GPSCRATCH2, fp);
        gi += 1; st += 1;
      }
    }
  }

  /* a local read before it is written reads zero */
  for (i = (size_t)pf->nparams; i < buflen(&pf->vars); ++i) {
    var_t *pv;
    iv_t *piv;
    opnd_t d;
    pv = (var_t *)bufref(&pf->vars, i);
    piv = rv_iv_own(&ra, pv->name);
    if (piv == NULL || piv->start != 0 || piv->end < 0) continue;
    memset(&d, 0, sizeof(opnd_t));
    d.kind = OK_VREG; d.sym = pv->name; d.vt = pv->vt;
    if (rv_isfp(pv->vt)) {
      fprintf(fp, "  fmv.%c.x %s, zero\n", (pv->vt == VT_F32) ? 'w' : 'd',
              RV_FPSCRATCH);
      rv_commit(&d, RV_FPSCRATCH, fp);
    } else {
      rv_commit(&d, "zero", fp);
    }
  }

  for (i = 0; i < buflen(&pf->code); ++i) {
    ir_t *pi;
    pi = (ir_t *)bufref(&pf->code, i);
    if (rv_brfuse(pf, i, fp)) { i += 1; continue; }
    g_rv_fuse = rv_fusable(pf, i);
    g_rv_fused = false;
    rv_ins(pi, fp);
    if (g_rv_fused) i += 1;          /* the copy was folded into the result */
  }

  {
    bool left;
    left = false;
    if (buflen(&pf->code) > 0) {
      ir_t *pl;
      /* Labels and block/loop ends emit no code; the last real
       * instruction decides whether the function already left for good.
       * Trampoline jumps count as leaving: they tear the frame down and
       * tail-transfer, exactly like return_call -- see rv_ins above. */
      pl = (ir_t *)bufref(&pf->code, buflen(&pf->code) - 1);
      while (pl->op == IR_LABEL || pl->op == IR_LOOPBEG || pl->op == IR_LOOPEND) {
        if (pl == (ir_t *)bufref(&pf->code, 0)) { pl = NULL; break; }
        pl -= 1;
      }
      left = (pl != NULL
              && (pl->op == IR_RETURN || pl->op == IR_JUMP || pl->op == IR_RETCALL
                  || pl->op == IR_RETCALLI || pl->op == IR_TJUMP
                  || pl->op == IR_TJUMPI || pl->op == IR_UNREACHABLE
                  || pl->op == IR_BRTABLE));
    }
    if (!left) {
      rv_teardown(fp);
      fputs("  ret\n", fp);
    }
  }
  fputs("\n", fp);
  rv_fini(&ra);
  g_rv_ra = NULL;
}

/* A (trampoline_func ...) is an ordinary function that enters a chain, and on
 * a compiler that cannot jump it is also the loop the chain resets to.  The
 * body is emitted under its own name and never exported; the wrapper carries
 * the name the rest of the program calls.  On a compiler that can jump the
 * wrapper is one tail call and the chain runs on the stack it was given.
 * [cco] */
void cc_tramp_func(const mod_t *pm, fn_t *pf, FILE *fp)
{
  sym_t realname;
  sym_t realexp;
  sym_t bodyname;
  sym_t letters;
  sig_t *ps;
  size_t i;
  int off;
  cbuf_t mn;
  cbuf_t bn;
  const char *mod;
  const char *rt;
  g_cc_mod = (mod_t *)pm;
  ps = mod_sig((mod_t *)pm, pf->name);
  letters = (ps != NULL) ? sig_letters(ps) : 0;
  rt = cc_type(pf->res);

  cbinit(&mn);
  cc_ident(symname(pm->name), &mn);
  cbputc(0, &mn);
  mod = (const char *)cbdata(&mn);

  realname = pf->name;
  realexp = pf->expname;
  {
    const char *s;
    s = symname(realname);
    if (s[0] == '$') s += 1;
    bodyname = internf("$%s$tcbody", s);
  }
  pf->name = bodyname;
  pf->expname = 0;
  /* the wrapper below calls this body and nothing else does, so a driver
   * cutting the file into translation units has to keep the two together */
  fputs("/* marv-keep-next */\n", fp);
  cc_func(pm, pf, fp);
  pf->name = realname;
  pf->expname = realexp;

  cbinit(&bn);
  cc_ident(symname(bodyname), &bn);
  cbputc(0, &bn);

  /* the wrapper's own header, the same shape as the body's */
  fputs((realexp != 0 || cc_iface_visible(pm, pf)) ? "" : "static ", fp);
  fprintf(fp, "%s %s_", rt, mod);
  { cbuf_t nb; cbinit(&nb); cc_ident(symname(realname), &nb); cbputc(0, &nb);
    fputs((const char *)cbdata(&nb), fp); cbfini(&nb); }
  fputs("(", fp);
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pf->vars, i);
    if (i > 0) fputs(", ", fp);
    fprintf(fp, "%s a%lu", cc_type(pv->vt), (unsigned long)i);
  }
  if (pf->nparams == 0) fputs("void", fp);
  fputs(")\n{\n", fp);

  fputs("#ifdef MARV_TAILCALL_GUARANTEE\n", fp);
  fprintf(fp, "  MARV_MUSTTAIL return %s_%s(", mod, (const char *)cbdata(&bn));
  for (i = 0; i < (size_t)pf->nparams; ++i)
    fprintf(fp, "%sa%lu", (i > 0) ? ", " : "", (unsigned long)i);
  fputs(");\n", fp);
  fputs("#else\n", fp);
  /* the loop: read the area, copy out of it, and enter what it names.  The
   * copy has to happen before the call, because the callee's own jump writes
   * the same area. */
  fprintf(fp, "  %s r;\n", rt);
  fputs("  unsigned char *s;\n", fp);
  fputs("  marv_ptr f;\n", fp);
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pf->vars, i);
    fprintf(fp, "  %s b%lu;\n", cc_type(pv->vt), (unsigned long)i);
  }
  fprintf(fp, "  r = %s_%s(", mod, (const char *)cbdata(&bn));
  for (i = 0; i < (size_t)pf->nparams; ++i)
    fprintf(fp, "%sa%lu", (i > 0) ? ", " : "", (unsigned long)i);
  fputs(");\n", fp);
  fputs("  while (r != 0) {\n", fp);
  fputs("    s = (unsigned char *)(uintptr_t)r;\n", fp);
  fputs("    f = MARV_LOAD(ptr, s, 8);\n", fp);
  off = 16;
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pf->vars, i);
    fprintf(fp, "    b%lu = MARV_LOAD(%s, s, %d);\n", (unsigned long)i,
            cc_slotname(pv->vt), off);
    off += 16;
  }
  fprintf(fp, "    r = ((%s (*)(", rt);
  for (i = 0; i < (size_t)pf->nparams; ++i) {
    var_t *pv;
    pv = (var_t *)bufref(&pf->vars, i);
    fprintf(fp, "%s%s", (i > 0) ? ", " : "", cc_type(pv->vt));
  }
  if (pf->nparams == 0) fputs("void", fp);
  fputs("))f)(", fp);
  for (i = 0; i < (size_t)pf->nparams; ++i)
    fprintf(fp, "%sb%lu", (i > 0) ? ", " : "", (unsigned long)i);
  fputs(");\n", fp);
  fputs("  }\n", fp);
  fprintf(fp, "  return (%s)0;\n", rt);
  fputs("#endif\n}\n\n", fp);
  (void)letters;
  cbfini(&bn);
  cbfini(&mn);
}
