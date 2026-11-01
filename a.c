/* a.c (marv driver) -- [cco]
 *
 *   marv -t TARGDES [-c] [-o FILE] [-T DIR] [-L DIR] [-I DIR] [-v] FILE...
 *
 * The target designation names a .mvt description, which gives the object
 * extension and the command that links.  With -c one .mvs file becomes one
 * object; without it, every .mvs file on the line becomes a temporary object,
 * the system adapters its interfaces need are gathered, and the target's
 * linker is run over the lot.
 *
 * There are diagnostic modes under long names: --count, --write, --stream,
 * --3ac and --3ac-write.  They read a MARV input and report what syntax it
 * contains, or write back what they read.
 *
 * This file also holds the one place that knows how an external command is
 * run, and the three search path lists.
 */

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

/* WCPL takes #ifdef/#ifndef __WCPL__ only at top level, never inside a body,
 * so anything the wasm build cannot have lives in its own function. [cco] */
#ifndef __WCPL__
#ifndef _MSC_VER
#include <sys/resource.h>
#include <unistd.h>
void report_peak(void)
{
  struct rusage ru;
  getrusage(RUSAGE_SELF, &ru);
  fprintf(stderr, "peak RSS %ld KB\n", (long)ru.ru_maxrss);
}
long marv_pid(void) { return (long)getpid(); }
#endif
#ifdef _MSC_VER
#include <process.h>
/* cl has neither getrusage nor unistd.h; the peak is a diagnostic, and the
 * process id is only there to keep a temporary name to itself */
void report_peak(void) {}
long marv_pid(void) { return (long)_getpid(); }
#endif
#else
void report_peak(void) {}
long marv_pid(void) { return 0; }
#endif

/* Running an external tool. The wasm build has no system() -- it is not in
 * WASI -- so it writes the command out and lets the caller pipe the output
 * into a shell. Natively, -n asks for exactly that. Compiled output never goes
 * to stdout, so the printed script stays usable on its own. [cco] */
int g_dryrun;

/* A command path uses a deliberately small character set shared by the
 * Unix shell and Windows cmd.  Target command templates remain shell text. */
static bool shell_path(cbuf_t *dst, const char *path, bool input, bool for_del);

#ifndef __WCPL__
#ifdef _WIN32
static bool host_windows(void) { return true; }
#else
static bool host_windows(void) { return false; }
#endif
#else
static bool host_windows(void) { return false; }
#endif

static bool shell_path(cbuf_t *dst, const char *path, bool input, bool for_del)
{
  const unsigned char *p;
  bool quote;
  if (path[0] == 0) { fprintf(stderr, "marv: empty command path\n"); return false; }
  quote = false;
  for (p = (const unsigned char *)path; *p != 0; ++p) {
    unsigned char c;
    c = *p;
    if (c == '\\' && host_windows()) c = '/';
    if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
          (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.' ||
          c == '/' || c == ':' || c == ' ')) {
      fprintf(stderr, "marv: unsupported character in command path: %s\n", path);
      return false;
    }
    if (c == ' ') quote = true;
  }
  if (quote) cbputc('"', dst);
  if (input && path[0] == '-') cbputs("./", dst);
  for (p = (const unsigned char *)path; *p != 0; ++p) {
    int c;
    c = *p;
    if (c == '\\' && host_windows()) c = '/';
    if (c == '/' && host_windows() && for_del) c = '\\';
    cbputc(c, dst);
  }
  if (quote) cbputc('"', dst);
  return true;
}

static void print_delete(const char *path)
{
  cbuf_t cmd;
  cbinit(&cmd);
  cbputs(host_windows() ? "del /q " : "rm -f ", &cmd);
  if (!shell_path(&cmd, path, true, host_windows())) { cbfini(&cmd); return; }
  cbputc(0, &cmd);
  fprintf(stdout, "%s\n", (const char *)cbdata(&cmd));
  cbfini(&cmd);
}

#ifndef __WCPL__
int runcmd(const char *cmd)
{
  if (g_dryrun != 0) { fprintf(stdout, "%s\n", cmd); return 0; }
  return system(cmd);
}
int delfile(const char *path)
{
  if (g_dryrun != 0) { print_delete(path); return 0; }
  return remove(path);
}
#else
int runcmd(const char *cmd)
{
  fprintf(stdout, "%s\n", cmd);
  return 0;
}
int delfile(const char *path)
{
  print_delete(path);
  return 0;
}
#endif

int g_forms;
int g_lists;
int g_syms;
int g_ints;
int g_flts;
int g_strs;
int g_depth;
int g_maxdepth;

void count(snode_t *pn)
{
  size_t i;
  g_depth += 1;
  if (g_depth > g_maxdepth) g_maxdepth = g_depth;
  switch (pn->nt) {
    case SN_LIST:
      g_lists += 1;
      for (i = 0; i < snlen(pn); ++i) count(snref(pn, i));
      break;
    case SN_SYM: g_syms += 1; break;
    case SN_INT: g_ints += 1; break;
    case SN_FLT: g_flts += 1; break;
    case SN_STR: g_strs += 1; break;
    default: break;
  }
  g_depth -= 1;
}

/* read a module the way the compiler will: take each member on its own, reuse
 * the node, so only the largest member is ever in memory at once */
void stream(sread_t *prd, snode_t *pnd)
{
  while (srlopen(prd, pnd)) {
    while (srlnext(prd, pnd)) {
      g_forms += 1;
      g_depth = 0;
      count(pnd);
    }
  }
}

/* ================================================================ */
/* ================================================================ */
typedef struct tgt {
  tt_t type;
  sym_t objext;             /* ".c", ".wo", ".s" */
  sym_t linkcmd;            /* with ~src and ~exe to fill in, or 0 */
  sym_t cc;                 /* used only when there is no link-cmd */
  sym_t flags;              /* cflags ldflags ldlibs, already joined */
} tgt_t;

const char *tt_comment(tt_t t)
{
  if (t == TT_WO) return ";; ";
  if (t == TT_RV) return "# ";
  return "/* ";
}

/* the clauses of a (marv-target ...) form */
static void tgt_clause(tgt_t *pt, snode_t *pc, cbuf_t *pfl)
{
  const char *k;
  size_t i;
  if (pc->nt != SN_LIST || snlen(pc) < 2 || snref(pc, 0)->nt != SN_SYM) return;
  k = symname(snref(pc, 0)->sym);
  if (streql(k, "type")) {
    const char *v;
    if (snref(pc, 1)->nt != SN_SYM) return;
    v = symname(snref(pc, 1)->sym);
    if (streql(v, "wcpl") || streql(v, "wo")) pt->type = TT_WO;
    else if (streql(v, "rv64g") || streql(v, "riscv") || streql(v, "s")) pt->type = TT_RV;
    else if (streql(v, "cc") || streql(v, "c99") || streql(v, "c")) pt->type = TT_CC;
    else sneprintf(pc, "unknown target type (expected wcpl, cc, or rv64g)");
  } else if (streql(k, "obj-ext")) {
    if (snref(pc, 1)->nt == SN_STR)
      pt->objext = intern((const char *)cbdata(&snref(pc, 1)->data));
  } else if (streql(k, "link-cmd")) {
    if (snref(pc, 1)->nt == SN_STR)
      pt->linkcmd = intern((const char *)cbdata(&snref(pc, 1)->data));
  } else if (streql(k, "cc")) {
    if (snref(pc, 1)->nt == SN_STR)
      pt->cc = intern((const char *)cbdata(&snref(pc, 1)->data));
  } else if (streql(k, "cflags") || streql(k, "ldflags") || streql(k, "ldlibs")) {
    snode_t *pl;
    pl = snref(pc, 1);
    if (pl->nt != SN_LIST) return;
    for (i = 0; i < snlen(pl); ++i) {
      if (snref(pl, i)->nt != SN_STR) continue;
      if (cblen(pfl) > 0) cbputc(' ', pfl);
      cbputs((const char *)cbdata(&snref(pl, i)->data), pfl);
    }
  }
}

static void load_mvt(const char *path, tgt_t *pt)
{
  sread_t rd;
  snode_t nd;
  cbuf_t fl;
  size_t i;
  pt->type = TT_CC; pt->objext = 0; pt->linkcmd = 0; pt->cc = 0; pt->flags = 0;
  if (!sropen(&rd, path)) { fprintf(stderr, "marv: cannot read %s\n", path); exit(1); }
  sninit(&nd);
  cbinit(&fl);
  if (!srform(&rd, &nd) || nd.nt != SN_LIST || snlen(&nd) < 1
      || snref(&nd, 0)->nt != SN_SYM
      || !(streql(symname(snref(&nd, 0)->sym), "marv-target")
           || streql(symname(snref(&nd, 0)->sym), "target")
           || streql(symname(snref(&nd, 0)->sym), "c-config")))
    { fprintf(stderr, "marv: not a .mvt target description: %s\n", path); exit(1); }
  for (i = 1; i < snlen(&nd); ++i) tgt_clause(pt, snref(&nd, i), &fl);
  if (pt->objext == 0)
    pt->objext = intern((pt->type == TT_WO) ? ".wo"
                        : ((pt->type == TT_RV) ? ".s" : ".c"));
  if (pt->cc == 0) pt->cc = intern("cc");
  cbputc(0, &fl);
  pt->flags = intern((const char *)cbdata(&fl));
  cbfini(&fl);
  snfini(&nd);
  srclose(&rd);
}

/* Flattening a module: declarations first, then each function on its own.
 *
 * Two passes over the file, because a function may call one defined later and
 * the unstacker needs its signature. Both passes stream the module's members,
 * so neither holds more than one function at a time.  [cco] */

int g_nfuncs;
int g_nir;
int g_ntemps;
int g_nvars;

static bool is_head(snode_t *pn, const char *name)
{
  return pn->nt == SN_LIST && snlen(pn) > 0 && snref(pn, 0)->nt == SN_SYM
      && streql(symname(snref(pn, 0)->sym), name);
}

/* pass 1: interfaces and what the module declares */
static void declare_pass(mod_t *pm, const char *path)
{
  sread_t rd;
  snode_t nd;
  if (!sropen(&rd, path)) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
  sninit(&nd);
  while (srlopen(&rd, &nd)) {
    snode_t head;
    bool ismod;
    sninit(&head);
    ismod = false;
    if (srlnext(&rd, &nd)) {
      sncpy(&head, &nd);
      if (head.nt == SN_SYM && streql(symname(head.sym), "module")) ismod = true;
      if (head.nt == SN_SYM && streql(symname(head.sym), "include")) {
        /* rebuild the form so do_include sees it whole */
        snode_t inc;
        sninit(&inc);
        inc.nt = SN_LIST;
        inc.fname = nd.fname; inc.line = nd.line; inc.col = nd.col;
        sncpy(snnewbk(&inc), &head);
        while (srlnext(&rd, &nd)) sncpy(snnewbk(&inc), &nd);
        mod_include(pm, path, &inc);
        snfini(&inc);
        snfini(&head);
        continue;
      }
      if (ismod) {
        if (srlnext(&rd, &nd) && nd.nt == SN_SYM) pm->name = nd.sym;
        while (srlnext(&rd, &nd)) mod_declare(pm, &nd);
        snfini(&head);
        continue;
      }
    }
    while (srlnext(&rd, &nd)) { }        /* some other top-level form */
    snfini(&head);
  }
  snfini(&nd);
  srclose(&rd);
}

/* pass 2: flatten every function the module defines */
static void flatten_pass(mod_t *pm, const char *path, bool show)
{
  sread_t rd;
  snode_t nd;
  if (!sropen(&rd, path)) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
  sninit(&nd);
  while (srlopen(&rd, &nd)) {
    bool ismod;
    ismod = false;
    if (srlnext(&rd, &nd)) {
      if (nd.nt == SN_SYM && streql(symname(nd.sym), "module")) ismod = true;
    }
    if (!ismod) { while (srlnext(&rd, &nd)) { } continue; }
    if (srlnext(&rd, &nd)) { }           /* the module name */
    while (srlnext(&rd, &nd)) {
      fn_t fn;
      if (!is_head(&nd, "func") && !is_head(&nd, "trampoline_func")) continue;
      if (!fn_unstack(pm, &nd, &fn)) continue;
      g_nfuncs += 1;
      g_nir += (int)buflen(&fn.code);
      g_ntemps += fn.ntemp;
      g_nvars += (int)buflen(&fn.vars);
      if (show) ir_write(&fn, stdout);
      fn_fini(&fn);
    }
  }
  snfini(&nd);
  srclose(&rd);
}

/* emit one module as a cc translation unit: globals, data, then functions */
static void emit_pass(mod_t *pm, const char *path, FILE *fp)
{
  sread_t rd;
  snode_t nd;
  bool ismod;
  if (!sropen(&rd, path)) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
  sninit(&nd);
  cc_prologue_write(fp);
  cc_decls_write(pm, fp);
  cc_tramp_areas(pm, fp);
  while (srlopen(&rd, &nd)) {
    ismod = false;
    if (srlnext(&rd, &nd))
      if (nd.nt == SN_SYM && streql(symname(nd.sym), "module")) ismod = true;
    if (!ismod) { while (srlnext(&rd, &nd)) { } continue; }
    if (srlnext(&rd, &nd)) { }           /* the module name */
    while (srlnext(&rd, &nd)) {
      fn_t fn;
      bool istramp;
      if (is_head(&nd, "data") || is_head(&nd, "global")) { cc_static(pm, &nd, fp); continue; }
      istramp = is_head(&nd, "trampoline_func");
      if (!is_head(&nd, "func") && !istramp) continue;
      if (!fn_unstack(pm, &nd, &fn)) continue;
      g_nfuncs += 1;
      if (istramp) cc_tramp_func(pm, &fn, fp);
      else cc_func(pm, &fn, fp);
      fn_fini(&fn);
    }
  }
  snfini(&nd);
  srclose(&rd);
}

/* emit one module as a .wo object */
/* a pass that only lowers, to see which imports the module really uses */
static void scan_wo_pass(mod_t *pm, const char *path)
{
  sread_t rd;
  snode_t nd;
  bool ismod;
  if (!sropen(&rd, path)) return;
  sninit(&nd);
  while (srlopen(&rd, &nd)) {
    ismod = false;
    if (srlnext(&rd, &nd))
      if (nd.nt == SN_SYM && streql(symname(nd.sym), "module")) ismod = true;
    if (!ismod) { while (srlnext(&rd, &nd)) { } continue; }
    if (srlnext(&rd, &nd)) { }
    while (srlnext(&rd, &nd)) {
      fn_t fn;
      snode_t body;
      size_t first;
      if (!is_head(&nd, "func") && !is_head(&nd, "trampoline_func")) continue;
      fn_init(&fn);
      first = fn_header(pm, &nd, &fn);
      sninit(&body);
      fn_lower(pm, &nd, first, &fn, &body);
      mod_scan_uses(pm, &body);
      snfini(&body);
      fn_fini(&fn);
    }
  }
  snfini(&nd);
  srclose(&rd);
}

/* emit one module as rv assembly.  The data comes first, then the text, so
 * the two passes over the file are a read for the statics and a read for the
 * functions; each function is unstacked on its own.  [cco] */
static void emit_rv_pass(mod_t *pm, const char *path, FILE *fp)
{
  sread_t rd;
  snode_t nd;
  bool ismod;
  int phase;
  for (phase = 0; phase < 2; ++phase) {
    if (!sropen(&rd, path)) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
    sninit(&nd);
    while (srlopen(&rd, &nd)) {
      ismod = false;
      if (srlnext(&rd, &nd))
        if (nd.nt == SN_SYM && streql(symname(nd.sym), "module")) ismod = true;
      if (!ismod) { while (srlnext(&rd, &nd)) { } continue; }
      if (srlnext(&rd, &nd)) { }
      if (phase == 0) rv_prologue(pm, fp);
      else { rv_text(fp); mod_reset_fresh(); }
      while (srlnext(&rd, &nd)) {
        fn_t fn;
        if (phase == 0) {
          if (is_head(&nd, "data") || is_head(&nd, "global")) rv_static(pm, &nd, fp);
          continue;
        }
        if (!is_head(&nd, "func") && !is_head(&nd, "trampoline_func")) continue;
        fn_init(&fn);
        if (fn_unstack(pm, &nd, &fn)) {
          g_nfuncs += 1;
          rv_func(pm, &fn, fp);
        }
        fn_fini(&fn);
      }
    }
    snfini(&nd);
    srclose(&rd);
  }
}

void emit_rv_file(const char *path, const char *out)
{
  mod_t md;
  FILE *fp;
  mod_init(&md);
  declare_pass(&md, path);
  fp = (out != NULL) ? fopen(out, "w") : stdout;
  if (fp == NULL) { fprintf(stderr, "cannot write %s\n", out); exit(1); }
  emit_rv_pass(&md, path, fp);
  if (out != NULL) fclose(fp);
  mod_fini(&md);
}

static void emit_wo_pass(mod_t *pm, const char *path, FILE *fp)

{
  sread_t rd;
  snode_t nd;
  bool ismod;
  if (!sropen(&rd, path)) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
  sninit(&nd);
  while (srlopen(&rd, &nd)) {
    ismod = false;
    if (srlnext(&rd, &nd))
      if (nd.nt == SN_SYM && streql(symname(nd.sym), "module")) ismod = true;
    if (!ismod) { while (srlnext(&rd, &nd)) { } continue; }
    if (srlnext(&rd, &nd)) { }
    mod_reset_fresh();
    wo_prologue(pm, fp);
    while (srlnext(&rd, &nd)) {
      fn_t fn;
      snode_t body;
      size_t first;
      if (is_head(&nd, "data") || is_head(&nd, "global")) { wo_static(pm, &nd, fp); continue; }
      if (!is_head(&nd, "func") && !is_head(&nd, "trampoline_func")) continue;
      fn_init(&fn);
      first = fn_header(pm, &nd, &fn);
      sninit(&body);
      fn_lower(pm, &nd, first, &fn, &body);
      g_nfuncs += 1;
      wo_func(pm, &fn, &body, fp);
      snfini(&body);
      fn_fini(&fn);
    }
    wo_epilogue(fp);
  }
  snfini(&nd);
  srclose(&rd);
}

void emit_wo_file(const char *path, const char *out)
{
  mod_t md;
  FILE *fp;
  mod_init(&md);
  declare_pass(&md, path);
  scan_wo_pass(&md, path);
  fp = (out != NULL) ? fopen(out, "w") : stdout;
  if (fp == NULL) { fprintf(stderr, "cannot write %s\n", out); exit(1); }
  emit_wo_pass(&md, path, fp);
  if (out != NULL) fclose(fp);
  mod_fini(&md);
}

void emit_file(const char *path, const char *out)
{
  mod_t md;
  FILE *fp;
  mod_init(&md);
  declare_pass(&md, path);
  fp = (out != NULL) ? fopen(out, "w") : stdout;
  if (fp == NULL) { fprintf(stderr, "cannot write %s\n", out); exit(1); }
  emit_pass(&md, path, fp);
  if (out != NULL) fclose(fp);
  mod_fini(&md);
}

void flatten_file(const char *path, bool show)
{
  mod_t md;
  mod_init(&md);
  declare_pass(&md, path);
  flatten_pass(&md, path, show);
  mod_fini(&md);
}

/* ================================================================ */
/* The driver proper.                                               */

static int g_verbose;
static const char *g_targdes = "";

static void mvusage(FILE *fp)
{
  fprintf(fp, "usage: marv -t TARGDES [-c] [OPTION]... FILE...\n");
  fprintf(fp, "  -t, --target=TARGDES   target description to compile for (required)\n");
  fprintf(fp, "  -T, --target-dir=DIR   add DIR to the .mvt lookup path\n");
  fprintf(fp, "  -c, --compile          compile one .mvs file, do not link\n");
  fprintf(fp, "      --split-funcs=N    in link mode on a cc target, cut the\n");
  fprintf(fp, "                         generated C into pieces of at most N\n");
  fprintf(fp, "                         functions (default 300; 0 does not cut)\n");
  fprintf(fp, "  -o, --output=FILE      output file; required unless -c is given\n");
  fprintf(fp, "  -L, --system-dir=DIR   add DIR to the system .mvh and .c lookup path\n");
  fprintf(fp, "  -I, --include-dir=DIR  add DIR to the local .mvh lookup path\n");
  fprintf(fp, "  -v, --verbose          print each step before running it\n");
  fprintf(fp, "  -n, --dry-run          print the commands instead of running them\n");
  fprintf(fp, "  -h, --help             print this\n");
  fprintf(fp, "      --count            report what syntax the inputs contain\n");
  fprintf(fp, "      --write            read every form and write it back out\n");
  fprintf(fp, "      --stream           stream module members one at a time\n");
  fprintf(fp, "      --3ac              flatten to three-address code\n");
  fprintf(fp, "      --3ac-write        flatten and list the code\n");
}

static bool has_suffix(const char *s, const char *sfx)
{
  size_t n, m;
  n = strlen(s); m = strlen(sfx);
  if (m > n) return false;
  return streql(s + (n - m), sfx);
}

/* <stem of path><ext>, the object beside its source */
static sym_t with_ext(const char *path, const char *ext)
{
  size_t n, k;
  cbuf_t cb;
  sym_t r;
  n = strlen(path);
  k = n;
  while (k > 0 && path[k - 1] != '.' && path[k - 1] != '/' && path[k - 1] != '\\') k -= 1;
  if (k == 0 || path[k - 1] != '.') k = n; else k -= 1;
  cbinit(&cb);
  cbput(path, k, &cb);
  cbputs(ext, &cb);
  cbputc(0, &cb);
  r = intern((const char *)cbdata(&cb));
  cbfini(&cb);
  return r;
}

static int g_tempno;

static sym_t temp_obj(const char *ext)
{
  cbuf_t cb;
  sym_t r;
  const char *tdir;
  tdir = getenv("TMPDIR");
  if (tdir == NULL || tdir[0] == 0) tdir = getenv("TMP");
  if (tdir == NULL || tdir[0] == 0) tdir = getenv("TEMP");
  if (tdir == NULL || tdir[0] == 0) tdir = host_windows() ? "." : "/tmp";
  cbinit(&cb);
  for (;;) {
    cbclear(&cb);
    cbsetf(&cb, "%s/marv_%ld_%d%s", tdir, marv_pid(), g_tempno, ext);
    g_tempno += 1;
    if (!file_readable((char *)cbdata(&cb))) break;
  }
  r = intern((const char *)cbdata(&cb));
  cbfini(&cb);
  return r;
}

/* compile one .mvs file to DEST in the target's object format */
static void compile_one(tgt_t *pt, const char *src, const char *dest)
{
  FILE *fp;
  sys_reset();
  if (g_verbose != 0)
    fprintf(stderr, "marv -t %s -c -o %s %s\n", g_targdes, dest, src);
  fp = fopen(dest, "w");
  if (fp == NULL) { fprintf(stderr, "marv: cannot write %s\n", dest); exit(1); }
  fclose(fp);
  if (pt->type == TT_WO) emit_wo_file(src, dest);
  else if (pt->type == TT_RV) emit_rv_file(src, dest);
  else emit_file(src, dest);
}

/* the adapters the system headers of the last compile name */
static void gather_adapters(buf_t *pcs)
{
  size_t i;
  cbuf_t cb;
  cbinit(&cb);
  for (i = 0; i < sys_count(); ++i) {
    sym_t p;
    size_t k;
    bool seen;
    if (!adapter_of(sys_path(i), &cb)) continue;
    p = intern((const char *)cbdata(&cb));
    seen = false;
    for (k = 0; k < buflen(pcs); ++k)
      if (*(sym_t *)bufref(pcs, k) == p) { seen = true; break; }
    if (!seen) *(sym_t *)bufnewbk(pcs) = p;
  }
  cbfini(&cb);
}

/* A precompiled object carries its own dependencies on its first line, so a
 * link that is handed one can still find the adapters it needs. */
static void adapters_of_object(const char *path, buf_t *pcs)
{
  FILE *fp;
  cbuf_t ln;
  cbuf_t cb;
  cbuf_t pathbuf;
  const char *s;
  const char *at;
  int c;
  fp = fopen(path, "r");
  if (fp == NULL) return;
  cbinit(&ln);
  cbinit(&cb);
  cbinit(&pathbuf);
  while ((c = fgetc(fp)) != EOF && c != '\n') cbputc(c, &ln);
  fclose(fp);
  cbputc(0, &ln);
  s = (const char *)cbdata(&ln);
  at = strstr(s, "marv-sys-deps:");
  if (at != NULL) {
    at += 14;
    for (;;) {
      size_t n;
      while (*at == ' ' || *at == '\t') at += 1;
      n = 0;
      while (at[n] != '\0' && at[n] != ' ' && at[n] != '\t') n += 1;
      if (n == 0) break;
      if (!(n == 2 && at[0] == '*' && at[1] == '/')) {
        cbclear(&cb);
        cbput(at, n, &cb);
        cbputc(0, &cb);
        if (find_in_dirs(&g_ldirs, NULL, (const char *)cbdata(&cb), ".mvh", &pathbuf)) {
          cbuf_t ad;
          cbinit(&ad);
          if (adapter_of((const char *)cbdata(&pathbuf), &ad)) {
            sym_t p;
            size_t k;
            bool seen;
            p = intern((const char *)cbdata(&ad));
            seen = false;
            for (k = 0; k < buflen(pcs); ++k)
              if (*(sym_t *)bufref(pcs, k) == p) { seen = true; break; }
            if (!seen) *(sym_t *)bufnewbk(pcs) = p;
          }
          cbfini(&ad);
        }
      }
      at += n;
    }
  }
  cbfini(&pathbuf);
  cbfini(&cb);
  cbfini(&ln);
}

/* ------------------------------------------------------------ splitting
 *
 * One generated C file per module is what a host C compiler has to swallow,
 * and for a module with thousands of functions that is where its memory goes.
 * In link mode on a cc target the driver therefore cuts the generated file
 * into pieces of at most g_split function definitions and hands every piece
 * to the link command; -c mode never comes here and always writes one file.
 *
 * The cut relies only on the emitter's own output shape: a definition starts
 * at an unindented line whose text runs to a `{` alone on the next line, and
 * ends at a `}` in column one.  Everything before the first definition is the
 * head, which every piece needs; in a piece after the first, a definition in
 * that head becomes an `extern` declaration, so the pieces hold one definition
 * of each symbol between them.  `main` and the pre-main constructors stay in
 * the first piece.  [cco]
 */

static long g_split = 300;      /* 0 turns the splitting off */

/* a line that opens a definition rather than declaring or directing */
static bool split_opens(const char *l, const char *next)
{
  if (l[0] == ' ' || l[0] == '\t' || l[0] == '#' || l[0] == '}' || l[0] == 0)
    return false;
  if (strchr(l, '(') == NULL) return false;
  if (strchr(l, ';') != NULL) return false;            /* a prototype */
  return next != NULL && next[0] == '{';
}

static const char *g_split_types[7] = { "marv_i32 ", "marv_i64 ", "marv_f32 ",
                                         "marv_f64 ", "marv_ptr ", "marv_u32 ",
                                         "marv_u64 " };

/* does this line start a definition of storage rather than of code */
static bool split_is_data(const char *l, cbuf_t *pex)
{
  const char *p;
  const char *nm;
  const char *br;
  size_t n;
  p = l;
  if (strstr(p, "static ") == p) p += 7;
  if (strstr(p, "MARV_ALIGNED(") == p) {
    p = strchr(p, ')');
    if (p == NULL) return false;
    p += 1;
    while (*p == ' ') p += 1;
    if (strstr(p, "static ") == p) p += 7;
  }
  if (strstr(p, "unsigned char ") != p) {
    /* a global: <marv type> <name> [= ...]; */
    size_t t;
    for (t = 0; t < 7; ++t) {
      if (strstr(p, g_split_types[t]) == p) {
        nm = p + strlen(g_split_types[t]);
        n = 0;
        while (nm[n] != 0 && nm[n] != ' ' && nm[n] != ';'
               && nm[n] != '=' && nm[n] != '(') n += 1;
        /* a name followed by a parameter list is a function, not storage */
        if (n == 0 || nm[n] == '(') return false;
        cbputs("extern ", pex);
        cbput(g_split_types[t], strlen(g_split_types[t]), pex);
        cbput(nm, n, pex);
        cbputs(";", pex);
        return true;
      }
    }
    return false;
  }
  nm = p + strlen("unsigned char ");
  br = strchr(nm, '[');
  if (br == NULL) return false;
  cbputs("extern unsigned char ", pex);
  cbput(nm, (size_t)(br - nm), pex);
  cbputs("[];", pex);
  return true;
}

/* Whatever one piece defines and another refers to loses its `static`, so the
 * reference has a definition to bind to.  The word is not always the first on
 * the line: a trampoline area wears its alignment in front of it, and a per
 * piece copy of that would be wrong anyway, since the chain shares it. */
static void split_put_unstatic(const char *l, FILE *fp)
{
  const char *st;
  if (strstr(l, "static ") == l) { fputs(l + 7, fp); fputs("\n", fp); return; }
  st = strstr(l, " static ");
  if (st != NULL && strstr(l, "MARV_ALIGNED(") == l) {
    fwrite(l, 1, (size_t)(st - l), fp);
    fputs(st + 7, fp);                 /* the space before it stays */
    fputs("\n", fp);
    return;
  }
  fputs(l, fp);
  fputs("\n", fp);
}

/* read the whole generated file into a buffer of lines */
static bool split_lines(const char *path, buf_t *plines, cbuf_t *ptext)
{
  FILE *fp;
  int c;
  size_t i, start;
  const char *t;
  fp = fopen(path, "rb");
  if (fp == NULL) return false;
  while ((c = fgetc(fp)) != EOF) {
    if (c == '\r') continue;
    cbputc((c == '\n') ? 0 : c, ptext);
  }
  fclose(fp);
  cbputc(0, ptext);
  t = (const char *)cbdata(ptext);
  start = 0;
  for (i = 0; i < cblen(ptext); ++i) {
    if (t[i] == 0) {
      *(const char **)bufnewbk(plines) = t + start;
      start = i + 1;
    }
  }
  return true;
}

/* Write the pieces of one generated C file, returning how many there are and
 * interning their names into pnames.  Answers 1 and the file itself when there
 * is nothing to gain. */
static size_t split_file(const char *path, const char *objext, buf_t *pnames,
                         buf_t *ptemps)
{
  buf_t lines;
  cbuf_t text;
  buf_t starts, ends, whose;
  size_t i, d, nl, nd, headend, piece, npieces, runtime, wrote;
  bufinit(&lines, sizeof(const char *));
  cbinit(&text);
  if (!split_lines(path, &lines, &text)) {
    buffini(&lines); cbfini(&text);
    *(sym_t *)bufnewbk(pnames) = intern(path);
    return 1;
  }
  nl = buflen(&lines);
  bufinit(&starts, sizeof(size_t));
  bufinit(&ends, sizeof(size_t));
  /* the fixed runtime at the top has definitions of its own, which belong to
   * every piece: the scan starts where the emitter says the module does */
  runtime = 0;
  for (i = 0; i < nl; ++i) {
    const char *l;
    l = *(const char **)bufref(&lines, i);
    if (strstr(l, "/* marv-runtime-end */") == l) { runtime = i + 1; break; }
  }
  for (i = runtime; i < nl; ++i) {
    const char *l;
    const char *nx;
    l = *(const char **)bufref(&lines, i);
    nx = (i + 1 < nl) ? *(const char **)bufref(&lines, i + 1) : NULL;
    if (!split_opens(l, nx)) continue;
    {
      size_t j;
      *(size_t *)bufnewbk(&starts) = i;
      for (j = i + 2; j < nl; ++j) {
        const char *e;
        e = *(const char **)bufref(&lines, j);
        if (e[0] == '}' && (e[1] == 0 || e[1] == ';')) break;
      }
      *(size_t *)bufnewbk(&ends) = (j < nl) ? j : nl - 1;
      i = (j < nl) ? j : nl - 1;
    }
  }
  nd = buflen(&starts);
  headend = (nd > 0) ? *(size_t *)bufref(&starts, 0) : nl;
  if (headend < runtime) headend = runtime;
  if (g_split <= 0 || nd <= (size_t)g_split) {
    buffini(&lines); cbfini(&text); buffini(&starts); buffini(&ends);
    *(sym_t *)bufnewbk(pnames) = intern(path);
    return 1;
  }
  /* Which piece each definition goes to.  The entry point and whatever runs
   * before it belong with the storage they touch, which is the first piece,
   * and they take up room there like anything else.  A pair the emitter asked
   * to keep together is not parted by a boundary. */
  bufinit(&whose, sizeof(size_t));
  for (d = 0; d < nd; ++d) *(size_t *)bufnewbk(&whose) = 0;
  {
    size_t at, used, npin;
    npin = 0;
    for (d = 0; d < nd; ++d) {
      const char *fl;
      fl = *(const char **)bufref(&lines, *(size_t *)bufref(&starts, d));
      if (strstr(fl, "int main(") == fl || strstr(fl, "MARV_CTOR(") == fl)
        npin += 1;
    }
    at = 0;
    used = npin;
    for (d = 0; d < nd; ++d) {
      const char *fl;
      size_t run, j;
      fl = *(const char **)bufref(&lines, *(size_t *)bufref(&starts, d));
      if (strstr(fl, "int main(") == fl || strstr(fl, "MARV_CTOR(") == fl)
        continue;                        /* already counted, and piece 0 */
      run = 1;
      for (j = d; j + 1 < nd; ++j) {
        size_t t0;
        const char *b;
        t0 = *(size_t *)bufref(&starts, j);
        b = (t0 > 0) ? *(const char **)bufref(&lines, t0 - 1) : "";
        if (strstr(b, "/* marv-keep-next */") != b) break;
        run += 1;
      }
      if (used > 0 && used + run > (size_t)g_split) { at += 1; used = 0; }
      for (j = 0; j < run; ++j) *(size_t *)bufref(&whose, d + j) = at;
      used += run;
      d += run - 1;
    }
    npieces = at + 1;
  }
  for (piece = 0; piece < npieces; ++piece) {
    sym_t nm;
    FILE *fp;
    nm = (piece == 0) ? intern(path) : temp_obj(objext);
    fp = fopen(symname(nm), "wb");
    if (fp == NULL) {
      fprintf(stderr, "marv: cannot write %s\n", symname(nm));
      exit(1);
    }
    /* the head: every piece needs it, and after the first the definitions in
     * it are other pieces' */
    for (i = 0; i < headend; ++i) {
      const char *l;
      l = *(const char **)bufref(&lines, i);
      if (piece > 0 && i >= runtime) {
        cbuf_t ex;
        cbinit(&ex);
        if (split_is_data(l, &ex)) {
          cbputc(0, &ex);
          fputs((const char *)cbdata(&ex), fp);
          fputs("\n", fp);
          cbfini(&ex);
          /* skip to the end of the definition it replaced */
          while (i < headend) {
            const char *s;
            s = *(const char **)bufref(&lines, i);
            if (s[0] != 0 && s[strlen(s) - 1] == ';') break;
            i += 1;
          }
          continue;
        }
        cbfini(&ex);
      }
      /* the runtime keeps its own linkage: every piece has a copy of it, and
       * they are static so they do not collide */
      if (i < runtime) { fputs(l, fp); fputs("\n", fp); }
      else split_put_unstatic(l, fp);
    }
    wrote = 0;
    for (d = 0; d < nd; ++d) {
      size_t s0, s1;
      if (*(size_t *)bufref(&whose, d) != piece) continue;
      s0 = *(size_t *)bufref(&starts, d);
      s1 = *(size_t *)bufref(&ends, d);
      for (i = s0; i <= s1; ++i)
        split_put_unstatic(*(const char **)bufref(&lines, i), fp);
      fputs("\n", fp);
      wrote += 1;
    }
    fclose(fp);
    /* a piece with no definition of its own is a compile for nothing */
    if (piece > 0 && wrote == 0) { delfile(symname(nm)); continue; }
    *(sym_t *)bufnewbk(pnames) = nm;
    if (piece > 0) *(sym_t *)bufnewbk(ptemps) = nm;
  }
  buffini(&lines); cbfini(&text); buffini(&starts); buffini(&ends);
  buffini(&whose);
  return npieces;
}

static bool bad_command_path(cbuf_t *cmd, cbuf_t *src)
{
  cbfini(cmd);
  cbfini(src);
  return false;
}

static bool run_linker(tgt_t *pt, const char *out, buf_t *pobjs, buf_t *pcs)
{
  cbuf_t src;
  cbuf_t cmd;
  size_t i;
  int rc;
  cbinit(&src);
  cbinit(&cmd);
  for (i = 0; i < buflen(pobjs); ++i) {
    if (cblen(&src) > 0) cbputc(' ', &src);
    if (!shell_path(&src, symname(*(sym_t *)bufref(pobjs, i)), true, false)) return bad_command_path(&cmd, &src);
  }
  for (i = 0; i < buflen(pcs); ++i) {
    if (cblen(&src) > 0) cbputc(' ', &src);
    if (!shell_path(&src, symname(*(sym_t *)bufref(pcs, i)), true, false)) return bad_command_path(&cmd, &src);
  }
  cbputc(0, &src);
  if (pt->linkcmd != 0) {
    const char *s;
    s = symname(pt->linkcmd);
    while (*s != '\0') {
      if (s[0] == '~' && strncmp(s, "~src", 4) == 0) {
        cbputs((const char *)cbdata(&src), &cmd); s += 4;
      } else if (s[0] == '~' && strncmp(s, "~exe", 4) == 0) {
        if (!shell_path(&cmd, out, false, false)) return bad_command_path(&cmd, &src);
        s += 4;
      } else { cbputc(*s, &cmd); s += 1; }
    }
  } else {
    cbsetf(&cmd, "%s %s -o ", symname(pt->cc), symname(pt->flags));
    if (!shell_path(&cmd, out, false, false)) return bad_command_path(&cmd, &src);
    cbputc(' ', &cmd);
    cbputs((const char *)cbdata(&src), &cmd);
  }
  cbputc(0, &cmd);
  if (g_verbose != 0) fprintf(stderr, "%s\n", (const char *)cbdata(&cmd));
  rc = runcmd((const char *)cbdata(&cmd));
  if (rc != 0) {
    fprintf(stderr, "marv: the target linker failed (%d)\n", rc);
    cbfini(&cmd); cbfini(&src);
    return false;
  }
  cbfini(&cmd);
  cbfini(&src);
  return true;
}

int main(int argc, char **argv)
{
  int i, dflag;
  const char *targdes;
  const char *outfile;
  bool conly, linked;
  tgt_t tgt;
  cbuf_t mvtpath;
  buf_t objs, temps, cs;
  sread_t rd;
  snode_t nd;

  setprogname("marv");
  init_instrs();
  dirs_init();
  targdes = NULL; outfile = NULL; conly = false; dflag = 0;

  for (i = 1; i < argc; ++i) {
    const char *s;
    s = argv[i];
    if (s[0] != '-' || s[1] == '\0') break;
    if (streql(s, "--")) { i += 1; break; }
    if (streql(s, "-c") || streql(s, "--compile")) { conly = true; continue; }
    if (streql(s, "-v") || streql(s, "--verbose")) { g_verbose = 1; continue; }
    if (streql(s, "-n") || streql(s, "--dry-run")) { g_dryrun = 1; continue; }
    if (streql(s, "-h") || streql(s, "--help")) { mvusage(stdout); return 0; }
    if (streql(s, "--count")) { dflag = 1; continue; }
    if (streql(s, "--write")) { dflag = 2; continue; }
    if (streql(s, "--stream")) { dflag = 3; continue; }
    if (streql(s, "--3ac")) { dflag = 4; continue; }
    if (streql(s, "--3ac-write")) { dflag = 5; continue; }
    {
      const char *val;
      char opt;
      opt = 0;
      val = NULL;
      if (s[1] == '-') {                /* --name=VALUE or --name VALUE */
        const char *eq;
        size_t n;
        eq = strchr(s, '=');
        n = (eq != NULL) ? (size_t)(eq - s) : strlen(s);
        if (n == 8 && strncmp(s, "--target", 8) == 0) opt = 't';
        else if (n == 12 && strncmp(s, "--target-dir", 12) == 0) opt = 'T';
        else if (n == 8 && strncmp(s, "--output", 8) == 0) opt = 'o';
        else if (n == 12 && strncmp(s, "--system-dir", 12) == 0) opt = 'L';
        else if (n == 13 && strncmp(s, "--include-dir", 13) == 0) opt = 'I';
        else if (n == 13 && strncmp(s, "--split-funcs", 13) == 0) opt = 'S';
        if (opt != 0) {
          if (eq != NULL) val = eq + 1;
          else if (i + 1 < argc) { i += 1; val = argv[i]; }
          else {
            fprintf(stderr, "marv: %s needs an argument\n", s);
            return 2;
          }
        }
      } else {              /* short flags cluster, and the last may take a value */
        size_t k;
        bool bad;
        bad = false;
        for (k = 1; s[k] != '\0'; ++k) {
          char c2;
          c2 = s[k];
          if (c2 == 'c') { conly = true; continue; }
          if (c2 == 'v') { g_verbose = 1; continue; }
          if (c2 == 'n') { g_dryrun = 1; continue; }
          if (c2 == 't' || c2 == 'T' || c2 == 'o' || c2 == 'L' || c2 == 'I') {
            opt = c2;
            if (s[k + 1] != '\0') val = s + k + 1;      /* -tcc */
            else if (i + 1 < argc) { i += 1; val = argv[i]; }   /* -t cc */
            else {
              fprintf(stderr, "marv: -%c needs an argument\n", c2);
              return 2;
            }
            break;
          }
          bad = true;
          break;
        }
        if (bad) {
          fprintf(stderr, "marv: unknown option %s\n\n", s);
          mvusage(stderr);
          return 2;
        }
        if (opt == 0) continue;         /* the cluster was all flags */
      }
      if (opt == 0 || val == NULL) {
        fprintf(stderr, "marv: unknown option %s\n\n", s);
        mvusage(stderr);
        return 2;
      }
      if (opt == 't') targdes = val;
      else if (opt == 'o') outfile = val;
      else if (opt == 'T') dirs_add(&g_tdirs, val);
      else if (opt == 'L') dirs_add(&g_ldirs, val);
      else if (opt == 'S') {
        char *end;
        long n2;
        n2 = strtol(val, &end, 10);
        if (end == val || *end != '\0' || n2 < 0) {
          fprintf(stderr, "marv: --split-funcs takes a count, not %s\n", val);
          return 2;
        }
        g_split = n2;
      }
      else dirs_add(&g_idirs, val);
    }
  }

  if (dflag != 0) {                 /* the diagnostic modes */
    if (dflag == 4 || dflag == 5) {
      for (; i < argc; ++i) flatten_file(argv[i], dflag == 5);
      if (dflag == 4)
        fprintf(stdout, "functions %d  ir %d  temps %d  vars %d\n",
                g_nfuncs, g_nir, g_ntemps, g_nvars);
      report_peak();
      return 0;
    }
    sninit(&nd);
    for (; i < argc; ++i) {
      if (!sropen(&rd, argv[i])) {
        fprintf(stderr, "marv: cannot open %s\n", argv[i]);
        return 1;
      }
      if (dflag == 3) {
        stream(&rd, &nd);
      } else {
        while (srform(&rd, &nd)) {
          g_forms += 1;
          g_depth = 0;
          count(&nd);
          if (dflag == 2) { snwrite(&nd, stdout); fputc('\n', stdout); }
        }
      }
      srclose(&rd);
    }
    snfini(&nd);
    if (dflag != 2)
      fprintf(stdout, "forms %d  lists %d  symbols %d  ints %d  floats %d  strings %d  max depth %d\n",
              g_forms, g_lists, g_syms, g_ints, g_flts, g_strs, g_maxdepth);
    report_peak();
    return 0;
  }

  if (targdes == NULL) {
    fprintf(stderr, "marv: missing required -t TARGDES option\n\n");
    mvusage(stderr);
    return 1;
  }
  if (i >= argc) {
    fprintf(stderr, "marv: no input files specified\n\n");
    mvusage(stderr);
    return 1;
  }

  cbinit(&mvtpath);
  if (has_suffix(targdes, ".mvt") && file_readable(targdes)) {
    cbputs(targdes, &mvtpath);
    cbputc(0, &mvtpath);
  } else if (!find_in_dirs(&g_tdirs, NULL, targdes, ".mvt", &mvtpath)) {
    fprintf(stderr, "marv: cannot find target description %s.mvt\n", targdes);
    return 1;
  }
  g_targdes = targdes;
  load_mvt((const char *)cbdata(&mvtpath), &tgt);
  cbfini(&mvtpath);

  if (conly) {
    const char *src;
    sym_t dest;
    if (argc - i != 1) {
      fprintf(stderr, "marv: -c mode expects exactly one .mvs file (got %d)\n",
              argc - i);
      return 1;
    }
    src = argv[i];
    if (!has_suffix(src, ".mvs")) {
      fprintf(stderr, "marv: -c input file must have .mvs extension: %s\n", src);
      return 1;
    }
    dest = (outfile != NULL) ? intern(outfile) : with_ext(src, symname(tgt.objext));
    compile_one(&tgt, src, symname(dest));
    dirs_fini();
    return 0;
  }

  if (outfile == NULL) {
    fprintf(stderr, "marv: -o FILE is required when -c is not specified\n");
    return 1;
  }
  bufinit(&objs, sizeof(sym_t));
  bufinit(&temps, sizeof(sym_t));
  bufinit(&cs, sizeof(sym_t));
  for (; i < argc; ++i) {
    if (has_suffix(argv[i], ".mvs")) {
      sym_t dest;
      dest = temp_obj(symname(tgt.objext));
      compile_one(&tgt, argv[i], symname(dest));
      *(sym_t *)bufnewbk(&temps) = dest;
      if (tgt.type == TT_CC && g_split > 0)
        split_file(symname(dest), symname(tgt.objext), &objs, &temps);
      else *(sym_t *)bufnewbk(&objs) = dest;
      gather_adapters(&cs);
    } else {
      *(sym_t *)bufnewbk(&objs) = intern(argv[i]);
      adapters_of_object(argv[i], &cs);
    }
  }
  linked = run_linker(&tgt, outfile, &objs, &cs);
  for (i = 0; (size_t)i < buflen(&temps); ++i)
    delfile(symname(*(sym_t *)bufref(&temps, (size_t)i)));
  buffini(&objs); buffini(&temps); buffini(&cs);
  dirs_fini();
  return linked ? 0 : 1;
}
