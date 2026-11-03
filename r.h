/* r.h (marv reader and front end) -- [cco]
 *
 * The R7RS subset MARV input actually uses, read into node trees in the same
 * shape WCPL's p.c builds: a fat node with a buffer of children, carrying the
 * position it came from so errors can point at it.
 *
 * Supported: line comments (;), lists, symbols, strings with \n \t \r \\ \" and
 * \xHH...; escapes, decimal and #x integers, decimal floats with exponents,
 * SRFI-270 hex floats (#x1.921fb54442d18p+1), R7RS character literals as
 * integer Unicode scalar values, and #u8(...) bytevectors of numeric bytes.
 *
 * A string escape names a Unicode character, so "\x09;" is one byte and
 * "\xe9;" is the two bytes of its utf-8 encoding. A data segment that needs
 * particular bytes rather than particular characters uses #u8(...), where each
 * element is written as a byte and stored as one.
 *
 * Not supported, because no MARV input uses it: #| |# and #; comments, vectors,
 * booleans, quote/quasiquote, |bar| symbols, dotted pairs,
 * exactness and radix prefixes other than #x.
 */

#pragma once

/* node types */
typedef enum snt {
  SN_NULL = 0,
  SN_LIST,      /* (...) */
  SN_SYM,       /* foo, $foo, i32.const, ... */
  SN_INT,       /* 42, -1, #x2a, #\space */
  SN_FLT,       /* 1.5, 1e3, #x1.8p+1 */
  SN_STR,       /* "...", bytes after utf-8 encoding */
  SN_BVEC       /* #u8(0 1 255), bytes as written */
} snt_t;

typedef buf_t snbuf_t;

typedef struct snode {
  snt_t nt;
  sym_t fname;        /* file the node was read from */
  int line;           /* 1-based line */
  int col;            /* 1-based column of the first character */
  sym_t sym;          /* SN_SYM */
  long long ival;     /* SN_INT */
  double dval;        /* SN_FLT */
  buf_t data;         /* SN_STR: bytes, not 0-terminated */
  snbuf_t body;       /* SN_LIST: children */
} snode_t;

extern snode_t mksn(void);
extern snode_t *sninit(snode_t *pn);
extern void snfini(snode_t *pn);
extern void snclear(snode_t *pn);
extern snode_t *sncpy(snode_t *pdst, const snode_t *psrc);

#define snbinit(mem) (bufinit(mem, sizeof(snode_t)))
extern void snbfini(snbuf_t *pb);
#define snblen(pb) (buflen(pb))
#define snbref(pb, i) ((snode_t*)bufref(pb, i))
#define snlen(pn) (snblen(&(pn)->body))
#define snref(pn, i) (snbref(&(pn)->body, i))
extern snode_t *snnewbk(snode_t *pn);

/* reader state over one file */
typedef struct sread {
  FILE *fp;
  sym_t fname;
  int line;
  int col;
  int ch;             /* lookahead character, or EOF */
  int tline;          /* position of the token being read */
  int tcol;
  cbuf_t tok;         /* scratch for the token being read */
} sread_t;

extern bool sropen(sread_t *pr, const char *fname);
extern void srclose(sread_t *pr);
/* read one top-level form; false at end of file */
extern bool srform(sread_t *pr, snode_t *pn);

/* Reading a list one element at a time. MARV input is one huge (module ...)
 * form, so reading whole top-level forms would hold the entire module in
 * memory; these let the caller take a member, compile it, and reuse the node.
 *   srlopen  consumes the '(' and leaves the reader inside the list
 *   srlnext  reads the next member into pn, or consumes the ')' and says false
 */
extern bool srlopen(sread_t *pr, snode_t *pn);
extern bool srlnext(sread_t *pr, snode_t *pn);
/* report an error at a node or at the reader's position, and exit */
extern void sneprintf(const snode_t *pn, const char *fmt, ...);
extern void sreprintf(sread_t *pr, const char *fmt, ...);

/* ===================================================================== */
/* Types, the instruction table, and the three-address IR the emitters
 * consume. The unstacker that produces it follows the Scheme implementation's
 * design: the abstract stack holds operand descriptors rather than temporaries,
 * so a local, an immediate or a frame address flows into an instruction
 * without a copy, and a copy is forced only when something would otherwise be
 * observed after it changed.  [cco] */

typedef enum vt {
  VT_VOID = 0, VT_I32, VT_I64, VT_F32, VT_F64, VT_PTR, VT_ANY
} vt_t;

extern const char *vtname(vt_t vt);
extern vt_t vt_of_sym(sym_t s);

/* instruction kinds, in the order the table uses them */
typedef enum ik {
  IK_CONST, IK_REF_DATA, IK_REF_FUNC, IK_FRAME_ADDR,
  IK_LOCAL_GET, IK_LOCAL_SET, IK_LOCAL_TEE, IK_GLOBAL_GET,
  IK_GLOBAL_SET, IK_NOP, IK_UNREACHABLE, IK_DROP,
  IK_SELECT, IK_BR, IK_BR_IF, IK_BR_TABLE,
  IK_RETURN, IK_CALL, IK_CALL_INDIRECT, IK_RETURN_CALL,
  IK_RETURN_CALL_INDIRECT, IK_BINARY, IK_UNARY, IK_COMPARE,
  IK_CONVERT, IK_LOAD, IK_STORE, IK_MEMORY,
  IK_COUNT
} ik_t;

typedef struct instr {
  const char *name;
  ik_t kind;
  int nimm;           /* immediate operands written in the form */
  int nops;           /* stack operands */
  vt_t ops[3];
  vt_t res;           /* VT_VOID: none; VT_ANY: taken from context */
  int size;           /* access size in bytes, 0 when not a memory op */
} instr_t;

extern const instr_t *g_instrs;   /* wcpl takes no unsized global array */
extern const int g_ninstrs;
extern void init_instrs(void);
extern const instr_t *instr_lookup(sym_t name);

/* operands */
typedef enum ok {
  OK_NONE = 0,
  OK_IMM,             /* integer or float constant */
  OK_FRAME,           /* (frame.addr K) */
  OK_DATA,            /* (ref.data $d), with an offset */
  OK_FUNC,            /* (ref.func $f) */
  OK_VREG             /* a parameter, local or temporary */
} ok_t;

typedef struct opnd {
  ok_t kind;
  vt_t vt;
  long long ival;     /* IMM value, FRAME offset, DATA offset */
  double dval;        /* IMM when vt is f32/f64 */
  sym_t sym;          /* DATA, FUNC, VREG name */
} opnd_t;

/* three-address instructions */
typedef enum iop {
  IR_MOV, IR_UNARY, IR_BINARY, IR_SELECT, IR_LOAD, IR_STORE,
  IR_GGET, IR_GSET, IR_LABEL, IR_JUMP, IR_BRFALSE, IR_BRTRUE, IR_BRTABLE,
  IR_LOOPBEG, IR_LOOPEND, IR_UNREACHABLE,
  IR_CALL, IR_CALLI, IR_RETCALL, IR_RETCALLI, IR_RETURN,
  IR_TJUMP,                /* a return_call that also resets the stack */
  IR_TJUMPI,               /* the same, to a callee given by value */
  IR_MEMORY                /* a run of bytes copied or filled at once */
} iop_t;

typedef struct ir {
  iop_t op;
  sym_t name;         /* instruction, callee, global, or label */
  sym_t sig;          /* a trampoline_jump's chain shape, in letters */
  vt_t vt;
  bool hasdst;
  opnd_t dst;
  buf_t args;         /* opnd_t: operands, arguments, or branch labels */
  long long off;      /* load/store offset */
  int aln;            /* load/store alignment hint */
  int line, col;
} ir_t;

/* a function, flattened */
typedef struct fn {
  sym_t name;
  sym_t expname;
  vt_t res;
  long long frame;
  buf_t vars;         /* var_t: parameters, then locals, then temporaries */
  int nparams;
  buf_t code;         /* ir_t */
  bool variadic;
  int ntemp, nlabel;
} fn_t;

typedef struct var { sym_t name; vt_t vt; } var_t;

/* what the module and its interfaces declare */
typedef struct sig {
  sym_t name;
  sym_t cname;        /* <module>_<name>, or <interface>_<name> */
  sym_t woname;       /* $<module>:<name> for the wo target */
  bool local;         /* defined by this module rather than imported */
  bool exported;      /* and visible outside it */
  bool variadic;      /* takes a trailing $ap$ slot array */
  bool used;          /* referred to by this module */
  int useno;          /* and in which order it was first referred to */
  vt_t res;
  buf_t ptypes;
} sig_t;

/* a record and its fields, laid out as the Scheme implementation lays them:
 * each field at its own alignment, the whole rounded up to the widest */
typedef struct fld {
  sym_t name;
  vt_t vt;            /* the type a load of it yields */
  long long off, size, align;
  sym_t lop, sop;     /* the load and store instructions; 0 for an aggregate */
} fld_t;

typedef struct rec {
  sym_t name;
  long long size, align;
  buf_t fields;       /* fld_t */
} rec_t;

/* an assembler macro: a template with declared private locals and labels */
typedef struct mac {
  sym_t name;
  buf_t params;       /* sym_t */
  buf_t locals;       /* var_t, renamed on every expansion */
  buf_t labels;       /* sym_t, renamed on every expansion */
  buf_t ein, eout;    /* vt_t: the declared (effect (ins) -> (outs)) */
  snode_t body;
} mac_t;

typedef struct mod {
  sym_t name;
  buf_t sigs;         /* sig_t: functions, from this module and its interfaces */
  buf_t globals;      /* glb_t */
  buf_t consts;       /* cst_t */
  buf_t records;      /* rec_t */
  buf_t macros;       /* mac_t */
  buf_t datas;        /* dat_t: data segments, ours and imported */
  buf_t tramps;       /* sym_t: the signature of each (trampoline_func ...) */
  bool hasmain;       /* it exports main, so it needs the crt wrapper */
} mod_t;

typedef struct dat {
  sym_t name; sym_t cname; sym_t woname;
  bool local, used, isconst;
  int useno;
  long long align, size;   /* what an import of it has to state */
} dat_t;

/* a pointer slot inside a (data ...) segment that only a link or a startup can
 * fill: C has no static form for "the address of X plus K, here".  [cco] */
typedef struct drel {
  size_t off;         /* byte offset of the eight-byte slot */
  sym_t sym;          /* the data or function it names, as written */
  long long addend;   /* added to a data address; always 0 for a function */
  bool isfunc;
} drel_t;

/* A numeric run retains its type, position and values independently of raw
 * bytes. C emits native-typed initializers; WebAssembly and RV64 request a
 * little-endian byte image from the same data declaration. */
typedef enum dnum_kind { DNUM_INT, DNUM_F32, DNUM_F64 } dnum_kind_t;
typedef struct dnum {
  size_t off, count, first;
  int width;
  dnum_kind_t kind;
} dnum_t;

/* A data clause's byte/zero image, numeric runs and pointer relocations. */
typedef struct dseg {
  sym_t name;
  const char *expname;  /* (export "s"), or NULL */
  long long align;
  bool isconst;
  bool allzero;         /* byte image is zero, independent of numeric runs */
  buf_t bytes;          /* char */
  buf_t nums;           /* dnum_t, numeric items wider than a byte */
  buf_t numvals;        /* unsigned long long: integer or IEEE bit patterns */
  buf_t relocs;         /* drel_t */
} dseg_t;

extern void dseg_read(snode_t *pn, dseg_t *pd, bool little_endian_image);
/* a (global ...) clause's mutability and its initial value, which a target
 * has to emit: dropping it would change what the program means */
typedef struct glb_init {
  bool ismut;
  bool hasval;        /* an initializer was written */
  bool isflo;
  long long ival;
  double dval;
} glb_init_t;
extern void glb_read(snode_t *pn, vt_t vt, glb_init_t *pg);
extern void dseg_fini(dseg_t *pd);

typedef struct glb {
  sym_t name; sym_t cname; sym_t woname; vt_t vt;
  bool local, used; int useno;
} glb_t;

typedef struct cst {
  sym_t name; vt_t vt;
  bool isnum; long long ival; double dval;
  snode_t val;        /* when it is an expression rather than a number */
} cst_t;

extern sig_t *mod_sig(mod_t *pm, sym_t name);
extern sym_t mod_dataname(mod_t *pm, sym_t name);
extern sym_t mod_datawoname(mod_t *pm, sym_t name);
extern sym_t mod_globalname(mod_t *pm, sym_t name);
extern sym_t mod_globalwoname(mod_t *pm, sym_t name);
/* the one rule that turns a MARV name into C identifier characters */
extern void c_mangle(const char *s, cbuf_t *pcb);
/* mark what a lowered body refers to, so only that is imported */
extern void mod_scan_uses(mod_t *pm, snode_t *pn);
/* how many imports the scan marked, so an emitter can walk them in the
 * order the module first mentions each one */
extern int mod_usemax(void);
/* put the fresh-name counter back, before the pass that emits */
extern void mod_reset_fresh(void);

/* The letters a chain's signature is named by: one per type, result
 * first.  (param i64 i64 ptr) (result ptr) is "pllp", and the area a
 * trampoline_jump of that shape writes is $tc$pllp. */
extern sym_t sig_letters(sig_t *ps);
extern sym_t tramp_area_name(sym_t letters);
/* how many argument slots an area of this shape holds */
extern int sig_nparams(mod_t *pm, sym_t letters);
extern void mod_init(mod_t *pm);
extern void mod_fini(mod_t *pm);
/* load an interface file, recording what it declares */
extern bool mod_iface(mod_t *pm, const char *path);
/* Resolving an (include ...) needs the search paths, which belong to the
 * driver, so the driver defines this and an interface's own nested
 * includes come back through it. */
extern void mod_include(mod_t *pm, const char *src, snode_t *pn);

/* The search paths a lookup walks, and the lookup itself.  These live
 * here because an interface's own includes are resolved while it is being
 * read, and WCPL links by module: the reader cannot call up into the
 * driver. */
extern buf_t g_tdirs;             /* -T, where a .mvt is looked up */
extern buf_t g_ldirs;             /* -L, system .mvh and its .c adapter */
extern buf_t g_idirs;             /* -I, a local .mvh */
extern void dirs_add(buf_t *pb, const char *d);
extern void dirs_init(void);
extern void dirs_fini(void);
extern bool file_readable(const char *path);
extern void path_dir(const char *path, cbuf_t *pcb);
extern bool find_in_dirs(buf_t *pb, const char *first, const char *name,
                         const char *ext, cbuf_t *pcb);
extern bool adapter_of(const char *mvh, cbuf_t *pcb);
/* what the compile just done pulled in */
extern void sys_reset(void);
extern size_t sys_count(void);
extern const char *sys_name(size_t i);
extern const char *sys_path(size_t i);
/* scan a module member for what later members may refer to */
extern void mod_declare(mod_t *pm, snode_t *pn);

extern void fn_init(fn_t *pf);
extern void fn_fini(fn_t *pf);
/* flatten one (func ...) form; false when the form is not a function */
extern bool fn_unstack(mod_t *pm, snode_t *pn, fn_t *pf);
/* the lowered body, which the .wo emitter walks directly */
extern void fn_lower(mod_t *pm, snode_t *pn, size_t first, fn_t *pf, snode_t *out);
/* parse a (func ...) header; returns where its body starts */
extern size_t fn_header(mod_t *pm, snode_t *pn, fn_t *pf);
