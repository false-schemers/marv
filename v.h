/* v.h (marv emitters) -- [cco]
 *
 * What the three writers in v.c expose, and the form writer that puts a form
 * back the way it was read.
 */

#pragma once

/* write a form, in the syntax the reader accepts */
extern void snwrite(const snode_t *pn, FILE *fp);

/* print three-address code, for reading and for checking the unstacker */
extern void ir_write(const fn_t *pf, FILE *fp);

/* the cc target: the module prologue, then a function at a time */
extern void cc_prologue_write(FILE *fp);
extern void cc_decls_write(const mod_t *pm, FILE *fp);
extern void cc_func(const mod_t *pm, const fn_t *pf, FILE *fp);

extern void cc_static(const mod_t *pm, snode_t *pn, FILE *fp);
extern void cc_data_init(const mod_t *pm, snode_t *pn, FILE *fp);

/* the wo target: the module wrapper, its statics, and a function at a time */
extern void wo_prologue(mod_t *pm, FILE *fp);
extern void wo_static(mod_t *pm, snode_t *pn, FILE *fp);
extern void wo_func(mod_t *pm, fn_t *pf, snode_t *body, FILE *fp);
extern void wo_epilogue(FILE *fp);

/* the rv target: the file header, its statics, and a function at a time */
extern void rv_prologue(mod_t *pm, FILE *fp);
extern void rv_static(mod_t *pm, snode_t *pn, FILE *fp);
extern void rv_text(FILE *fp);
extern void rv_func(mod_t *pm, fn_t *pf, FILE *fp);

/* the areas a trampoline_jump writes, and the loop a chain resets to */
extern void cc_tramp_areas(const mod_t *pm, FILE *fp);
extern void cc_tramp_func(const mod_t *pm, fn_t *pf, FILE *fp);
