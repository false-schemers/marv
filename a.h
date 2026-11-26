/* a.h (marv driver) -- [cco]
 *
 * The driver owns the one place that knows how an external command runs. On
 * the WCPL target there is no system(), so commands are written to stdout for
 * the caller to pipe into a shell; natively, -n asks for the same thing.
 */

#pragma once

/* which of the three targets a .mvt describes */
typedef enum tt { TT_CC, TT_WO, TT_RV } tt_t;

/* how that target spells the start of a comment */
extern const char *tt_comment(tt_t t);

extern int g_dryrun;                  /* -n: print commands, run nothing */

/* run a command, or print it; returns an exit status (0 when printed) */
extern int runcmd(const char *cmd);
/* delete a file, or print the command that would */
extern int delfile(const char *path);
/* peak memory, where the host can tell us */
extern void report_peak(void);

/* sys_count and sys_rel, which an emitter writes onto an object's first
 * line, are declared in r.h with the rest of the lookup machinery. */
