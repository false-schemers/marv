# MARV Standard Library (`lib/*.mvh` & `lib/*.c`)

The `lib/` directory contains MARV's standard library interface headers (`lib/*.mvh`) and their corresponding single-source C adapter implementations (`lib/*.c`).

This directory is MARV's cover for the host C library. The runtime support
for Scheme code compiled to MARV lives in [`srt/`](../srt/README.md), which
is maintained with the S-MARV compiler rather than with MARV.

Include a system module at the top of a `.mvs` or `.mvh` file using:

```scheme
(include system $stdio)
(include system $stdlib)
(include system $sys.stat)
```

In compile-and-link mode, `marv` automatically resolves each included system `.mvh` header (`lib/<mod>.mvh`) to its companion `lib/<mod>.c` adapter file in the same directory and links it into the target binary.

---

## Type & Calling Convention Mapping in Prototypes Below

In the informal C-style prototypes below:
- `i32` / `int32_t`, `i64` / `int64_t`, `f32` / `float`, `f64` / `double`, and `ptr` / `void *` denote MARV's five scalar types.
- **Pointer arrays in memory** (such as `dirent **namelist` or `char **endptr`) use MARV's uniform **8-byte `ptr` slot** in memory across all targets.
- **By-value record returns** (`div_t`, `ldiv_t`, `lldiv_t`, `vec3`) use MARV's `rp$` destination-pointer convention: at the MARV call site, pass the destination `ptr` as the first argument (`(call $div dst_ptr num denom)`).
- **Variadic functions** (`...`) use MARV's `ap$` slot-array convention: at the MARV call site, simply pass the extra scalar arguments (`(call $snprintf buf size fmt arg1 arg2 ...)`).

---

## 1. `ctype` (`lib/ctype.mvh`, `lib/ctype.c`)

Character classification and case conversion.

```c
i32 isalnum(i32 c);
i32 isalpha(i32 c);
i32 isascii(i32 c);
i32 isblank(i32 c);
i32 iscntrl(i32 c);
i32 isdigit(i32 c);
i32 isgraph(i32 c);
i32 islower(i32 c);
i32 isprint(i32 c);
i32 ispunct(i32 c);
i32 isspace(i32 c);
i32 isupper(i32 c);
i32 isxdigit(i32 c);
i32 tolower(i32 c);
i32 toupper(i32 c);
```

---

## 2. `dirent` (`lib/dirent.mvh`, `lib/dirent.c`)

Directory stream traversal and sorting (`scandir` / `alphasort`).

### Constants (`i32`)
- `$DT_UNKNOWN` (`0`), `$DT_BLK` (`1`), `$DT_CHR` (`2`), `$DT_DIR` (`3`), `$DT_REG` (`4`), `$DT_FIFO` (`6`), `$DT_LNK` (`7`)

### Record `$dirent` (272 bytes, align 8)
```c
struct dirent {
  i64  d_ino;        // offset 0
  u8   d_type;       // offset 8
  char d_name[256];  // offset 9 (NUL-terminated)
};
```

### Functions
```c
ptr  fdopendir(i32 fd);
i32  fdclosedir(ptr dirp);
ptr  opendirat(i32 dirfd, ptr dirname);
ptr  opendir(ptr dirname);
i32  dirfd(ptr dirp);
ptr  readdir(ptr dirp);
void rewinddir(ptr dirp);
i64  telldir(ptr dirp);
void seekdir(ptr dirp, i64 loc);
i32  closedir(ptr dirp);

// Field accessors on a `struct dirent *` pointer:
i64  d_ino(ptr entry);
i32  d_type(ptr entry);
i32  d_namlen(ptr entry);
ptr  d_name(ptr entry);

// Directory scanning & sorting (namelist is an array of 8-byte ptr slots):
i32  alphasort(ptr d1_slot_ptr, ptr d2_slot_ptr);
i32  scandirat(i32 dirfd, ptr dirname, ptr namelist_out, ptr filter_fn, ptr compar_fn);
i32  scandir(ptr dirname, ptr namelist_out, ptr filter_fn, ptr compar_fn);
void freelist(ptr namelist, i32 count);
```

---

## 3. `errno` (`lib/errno.mvh`, `lib/errno.c`)

System error numbers and the mutable `$errno:errno` global.

### Global
```c
extern i32 errno; // Accessed via (global.get $errno) / (global.set $errno val)
```

### Constants (`i32`)
- `$ESUCCESS` (`0`), `$E2BIG` (`1`), `$EACCES` (`2`), `$EAGAIN` (`6`), `$EBADF` (`8`), `$EBUSY` (`10`), `$ECHILD` (`12`), `$EDEADLK` (`16`), `$EDOM` (`18`), `$EEXIST` (`20`), `$EFAULT` (`21`), `$EFBIG` (`22`), `$EILSEQ` (`25`), `$EINTR` (`27`), `$EINVAL` (`28`), `$EIO` (`29`), `$EISDIR` (`31`), `$ELOOP` (`32`), `$EMFILE` (`33`), `$EMLINK` (`34`), `$ENAMETOOLONG` (`37`), `$ENFILE` (`41`), `$ENODEV` (`43`), `$ENOENT` (`44`), `$ENOEXEC` (`45`), `$ENOLCK` (`46`), `$ENOMEM` (`48`), `$ENOSPC` (`51`), `$ENOSYS` (`52`), `$ENOTDIR` (`54`), `$ENOTEMPTY` (`55`), `$ENOTTY` (`59`), `$ENXIO` (`60`), `$EOVERFLOW` (`61`), `$EPERM` (`63`), `$EPIPE` (`64`), `$ERANGE` (`68`), `$EROFS` (`69`), `$ESPIPE` (`70`), `$ESRCH` (`71`), `$EXDEV` (`75`)

---

## 4. `fcntl` (`lib/fcntl.mvh`, `lib/fcntl.c`)

File descriptor opening and control operations.

### Constants (`i32`)
- Open flags: `$O_RDONLY` (`#x04000000`), `$O_WRONLY` (`#x10000000`), `$O_RDWR` (`#x14000000`), `$O_APPEND` (`#x0001`), `$O_NONBLOCK` (`#x0004`), `$O_SYNC` (`#x0010`), `$O_CREAT` (`#x1000`), `$O_DIRECTORY` (`#x2000`), `$O_EXCL` (`#x4000`), `$O_TRUNC` (`#x8000`)
- Commands: `$F_DUPFD` (`0`), `$F_GETFD` (`1`), `$F_SETFD` (`2`), `$F_GETFL` (`3`), `$F_SETFL` (`4`), `$FD_CLOEXEC` (`1`), `$AT_FDCWD` (`-2`)

### Functions
```c
i32 open(ptr path, i32 oflags);
i32 openat(i32 dirfd, ptr path, i32 oflags);
i32 creat(ptr path, i32 mode);
i32 fcntl(i32 fd, i32 cmd, ...);
```

---

## 5. `fenv` (`lib/fenv.mvh`, `lib/fenv.c`)

Floating-point environment and rounding modes.

### Constants (`i32`)
- `$FE_TONEAREST` (`0`), `$FE_DOWNWARD` (`1`), `$FE_UPWARD` (`2`), `$FE_TOWARDZERO` (`3`)
- `$FE_ALL_EXCEPT` (`0`)

### Functions
```c
void feclearexcept(i32 excepts);
void fegetexceptflag(ptr flagp, i32 excepts);
void feraiseexcept(i32 excepts);
void fesetexceptflag(ptr flagp, i32 excepts);
i32  fetestexcept(i32 excepts);
i32  fegetround(void);
i32  fesetround(i32 round);
void fegetenv(ptr envp);
i32  feholdexcept(ptr envp);
void fesetenv(ptr envp);
void feupdateenv(ptr envp);
```

---

## 6. `math` (`lib/math.mvh`, `lib/math.c`)

Double-precision mathematical functions and constants.

### Constants (`f64`)
- `$M_E`, `$M_LOG2E`, `$M_LOG10E`, `$M_LN2`, `$M_LN10`, `$M_PI`, `$M_PI_2`, `$M_PI_4`, `$M_1_PI`, `$M_2_PI`, `$M_2_SQRTPI`, `$M_SQRT2`, `$M_SQRT1_2`

### Functions
```c
f64 acos(f64 x);
f64 acosh(f64 x);
f64 asin(f64 x);
f64 asinh(f64 x);
f64 atan(f64 x);
f64 atan2(f64 y, f64 x);
f64 atanh(f64 x);
f64 cbrt(f64 x);
f64 ceil(f64 x);
f64 copysign(f64 x, f64 y);
f64 cos(f64 x);
f64 cosh(f64 x);
f64 erf(f64 x);
f64 erfc(f64 x);
f64 exp(f64 x);
f64 exp2(f64 x);
f64 expm1(f64 x);
f64 fabs(f64 x);
f64 fdim(f64 x, f64 y);
f64 floor(f64 x);
f64 fma(f64 x, f64 y, f64 z);
f64 fmax(f64 x, f64 y);
f64 fmin(f64 x, f64 y);
f64 fmod(f64 x, f64 y);
f64 frexp(f64 x, ptr exp_i32_out);
f64 hypot(f64 x, f64 y);
i32 ilogb(f64 x);
f64 ldexp(f64 x, i32 exp);
f64 lgamma(f64 x);
f64 log(f64 x);
f64 log10(f64 x);
f64 log1p(f64 x);
f64 log2(f64 x);
f64 logb(f64 x);
f64 modf(f64 x, ptr iptr_f64_out);
f64 nearbyint(f64 x);
f64 nextafter(f64 x, f64 y);
f64 pow(f64 x, f64 y);
f64 remainder(f64 x, f64 y);
f64 remquo(f64 x, f64 y, ptr quo_i32_out);
f64 rint(f64 x);
f64 round(f64 x);
f64 scalbn(f64 x, i32 n);
f64 sin(f64 x);
f64 sinh(f64 x);
f64 sqrt(f64 x);
f64 tan(f64 x);
f64 tanh(f64 x);
f64 tgamma(f64 x);
f64 trunc(f64 x);
```

---

## 7. `math.vec` (`lib/math.vec.mvh`, `lib/math.vec.c`)

3D vector math module demonstrating nested module naming (`$math.vec`) and by-value record returns (`rp$`).

### Record `$vec3` (24 bytes, align 8)
```c
struct vec3 {
  f64 x; // offset 0
  f64 y; // offset 8
  f64 z; // offset 16
};
```

### Functions
```c
f64  dot(ptr a, ptr b);
vec3 cross(ptr a, ptr b); // Call via: (call $cross dst_ptr a_ptr b_ptr)
f64  norm(ptr v);
```

---

## 8. `stdio` (`lib/stdio.mvh`, `lib/stdio.c`)

Buffered stream I/O and formatted printing/scanning.

### Constants & Standard Streams
- `$EOF` (`-1`), `$BUFSIZ` (`1024`), `$FOPEN_MAX` (`20`), `$FILENAME_MAX` (`260`)
- `$SEEK_SET` (`0`), `$SEEK_CUR` (`1`), `$SEEK_END` (`2`)
- `$_IONBF` (`4`), `$_IOLBF` (`64`), `$_IOFBF` (`0`)
- Standard stream expressions (`(const $stdin)`, `(const $stdout)`, `(const $stderr)`), backed by `$stdio:_iob` (`+0`, `+32`, `+64`).

### Functions
```c
ptr  fopen(ptr filename, ptr mode);
ptr  freopen(ptr filename, ptr mode, ptr stream);
ptr  fdopen(i32 fd, ptr mode);
i32  fclose(ptr stream);
i32  fflush(ptr stream);
void setbuf(ptr stream, ptr buf);
i32  setvbuf(ptr stream, ptr buf, i32 mode, i32 size);

i32  fread(ptr ptr_buf, i32 size, i32 nmemb, ptr stream);
i32  fwrite(ptr ptr_buf, i32 size, i32 nmemb, ptr stream);

i32  fgetc(ptr stream);
i32  ungetc(i32 c, ptr stream);

i32  fputc(i32 c, ptr stream);

ptr  fgets(ptr s, i32 n, ptr stream);
i32  fputs(ptr s, ptr stream);
i32  puts(ptr s);

i32  fseek(ptr stream, i32 offset, i32 whence);
i32  ftell(ptr stream);
void rewind(ptr stream);
i32  fgetpos(ptr stream, ptr pos);
i32  fsetpos(ptr stream, ptr pos);

void perror(ptr s);

i32  remove(ptr pathname);
i32  rename(ptr oldpath, ptr newpath);
i32  renameat(i32 oldfd, ptr oldpath, i32 newfd, ptr newpath);

// Variadic formatted output & input:
i32  printf(ptr format, ...);
i32  fprintf(ptr stream, ptr format, ...);
i32  sprintf(ptr s, ptr format, ...);
i32  snprintf(ptr s, i32 n, ptr format, ...);

i32  vprintf(ptr format, ptr ap);
i32  vfprintf(ptr stream, ptr format, ptr ap);
i32  vsprintf(ptr s, ptr format, ptr ap);
i32  vsnprintf(ptr s, i32 n, ptr format, ptr ap);

i32  scanf(ptr format, ...);
i32  fscanf(ptr stream, ptr format, ...);
i32  sscanf(ptr s, ptr format, ...);
i32  snscanf(ptr s, i32 count, ptr format, ...);
i32  vscanf(ptr format, ptr ap);
i32  vfscanf(ptr stream, ptr format, ptr ap);
i32  vsscanf(ptr s, ptr format, ptr ap);
i32  vsnscanf(ptr s, i32 count, ptr format, ptr ap);
```

---

## 9. `stdlib` (`lib/stdlib.mvh`, `lib/stdlib.c`)

Memory allocation, number parsing, sorting/searching, integer division records, and process control.

### Constants (`i32`)
- `$EXIT_SUCCESS` (`0`), `$EXIT_FAILURE` (`1`), `$RAND_MAX` (`2147483647`)

### Records
```c
struct div_t   { i32 quot; i32 rem; }; // 8 bytes, align 4
struct ldiv_t  { i32 quot; i32 rem; }; // 8 bytes, align 4
struct lldiv_t { i64 quot; i64 rem; }; // 16 bytes, align 8
```

### Functions
```c
f64     atof(ptr nptr);
i32     atoi(ptr nptr);
i32     atol(ptr nptr);
i64     atoll(ptr nptr);

f64     strtod(ptr nptr, ptr endptr_slot);
f32     strtof(ptr nptr, ptr endptr_slot);
i32     strtol(ptr nptr, ptr endptr_slot, i32 base);
i32     strtoul(ptr nptr, ptr endptr_slot, i32 base);
i64     strtoll(ptr nptr, ptr endptr_slot, i32 base);
i64     strtoull(ptr nptr, ptr endptr_slot, i32 base);

i32     rand(void);
void    srand(i32 seed);

ptr     malloc(i32 size);
ptr     calloc(i32 nmemb, i32 size);
ptr     realloc(ptr p, i32 size);
void    free(ptr p);
ptr     aligned_alloc(i32 alignment, i32 size);

void    abort(void);
void    exit(i32 status);
void    _Exit(i32 status);
i32     atexit(ptr func);
ptr     getenv(ptr name);
i32     system(ptr command);

ptr     bsearch(ptr key, ptr base, i32 nmemb, i32 size, ptr compar);
void    qsort(ptr base, i32 nmemb, i32 size, ptr compar);

i32     abs(i32 j);
i32     labs(i32 j);
i64     llabs(i64 j);

div_t   div(i32 numer, i32 denom);     // Call via: (call $div dst_ptr numer denom)
ldiv_t  ldiv(i32 numer, i32 denom);    // Call via: (call $ldiv dst_ptr numer denom)
lldiv_t lldiv(i64 numer, i64 denom);   // Call via: (call $lldiv dst_ptr numer denom)
```

---

## 10. `string` (`lib/string.mvh`, `lib/string.c`)

Byte-buffer and NUL-terminated string operations.

```c
ptr  memcpy(ptr dst, ptr src, i32 n);
ptr  memmove(ptr dst, ptr src, i32 n);
ptr  memccpy(ptr dst, ptr src, i32 c, i32 n);
ptr  memset(ptr dst, i32 c, i32 n);
i32  memcmp(ptr s1, ptr s2, i32 n);
ptr  memchr(ptr s, i32 c, i32 n);
ptr  memmem(ptr haystack, i32 haystacklen, ptr needle, i32 needlelen);
void memswap(ptr m1, ptr m2, i32 n);
void bzero(ptr dst, i32 n);

ptr  strcpy(ptr dst, ptr src);
ptr  strncpy(ptr dst, ptr src, i32 n);
ptr  strcat(ptr dst, ptr src);
ptr  strncat(ptr dst, ptr src, i32 n);

i32  strcmp(ptr s1, ptr s2);
i32  strncmp(ptr s1, ptr s2, i32 n);
i32  strcoll(ptr s1, ptr s2);
i32  strxfrm(ptr dst, ptr src, i32 n);
i32  strcasecmp(ptr s1, ptr s2);
i32  strncasecmp(ptr s1, ptr s2, i32 n);

ptr  strdup(ptr s);
ptr  strndup(ptr s, i32 n);
ptr  strchr(ptr s, i32 c);
ptr  strrchr(ptr s, i32 c);
i32  strcspn(ptr s, ptr reject);
i32  strspn(ptr s, ptr accept);
ptr  strpbrk(ptr s, ptr accept);
ptr  strstr(ptr haystack, ptr needle);
ptr  strtok(ptr str, ptr delim);
ptr  strtok_r(ptr str, ptr delim, ptr saveptr_slot);

i32  strlen(ptr s);
i32  strnlen(ptr s, i32 maxlen);
ptr  strerror(i32 errnum);
```

---

## 11. `sys.stat` (`lib/sys.stat.mvh`, `lib/sys.stat.c`)

File status metadata and directory creation (`module "stat"`).

### Constants (`i32`)
- File type masks: `$S_IFMT` (`#o170000`), `$S_IFBLK` (`#o060000`), `$S_IFCHR` (`#o020000`), `$S_IFDIR` (`#o040000`), `$S_IFIFO` (`#o010000`), `$S_IFREG` (`#o100000`), `$S_IFLNK` (`#o120000`)
- Permission bits: `$S_IRWXU` (`#o0700`), `$S_IRUSR` (`#o0400`), `$S_IWUSR` (`#o0200`), `$S_IXUSR` (`#o0100`), `$S_IRWXG` (`#o0070`), `$S_IRWXO` (`#o0007`)

### Record `$stat` (80 bytes, align 8)
```c
struct stat {
  i64 st_dev;     // offset 0
  i64 st_ino;     // offset 8
  i32 st_mode;    // offset 16
  i32 _pad0;      // offset 20
  i64 st_nlink;   // offset 24
  i64 st_size;    // offset 32
  i64 st_atim;    // offset 40
  i64 st_mtim;    // offset 48
  i64 st_ctim;    // offset 56
  i64 st_blksize; // offset 64
  i64 st_blocks;  // offset 72
};
```

### Functions
```c
i32 stat(ptr pathname, ptr statbuf);
i32 lstat(ptr pathname, ptr statbuf);
i32 fstat(i32 fd, ptr statbuf);
i32 fstatat(i32 dirfd, ptr pathname, ptr statbuf, i32 flags);
i32 mkdir(ptr pathname, i32 mode);
i32 mkdirat(i32 dirfd, ptr pathname, i32 mode);
```

---

## 12. `time` (`lib/time.mvh`, `lib/time.c`)

Timekeeping, calendar conversion, and formatting.

### Constants (`i32`)
- `$CLOCKS_PER_SEC` (`1000000`)
- `$CLOCK_REALTIME` (`0`), `$CLOCK_MONOTONIC` (`1`), `$CLOCK_PROCESS_CPUTIME_ID` (`2`), `$CLOCK_THREAD_CPUTIME_ID` (`3`)

### Records
```c
struct timespec {
  i64 tv_sec;   // offset 0
  i32 tv_nsec;  // offset 8
}; // 16 bytes, align 8

struct tm {
  i32 tm_sec;    // offset 0
  i32 tm_min;    // offset 4
  i32 tm_hour;   // offset 8
  i32 tm_mday;   // offset 12
  i32 tm_mon;    // offset 16
  i32 tm_year;   // offset 20
  i32 tm_wday;   // offset 24
  i32 tm_yday;   // offset 28
  i32 tm_isdst;  // offset 32
}; // 36 bytes, align 4
```

### Functions
```c
i64 clock(void);
f64 difftime(i64 time1, i64 time0);
i64 mktime(ptr tm_ptr);
i64 time(ptr tloc_i64_out);
ptr asctime(ptr tm_ptr);
ptr asctime_r(ptr tm_ptr, ptr buf);
ptr ctime(ptr timep_i64);
ptr ctime_r(ptr timep_i64, ptr buf);
ptr gmtime(ptr timep_i64);
ptr gmtime_r(ptr timep_i64, ptr result_tm);
ptr localtime(ptr timep_i64);
ptr localtime_r(ptr timep_i64, ptr result_tm);
i32 strftime(ptr s, i32 max, ptr format, ptr tm_ptr);
i32 clock_gettime(i32 clk_id, ptr tp_timespec);
i32 clock_getres(i32 clk_id, ptr res_timespec);
```

---

## 13. `unistd` (`lib/unistd.mvh`, `lib/unistd.c`)

POSIX file descriptor and filesystem primitives.

### Constants (`i32`)
- `$STDIN_FILENO` (`0`), `$STDOUT_FILENO` (`1`), `$STDERR_FILENO` (`2`)
- `$F_OK` (`0`), `$X_OK` (`1`), `$W_OK` (`2`), `$R_OK` (`4`)
- `$SEEK_SET` (`0`), `$SEEK_CUR` (`1`), `$SEEK_END` (`2`)

### Functions
```c
i32 access(ptr pathname, i32 mode);
i32 faccessat(i32 dirfd, ptr pathname, i32 mode, i32 flags);
i32 close(i32 fd);
i32 dup(i32 oldfd);
i32 dup2(i32 oldfd, i32 newfd);
i32 fsync(i32 fd);
i32 fdatasync(i32 fd);
i32 ftruncate(i32 fd, i64 length);
i32 truncate(ptr path, i64 length);
i32 isatty(i32 fd);
i64 lseek(i32 fd, i64 offset, i32 whence);
i32 read(i32 fd, ptr buf, i32 count);
i32 pread(i32 fd, ptr buf, i32 count, i64 offset);
i32 write(i32 fd, ptr buf, i32 count);
i32 pwrite(i32 fd, ptr buf, i32 count, i64 offset);
i32 link(ptr oldpath, ptr newpath);
i32 linkat(i32 olddirfd, ptr oldpath, i32 newdirfd, ptr newpath, i32 flags);
i32 symlink(ptr target, ptr linkpath);
i32 symlinkat(ptr target, i32 newdirfd, ptr linkpath);
i32 readlink(ptr pathname, ptr buf, i32 bufsiz);
i32 readlinkat(i32 dirfd, ptr pathname, ptr buf, i32 bufsiz);
i32 unlink(ptr pathname);
i32 unlinkat(i32 dirfd, ptr pathname, i32 flags);
i32 rmdir(ptr pathname);
i32 chdir(ptr path);
ptr getcwd(ptr buf, i32 size);
i32 sleep(i32 seconds);
i32 usleep(i32 usec);
```
