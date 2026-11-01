/* Single-source MARV-ABI adapter for `stdio` (WCPL + C99 + RV64G) */
#if defined(__WCPL__)
#pragma module "stdio"
#include <wasi/api.h>
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <float.h>
#include <stdio.h>
#include <sys/intrs.h>

/* stream i/o after K&R TCPL */

static bool initialized = false;

/* by detault, use full buffering for stdin and stdout */
FILE _iob[FOPEN_MAX] = {
  { 0, NULL, NULL, NULL, _IOREAD,       0, {} }, /* stdin */
  { 0, NULL, NULL, NULL, _IOWRT,        1, {} }, /* stdout */
  { 0, NULL, NULL, NULL, _IOWRT|_IONBF, 2, {} }  /* stderr */
};

static FILE *findfp(void)
{
  FILE *fp;
  for (fp = &_iob[0]; fp < &_iob[FOPEN_MAX]; ++fp)
    if ((fp->flags & (_IOREAD|_IOWRT|_IORW)) == 0)
      return fp; /* found free slot */
  return NULL;
}

static void closeall(void)
{
  FILE *fpi;
  for (fpi = &_iob[0]; fpi < &_iob[FOPEN_MAX]; ++fpi) {
    if (fpi->flags & (_IOREAD|_IOWRT|_IORW)) 
      fclose(fpi);
  }
}

static void initialize(void)
{
  if (!initialized) {
    /* make stdin and stdout line-buffered if associated with a tty */
    if (isatty(STDIN_FILENO)) stdin->flags |= _IOLBF; 
    if (isatty(STDOUT_FILENO)) stdout->flags |= _IOLBF; 
    atexit(&closeall);
    initialized = true;
  }
}

FILE *freopen(const char *filename, const char *mode, FILE *fp)
{
  int plus, oflags, fd;
  if (!initialized) initialize();
  if (fp != NULL) fclose(fp); else fp = findfp();
  if (fp == NULL || filename == NULL || mode == NULL) return NULL;
  switch (*mode++) {
    case 'w': oflags = O_WRONLY | O_TRUNC | O_CREAT; break;
    case 'a': oflags = O_WRONLY | O_APPEND | O_CREAT; break;
    case 'r': oflags = O_RDONLY; break;
    default: return NULL;
  }
  while (*mode) switch (*mode++) {
    case '+': oflags |= O_RDWR; break;
    case 'x': oflags |= O_EXCL; break;
    default:; /* other flags are ignored */
  }
  if ((fd = open(filename, oflags)) < 0) return NULL;
  fp->fd = fd; fp->cnt = 0;
  fp->base = fp->ptr = fp->end = NULL;
  fp->flags = ((oflags & O_RDWR) == O_RDWR) ? _IORW : (oflags & O_RDONLY) ? _IOREAD : _IOWRT;
  if ((oflags & O_APPEND) && (oflags & O_RDWR) != O_RDWR && lseek(fd, 0L, SEEK_END) < 0) {
    fclose(fp); fp = NULL;
  }
  return fp;
}

FILE *fopen(const char *name, const char *mode)
{
  return freopen(name, mode, NULL);
}

FILE *fdopen(int fd, const char *mode)
{
  if (!initialized) initialize();
  FILE *fp = findfp();
  if (fp == NULL) return NULL;
  fp->fd = fd;
  fp->cnt = 0; fp->flags = 0;
  fp->base = fp->ptr = fp->end = NULL;
  switch (*mode++) {
    case 'r': fp->flags |= _IOREAD; break;
    case 'a': lseek(fd, 0L, 2); /* fall thru */
    case 'w': fp->flags |= _IOWRT; break;
    default: return NULL;
  }
  while (*mode) switch (*mode++) {
    case '+': fp->flags = (fp->flags & ~(_IOREAD|_IOWRT)) | _IORW; break;
    default:; /* other flags are ignored */
  }
  return fp;
}

int fclose(FILE *fp)
{
  int res = -1;
  if (fp == NULL) return res;
  if (fp->flags & (_IOREAD|_IOWRT|_IORW)) {
    res = (fp->flags & _IONBF)? 0 : fflush(fp);
    if (close(fp->fd) < 0) {
      errno = ENOENT;
      res = -1; 
    }
  }
  if (fp->base && fp->base != &fp->sbuf[0]) {
    if (fp->flags & _IOMYBUF) free(fp->base);
  }
  _bzero(fp, sizeof(FILE));
  return res;
}

static void addbuf(FILE *fp)
{
  if (fp->flags & _IONBF) {
    fp->base = (unsigned char *)&fp->sbuf[0];
    fp->end = fp->base + SBFSIZ;
  } else {
    fp->base = (unsigned char *)malloc(BUFSIZ + SBFSIZ);
    if (fp->base != NULL) {
      fp->end = fp->base + BUFSIZ + SBFSIZ;
      fp->flags |= _IOMYBUF;
    } else { 
      fp->base = (unsigned char *)&fp->sbuf[0]; 
      fp->end = fp->base + SBFSIZ; 
    }
  }
  fp->ptr = fp->base;
}

/* getc helper (returns next char); precondition: f->cnt is <= 0 */
int _fillbuf(FILE *fp)
{
  FILE *fpi; size_t readc;
  if (!initialized) initialize();
  if (fp->base == NULL) addbuf(fp);
  if (!(fp->flags & _IOREAD)) {
    if (fp->flags & _IORW) fp->flags |= _IOREAD;
    else return EOF;
  }
  /* if this device is a terminal (line-buffered) or unbuffered, then
   * flush buffers of all line-buffered devices currently writing */
  if (fp->flags & (_IOLBF|_IONBF))
    for (fpi = &_iob[0]; fpi < &_iob[FOPEN_MAX]; ++fpi) {
      if ((fpi->flags & (_IOREAD|_IOLBF)) == _IOLBF) 
        fflush(fpi);
    }
  fp->ptr = fp->base;
  readc = (fp->flags & _IONBF) ? 1 : fp->end - fp->base;
  fp->cnt = read(fp->fd, (char *)fp->base, readc);
  if (--fp->cnt >= 0) return (*fp->ptr++);
  else if (fp->cnt != -1) fp->flags |= _IOERR;
  else { /* EOF */
    fp->flags |= _IOEOF;
    if (fp->flags & _IORW) fp->flags &= ~_IOREAD;
  }
  fp->cnt = 0;
  return EOF;
}

size_t fread(void *buf, size_t size, size_t count, FILE *fp)
{
  char *ptr = buf; size_t nleft; ssize_t n;
  if (size == 0 || count == 0) return 0;
  nleft = count * size;
  for (;;) {
    if (fp->cnt <= 0) {
      if (_fillbuf(fp) == EOF)
        return (count - (nleft+size-1)/size);
      fp->ptr--; fp->cnt++; /* ungetc */
    }
    n = fp->cnt; if (nleft < (size_t)n) n = (ssize_t)nleft;
    _bcopy(ptr, (char *)fp->ptr, n);
    ptr += n; fp->ptr += n; fp->cnt -= n; 
    if (nleft <= (size_t)n) return count;
    nleft -= (size_t)n;
  }
  return 0; /* unreacheable */
}

static int writebuf(FILE *fp)
{
  unsigned char *base = fp->base;
  ssize_t n = fp->ptr - base;
  fp->ptr = base;
  fp->cnt = (fp->flags & (_IONBF|_IOLBF)) ? 0 : fp->end - base;
  if (n > 0 && n != write(fp->fd, (char*)base, (size_t)n)) {
    fp->flags |= _IOERR;
    return EOF;
  }
  return 0;
}

static int wrtchk(FILE *fp)
{
  if (!initialized) initialize();
  if ((fp->flags & (_IOWRT|_IOEOF)) != _IOWRT) {
    if (!(fp->flags & (_IOWRT|_IORW))) return EOF; /* read-only */
    fp->flags = fp->flags & ~_IOEOF | _IOWRT; /* clear flags */
  }
  if (fp->base == NULL) {
    /* first attempt to bufferize output */
    addbuf(fp);
  }
  if (fp->ptr == fp->base && !(fp->flags & (_IONBF|_IOLBF)))  {
    fp->cnt = fp->end - fp->base; /* first write since seek--set cnt */
  }
  return 0;
}

/* putc helper; precondition: f->cnt <= 0 */
int _flushbuf(int c, FILE *fp)
{
  unsigned char c1;
  if (wrtchk(fp) != 0) return EOF;
  do {
    /* check for linebuffered with write perm, but no EOF */
    if ((fp->flags & (_IOLBF|_IOWRT|_IOEOF)) == (_IOLBF|_IOWRT)) {
      if (fp->ptr >= fp->end) break; /* flush buf and be done */
      if ((*fp->ptr++ = c) != '\n') return c;
      return writebuf(fp) == EOF ? EOF : c;
    }
    /* write out an unbuffered file, if have write perm, but no EOF */
    if ((fp->flags & (_IONBF|_IOWRT|_IOEOF)) == (_IONBF|_IOWRT)) {
      c1 = c; fp->cnt = 0;
      if (write(fp->fd, (char *)&c1, 1) == 1) return c;
      fp->flags |= _IOERR;
      return EOF;
    }
  } while (fp->flags & (_IONBF|_IOLBF));
  writebuf(fp); /* full buffer: flush it */
  putc((char)c, fp); /* then put "c" in newly emptied buf */
  return ferror(fp) ? EOF : c;
}

size_t fwrite(const void *buf, size_t size, size_t count, FILE *fp)
{
  char *ptr = buf; size_t nleft; ssize_t n;
  unsigned char *cptr;
  if (size == 0 || count == 0 || wrtchk(fp)) return 0;
  nleft = count*size;
  /* if the file is unbuffered, or if the fp->ptr == fp->base, and there
   * is > BUFSIZ chars to write, we can do a direct write */
  if (fp->base >= fp->ptr) { /* this covers the unbuffered case, too */
    if ((fp->flags & _IONBF) != 0 || nleft >= BUFSIZ) {
      n = write(fp->fd, ptr, nleft);
      if (n != nleft) { fp->flags |= _IOERR; n = (n >= 0) ? n : 0; }
      return n/size;
    }
  }
  /* put characters in the buffer */
  for (;; ptr += n) {
    while ((n = fp->end - (cptr = fp->ptr)) <= 0) { /* full buf */
      if (writebuf(fp) == EOF) return count - (nleft + size - 1)/size;
    }
    if (nleft < n) n = (ssize_t)nleft;
    _bcopy(cptr, ptr, n);
    fp->cnt -= n; fp->ptr += n;
    /* done; flush if linebuffered with a newline */
    if ((nleft -= n) == 0) { 
      if (fp->flags & (_IOLBF|_IONBF)) {
        if ((fp->flags & _IONBF) || (memchr(fp->base, '\n', count*size) != NULL)) 
          writebuf(fp);
      }
      return (count);
    }
  }
  return 0; /* unreacheable */
}

int fputs(const char *s, FILE *fp)
{
  ssize_t ndone = 0, n;
  unsigned char *cptr; char *p;
  if (wrtchk(fp) != 0) return 0;
  if ((fp->flags & _IONBF) == 0) {
    for (;; s += n) {
      while ((n = fp->end - (cptr = fp->ptr)) <= 0) { /* full buf */
        if (writebuf(fp) == EOF) return EOF;
      }
      if ((p = memccpy((char *)cptr, s, '\0', n)) != NULL) n = (p - (char *)cptr) - 1;
      fp->cnt -= n; fp->ptr += n;
      ndone += n;
      if (p != NULL)  { 
        /* done; flush buffer if line-buffered */
        if (fp->flags & _IOLBF) if (writebuf(fp) == EOF) return EOF;
        return (int)ndone;
      }
    }
  } else {
    /* write out to an unbuffered file */
    size_t cnt = strlen(s);
    write(fp->fd, s, cnt);
    return (int)cnt;
  }
  return 0; /* unreacheable */
}

int puts(const char *s)
{
  if (fputs(s, stdout) == EOF) return EOF;
  return fputc('\n', stdout);
}

int fflush(FILE *fp)
{
  if (!(fp->flags & _IOWRT)) {
    fp->cnt = 0;
    return 0;
  }
  while (!(fp->flags & _IONBF) && (fp->flags & _IOWRT) &&
         (fp->base != NULL) && (fp->ptr > fp->base)) {
    writebuf(fp);
  }
  return ferror(fp) ? EOF : 0;
}

int fgetc(FILE *fp)
{
  return getc(fp);
}

int fputc(int c, FILE *fp)
{
  return putc(c, fp);
}

char *fgets(char *s, int size, FILE *fp)
{
  char *p, *s0 = s; ssize_t n;
  for (size--; size > 0; size -= (int)n) {
    if (fp->cnt <= 0) { /* empty buffer */
      if (_fillbuf(fp) == EOF) {
        if (s0 == s) return NULL;
        break; /* no more data */
      }
      fp->ptr--; fp->cnt++;
    }
    n = (size < fp->cnt) ? size : fp->cnt;
    if ((p = memccpy(s, fp->ptr, '\n', n)) != NULL) n = p - s;
    s += n;
    fp->cnt -= n; fp->ptr += n;
    if (p != NULL) break; /* found '\n' in buffer */
  }
  *s = '\0';
  return s0;
}

int ungetc(int c, FILE *fp)
{
  if (c == EOF) return -1;
  if ((fp->flags & _IOREAD) == 0 || fp->ptr <= fp->base) {
    if (fp->ptr == fp->base && fp->cnt == 0) ++fp->ptr;
    else return -1;
  }
  *--fp->ptr = c; ++fp->cnt;
  return c;
}

int fseek(FILE *fp, long offset, int origin)
{
  ssize_t c; long p;
  fp->flags &= ~_IOEOF;
  if (fp->flags & _IOREAD) {
    if (origin < 2 && fp->base != NULL && !(fp->flags & _IONBF)) {
      c = fp->cnt; p = offset;
      if (origin == 0) p += (long)c - (long)lseek(fp->fd, 0, SEEK_CUR);
      else offset -= (long)c;
      if (!(fp->flags & _IORW) && c > 0 && p <= c && p >= fp->base - fp->ptr) {
        fp->ptr += (int)p; fp->cnt -= (int)p;
        return 0;
      }
    }
    if (fp->flags & _IORW) {
      fp->ptr = fp->base;
      fp->flags &= ~_IOREAD;
    }
    p = (long)lseek(fp->fd, offset, origin);
    fp->cnt = 0;
  } else if (fp->flags & (_IOWRT|_IORW)) {
    fflush(fp);
    fp->cnt = 0;
    if (fp->flags & _IORW) {
      fp->flags &= ~_IOWRT;
      fp->ptr = fp->base;
    }
    p = (long)lseek(fp->fd, offset, origin);
  }
  return (p == -1) ? -1 : 0;
}

long ftell(FILE *fp)
{
  long offset; ssize_t adjust;
  if (fp->cnt < 0) fp->cnt = 0;
  if (fp->flags & _IOREAD) {
    adjust = - fp->cnt;
  } else if (fp->flags & (_IOWRT|_IORW)) {
    adjust = 0;
    if (fp->flags & _IOWRT && fp->base != NULL && (fp->flags & _IONBF) == 0)
      adjust = fp->ptr - fp->base;
  } else return -1;
  offset = (long)lseek(fp->fd, 0, SEEK_CUR);
  if (offset >= 0) offset += (long)adjust;
  return offset;
}

void rewind(FILE *fp)
{
  fflush(fp);
  lseek(fp->fd, 0, SEEK_SET);
  fp->cnt = 0; fp->ptr = fp->base;
  fp->flags &= ~(_IOERR|_IOEOF);
  if (fp->flags & _IORW) fp->flags &= ~(_IOREAD|_IOWRT);
}

int fgetpos(FILE *fp, fpos_t *ptr)
{
  static_assert(sizeof(fpos_t) == sizeof(off_t));
  off_t offset; ssize_t adjust;
  if (fp->cnt < 0) fp->cnt = 0;
  if (fp->flags & _IOREAD) {
    adjust = - fp->cnt;
  } else if (fp->flags & (_IOWRT|_IORW)) {
    adjust = 0;
    if (fp->flags & _IOWRT && fp->base != NULL && (fp->flags & _IONBF) == 0)
      adjust = fp->ptr - fp->base;
  } else return -1;
  offset = lseek(fp->fd, 0, SEEK_CUR);
  if (offset >= 0) offset += adjust;
  *ptr = offset;
  return 0;
}

int fsetpos(FILE *fp, const fpos_t *ptr)
{
  static_assert(sizeof(fpos_t) == sizeof(off_t));
  ssize_t c; off_t offset = *ptr, p;
  fp->flags &= ~_IOEOF;
  if (fp->flags & _IOREAD) {
    if (fp->base != NULL && !(fp->flags & _IONBF)) {
      c = fp->cnt; p = offset;
      p += (off_t)c - lseek(fp->fd, 0, SEEK_CUR);
      if (!(fp->flags & _IORW) && c > 0 && p <= c && p >= fp->base - fp->ptr) {
        fp->ptr += (int)p; fp->cnt -= (int)p;
        return 0;
      }
    }
    if (fp->flags & _IORW) {
      fp->ptr = fp->base;
      fp->flags &= ~_IOREAD;
    }
    p = lseek(fp->fd, offset, SEEK_SET);
    fp->cnt = 0;
  } else if (fp->flags & (_IOWRT|_IORW)) {
    fflush(fp);
    fp->cnt = 0;
    if (fp->flags & _IORW) {
      fp->flags &= ~_IOWRT;
      fp->ptr = fp->base;
    }
    p = lseek(fp->fd, offset, SEEK_SET);
  }
  return (p == -1) ? -1 : 0;
}

int setvbuf(FILE *fp, char *buf, int mode, size_t size)
{
  if (fp->base != NULL && fp->base != &fp->sbuf[0]) {
    if (fp->flags & _IOMYBUF) free(fp->base);
    fp->base = fp->end = NULL;
  }
  fp->flags &= ~(_IONBF|_IOLBF|_IOFBF|_IOMYBUF);
  switch (mode) {
    case _IONBF: /* file is unbuffered */
      fp->flags |= _IONBF;
      fp->base = fp->end = NULL;
      break;
    case _IOLBF:
    case _IOFBF:
      fp->flags |= mode;
      size = (size == 0) ? BUFSIZ : size;
      if (buf != NULL && size > SBFSIZ) {
        fp->base = (unsigned char *)buf;
        fp->end = fp->base + size - SBFSIZ;
      } else if (size > SBFSIZ && (fp->base = malloc(size + SBFSIZ)) != NULL) {
        fp->end = fp->base + size + SBFSIZ;
        fp->flags |= _IOMYBUF;
      } else {
        fp->base = (unsigned char *)&fp->sbuf[0]; 
        fp->end = fp->base + SBFSIZ;
      }
      break;
    default:
      return -1;
  }
  fp->ptr = fp->base;
  fp->cnt = 0;
  return 0;
}

void setbuf(FILE *fp, char *buf)
{
  setvbuf(fp, buf, buf == NULL ? _IONBF : _IOFBF, BUFSIZ);
}


/* formatted i/o helpers */

/* todo: use macros? */
static bool _is_oct_digit(char ch)
{
  return (ch >= '0') && (ch <= '7');
}

static bool _is_dec_digit(char ch)
{
  return (ch >= '0') && (ch <= '9');
}

static bool _is_hex_digit(char ch)
{
  return (ch >= '0') && (ch <= '9') 
      || (ch >= 'a') && (ch <= 'f')
      || (ch >= 'A') && (ch <= 'F');
}

/* length of the string (excluding the terminating 0) limited by 'maxsize' */
static unsigned int _strnlen_s(const char* str, size_t maxsize)
{
  const char* s;
  for (s = str; *s && maxsize--; ++s);
  return (unsigned int)(s - str);
}

static unsigned int _atoi(const char** str)
{
  unsigned i = 0U;
  while (_is_dec_digit(**str)) {
    i = i * 10U + (unsigned)(*((*str)++) - '0');
  }
  return i;
}


/* formatted input */

/* 'aton' conversion buffer size, this must be big enough to hold one converted
 *  integer number not including sign/prefixes (dynamically created on stack) */
#define SCANF_ATON_BUFFER_SIZE    32U

 /* 'atod' conversion buffer size, this must be big enough to hold one converted
  * floating-point number including prefixes (dynamically created on stack) */
#define SCANF_ATOD_BUFFER_SIZE    32U

/* internal size definitions */
#define SIZE_HH  (-2)
#define SIZE_H   (-1)
#define SIZE_DEF (0)
#define SIZE_L   (1)
#define SIZE_LL  (2)

/* input function type */
typedef int(*in_fct_t)(void *data, size_t idx, size_t maxlen, bool get);

/* internal buffer input */
static int _in_buffer(void *data, size_t idx, size_t maxlen, bool get)
{
  if (idx < maxlen) return ((char*)data)[idx];
  return EOF;
}

/* internal file input */
static int _in_file(void *data, size_t idx, size_t maxlen, bool get)
{
  FILE *fp = (FILE*)data;
  int c = getc(fp);
  if (c != EOF && !get) ungetc(c, fp);
  return c;
}

static void store_int(void *dst, int size, unsigned long long i)
{
  if (!dst) return;
  switch (size) {
    case SIZE_HH:       *(char*)dst = (char)i;      break;
    case SIZE_H:       *(short*)dst = (short)i;     break;
    case SIZE_DEF:       *(int*)dst = (int)i;       break;
    case SIZE_L:        *(long*)dst = (long)i;      break;
    case SIZE_LL:  *(long long*)dst = (long long)i; break;
  }
}

static void store_float(void *dst, int size, double f)
{
  if (!dst) return;
  switch (size) {
    case SIZE_DEF:     *(float*)dst = (float)f;     break;
    case SIZE_L:      *(double*)dst = (double)f;    break;
  }
}

/* internal aton for integers */
static unsigned long long _aton(in_fct_t in, void *data, const size_t maxlen, int base, int width, size_t *pidx)
{
  char buf[SCANF_ATON_BUFFER_SIZE];
  size_t len = 0, dc = 0; int c; bool gotsign = false;
  unsigned long long res; char *end;
  size_t maxidx = (width == 0) ? SIZE_MAX : *pidx + width;
  
  { /* scan chars up to maxidx */
    if (*pidx >= maxidx) goto stop;
    c = (*in)(data, *pidx, maxlen, false);
    if (c == '-' || c == '+') {
      gotsign = (c == '-');
      buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
    }
    if (base == 0) { /* determine radix by prefix */
      if (*pidx >= maxidx) goto stop;
      c = (*in)(data, *pidx, maxlen, false);
      if (c == '0') {
        (*in)(data, (*pidx)++, maxlen, true);
        ++dc;
        if (*pidx >= maxidx) goto stop;
        c = (*in)(data, *pidx, maxlen, false);
        if (c == 'x' || c == 'X') {
          (*in)(data, (*pidx)++, maxlen, true);
          base = 16; dc = 0;
        }
        else base = 8;
      }
      else base = 10;
    } else if (base == 16) { /* eat optional prefix */
      if (*pidx >= maxidx) goto stop;
      c = (*in)(data, *pidx, maxlen, false);
      if (c == '0') {
        (*in)(data, (*pidx)++, maxlen, true);
        ++dc;
        if (*pidx >= maxidx) goto stop;
        c = (*in)(data, *pidx, maxlen, false);
        if (c == 'x' || c == 'X') {
          (*in)(data, (*pidx)++, maxlen, true);
          dc = 0;
        }
      }
    }
    switch (base) {
    case 8:
      while (len < SCANF_ATON_BUFFER_SIZE - 1) {
        if (*pidx >= maxidx) goto stop;
        c = (*in)(data, *pidx, maxlen, false);
        if (c != EOF && _is_oct_digit(c)) {
          buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
          ++dc;
        }
        else break;
      }
      break;
    case 10:
      while (len < SCANF_ATON_BUFFER_SIZE - 1) {
        if (*pidx >= maxidx) goto stop;
        c = (*in)(data, *pidx, maxlen, false);
        if (c != EOF && _is_dec_digit(c)) {
          buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
          ++dc;
        }
        else break;
      }
      break;
    case 16:
      while (len < SCANF_ATON_BUFFER_SIZE - 1) {
        if (*pidx >= maxidx) goto stop;
        c = (*in)(data, *pidx, maxlen, false);
        if (c != EOF && _is_hex_digit(c)) {
          buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
          ++dc;
        }
        else break;
      }
      break;
    }
  stop:;
  }
  if (!dc) { /* no digits */
    errno = EILSEQ;
    return 0;
  }
  buf[len] = 0; errno = 0;
  if (gotsign) res = (unsigned long long)strtoll(buf, &end, base);
  else res = strtoull(buf, &end, base);
  if (*end != 0) errno = EILSEQ;
  return res;
}

/* internal scanner for special values */
static int _atos(in_fct_t in, void *data, const size_t maxlen, const char *s, size_t *pidx)
{
  int n = 0; bool full = true;
  while (*s) {
    int c = (*in)(data, *pidx, maxlen, false);
    if (c == EOF) break;
    if (*s == '*') {
      full = false;
      if (c == ')') { ++s; full = true; }
      else (*in)(data, (*pidx)++, maxlen, true);
    } else if (tolower(*s) == tolower(c)) {
      ++s;
      (*in)(data, (*pidx)++, maxlen, true);
    } else break;
    ++n;
  }
  return full ? n : -1;
}

/* internal atod for decimal floating point */
static double _atod(in_fct_t in, void *data, const size_t maxlen, int width, size_t *pidx)
{
  char buf[SCANF_ATON_BUFFER_SIZE];
  size_t len = 0, dc = 0; int c; bool hex = false;
  double res; char *end; bool neg = false;
  size_t maxidx = (width == 0) ? SIZE_MAX : *pidx + width;

  { /* scan chars up to maxidx */
    if (*pidx >= maxidx) goto stop;
    c = (*in)(data, *pidx, maxlen, false);
    if (c == '-' || c == '+') {
      (*in)(data, (*pidx)++, maxlen, true);
      neg = (c == '-');
      buf[len++] = c;
    }
    if (*pidx >= maxidx) goto stop;
    c = (*in)(data, *pidx, maxlen, false);
    if (c == 'i' || c == 'I') {
      int n = _atos(in, data, maxlen, "INFINITY", pidx);
      if (n == 3 || n == 8) return neg ? -HUGE_VAL : HUGE_VAL;
      errno = EILSEQ; return 0;
    } else if (c == 'n' || c == 'N') {
      int n = _atos(in, data, maxlen, "NAN(*)", pidx);
      if (n >= 3) return -HUGE_VAL + HUGE_VAL; /* fixme */
      errno = EILSEQ; return 0;
    }
    while (len < SCANF_ATON_BUFFER_SIZE - 1) {
      if (*pidx >= maxidx) goto stop;
      c = (*in)(data, *pidx, maxlen, false);
      if (c == EOF) break;
      else if (c == 'x' || c == 'X') {
        if (!((len == 2 && (buf[0] == '+' || buf[0] == '-') && buf[1] == '0') || 
              (len == 1 && buf[0] == '0'))) break;
        buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
        hex = true; dc = 0;
      } else if (hex ? _is_hex_digit(c) : _is_dec_digit(c)) {
        buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
        ++dc;
      } else break;
    }
    if (len < SCANF_ATON_BUFFER_SIZE - 1) {
      if (*pidx >= maxidx) goto stop;
      c = (*in)(data, *pidx, maxlen, false);
      if (c == '.') {
        buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
        while (len < SCANF_ATON_BUFFER_SIZE - 1) {
          if (*pidx >= maxidx) goto stop;
          c = (*in)(data, *pidx, maxlen, false);
          if (c != EOF && hex ? _is_hex_digit(c) : _is_dec_digit(c)) {
            buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
            ++dc;
          } else break;
        }
      }
    }
    if (len < SCANF_ATON_BUFFER_SIZE - 1) {
      if (*pidx >= maxidx) goto stop;
      c = (*in)(data, *pidx, maxlen, false);
      if (hex ? (c == 'p' || c == 'P')  : (c == 'e' || c == 'E')) {
        buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
        if (len < SCANF_ATON_BUFFER_SIZE - 1) {
          if (*pidx >= maxidx) goto stop;
          c = (*in)(data, *pidx, maxlen, false);
          if (c == '-' || c == '+') {
            (*in)(data, (*pidx)++, maxlen, true);
            buf[len++] = c;
          }
          while (len < SCANF_ATON_BUFFER_SIZE - 1) {
            if (*pidx >= maxidx) goto stop;
            c = (*in)(data, *pidx, maxlen, false);
            if (c != EOF && _is_dec_digit(c))
              buf[len++] = (*in)(data, (*pidx)++, maxlen, true);
            else break;
          }
        }
      }
    }
  stop:;
  }
  if (!dc) { /* no digits */
    errno = EILSEQ;
    return 0;
  }
  buf[len] = 0; errno = 0;
  res = strtod(buf, &end);
  if (*end != 0) errno = EILSEQ;
  return res;
}

/* internal vsnscanf */
int _vsnscanf(in_fct_t in, void *data, size_t maxlen, const char *format, va_list va)
{
  int width, size, base;
  const unsigned char *p;
  int c, t; char *s; void *dest = NULL;
  int invert, matchc = 0;
  unsigned long long x; double y;
  unsigned char scanset[257];
  size_t i, idx = 0;

  for (p = (const unsigned char *)format; *p; ++p) {

    if (isspace(*p)) {
      while (isspace(p[1])) ++p;
      while (true) {
        c = (*in)(data, idx, maxlen, false);
        if (c != EOF && isspace(c)) (*in)(data, idx++, maxlen, true);
        else break;
      }
      continue;
    }

    if (*p != '%' || p[1] == '%') {
      if (*p == '%') {
        ++p;
        while (true) {
          c = (*in)(data, idx, maxlen, false);
          if (c != EOF && isspace(c)) (*in)(data, idx++, maxlen, true);
          else break;
        }
      } else {
        c = (*in)(data, idx, maxlen, false);
      }
      if (c == *p) {
        (*in)(data, idx++, maxlen, true);
      } else {
        if (c == EOF) return matchc ? matchc : EOF;
        else return matchc;
      }
      continue;
    }

    ++p;
    if (*p == '*') {
      dest = NULL; ++p;
    } else {
      dest = va_arg(va, void *);
    }

    for (width = 0; isdigit(*p); ++p) {
      width = 10 * width + *p - '0';
    }

    size = SIZE_DEF;
    switch (*p++) {
      case 'h':
        if (*p == 'h') ++p, size = SIZE_HH;
        else size = SIZE_H;
        break;
      case 'l':
        if (*p == 'l') ++p, size = SIZE_LL;
        else size = SIZE_L;
        break;
      case 'j':
        size = SIZE_LL;
        break;
      case 'z': case 't':
        size = SIZE_L;
        break;
      case 'd': case 'i': case 'o': case 'u': case 'x':
      case 'a': case 'e': case 'f': case 'g':
      case 'A': case 'E': case 'F': case 'G': case 'X':
      case 's': case 'c': case '[':
      case 'p': case 'n':
        --p;
        break;
      default:
        return EOF;
    }

    t = *p;

    switch (t) {
      case 'c':
        if (width < 1) width = 1;
      case '[':
        break;
      case 'n':
        store_int(dest, size, idx);
        continue;
      default: {
        while (true) {
          c = (*in)(data, idx, maxlen, false);
          if (c != EOF && isspace(c)) (*in)(data, idx++, maxlen, true);
          else break;
        }
      } break;
    }

    if ((*in)(data, idx, maxlen, false) == EOF) 
      return matchc ? matchc : EOF;

    switch (t) {
      case 's':
      case 'c':
      case '[':
        if (t == 'c' || t == 's') {
          _bfill(&scanset[0], -1, 257);
          scanset[0] = 0;
          if (t == 's') {
            scanset[1 + '\t'] = 0;
            scanset[1 + '\n'] = 0;
            scanset[1 + '\v'] = 0;
            scanset[1 + '\f'] = 0;
            scanset[1 + '\r'] = 0;
            scanset[1 + ' '] = 0;
          }
        } else {
          if (*++p == '^') p++, invert = 1;
          else invert = 0;
          _bfill(&scanset[0], invert, 257);
          scanset[0] = 0;
          if (*p == '-') p++, scanset[1 + '-'] = 1 - invert;
          else if (*p == ']') p++, scanset[1 + ']'] = 1 - invert;
          for (; *p != ']'; p++) {
            if (!*p) return EOF;
            if (*p == '-' && p[1] && p[1] != ']')
              for (c = p++[-1]; c < *p; c++)
                scanset[1 + c] = 1 - invert;
            scanset[1 + *p] = 1 - invert;
          }
        }
        i = 0;
        if ((s = dest) != NULL) {
          while (!width || (int)i < width) {
            c = (*in)(data, idx, maxlen, false);
            if (c == EOF || !scanset[(c & 0xFF) + 1]) break;
            s[i++] = c; (*in)(data, idx++, maxlen, true);
          }
        } else {
          while (!width || (int)i < width) {
            c = (*in)(data, idx, maxlen, false);
            if (c == EOF || !scanset[(c & 0xFF) + 1]) break;
            ++i; (*in)(data, idx++, maxlen, true);
          }
        }
        if (!i || (t == 'c' && i != width)) 
          return matchc ? matchc : EOF;
        if (t != 'c') {
          if (s) s[i] = 0;
        }
        break;
      case 'p': case 'X': case 'x': case 'o':
      case 'd': case 'u': case 'i':
        base = 0;
        switch (t) {
          case 'p': case 'X': case 'x': base = 16; break;
          case 'o': base = 8; break;
          case 'd': case 'u': base = 10; break;
        }
        i = idx; 
        x = _aton(in, data, maxlen, base, width, &idx);
        if (idx == i || errno) return matchc; // matchc ? matchc : EOF;
        if (t == 'p' && dest != NULL) *(void **)dest = (void *)(uintptr_t)x;
        else store_int(dest, size, x);
        break;
      case 'a': case 'A':
      case 'e': case 'E':
      case 'f': case 'F':
      case 'g': case 'G':
        i = idx; 
        y = _atod(in, data, maxlen, width, &idx);
        if (idx == i || errno) return matchc; // matchc ? matchc : EOF;
        else store_float(dest, size, y);
        break;
    }
    if (dest) ++matchc;
  }
  
  return matchc;
}

int fscanf(FILE *fp, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  int ret = _vsnscanf(&_in_file, fp, SIZE_MAX, format, va);
  va_end(va);
  return ret;
}

int scanf(const char *format, ...)
{
  va_list va;
  va_start(va, format);
  int ret = _vsnscanf(&_in_file, stdin, SIZE_MAX, format, va);
  va_end(va);
  return ret;
}

int sscanf(const char *buf, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  int ret = _vsnscanf(&_in_buffer, buf, strlen(buf), format, va);
  va_end(va);
  return ret;
}

int snscanf(const char *buf, size_t count, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  int ret = _vsnscanf(&_in_buffer, buf, _strnlen_s(buf, count), format, va);
  va_end(va);
  return ret;
}

int vfscanf(FILE *fp, const char *format, va_list va)
{
  return _vsnscanf(&_in_file, fp, SIZE_MAX, format, va);
}

int vscanf(const char *format, va_list va)
{
  return _vsnscanf(&_in_file, stdin, SIZE_MAX, format, va);
}

int vsscanf(const char *buf, const char *format, va_list va)
{
  return _vsnscanf(&_in_buffer, buf, strlen(buf), format, va);
}

int vsnscanf(const char *buf, size_t count, const char *format, va_list va)
{
  return _vsnscanf(&_in_buffer, buf, _strnlen_s(buf, count), format, va);
}


/* formatted output */

/* derived from: https://github.com/mpaland/printf (c) Marco Paland (info@paland.com) */

/* 'ntoa' conversion buffer size, this must be big enough to hold one converted
 * integer number including padded zeros (dynamically created on stack) */
#define PRINTF_NTOA_BUFFER_SIZE    32U

/* 'ftoa' conversion buffer size, this must be big enough to hold one converted
 * float number including padded zeros (dynamically created on stack) */
#define PRINTF_FTOA_BUFFER_SIZE    32U

/* 'atoa' conversion buffer size, this must be big enough to hold one converted
 * hex float number including padded zeros (dynamically created on stack) */
#define PRINTF_ATOA_BUFFER_SIZE    32U

 /* default precision for decimal doubles */
#define PRINTF_DEFAULT_FLOAT_PRECISION  6U

/* default precision for hexadecimal doubles */
#define PRINTF_DEFAULT_HEXFL_PRECISION  13U 

/* largest float suitable to print with %f */
#define PRINTF_MAX_FLOAT  1e9

/* internal flag definitions */
#define FLAGS_ZEROPAD   (1U <<  0U)
#define FLAGS_LEFT      (1U <<  1U)
#define FLAGS_PLUS      (1U <<  2U)
#define FLAGS_SPACE     (1U <<  3U)
#define FLAGS_HASH      (1U <<  4U)
#define FLAGS_UPPERCASE (1U <<  5U)
#define FLAGS_CHAR      (1U <<  6U)
#define FLAGS_SHORT     (1U <<  7U)
#define FLAGS_LONG      (1U <<  8U)
#define FLAGS_LONG_LONG (1U <<  9U)
#define FLAGS_PRECISION (1U << 10U)
#define FLAGS_ADAPT_EXP (1U << 11U)

/* output function type */
typedef void (*out_fct_t)(char c, void *data, size_t idx, size_t maxlen);

/* internal buffer output */
static void _out_buffer(char c, void *data, size_t idx, size_t maxlen)
{
  if (idx < maxlen) ((char*)data)[idx] = c;
}

/* internal null output */
static void _out_null(char c, void *data, size_t idx, size_t maxlen)
{
}

/* internal file output */
static void _out_file(char c, void *data, size_t idx, size_t maxlen)
{
  FILE *fp = (FILE *)data;
  if (c) putc(c, fp);
} 

/* output the specified string in reverse, taking care of any zero-padding */
static size_t _out_rev(out_fct_t out, void *data, size_t idx, size_t maxlen, const char *buf, size_t len, unsigned int width, unsigned int flags)
{
  const size_t start_idx = idx; size_t i;
  if (!(flags & FLAGS_LEFT) && !(flags & FLAGS_ZEROPAD)) {
    for (i = len; i < width; i++) (*out)(' ', data, idx++, maxlen);
  }
  while (len) (*out)(buf[--len], data, idx++, maxlen);
  if (flags & FLAGS_LEFT) {
    while (idx - start_idx < width) (*out)(' ', data, idx++, maxlen);
  }
  return idx;
}

/* internal itoa format */
static size_t _ntoa_format(out_fct_t out, void *data, size_t idx, size_t maxlen, char *buf, size_t len, bool negative, unsigned int base, unsigned int prec, unsigned int width, unsigned int flags)
{
  if (!(flags & FLAGS_LEFT)) {
    if (width && (flags & FLAGS_ZEROPAD) && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) --width;
    while ((len < prec) && (len < PRINTF_NTOA_BUFFER_SIZE)) buf[len++] = '0';
    while ((flags & FLAGS_ZEROPAD) && (len < width) && (len < PRINTF_NTOA_BUFFER_SIZE)) buf[len++] = '0';
  }
  if (flags & FLAGS_HASH) {
    if (!(flags & FLAGS_PRECISION) && len && ((len == prec) || (len == width))) {
      --len; if (len && (base == 16U)) --len;
    }
    if ((base == 16U) && !(flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) buf[len++] = 'x';
    else if ((base == 16U) && (flags & FLAGS_UPPERCASE) && (len < PRINTF_NTOA_BUFFER_SIZE)) buf[len++] = 'X';
    else if ((base == 2U) && (len < PRINTF_NTOA_BUFFER_SIZE)) buf[len++] = 'b';
    if (len < PRINTF_NTOA_BUFFER_SIZE) buf[len++] = '0';
  }
  if (len < PRINTF_NTOA_BUFFER_SIZE) {
    if (negative) buf[len++] = '-';
    else if (flags & FLAGS_PLUS) buf[len++] = '+'; 
    else if (flags & FLAGS_SPACE) buf[len++] = ' ';
  }
  return _out_rev(out, data, idx, maxlen, buf, len, width, flags);
}

/* internal itoa for 'long' type */
static size_t _ntoa_long(out_fct_t out, void *data, size_t idx, size_t maxlen, unsigned long value, bool negative, unsigned long base, unsigned int prec, unsigned int width, unsigned int flags)
{
  char buf[PRINTF_NTOA_BUFFER_SIZE];
  size_t len = 0U;
  if (!value) flags &= ~FLAGS_HASH;
  if (!(flags & FLAGS_PRECISION) || value) {
    do {
      const char digit = (char)(value % base);
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
      value /= base;
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
  }
  return _ntoa_format(out, data, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
}

/* internal itoa for 'long long' type */
static size_t _ntoa_long_long(out_fct_t out, void *data, size_t idx, size_t maxlen, unsigned long long value, bool negative, unsigned long long base, unsigned int prec, unsigned int width, unsigned int flags)
{
  char buf[PRINTF_NTOA_BUFFER_SIZE];
  size_t len = 0U;
  if (!value) flags &= ~FLAGS_HASH;
  if (!(flags & FLAGS_PRECISION) || value) {
    do {
      const char digit = (char)(value % base);
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
      value /= base;
    } while (value && (len < PRINTF_NTOA_BUFFER_SIZE));
  }
  return _ntoa_format(out, data, idx, maxlen, buf, len, negative, (unsigned int)base, prec, width, flags);
}

/* powers of 10 for _ftoa's 9 digits of precision */
static const double pow10[] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };

/* forward declaration so that _ftoa can switch to exp notation for values > PRINTF_MAX_FLOAT */
static size_t _etoa(out_fct_t out, void *data, size_t idx, size_t maxlen, double value, unsigned int prec, unsigned int width, unsigned int flags);

/* internal ftoa for fixed decimal floating point */
static size_t _ftoa(out_fct_t out, void *data, size_t idx, size_t maxlen, double value, unsigned int prec, unsigned int width, unsigned int flags)
{
  char buf[PRINTF_FTOA_BUFFER_SIZE];
  size_t len  = 0U; double diff = 0.0;
  
  if (value != value)   /* NAN */
    return _out_rev(out, data, idx, maxlen, "nan", 3, width, flags);
  if (value < -DBL_MAX) /* -INF */
    return _out_rev(out, data, idx, maxlen, "fni-", 4, width, flags);
  if (value > DBL_MAX)  /* +INF */
    return _out_rev(out, data, idx, maxlen, (flags & FLAGS_PLUS) ? "fni+" : "fni", (flags & FLAGS_PLUS) ? 4U : 3U, width, flags);
  /* fall back to etoa for very large values */
  if ((value > PRINTF_MAX_FLOAT) || (value < -PRINTF_MAX_FLOAT)) {
    return _etoa(out, data, idx, maxlen, value, prec, width, flags);
  }
  bool negative = false;
  if (value < 0) { negative = true; value = -value; }
  if (!(flags & FLAGS_PRECISION)) {
    prec = PRINTF_DEFAULT_FLOAT_PRECISION;
  }
  /* limit precision to 9, cause a prec >= 10 can lead to overflow errors */
  while ((len < PRINTF_FTOA_BUFFER_SIZE) && (prec > 9U)) {
    buf[len++] = '0';
    --prec;
  }
  
  int whole = (int)value;
  double tmp = (value - whole) * pow10[prec];
  unsigned long frac = (unsigned long)tmp;
  diff = tmp - frac;
  if (diff > 0.5) {
    ++frac;
    /* handle rollover, e.g. case 0.99 with prec 1 is 1.0 */
    if (frac >= pow10[prec]) {
      frac = 0;
      ++whole;
    }
  } else if (diff < 0.5) {
  } else if ((frac == 0U) || (frac & 1U)) {
    /* if halfway, round up if odd OR if last digit is 0 */
    ++frac;
  }
  if (prec == 0U) {
    diff = value - (double)whole;
    if ((!(diff < 0.5) || (diff > 0.5)) && (whole & 1)) {
      /* exactly 0.5 and ODD, then round up:  1.5 -> 2, but 2.5 -> 2 */
      ++whole;
    }
  } else {
    unsigned int count = prec;
    /* now do fractional part, as an unsigned number */
    while (len < PRINTF_FTOA_BUFFER_SIZE) {
      --count;
      buf[len++] = (char)(48U + (frac % 10U));
      if (!(frac /= 10U)) break;
    }
    /* add extra 0s */
    while ((len < PRINTF_FTOA_BUFFER_SIZE) && (count-- > 0U)) {
      buf[len++] = '0';
    }
    if (len < PRINTF_FTOA_BUFFER_SIZE) {
      /* add decimal dot */
      buf[len++] = '.';
    }
  }

  /* do whole part, number is reversed */
  while (len < PRINTF_FTOA_BUFFER_SIZE) {
    buf[len++] = (char)(48 + (whole % 10));
    if (!(whole /= 10)) break;
  }
  if (!(flags & FLAGS_LEFT) && (flags & FLAGS_ZEROPAD)) {
    if (width && (negative || (flags & (FLAGS_PLUS | FLAGS_SPACE)))) --width;
    while ((len < width) && (len < PRINTF_FTOA_BUFFER_SIZE)) buf[len++] = '0';
  }
  if (len < PRINTF_FTOA_BUFFER_SIZE) {
    if (negative) buf[len++] = '-';
    else if (flags & FLAGS_PLUS) buf[len++] = '+';
    else if (flags & FLAGS_SPACE) buf[len++] = ' ';
  }

  return _out_rev(out, data, idx, maxlen, buf, len, width, flags);
}

/* internal ftoa variant for exponential floating-point type, contributed by Martijn Jasperse <m.jasperse@gmail.com> */
static size_t _etoa(out_fct_t out, void *data, size_t idx, size_t maxlen, double value, unsigned int prec, unsigned int width, unsigned int flags)
{
  if ((value != value) || (value > DBL_MAX) || (value < -DBL_MAX)) {
    return _ftoa(out, data, idx, maxlen, value, prec, width, flags);
  }
  const bool negative = value < 0; if (negative) value = -value;
  if (!(flags & FLAGS_PRECISION)) prec = PRINTF_DEFAULT_FLOAT_PRECISION;

  /* determine the decimal exponent
   * based on the algorithm by David Gay (https://www.ampl.com/netlib/fp/dtoa.c) */
  union { uint64_t U; double F; } conv; conv.F = value;
  int exp2 = (int)((conv.U >> 52U) & 0x07FFU) - 1023;  /* effectively log2 */
  conv.U = (conv.U & ((1ULL << 52U) - 1U)) | (1023ULL << 52U);  /* drop the exponent so conv.F is now in [1,2) */
  /* now approximate log10 from the log2 integer part and an expansion of ln around 1.5 */
  int expval = (int)(0.1760912590558 + exp2 * 0.301029995663981 + (conv.F - 1.5) * 0.289529654602168);
  /* now we want to compute 10^expval but we want to be sure it won't overflow */
  exp2 = (int)(expval * 3.321928094887362 + 0.5);
  const double z  = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
  const double z2 = z * z;
  conv.U = (uint64_t)(exp2 + 1023) << 52U;
  /* compute exp(z) using continued fractions, see 
   * https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex */
  conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
  /* correct for rounding errors */
  if (value < conv.F) { --expval; conv.F /= 10; }

  /* the exponent format is "%+03d" and largest value is "307", so set aside 4-5 characters */
  unsigned int minwidth = ((expval < 100) && (expval > -100)) ? 4U : 5U;

  /* in "%g" mode, "prec" is the number of *significant figures* not decimals */
  if (flags & FLAGS_ADAPT_EXP) {
    /* do we want to fall-back to "%f" mode? */
    if ((value >= 1e-4) && (value < 1e6)) {
      if ((int)prec > expval) prec = (unsigned)((int)prec - expval - 1);
      else prec = 0;
      flags |= FLAGS_PRECISION; /* make sure _ftoa respects precision */
      /* no characters in exponent */
      minwidth = 0U; expval = 0;
    } else {
      /* we use one sigfig for the whole part */
      if ((prec > 0) && (flags & FLAGS_PRECISION)) --prec;
    }
  }

  unsigned int fwidth = width;
  if (width > minwidth) { /* we didn't fall-back so subtract the characters required for the exponent */
    fwidth -= minwidth;
  } else { /* not enough characters, so go back to default sizing */
    fwidth = 0U;
  }
  if ((flags & FLAGS_LEFT) && minwidth) { /* if we're padding on the right, DON'T pad the floating part */
    fwidth = 0U;
  }

  /* rescale the float value */
  if (expval) value /= conv.F;

  /* output the floating part */
  const size_t start_idx = idx;
  idx = _ftoa(out, data, idx, maxlen, negative ? -value : value, prec, fwidth, flags & ~FLAGS_ADAPT_EXP);
  /* output the exponent part */
  if (minwidth) {
    (*out)((flags & FLAGS_UPPERCASE) ? 'E' : 'e', data, idx++, maxlen);
    idx = _ntoa_long(out, data, idx, maxlen, (expval < 0) ? -expval : expval, expval < 0, 10, 0, minwidth-1, FLAGS_ZEROPAD | FLAGS_PLUS);
    if (flags & FLAGS_LEFT) { /* right-pad spaces */ 
      while (idx - start_idx < width) (*out)(' ', data, idx++, maxlen);
    }
  }

  return idx;
}

/* internal hexadecimal ftoa variant for exponential floating-point type */
static size_t _atoa(out_fct_t out, void *data, size_t idx, size_t maxlen, double value, unsigned int prec, unsigned int width, unsigned int flags)
{
  if ((value != value) || (value > DBL_MAX) || (value < -DBL_MAX)) {
    return _ftoa(out, data, idx, maxlen, value, prec, width, flags);
  }
  const bool negative = value < 0.0; if (negative) value = -value;
  if (!(flags & FLAGS_PRECISION)) prec = PRINTF_DEFAULT_HEXFL_PRECISION;
  union { uint64_t U; double F; } conv; conv.F = value;
  int exp2 = (int)((conv.U >> 52U) & 0x07FFU);
  uint64_t mantissa = (conv.U & 0xFFFFFFFFFFFFFULL);
  int mandigits = 13;
  char buf[PRINTF_ATOA_BUFFER_SIZE];
  size_t len = 0U; int val, vneg = false;
  if (exp2 == 0) val = mantissa == 0 ? 0 : -1022; else val = exp2 - 1023;
  if (val < 0) val = -val, vneg = true;
  if (len < PRINTF_ATOA_BUFFER_SIZE) do {
    const char digit = (char)(val % 10);
    buf[len++] = '0' + digit; val /= 10;
  } while (val && (len < PRINTF_ATOA_BUFFER_SIZE));
  if (len < PRINTF_ATOA_BUFFER_SIZE) buf[len++] = vneg ? '-' : '+';
  if (len < PRINTF_ATOA_BUFFER_SIZE) buf[len++] = flags & FLAGS_UPPERCASE ? 'P' : 'p';
  while ((len < PRINTF_FTOA_BUFFER_SIZE) && (prec > PRINTF_DEFAULT_HEXFL_PRECISION)) {
    buf[len++] = '0';
    --prec;
  }
  size_t rlen = prec < PRINTF_DEFAULT_HEXFL_PRECISION ? PRINTF_DEFAULT_HEXFL_PRECISION - prec : 0;
  double diff = 0.0; /* [0..16) */
  if (len < PRINTF_ATOA_BUFFER_SIZE) do {
    int digit = (int)(mantissa % 16);
    if (rlen > 0) { /* no output, keep tracking diff */
      diff = diff/16.0 + digit;
      --rlen;
    } else { /* output, absorb diff */
      if (diff > 8.0 || (diff == 8.0 && (digit & 1))) {
        if (++digit == 16) {
          digit = 0; diff = diff / 16.0 + 15;
        } else { /* diff fully absorbed */
          diff = 0.0;
        }
      } else { /* diff fully absorbed */
        diff = 0.0;
      }
      buf[len++] = digit < 10 ? '0' + digit : (flags & FLAGS_UPPERCASE ? 'A' : 'a') + digit - 10;
    }
    mantissa /= 16; --mandigits;
  } while (mandigits && (len < PRINTF_ATOA_BUFFER_SIZE));
  if (prec > 0 && len < PRINTF_ATOA_BUFFER_SIZE) buf[len++] = '.';
  if (len < PRINTF_ATOA_BUFFER_SIZE) {
    int digit = (exp2 == 0) ? 0 : 1;
    if (diff > 8.0 || (diff == 8.0 && (digit & 1))) ++digit;
    buf[len++] = '0' + digit;
  }
  if (!(flags & FLAGS_UPPERCASE) && (len < PRINTF_ATOA_BUFFER_SIZE)) buf[len++] = 'x';
  else if (len < PRINTF_ATOA_BUFFER_SIZE) buf[len++] = 'X';
  if (len < PRINTF_ATOA_BUFFER_SIZE) buf[len++] = '0';
  if (len < PRINTF_ATOA_BUFFER_SIZE) {
    if (negative) buf[len++] = '-';
    else if (flags & FLAGS_PLUS) buf[len++] = '+';
    else if (flags & FLAGS_SPACE) buf[len++] = ' ';
  }
  return _out_rev(out, data, idx, maxlen, buf, len, width, flags);
}

/* internal vsnprintf */
static int _vsnprintf(out_fct_t out, void *data, const size_t maxlen, const char *format, va_list va)
{
  unsigned int flags, width, precision, n;
  size_t idx = 0U;
  if (!data) out = &_out_null; /* use null output function */

  while (*format) {
    /* format specifier?  %[flags][width][.precision][length] */
    if (*format != '%') { /* no */
      (*out)(*format, data, idx++, maxlen);
      ++format;
      continue;
    } else { /* yes */
      ++format;
    }

    flags = 0U;
    do {
      switch (*format) {
        case '0': flags |= FLAGS_ZEROPAD; ++format; n = 1U; break;
        case '-': flags |= FLAGS_LEFT;    ++format; n = 1U; break;
        case '+': flags |= FLAGS_PLUS;    ++format; n = 1U; break;
        case ' ': flags |= FLAGS_SPACE;   ++format; n = 1U; break;
        case '#': flags |= FLAGS_HASH;    ++format; n = 1U; break;
        default :                                   n = 0U; break;
      }
    } while (n);

    width = 0U;
    if (_is_dec_digit(*format)) {
      width = _atoi(&format);
    } else if (*format == '*') {
      const int w = va_arg(va, int);
      if (w < 0) {
        flags |= FLAGS_LEFT; /* reverse padding */
        width = (unsigned int)-w;
      } else {
        width = (unsigned int)w;
      }
      ++format;
    }

    precision = 0U;
    if (*format == '.') {
      flags |= FLAGS_PRECISION;
      format++;
      if (_is_dec_digit(*format)) {
        precision = _atoi(&format);
      } else if (*format == '*') {
        const int prec = (int)va_arg(va, int);
        precision = prec > 0 ? (unsigned int)prec : 0U;
        ++format;
      }
    }

    switch (*format) {
      case 'l':
        flags |= FLAGS_LONG; ++format;
        if (*format == 'l') { flags |= FLAGS_LONG_LONG; ++format; }
        break;
      case 'h':
        flags |= FLAGS_SHORT; ++format;
        if (*format == 'h') { flags |= FLAGS_CHAR; ++format; }
        break;
      case 't':
        flags |= (sizeof(ptrdiff_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        ++format;
        break;
      case 'j':
        flags |= (sizeof(intmax_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        ++format;
        break;
      case 'z':
        flags |= (sizeof(size_t) == sizeof(long) ? FLAGS_LONG : FLAGS_LONG_LONG);
        ++format;
        break;
      default :
        break;
    }

    switch (*format) {
      case 'd': case 'i': case 'u':  case 'x':
      case 'X': case 'o': case 'b': {
        unsigned int base;
        if (*format == 'x' || *format == 'X') base = 16U;
        else if (*format == 'o') base = 8U;
        else if (*format == 'b') base = 2U;
        else { base = 10U; flags &= ~FLAGS_HASH; }  /* no hash for dec format */
        if (*format == 'X') flags |= FLAGS_UPPERCASE;
        /* no plus or space flag for u, x, X, o, b */
        if ((*format != 'i') && (*format != 'd')) flags &= ~(FLAGS_PLUS | FLAGS_SPACE);
        /* ignore '0' flag when precision is given */
        if (flags & FLAGS_PRECISION) flags &= ~FLAGS_ZEROPAD;
        if (*format == 'i' || *format == 'd') { /* signed */
          if (flags & FLAGS_LONG_LONG) {
            const long long value = va_arg(va, long long);
            idx = _ntoa_long_long(out, data, idx, maxlen, (unsigned long long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
          } else if (flags & FLAGS_LONG) {
            const long value = va_arg(va, long);
            idx = _ntoa_long(out, data, idx, maxlen, (unsigned long)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
          } else {
            const int value = (flags & FLAGS_CHAR) ? (char)va_arg(va, int) : (flags & FLAGS_SHORT) ? (short int)va_arg(va, int) : va_arg(va, int);
            idx = _ntoa_long(out, data, idx, maxlen, (unsigned int)(value > 0 ? value : 0 - value), value < 0, base, precision, width, flags);
          }
        } else { /* unsigned */
          if (flags & FLAGS_LONG_LONG) {
            idx = _ntoa_long_long(out, data, idx, maxlen, va_arg(va, unsigned long long), false, base, precision, width, flags);
          } else if (flags & FLAGS_LONG) {
            idx = _ntoa_long(out, data, idx, maxlen, va_arg(va, unsigned long), false, base, precision, width, flags);
          } else {
            const unsigned int value = (flags & FLAGS_CHAR) ? (unsigned char)va_arg(va, unsigned int) : (flags & FLAGS_SHORT) ? (unsigned short int)va_arg(va, unsigned int) : va_arg(va, unsigned int);
            idx = _ntoa_long(out, data, idx, maxlen, value, false, base, precision, width, flags);
          }
        }
        ++format;
      } break;
      case 'a': case 'A': {
        if (*format == 'A') flags |= FLAGS_UPPERCASE;
        idx = _atoa(out, data, idx, maxlen, va_arg(va, double), precision, width, flags);
        ++format;
      } break;
      case 'f': case 'F': {
        if (*format == 'F') flags |= FLAGS_UPPERCASE;
        idx = _ftoa(out, data, idx, maxlen, va_arg(va, double), precision, width, flags);
        ++format;
      } break;
      case 'e': case 'E': case 'g': case 'G': {
        if (*format == 'g' || *format == 'G') flags |= FLAGS_ADAPT_EXP;
        if (*format == 'E' || *format == 'G') flags |= FLAGS_UPPERCASE;
        idx = _etoa(out, data, idx, maxlen, va_arg(va, double), precision, width, flags);
        ++format;
      } break;
      case 'c': {
        unsigned int l = 1U;
        if (!(flags & FLAGS_LEFT)) {
          while (l++ < width) (*out)(' ', data, idx++, maxlen);
        }
        (*out)((char)va_arg(va, int), data, idx++, maxlen);
        if (flags & FLAGS_LEFT) {
          while (l++ < width) (*out)(' ', data, idx++, maxlen);
        }
        ++format;
      } break;
      case 's': {
        const char* p = va_arg(va, char*);
        unsigned int l = _strnlen_s(p, precision ? precision : (size_t)-1);
        if (flags & FLAGS_PRECISION) l = (l < precision ? l : precision);
        if (!(flags & FLAGS_LEFT)) {
          while (l++ < width) (*out)(' ', data, idx++, maxlen);
        }
        while (*p != 0 && (!(flags & FLAGS_PRECISION) || precision--)) {
          (*out)(*(p++), data, idx++, maxlen);
        }
        if (flags & FLAGS_LEFT) {
          while (l++ < width) (*out)(' ', data, idx++, maxlen);
        }
        ++format;
      } break;
      case 'p': {
        width = sizeof(void*) * 2U;
        flags |= FLAGS_ZEROPAD | FLAGS_UPPERCASE;
        const bool is_ll = sizeof(uintptr_t) == sizeof(long long);
        if (is_ll) {
          idx = _ntoa_long_long(out, data, idx, maxlen, (uintptr_t)va_arg(va, void*), false, 16U, precision, width, flags);
        } else {
          idx = _ntoa_long(out, data, idx, maxlen, (unsigned long)((uintptr_t)va_arg(va, void*)), false, 16U, precision, width, flags);
        }
        ++format;
      } break;
      case '%': {
        (*out)('%', data, idx++, maxlen);
        ++format;
      } break;
      default: {
        (*out)(*format, data, idx++, maxlen);
        ++format;
      } break;
    }
  }

  (*out)((char)0, data, idx < maxlen ? idx : maxlen - 1U, maxlen); /* \0 */
  return (int)idx; /* # of written chars sans \0 */
}

int fprintf(FILE *fp, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  const int ret = _vsnprintf(&_out_file, fp, SIZE_MAX, format, va);
  va_end(va);
  return ret;
}

int printf(const char* format, ...)
{
  va_list va;
  va_start(va, format);
  const int ret = _vsnprintf(&_out_file, stdout, SIZE_MAX, format, va);
  va_end(va);
  return ret;
}

int sprintf(char* buf, const char* format, ...)
{
  va_list va;
  va_start(va, format);
  const int ret = _vsnprintf(&_out_buffer, buf, SIZE_MAX, format, va);
  va_end(va);
  return ret;
}

int snprintf(char* buf, size_t count, const char* format, ...)
{
  va_list va;
  va_start(va, format);
  const int ret = _vsnprintf(&_out_buffer, buf, count, format, va);
  va_end(va);
  return ret;
}

int vprintf(const char* format, va_list va)
{
  return _vsnprintf(&_out_file, stdout, SIZE_MAX, format, va);
}

int vfprintf(FILE *fp, const char *format, va_list va)
{
  return _vsnprintf(&_out_file, fp, SIZE_MAX, format, va);
}

int vsprintf(char* buf, const char* format, va_list va)
{
  return _vsnprintf(&_out_buffer, buf, SIZE_MAX, format, va);
}

int vsnprintf(char* buf, size_t count, const char* format, va_list va)
{
  return _vsnprintf(&_out_buffer, buf, count, format, va);
}


/* misc filesystem functions */

int renameat(int oldfd, const char *oldpath, int newfd, const char *newpath)
{
  errno_t error = path_rename(oldfd, oldpath, strlen(oldpath), newfd, newpath, strlen(newpath));
  if (error != 0) {
    errno = (int)error;
    return -1;
  }
  return 0;
}

int rename(const char *oldpath, const char *newpath) 
{
  char *oldrelpath;
  int olddirfd = find_relpath(oldpath, &oldrelpath);
  if (olddirfd != -1) {
    char *newrelpath;
    int newdirfd = find_relpath(newpath, &newrelpath);
    if (newdirfd != -1)
      return renameat(olddirfd, oldrelpath, newdirfd, newrelpath);
  }
  errno = ENOTCAPABLE;
  return -1;
}

int remove(const char *path) 
{
  char *relpath;
  int dirfd = find_relpath(path, &relpath);
  if (dirfd == -1) {
    errno = ENOTCAPABLE;
    return -1;
  }
  int r = unlinkat(dirfd, relpath, 0);
  if (r != 0 && (errno == EISDIR || errno == ENOTCAPABLE)) {
    r = rmdirat(dirfd, relpath);
    if (errno == ENOTDIR) errno = ENOTCAPABLE;
  }
  return r;
}

void perror(const char *s)
{
	FILE *fp = stderr;
	char *err = strerror(errno);
	if (s != NULL && *s) {
		fwrite(s, strlen(s), 1, fp);
		fputc(':', fp);
		fputc(' ', fp);
	}
	fwrite(err, strlen(err), 1, fp);
	fputc('\n', fp);
}

#else
/*
 * lib/stdio.c — MARV-ABI C library adapter for `stdio.mvh`
 *
 * Adapts host C <stdio.h> streams and formatted I/O to MARV's internal ABI:
 *   - `stdio__iob[640]` provides 32-byte slots for stdin (0), stdout (32), stderr (64)
 *   - Variadic functions (`printf`, `fprintf`, `sprintf`, `snprintf`, `scanf`,
 *     `fscanf`, `sscanf`, `snscanf`) take a trailing `marv_ptr ap` pointing to
 *     an array of 16-byte `marv_va_arg_t` slots (matching WCPL's `va_arg_t`).
 */

#define _DEFAULT_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

typedef int32_t  marv_i32;
typedef uint32_t marv_u32;
typedef int64_t  marv_i64;
typedef uint64_t marv_u64;
typedef float    marv_f32;
typedef double   marv_f64;
typedef void*    marv_ptr;

#define MARV_NULL ((marv_ptr)0)

typedef union marv_va_arg {
  marv_i32 va_int;
  marv_u32 va_uint;
  marv_i64 va_llong;
  marv_u64 va_ullong;
  marv_f64 va_double;
  uint64_t va_ptr_slot;
  uint64_t _pad[2];
} marv_va_arg_t;

#define MARV_VA_COUNT(ap) \
  (((const marv_va_arg_t *)(uintptr_t)(ap))[-1].va_int)

#define MARV_VA_PTR(slot_ptr) \
  ((marv_ptr)(uintptr_t)((slot_ptr)->va_ptr_slot))

#if defined(_MSC_VER) && !defined(__cplusplus) && (_MSC_VER < 1900)
#define inline __inline
#endif

static inline marv_ptr marv_ptr_load(marv_ptr base, marv_i64 offset) {
  uint64_t slot = 0;
  memcpy(&slot, (const uint8_t*)base + offset, 8);
  return (marv_ptr)(uintptr_t)slot;
}

static inline void marv_ptr_store(marv_ptr base, marv_i64 offset, marv_ptr val) {
  uint64_t slot = (uint64_t)(uintptr_t)val;
  memcpy((uint8_t*)base + offset, &slot, 8);
}

static inline marv_i64 marv_i64_load(marv_ptr base, marv_i64 off) {
  marv_i64 v; memcpy(&v, (const uint8_t*)base + off, 8); return v;
}
static inline void marv_i32_store(marv_ptr base, marv_i64 off, marv_i32 v) {
  memcpy((uint8_t*)base + off, &v, 4);
}
static inline void marv_i32_store8(marv_ptr base, marv_i64 off, marv_i32 v) {
  uint8_t b = (uint8_t)v; memcpy((uint8_t*)base + off, &b, 1);
}
static inline void marv_i32_store16(marv_ptr base, marv_i64 off, marv_i32 v) {
  uint16_t h = (uint16_t)v; memcpy((uint8_t*)base + off, &h, 2);
}
static inline void marv_i64_store(marv_ptr base, marv_i64 off, marv_i64 v) {
  memcpy((uint8_t*)base + off, &v, 8);
}
static inline void marv_f32_store(marv_ptr base, marv_i64 off, marv_f32 v) {
  memcpy((uint8_t*)base + off, &v, 4);
}
static inline void marv_f64_store(marv_ptr base, marv_i64 off, marv_f64 v) {
  memcpy((uint8_t*)base + off, &v, 8);
}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

extern marv_i32 errno_errno;
extern void errno_sync_from_host(int herr);

/* 20 * 32-byte FILE slots matching WCPL `$stdio:_iob` */
uint8_t stdio__iob[640];

static FILE *stdio_resolve_fp(marv_ptr stream) {
  uintptr_t u = (uintptr_t)stream;
  uintptr_t base = (uintptr_t)&stdio__iob[0];
  if (u == base + 0)  return stdin;
  if (u == base + 32) return stdout;
  if (u == base + 64) return stderr;
  return (FILE *)u;
}

marv_ptr stdio_fopen(marv_ptr filename, marv_ptr mode) {
  errno = 0;
  FILE *fp = fopen((const char *)filename, (const char *)mode);
  if (!fp) errno_sync_from_host(errno);
  return (marv_ptr)fp;
}

marv_ptr stdio_fdopen(marv_i32 fd, marv_ptr mode) {
#if defined(_WIN32)
  (void)fd; (void)mode;
  return MARV_NULL;
#else
  errno = 0;
  FILE *fp = fdopen((int)fd, (const char *)mode);
  if (!fp) errno_sync_from_host(errno);
  return (marv_ptr)fp;
#endif
}

marv_ptr stdio_freopen(marv_ptr filename, marv_ptr mode, marv_ptr stream) {
  errno = 0;
  FILE *fp = freopen((const char *)filename, (const char *)mode, stdio_resolve_fp(stream));
  if (!fp) errno_sync_from_host(errno);
  return (marv_ptr)fp;
}

marv_i32 stdio_fclose(marv_ptr stream) {
  FILE *fp = stdio_resolve_fp(stream);
  if (!fp || fp == stdin || fp == stdout || fp == stderr) return 0;
  return (marv_i32)fclose(fp);
}

marv_i32 stdio_fgetc(marv_ptr stream) {
  return (marv_i32)fgetc(stdio_resolve_fp(stream));
}

marv_ptr stdio_fgets(marv_ptr s, marv_i32 n, marv_ptr stream) {
  return (marv_ptr)fgets((char *)s, (int)n, stdio_resolve_fp(stream));
}

marv_i32 stdio_fputc(marv_i32 c, marv_ptr stream) {
  return (marv_i32)fputc((int)c, stdio_resolve_fp(stream));
}

marv_i32 stdio_fputs(marv_ptr s, marv_ptr stream) {
  return (marv_i32)fputs((const char *)s, stdio_resolve_fp(stream));
}

marv_i32 stdio_puts(marv_ptr s) {
  return (marv_i32)puts((const char *)s);
}

marv_i32 stdio_ungetc(marv_i32 c, marv_ptr stream) {
  return (marv_i32)ungetc((int)c, stdio_resolve_fp(stream));
}

marv_i32 stdio_fread(marv_ptr ptr, marv_i32 size, marv_i32 nobj, marv_ptr stream) {
  return (marv_i32)fread(ptr, (size_t)(uint32_t)size, (size_t)(uint32_t)nobj, stdio_resolve_fp(stream));
}

marv_i32 stdio_fwrite(marv_ptr ptr, marv_i32 size, marv_i32 nobj, marv_ptr stream) {
  return (marv_i32)fwrite(ptr, (size_t)(uint32_t)size, (size_t)(uint32_t)nobj, stdio_resolve_fp(stream));
}

marv_i32 stdio_fseek(marv_ptr stream, marv_i32 offset, marv_i32 origin) {
  return (marv_i32)fseek(stdio_resolve_fp(stream), (long)offset, (int)origin);
}

marv_i32 stdio_ftell(marv_ptr stream) {
  return (marv_i32)ftell(stdio_resolve_fp(stream));
}

void stdio_rewind(marv_ptr stream) {
  rewind(stdio_resolve_fp(stream));
}

marv_i32 stdio_fgetpos(marv_ptr stream, marv_ptr pos) {
  long p = ftell(stdio_resolve_fp(stream));
  if (p < 0) return -1;
  if (pos) marv_i64_store(pos, 0, (marv_i64)p);
  return 0;
}

marv_i32 stdio_fsetpos(marv_ptr stream, marv_ptr pos) {
  marv_i64 p = pos ? marv_i64_load(pos, 0) : 0;
  return (marv_i32)fseek(stdio_resolve_fp(stream), (long)p, SEEK_SET);
}

marv_i32 stdio_fflush(marv_ptr stream) {
  return (marv_i32)fflush(stream ? stdio_resolve_fp(stream) : NULL);
}

marv_i32 stdio_setvbuf(marv_ptr stream, marv_ptr buf, marv_i32 mode, marv_i32 size) {
  int cmode = _IOFBF;
  if (mode == 4) cmode = _IONBF;
  else if (mode == 64) cmode = _IOLBF;
  return (marv_i32)setvbuf(stdio_resolve_fp(stream), (char *)buf, cmode, (size_t)(uint32_t)size);
}

void stdio_setbuf(marv_ptr stream, marv_ptr buf) {
  setbuf(stdio_resolve_fp(stream), (char *)buf);
}

marv_i32 stdio_remove(marv_ptr filename) {
  errno = 0;
  int r = remove((const char *)filename);
  if (r != 0) errno_sync_from_host(errno);
  return (marv_i32)r;
}

marv_i32 stdio_rename(marv_ptr oldname, marv_ptr newname) {
  errno = 0;
  int r = rename((const char *)oldname, (const char *)newname);
  if (r != 0) errno_sync_from_host(errno);
  return (marv_i32)r;
}

marv_i32 stdio_renameat(marv_i32 oldfd, marv_ptr oldpath, marv_i32 newfd, marv_ptr newpath) {
  (void)oldfd; (void)newfd;
  return stdio_rename(oldpath, newpath);
}

void stdio_perror(marv_ptr s) {
  const char *str = (const char *)s;
  if (str && *str) fprintf(stderr, "%s: error %d\n", str, (int)errno_errno);
  else fprintf(stderr, "error %d\n", (int)errno_errno);
}

/* --------------------------------------------------------------------------
 * Formatted Output Engine (stepping through 16-byte `marv_va_arg_t` slots)
 * -------------------------------------------------------------------------- */

typedef struct {
  char  *buf;
  size_t maxlen;
  size_t idx;
  FILE  *fp;
} marv_out_sink_t;

static void sink_put_char(marv_out_sink_t *s, char c) {
  if (s->fp) {
    fputc((unsigned char)c, s->fp);
  } else if (s->buf && s->maxlen > 0) {
    if (s->idx + 1 < s->maxlen) {
      s->buf[s->idx] = c;
    }
  }
  s->idx++;
}

static void sink_put_mem(marv_out_sink_t *s, const char *p, size_t n) {
  for (size_t i = 0; i < n; i++) {
    sink_put_char(s, p[i]);
  }
}

/* Appending to a host conversion format must never silently truncate it. */
static int fmt_char(char *spec, size_t *len, char ch) {
  if (*len + 1 >= 64) return 0;
  spec[(*len)++] = ch;
  spec[*len] = '\0';
  return 1;
}

static int fmt_uint(char *spec, size_t *len, unsigned int n) {
  char digits[32];
  int count, i;
  count = snprintf(digits, sizeof(digits), "%u", n);
  if (count < 0 || (size_t)count >= sizeof(digits)) return 0;
  for (i = 0; i < count; ++i)
    if (!fmt_char(spec, len, digits[i])) return 0;
  return 1;
}

/* V must be a local without side effects: a long result is formatted twice. */
#define MARV_EMIT_FORMATTED(V) do { \
  int marv_n_ = snprintf(tmp, sizeof(tmp), spec, (V)); \
  if (marv_n_ < 0) return -1; \
  if ((size_t)marv_n_ < sizeof(tmp)) { \
    sink_put_mem(sink, tmp, (size_t)marv_n_); \
  } else { \
    char *marv_buf_ = (char *)malloc((size_t)marv_n_ + 1); \
    int marv_n2_; \
    if (marv_buf_ == NULL) return -1; \
    marv_n2_ = snprintf(marv_buf_, (size_t)marv_n_ + 1, spec, (V)); \
    if (marv_n2_ != marv_n_) { free(marv_buf_); return -1; } \
    sink_put_mem(sink, marv_buf_, (size_t)marv_n_); \
    free(marv_buf_); \
  } \
} while (0)

static marv_i32 marv_format_engine(marv_out_sink_t *sink, const char *fmt, const marv_va_arg_t *va) {
  const char *p;
  if (!fmt) return 0;
  p = fmt;
  while (*p) {
    char spec[64], tmp[512];
    size_t si;
    int left, plus, space, alternate, zero, len_mod;
    char conv;
    if (*p != '%') { sink_put_char(sink, *p++); continue; }
    ++p;
    if (*p == '%') { sink_put_char(sink, *p++); continue; }
    left = plus = space = alternate = zero = 0;
    while (*p == '-' || *p == '+' || *p == ' ' || *p == '#' || *p == '0') {
      if (*p == '-') left = 1;
      else if (*p == '+') plus = 1;
      else if (*p == ' ') space = 1;
      else if (*p == '#') alternate = 1;
      else zero = 1;
      ++p;
    }
    si = 0; spec[0] = '\0';
    if (!fmt_char(spec, &si, '%')) return -1;
    /* A negative dynamic width is equivalent to the '-' flag. */
    if (*p == '*') {
      int w = va ? (int)(va++)->va_int : 0;
      unsigned int magnitude = w < 0 ? 0u - (unsigned int)w : (unsigned int)w;
      if (w < 0) left = 1;
      if (left && !fmt_char(spec, &si, '-')) return -1;
      if (plus && !fmt_char(spec, &si, '+')) return -1;
      if (space && !fmt_char(spec, &si, ' ')) return -1;
      if (alternate && !fmt_char(spec, &si, '#')) return -1;
      if (zero && !fmt_char(spec, &si, '0')) return -1;
      if (magnitude != 0 && !fmt_uint(spec, &si, magnitude)) return -1;
      ++p;
    } else {
      if (left && !fmt_char(spec, &si, '-')) return -1;
      if (plus && !fmt_char(spec, &si, '+')) return -1;
      if (space && !fmt_char(spec, &si, ' ')) return -1;
      if (alternate && !fmt_char(spec, &si, '#')) return -1;
      if (zero && !fmt_char(spec, &si, '0')) return -1;
      while (*p >= '0' && *p <= '9')
        if (!fmt_char(spec, &si, *p++)) return -1;
    }
    if (*p == '.') {
      ++p;
      if (*p == '*') {
        int precision = va ? (int)(va++)->va_int : 0;
        ++p;
        if (precision >= 0 && (!fmt_char(spec, &si, '.') ||
                               !fmt_uint(spec, &si, (unsigned int)precision))) return -1;
      } else {
        if (!fmt_char(spec, &si, '.')) return -1;
        while (*p >= '0' && *p <= '9')
          if (!fmt_char(spec, &si, *p++)) return -1;
      }
    }
    /* 0=32-bit, 1=8-bit, 2=16-bit, 3=64-bit. */
    len_mod = 0;
    if (*p == 'h') { ++p; if (*p == 'h') { ++p; len_mod = 1; } else len_mod = 2; }
    else if (*p == 'l') { ++p; if (*p == 'l') { ++p; len_mod = 3; } }
    else if (*p == 'j') { ++p; len_mod = 3; }
    else if (*p == 'z' || *p == 't') ++p;
    conv = *p;
    if (!conv) break;
    ++p;
    if (conv == 'n') {
      marv_ptr np = va ? MARV_VA_PTR(va++) : MARV_NULL;
      if (np) {
        if (len_mod == 1) marv_i32_store8(np, 0, (marv_i32)sink->idx);
        else if (len_mod == 2) marv_i32_store16(np, 0, (marv_i32)sink->idx);
        else if (len_mod == 3) marv_i64_store(np, 0, (marv_i64)sink->idx);
        else marv_i32_store(np, 0, (marv_i32)sink->idx);
      }
      continue;
    }
    if (len_mod == 3 && (conv == 'd' || conv == 'i' || conv == 'u' ||
                         conv == 'x' || conv == 'X' || conv == 'o')) {
      if (!fmt_char(spec, &si, 'l') || !fmt_char(spec, &si, 'l')) return -1;
    }
    if (!fmt_char(spec, &si, conv)) return -1;
    switch (conv) {
      case 'd': case 'i':
        if (len_mod == 3) {
          long long v = va ? (long long)(va++)->va_llong : 0LL;
          MARV_EMIT_FORMATTED(v);
        } else {
          int v = va ? (int)(va++)->va_int : 0;
          if (len_mod == 1 || len_mod == 2) {
            unsigned int mask = len_mod == 1 ? 255u : 65535u;
            unsigned int sign = len_mod == 1 ? 128u : 32768u;
            unsigned int bits = (unsigned int)v & mask;
            v = bits < sign ? (int)bits : (int)bits - (int)(mask + 1u);
          }
          MARV_EMIT_FORMATTED(v);
        }
        break;
      case 'u': case 'x': case 'X': case 'o':
        if (len_mod == 3) {
          unsigned long long v = va ? (unsigned long long)(va++)->va_ullong : 0ULL;
          MARV_EMIT_FORMATTED(v);
        } else {
          unsigned int v = va ? (unsigned int)(va++)->va_uint : 0U;
          if (len_mod == 1) v &= 255u;
          else if (len_mod == 2) v &= 65535u;
          MARV_EMIT_FORMATTED(v);
        }
        break;
      case 'f': case 'F': case 'e': case 'E': case 'g': case 'G': case 'a': case 'A': {
        double v = va ? (double)(va++)->va_double : 0.0;
        MARV_EMIT_FORMATTED(v);
        break;
      }
      case 'c': {
        int v = va ? (int)(va++)->va_int : 0;
        MARV_EMIT_FORMATTED(v);
        break;
      }
      case 's': {
        const char *v = va ? (const char *)MARV_VA_PTR(va++) : "(null)";
        if (!v) v = "(null)";
        MARV_EMIT_FORMATTED(v);
        break;
      }
      case 'p': {
        void *v = va ? (void *)MARV_VA_PTR(va++) : NULL;
        MARV_EMIT_FORMATTED(v);
        break;
      }
      default: sink_put_char(sink, conv); break;
    }
  }
  if (!sink->fp && sink->buf && sink->maxlen > 0) {
    size_t term = (sink->idx < sink->maxlen) ? sink->idx : (sink->maxlen - 1);
    sink->buf[term] = '\0';
  }
  return (marv_i32)sink->idx;
}
#undef MARV_EMIT_FORMATTED

marv_i32 stdio_vsnprintf(marv_ptr s, marv_i32 count, marv_ptr format, marv_ptr va) {
  marv_out_sink_t sink;
  sink.buf = (char *)s;
  sink.maxlen = (size_t)(uint32_t)count;
  sink.idx = 0;
  sink.fp = NULL;
  return marv_format_engine(&sink, (const char *)format, (const marv_va_arg_t *)(uintptr_t)va);
}

marv_i32 stdio_vsprintf(marv_ptr s, marv_ptr format, marv_ptr va) {
  marv_out_sink_t sink;
  sink.buf = (char *)s;
  sink.maxlen = (size_t)0x7fffffffU;
  sink.idx = 0;
  sink.fp = NULL;
  return marv_format_engine(&sink, (const char *)format, (const marv_va_arg_t *)(uintptr_t)va);
}

marv_i32 stdio_vfprintf(marv_ptr stream, marv_ptr format, marv_ptr va) {
  marv_out_sink_t sink;
  sink.buf = NULL;
  sink.maxlen = 0;
  sink.idx = 0;
  sink.fp = stdio_resolve_fp(stream);
  return marv_format_engine(&sink, (const char *)format, (const marv_va_arg_t *)(uintptr_t)va);
}

marv_i32 stdio_vprintf(marv_ptr format, marv_ptr va) {
  return stdio_vfprintf((marv_ptr)(&stdio__iob[32]), format, va);
}

/* In MARV's WCPL-aligned ABI, `snprintf`, `sprintf`, `fprintf`, `printf` have
 * the exact same signature as their `v*` counterparts (`ap` is the trailing `ptr`). */
marv_i32 stdio_snprintf(marv_ptr s, marv_i32 count, marv_ptr format, marv_ptr ap) {
  return stdio_vsnprintf(s, count, format, ap);
}

marv_i32 stdio_sprintf(marv_ptr s, marv_ptr format, marv_ptr ap) {
  return stdio_vsprintf(s, format, ap);
}

marv_i32 stdio_fprintf(marv_ptr stream, marv_ptr format, marv_ptr ap) {
  return stdio_vfprintf(stream, format, ap);
}

marv_i32 stdio_printf(marv_ptr format, marv_ptr ap) {
  return stdio_vprintf(format, ap);
}

/* --------------------------------------------------------------------------
 * Formatted Input Engine (`scanf`, `fscanf`, `sscanf`, `snscanf`)
 * -------------------------------------------------------------------------- */

/* A single scanner for streams and strings.  pos is the number of consumed
 * bytes, not the amount read ahead: file peeking uses one guaranteed ungetc. */
typedef struct marv_scan_source {
  FILE *fp;
  const char *text;
  size_t pos;
} marv_scan_source_t;

static int scan_peek(marv_scan_source_t *src) {
  int ch;
  if (!src->fp) {
    ch = (unsigned char)src->text[src->pos];
    return ch ? ch : EOF;
  }
  ch = fgetc(src->fp);
  if (ch != EOF && ungetc(ch, src->fp) == EOF) return EOF;
  return ch;
}

static int scan_take(marv_scan_source_t *src) {
  int ch;
  if (src->fp) ch = fgetc(src->fp);
  else {
    ch = (unsigned char)src->text[src->pos];
    if (!ch) ch = EOF;
  }
  if (ch != EOF) ++src->pos;
  return ch;
}

/* The host scanner consumes just one conversion; %n records the exact input
 * consumed including its leading whitespace.  Arguments are typed at each use. */
#define MARV_SCAN_ONE(SRC, SPEC, VALUE, USED) \
  ((SRC)->fp ? fscanf((SRC)->fp, (SPEC), (VALUE), (USED)) : \
               sscanf((SRC)->text + (SRC)->pos, (SPEC), (VALUE), (USED)))
#define MARV_SCAN_SUPPRESSED(SRC, SPEC, USED) \
  ((SRC)->fp ? fscanf((SRC)->fp, (SPEC), (USED)) : \
               sscanf((SRC)->text + (SRC)->pos, (SPEC), (USED)))

static marv_i32 marv_scan_engine(marv_scan_source_t *src, const char *format,
                                  const marv_va_arg_t *ap) {
  const char *fp = format;
  int assigned = 0;
  if (!format) return -1;
  while (*fp) {
    char spec[128];
    size_t si = 0;
    int suppress, len_mod, consumed, r, ch;
    char conv;
    marv_ptr dst;
    if (isspace((unsigned char)*fp)) {
      do { ++fp; } while (isspace((unsigned char)*fp));
      while ((ch = scan_peek(src)) != EOF && isspace((unsigned char)ch)) scan_take(src);
      continue;
    }
    if (*fp != '%' || fp[1] == '%') {
      if (*fp == '%') ++fp;
      ch = scan_peek(src);
      if (ch == EOF) return assigned ? assigned : -1;
      if (ch != (unsigned char)*fp) return assigned;
      scan_take(src); ++fp;
      continue;
    }
    ++fp;
    suppress = (*fp == '*');
    if (suppress) ++fp;
    spec[si++] = '%';
    if (suppress) spec[si++] = '*';
    while (*fp >= '0' && *fp <= '9') {
      if (si + 8 >= sizeof(spec)) return assigned;
      spec[si++] = *fp++;
    }
    /* 0=32-bit, 1=8-bit, 2=16-bit, 3=64-bit, 4=double float. */
    len_mod = 0;
    if (*fp == 'h') { ++fp; if (*fp == 'h') { ++fp; len_mod = 1; } else len_mod = 2; }
    else if (*fp == 'l') { ++fp; if (*fp == 'l') { ++fp; len_mod = 3; } else len_mod = 4; }
    else if (*fp == 'j') { ++fp; len_mod = 3; }
    else if (*fp == 'z' || *fp == 't') ++fp;
    conv = *fp;
    if (!conv) break;
    ++fp;
    if (conv == 'n') {
      if (!suppress && ap) {
        dst = MARV_VA_PTR(ap++);
        if (dst) {
          if (len_mod == 1) marv_i32_store8(dst, 0, (marv_i32)src->pos);
          else if (len_mod == 2) marv_i32_store16(dst, 0, (marv_i32)src->pos);
          else if (len_mod == 3) marv_i64_store(dst, 0, (marv_i64)src->pos);
          else marv_i32_store(dst, 0, (marv_i32)src->pos);
        }
      }
      continue;
    }
    if (conv != 'd' && conv != 'i' && conv != 'u' && conv != 'x' &&
        conv != 'X' && conv != 'o' && conv != 'f' && conv != 'F' &&
        conv != 'e' && conv != 'E' && conv != 'g' && conv != 'G' &&
        conv != 'a' && conv != 'A' && conv != 's' && conv != 'c' &&
        conv != '[' && conv != 'p') return assigned;
    if (!suppress && (conv == 'd' || conv == 'i' || conv == 'u' ||
                      conv == 'x' || conv == 'X' || conv == 'o')) {
      spec[si++] = 'l'; spec[si++] = 'l';
    } else if (!suppress && (conv == 'f' || conv == 'F' || conv == 'e' ||
                             conv == 'E' || conv == 'g' || conv == 'G' ||
                             conv == 'a' || conv == 'A')) spec[si++] = 'l';
    spec[si++] = conv;
    if (conv == '[') {
      if (*fp == '^') spec[si++] = *fp++;
      if (*fp == ']') spec[si++] = *fp++;
      while (*fp && *fp != ']') {
        if (si + 4 >= sizeof(spec)) return assigned;
        spec[si++] = *fp++;
      }
      if (*fp != ']') return assigned;
      spec[si++] = *fp++;
    }
    spec[si++] = '%'; spec[si++] = 'n'; spec[si] = '\0';
    consumed = -1;
    if (suppress) {
      r = MARV_SCAN_SUPPRESSED(src, spec, &consumed);
      if (consumed < 0) return r == EOF && !assigned ? -1 : assigned;
      src->pos += (size_t)consumed;
      continue;
    }
    dst = ap ? MARV_VA_PTR(ap++) : MARV_NULL;
    switch (conv) {
      case 'd': case 'i': {
        long long v = 0;
        r = MARV_SCAN_ONE(src, spec, &v, &consumed);
        if (r != 1) return r == EOF && !assigned ? -1 : assigned;
        if (dst) {
          if (len_mod == 1) marv_i32_store8(dst, 0, (marv_i32)v);
          else if (len_mod == 2) marv_i32_store16(dst, 0, (marv_i32)v);
          else if (len_mod == 3) marv_i64_store(dst, 0, (marv_i64)v);
          else marv_i32_store(dst, 0, (marv_i32)v);
        }
        break;
      }
      case 'u': case 'x': case 'X': case 'o': {
        unsigned long long v = 0;
        r = MARV_SCAN_ONE(src, spec, &v, &consumed);
        if (r != 1) return r == EOF && !assigned ? -1 : assigned;
        if (dst) {
          if (len_mod == 1) marv_i32_store8(dst, 0, (marv_i32)v);
          else if (len_mod == 2) marv_i32_store16(dst, 0, (marv_i32)v);
          else if (len_mod == 3) marv_i64_store(dst, 0, (marv_i64)v);
          else marv_i32_store(dst, 0, (marv_i32)v);
        }
        break;
      }
      case 'f': case 'F': case 'e': case 'E': case 'g': case 'G': case 'a': case 'A': {
        double v = 0;
        r = MARV_SCAN_ONE(src, spec, &v, &consumed);
        if (r != 1) return r == EOF && !assigned ? -1 : assigned;
        if (dst) {
          if (len_mod == 4) marv_f64_store(dst, 0, (marv_f64)v);
          else marv_f32_store(dst, 0, (marv_f32)v);
        }
        break;
      }
      case 'p': {
        void *v = NULL;
        r = MARV_SCAN_ONE(src, spec, &v, &consumed);
        if (r != 1) return r == EOF && !assigned ? -1 : assigned;
        if (dst) marv_ptr_store(dst, 0, v);
        break;
      }
      default:
        if (!dst) return assigned;
        r = MARV_SCAN_ONE(src, spec, (char *)dst, &consumed);
        if (r != 1) return r == EOF && !assigned ? -1 : assigned;
        break;
    }
    if (consumed < 0) return assigned;
    src->pos += (size_t)consumed;
    ++assigned;
  }
  return (marv_i32)assigned;
}
#undef MARV_SCAN_ONE
#undef MARV_SCAN_SUPPRESSED

marv_i32 stdio_vsscanf(marv_ptr s, marv_ptr format, marv_ptr va) {
  marv_scan_source_t src;
  if (!s || !format) return -1;
  src.fp = NULL; src.text = (const char *)s; src.pos = 0;
  return marv_scan_engine(&src, (const char *)format, (const marv_va_arg_t *)(uintptr_t)va);
}

marv_i32 stdio_vsnscanf(marv_ptr s, marv_i32 count, marv_ptr format, marv_ptr va) {
  const char *input = (const char *)s;
  size_t limit = (size_t)(uint32_t)count, len = 0;
  char *copy;
  marv_i32 result;
  if (!input || !format) return -1;
  while (len < limit && input[len] != '\0') ++len;
  if (len == (size_t)-1) return -1;
  copy = (char *)malloc(len + 1);
  if (!copy) return -1;
  memcpy(copy, input, len);
  copy[len] = '\0';
  result = stdio_vsscanf(copy, format, va);
  free(copy);
  return result;
}

marv_i32 stdio_vfscanf(marv_ptr stream, marv_ptr format, marv_ptr va) {
  marv_scan_source_t src;
  src.fp = stdio_resolve_fp(stream); src.text = NULL; src.pos = 0;
  if (!src.fp || !format) return -1;
  return marv_scan_engine(&src, (const char *)format, (const marv_va_arg_t *)(uintptr_t)va);
}

marv_i32 stdio_vscanf(marv_ptr format, marv_ptr va) {
  return stdio_vfscanf((marv_ptr)(&stdio__iob[0]), format, va);
}

marv_i32 stdio_sscanf(marv_ptr s, marv_ptr format, marv_ptr ap) {
  return stdio_vsscanf(s, format, ap);
}

marv_i32 stdio_snscanf(marv_ptr s, marv_i32 count, marv_ptr format, marv_ptr ap) {
  return stdio_vsnscanf(s, count, format, ap);
}

marv_i32 stdio_fscanf(marv_ptr stream, marv_ptr format, marv_ptr ap) {
  return stdio_vfscanf(stream, format, ap);
}

marv_i32 stdio_scanf(marv_ptr format, marv_ptr ap) {
  return stdio_vscanf(format, ap);
}

#endif
