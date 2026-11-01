/* Native MARV stdio adapter regressions.  Arguments are 16-byte MARV slots. */
#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef union slot {
  int32_t i;
  uint32_t u;
  int64_t ll;
  uint64_t p;
  double d;
  uint64_t pad[2];
} slot_t;
extern int32_t stdio_snprintf(void *, int32_t, void *, void *);
extern int32_t stdio_sscanf(void *, void *, void *);
extern int32_t stdio_snscanf(void *, int32_t, void *, void *);
extern int32_t stdio_fscanf(void *, void *, void *);

static int failures;
#define CHECK(C) do { if (!(C)) { \
  fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__, #C); ++failures; \
} } while (0)
#define PTR(S, P) ((S).p = (uint64_t)(uintptr_t)(P))

int main(void)
{
  slot_t a[4];
  char buf[1024], tiny[16], word[32], raw[2] = {'1', '2'};
  unsigned char counts[8];
  int32_t x, y, count;
  FILE *fp;
  int n;
  CHECK(sizeof(slot_t) == 16);
  memset(a, 0, sizeof(a)); a[0].i = 42;
  memset(tiny, 'x', sizeof(tiny));
  n = stdio_snprintf(tiny, sizeof(tiny), "%600d", a);
  CHECK(n == 600 && tiny[15] == '\0' && tiny[0] == ' ');
  n = stdio_snprintf(buf, sizeof(buf), "%600d", a);
  CHECK(n == 600 && strlen(buf) == 600 && buf[598] == '4' && buf[599] == '2');
  memset(a, 0, sizeof(a)); a[0].d = 1.0;
  n = stdio_snprintf(tiny, sizeof(tiny), "%.600f", a);
  CHECK(n == 602 && !strncmp(tiny, "1.000", 5) && tiny[15] == '\0');
  memset(a, 0, sizeof(a)); a[0].i = 257; a[1].i = 65535;
  stdio_snprintf(buf, sizeof(buf), "%hhd %hd", a);
  CHECK(!strcmp(buf, "1 -1"));
  memset(a, 0, sizeof(a)); a[0].i = -6; a[1].i = 12;
  stdio_snprintf(buf, sizeof(buf), "%*d", a);
  CHECK(!strcmp(buf, "12    "));
  memset(a, 0, sizeof(a)); a[0].i = -1; a[1].d = 1.25;
  stdio_snprintf(buf, sizeof(buf), "%.*f", a);
  CHECK(!strcmp(buf, "1.250000"));
  memset(a, 0, sizeof(a)); a[0].i = 0; PTR(a[1], "ok");
  stdio_snprintf(buf, sizeof(buf), "%*s", a);
  CHECK(!strcmp(buf, "ok"));
  memset(counts, 0xaa, sizeof(counts)); memset(a, 0, sizeof(a)); PTR(a[0], counts + 1);
  stdio_snprintf(buf, sizeof(buf), "abc%hhn", a);
  CHECK(counts[0] == 0xaa && counts[1] == 3 && counts[2] == 0xaa);

  memset(a, 0, sizeof(a)); x = -1; PTR(a[0], &x);
  CHECK(stdio_snscanf(raw, 2, "%d", a) == 1 && x == 12);
  x = -1; CHECK(stdio_snscanf("12345", 2, "%d", a) == 1 && x == 12);
  CHECK(stdio_snscanf(raw, 0, "%d", a) == -1);
  x = y = count = -1; memset(a, 0, sizeof(a));
  PTR(a[0], &x); PTR(a[1], &count); PTR(a[2], &y);
  CHECK(stdio_sscanf(" 12 34", "%d%n%d", a) == 2);
  CHECK(x == 12 && count == 3 && y == 34);
  x = -1; memset(a, 0, sizeof(a)); PTR(a[0], &x);
  CHECK(stdio_sscanf("19 42", "%*d %d", a) == 1 && x == 42);
  memset(word, 0, sizeof(word)); memset(a, 0, sizeof(a)); PTR(a[0], word);
  CHECK(stdio_sscanf("abc123", "%3[a-z]", a) == 1 && !strcmp(word, "abc"));
  x = -1; memset(a, 0, sizeof(a)); PTR(a[0], &x);
  CHECK(stdio_sscanf("x", "%d", a) == 0 && x == -1);
  CHECK(stdio_sscanf("", "%d", a) == -1);
  fp = tmpfile(); CHECK(fp != NULL);
  if (fp) {
    fputs("12 34\n", fp); rewind(fp);
    x = y = -1; memset(a, 0, sizeof(a)); PTR(a[0], &x);
    CHECK(stdio_fscanf(fp, "%d", a) == 1 && x == 12);
    PTR(a[0], &y);
    CHECK(stdio_fscanf(fp, "%d", a) == 1 && y == 34);
    CHECK(stdio_fscanf(fp, "%d", a) == -1);
    fclose(fp);
  }
  if (failures) return 1;
  puts("[PASS] native stdio adapter regressions");
  return 0;
}
