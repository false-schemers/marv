/* Exercise the actual bytes emitted for a MARV data segment on this host. */
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

extern unsigned char data_test_mixed[];
extern unsigned char data_test_other[];
extern unsigned char data_test_zeros[];
extern unsigned char data_test_typed[];
extern unsigned char data_test_floats[];
extern int32_t data_test_answer(void);

static int check(const void *p, size_t n, size_t *off, const char *name)
{
  if (memcmp(data_test_mixed + *off, p, n) != 0) {
    fprintf(stderr, "%s differs at offset %lu\n", name, (unsigned long)*off);
    return 1;
  }
  *off += n;
  return 0;
}

int main(void)
{
  size_t off, i;
  unsigned char raw1[] = {170}, raw2[] = {255,128,1,0,0,0};
  uint16_t a16[] = {65534u, 0x1234u, 0xfedcu};
  uint32_t a32[] = {0x01020304u, 0xffffffffu, 0x89abcdefu};
  uint64_t a64[] = {UINT64_C(0x1122334455667788), UINT64_C(0xffffffffffffffff)};
  float a32f[] = {1.5f, -0.0f, (float)HUGE_VAL};
  double a64f[] = {-2.25, -0.0, -HUGE_VAL};
  float f_nan;
  double d_nan;
  void *p, *fn;
  int32_t typed32;
  double typed64;
  float extremes32[] = {0x1.0p-149f, 0x1.fffffep+127f};
  double extremes64[] = {0x1.0p-1074, 0x1.fffffffffffffp+1023};
#ifdef EXPECT_BIG_ENDIAN
  {
    uint16_t endian_test = 1;
    if (*(unsigned char *)&endian_test != 0) {
      fputs("expected a big-endian test target\n", stderr); return 12;
    }
  }
#endif
  off = 0;
  if (check(raw1, sizeof raw1, &off, "raw") ||
      check(a16, sizeof a16, &off, "i16/u16") ||
      check(a32, sizeof a32, &off, "i32/u32") ||
      check(a64, sizeof a64, &off, "i64") ||
      check(a32f, sizeof a32f, &off, "f32")) return 1;
  memcpy(&f_nan, data_test_mixed + off, sizeof f_nan); off += sizeof f_nan;
  if (f_nan == f_nan) return 2;
  if (check(a64f, sizeof a64f, &off, "f64")) return 3;
  memcpy(&d_nan, data_test_mixed + off, sizeof d_nan); off += sizeof d_nan;
  if (d_nan == d_nan) return 4;
  if (check(raw2, sizeof raw2, &off, "bytes and zeros")) return 5;
  memcpy(&p, data_test_mixed + off, sizeof p); off += 8;
  if (p != (void *)(data_test_other + 2)) return 6;
  memcpy(&fn, data_test_mixed + off, sizeof fn); off += 8;
  if (fn != (void *)data_test_answer || ((int32_t (*)(void))fn)() != 42) return 7;
  memcpy(&p, data_test_mixed + off, sizeof p);
  if (p != NULL) return 8;
  for (i = 0; i < 144; ++i) if (data_test_zeros[i]) return 9;
  memcpy(&typed32, data_test_typed, sizeof typed32);
  memcpy(&typed64, data_test_typed + 4, sizeof typed64);
  if (typed32 != 123456789 || typed64 != 3.25) return 10;
  if (memcmp(data_test_floats, extremes32, sizeof extremes32) ||
      memcmp(data_test_floats + sizeof extremes32, extremes64, sizeof extremes64)) return 11;
  puts("native data OK");
  return 0;
}
