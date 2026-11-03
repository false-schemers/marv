#!/usr/bin/env bash
# Data layout on native C, a big-endian C target when available, WCPL and RV64.
set -euo pipefail
MARV=${1:?usage: bash test/native-data.sh /path/to/marv}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"
CC=${CC:-cc}
work=$(mktemp -d /tmp/marv-data.XXXXXXXX)
trap 'rm -rf "$work"' EXIT

"$MARV" -T target -t cc -c -o "$work/data.c" test/native-data.mvs
"$CC" -std=c99 -O2 "$work/data.c" test/native-data.c -lm -o "$work/native"
test "$("$work/native")" = 'native data OK'
# The zero-only segment remains a compact uninitialized object.
grep -Eq 'unsigned char data_test_zeros\[144\];' "$work/data.c"
# Native scalar anchors preserve run types; copies permit unaligned destinations.
grep -q 'static const uint16_t marv_run_' "$work/data.c"
grep -q 'static const uint32_t marv_run_' "$work/data.c"
grep -q 'static const uint64_t marv_run_' "$work/data.c"
grep -q 'static const float marv_run_' "$work/data.c"
grep -q 'static const double marv_run_' "$work/data.c"
grep -q 'memcpy(data_test_mixed + 1,' "$work/data.c"

# Data, constructors, and later functions must remain correctly linked when
# translation units are split, including the makefile packaging path.
"$MARV" -T target -t cc --split-funcs=1 -o "$work/split" test/native-data.mvs test/native-data.c
test "$("$work/split")" = 'native data OK'
"$MARV" -T target -t cc --split-funcs=1 -M "$work/package" -o app test/native-data.mvs test/native-data.c
make -s -C "$work/package"
test "$("$work/package/app")" = 'native data OK'

# Exercise the WCPL/WASI-hosted compiler too when the suite supplies it.
if [ -n "${TEST_WASI_COMPILER:-}" ] && command -v node >/dev/null 2>&1; then
  MARV_TEST_ROOT="$ROOT" MARV_TEST_OUT="$work" node --no-warnings test/native-data-wasi.js "$TEST_WASI_COMPILER"
  "$CC" -std=c99 -O2 "$work/wasi-data.c" test/native-data.c -lm -o "$work/wasi-native"
  test "$("$work/wasi-native")" = 'native data OK'
  echo '  [PASS] WCPL/WASI-hosted compiler emits correctly initialized native data'
fi

# The WCPL and RV64 targets retain their little-endian scalar images.
"$MARV" -T target -t wcpl -c -o "$work/data.wo" test/native-data.mvs
"$MARV" -T target -t rv64g -c -o "$work/data.s" test/native-data.mvs
grep -Fq '\aa\fe\ff4\12\dc\fe\04\03\02\01' "$work/data.wo"
sed -n '/^data_test_mixed:/,/^  .dword/p' "$work/data.s" | grep -q '^  .byte 170$'
sed -n '/^data_test_mixed:/,/^  .dword/p' "$work/data.s" | grep -q '^  .byte 4$'
grep -q 'ref.data \$data_test:other offset=2' "$work/data.wo"
grep -q '\.dword data_test_other + 2' "$work/data.s"

# Optional real big-endian run. A caller may override both tool names; the
# harness itself verifies that the test actually runs on a big-endian CPU.
BE_CC=${DATA_BE_CC:-powerpc64-linux-gnu-gcc}
BE_RUN=${DATA_BE_RUN:-qemu-ppc64}
if command -v "$BE_CC" >/dev/null 2>&1 && command -v "$BE_RUN" >/dev/null 2>&1; then
  "$BE_CC" -std=c99 -O2 -static -DEXPECT_BIG_ENDIAN "$work/data.c" test/native-data.c -lm -o "$work/big-endian"
  test "$("$BE_RUN" "$work/big-endian")" = 'native data OK'
  if [ -f "$work/wasi-data.c" ]; then
    "$BE_CC" -std=c99 -O2 -static -DEXPECT_BIG_ENDIAN "$work/wasi-data.c" test/native-data.c -lm -o "$work/wasi-big-endian"
    test "$("$BE_RUN" "$work/wasi-big-endian")" = 'native data OK'
  fi
  echo '  [PASS] big-endian PowerPC C execution'
else
  echo '  [SKIP] big-endian C execution (cross compiler or emulator not found)'
fi
echo '  [PASS] native typed data, raw bytes, zeros, relocations, split and package builds'
