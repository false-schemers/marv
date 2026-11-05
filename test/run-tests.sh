#!/usr/bin/env bash
set -euo pipefail

SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# All generated files live in a private copy; the runner never deletes user
# examples or clobbers another run's /tmp files.
TEST_WORK=$(mktemp -d /tmp/marv-suite.XXXXXXXX)
trap 'rm -rf "$TEST_WORK"' EXIT
ROOT_DIR="$TEST_WORK/repo"
mkdir -p "$ROOT_DIR"
cp "$SOURCE_DIR"/{m,r,v,a}.{c,h} "$ROOT_DIR/"
cp -R "$SOURCE_DIR"/{test,examples,lib,srt,target} "$ROOT_DIR/"
cd "$ROOT_DIR"

# Tool locations. Everything can be overridden from the environment so the
# suite runs outside the author's machine; the /home/user paths stay as the
# fallback for that machine.  [cco]
#   CC, SKINT_BIN, SKINT_LIB, WCPL_BIN, WCPL_HOME, MARV_BINDIR
CC=${CC:-cc}
SKINT_BIN=${SKINT_BIN:-skint}
WCPL_BIN=${WCPL_BIN:-wcpl}

if [ -d "/home/user/bin" ]; then
  export PATH="/home/user/bin:$PATH"
fi

SKINT_LIB=${SKINT_LIB:-}
if [ -z "$SKINT_LIB" ] && [ -d "/home/user/skint/lib" ]; then
  SKINT_LIB="/home/user/skint/lib"
fi
SKINT_FLAGS=""
[ -n "$SKINT_LIB" ] && SKINT_FLAGS="-I $SKINT_LIB"

SKINT="$SKINT_BIN $SKINT_FLAGS -q"
MARV_BIN="$TEST_WORK/gh_marv_bin"

WCPL_HOME=${WCPL_HOME:-/home/user/wcpl}
WCPL_INC=""
if [ -d "$WCPL_HOME/lib/include" ]; then
  WCPL_INC="-I$WCPL_HOME/lib/include/ -L$WCPL_HOME/lib/"
fi
# srt/scmi.c includes srt/scmrt.h: wcpl joins an include base to the file name
# as given, so these bases need their trailing slash.
MARV_INC="-I$ROOT_DIR/lib/ -I$ROOT_DIR/srt/"


NADAPTERS=$(ls lib/*.c srt/*.c | wc -l)

echo "0. Building the marv compiler from m.c r.c v.c a.c..."
$CC -O2 -o "$MARV_BIN" "$ROOT_DIR/m.c" "$ROOT_DIR/r.c" "$ROOT_DIR/v.c" "$ROOT_DIR/a.c" -lm
if [ -n "${MARV_BINDIR:-}" ]; then
  mkdir -p "$MARV_BINDIR"
  cp -f "$MARV_BIN" "$MARV_BINDIR/marv"
fi
echo "  [PASS] Built the compiler ($MARV_BIN) from m.c r.c v.c a.c!"

echo ""
echo "1. Verifying all $NADAPTERS single-source adapter files in lib/*.c and srt/*.c compile under both cc and wcpl..."
for f in lib/*.c srt/*.c; do
  b=$(basename "$f" .c)
  $CC -std=c99 -O2 -Wall -Wextra -Werror -c "$f" -o "$TEST_WORK/gh_marv_${b}.o"
  $WCPL_BIN -q $WCPL_INC $MARV_INC -c -o "$TEST_WORK/gh_marv_${b}.wo" "$f"
done
echo "  [PASS] All $NADAPTERS lib/*.c and srt/*.c adapters compiled cleanly under both cc and wcpl -c!"

echo ""
echo "2. Testing backend optimization selection..."
bash test/optimization.sh "$MARV_BIN"

echo ""
echo "3. Testing the marv binary (-c mode and link mode)..."
for ex in dirent_demo libc_demo queens; do
  for t in cc wcpl rv64g; do
    case $t in cc) ext=c;; wcpl) ext=wo;; rv64g) ext=s;; esac
    "$MARV_BIN" -T target -t "$t" -c "examples/$ex.mvs"
    test -f "examples/$ex.$ext"
  done
done
rm -f examples/*.c examples/*.wo examples/*.s
echo "  [PASS] marv generated .c, .wo and .s for every example!"

"$MARV_BIN" -v -T target -t cc -o $TEST_WORK/gh_marv_dirent examples/dirent_demo.mvs 2> $TEST_WORK/gh_marv_verbose.err
grep -q "^marv -t cc -c -o .* examples/dirent_demo.mvs$" $TEST_WORK/gh_marv_verbose.err
grep -q "^cc .* -o $TEST_WORK/gh_marv_dirent " $TEST_WORK/gh_marv_verbose.err
test ! -f examples/dirent_demo.c
"$MARV_BIN" -T target -t cc -o $TEST_WORK/gh_marv_libc   examples/libc_demo.mvs
test ! -f examples/libc_demo.c
"$MARV_BIN" -T target -t cc -o $TEST_WORK/gh_marv_queens examples/queens.mvs
test ! -f examples/queens.c

$TEST_WORK/gh_marv_dirent > $TEST_WORK/gh_marv_dirent_cc.out
$TEST_WORK/gh_marv_libc
$TEST_WORK/gh_marv_queens > $TEST_WORK/gh_marv_queens_cc.out
echo "  [PASS] Standalone marv compile-and-link mode (-t cc) built and ran all examples using cleaned-up temporary objects!"

echo ""
echo "4. Testing 3-module separate compilation & mixed (.c + .mvs) linking with standalone marv..."
"$MARV_BIN" -T target -t cc -c examples/multimod/vec3.mvs
"$MARV_BIN" -T target -t cc -o $TEST_WORK/gh_marv_multimod \
  examples/multimod/vec3.c examples/multimod/stats.mvs examples/multimod/app.mvs
$TEST_WORK/gh_marv_multimod > $TEST_WORK/gh_marv_mm_cc.out
cat $TEST_WORK/gh_marv_mm_cc.out
echo "  [PASS] 3-module mixed object + .mvs build succeeded!"

echo ""
echo "5. Testing SRFI-270 hexadecimal floating-point literals with standalone marv..."
cat > $TEST_WORK/gh_marv_hexflt.mvs << 'EOF'
(module $hexflt
  (func $main (export "main")
    (result i32)
    (local $pi f64)
    (local $half f32)
    (local.set $pi (f64.const #x1.921fb54442d18p+1))
    (local.set $half (f32.const #x1.0p-1))
    (if (i32.and
          (f64.eq (local.get $pi) (f64.const 3.141592653589793))
          (f32.eq (local.get $half) (f32.const 0.5)))
      (then (return (i32.const 0)))
      (else (return (i32.const 1))))))
EOF
"$MARV_BIN" -T target -t cc   -c -o $TEST_WORK/gh_marv_hexflt.c  $TEST_WORK/gh_marv_hexflt.mvs
"$MARV_BIN" -T target -t wcpl -c -o $TEST_WORK/gh_marv_hexflt.wo $TEST_WORK/gh_marv_hexflt.mvs
"$MARV_BIN" -T target -t rv64g -c -o $TEST_WORK/gh_marv_hexflt.s $TEST_WORK/gh_marv_hexflt.mvs
"$MARV_BIN" -T target -t cc -o $TEST_WORK/gh_marv_hexflt_bin $TEST_WORK/gh_marv_hexflt.mvs
$TEST_WORK/gh_marv_hexflt_bin
$WCPL_BIN -q $WCPL_INC -o $TEST_WORK/gh_marv_hexflt.wasm $TEST_WORK/gh_marv_hexflt.wo
test -s $TEST_WORK/gh_marv_hexflt.wasm
echo "  [PASS] SRFI-270 hex float parsing and emission verified across cc, wcpl, and rv64g!"

echo ""
echo "6. Checking instruction edge cases (test/edge.mvs) against test/edge.expected..."
$SKINT test/rv64-sim-numbers.scm
# shift masking, signed/unsigned division, clz/ctz of zero, sign extension,
# truncation, nearest-ties-to-even, narrow loads, br_table defaults.  [cco]
"$MARV_BIN" -T target -t cc -o $TEST_WORK/gh_marv_edge test/edge.mvs
$TEST_WORK/gh_marv_edge > $TEST_WORK/gh_marv_edge.out
diff -u test/edge.expected $TEST_WORK/gh_marv_edge.out
# the same values must come out of the rv64g assembly
"$MARV_BIN" -T target -t rv64g -c -o $TEST_WORK/gh_marv_edge.s test/edge.mvs
$SKINT test/edge-rv64.scm "$TEST_WORK/gh_marv_edge.s" > $TEST_WORK/gh_marv_edge_rv.out
diff -u test/edge.expected $TEST_WORK/gh_marv_edge_rv.out
echo "  [PASS] All 24 edge cases agree between cc, rv64g and WebAssembly semantics!"

echo ""
echo "7. Testing configure --prefix and -D PREFIX installation lookup..."
mkdir -p $TEST_WORK/gh_marv_pfx/share/marv/target $TEST_WORK/gh_marv_pfx/share/marv/lib
cp target/*.mvt $TEST_WORK/gh_marv_pfx/share/marv/target/
cp lib/*.mvh lib/*.c $TEST_WORK/gh_marv_pfx/share/marv/lib/
$CC -O2 -D PREFIX=$TEST_WORK/gh_marv_pfx/share/marv/ -o $TEST_WORK/gh_marv_pfx_bin "$ROOT_DIR/m.c" "$ROOT_DIR/r.c" "$ROOT_DIR/v.c" "$ROOT_DIR/a.c" -lm
(cd /tmp && $TEST_WORK/gh_marv_pfx_bin -t cc -o $TEST_WORK/gh_marv_pfx_queens "$ROOT_DIR/examples/queens.mvs")
$TEST_WORK/gh_marv_pfx_queens > $TEST_WORK/gh_marv_pfx_queens.out
diff -u $TEST_WORK/gh_marv_queens_cc.out $TEST_WORK/gh_marv_pfx_queens.out
rm -rf $TEST_WORK/gh_marv_pfx
echo "  [PASS] -D PREFIX target and library lookup verified from out-of-tree directory!"

echo ""
echo "8. Checking that the compiler sources in the root are the complete set..."
# Each unit is one .c with its own .h, and nothing generates them: a build that
# finds a unit without its header, or a .c that is not the one it should be, is
# a tree someone has overwritten.  [cco]
for u in m r v a; do
  test -f "$ROOT_DIR/$u.c"
  test -f "$ROOT_DIR/$u.h"
  head -1 "$ROOT_DIR/$u.c" | grep -qE "^/\* $u\.c \(marv "
  head -1 "$ROOT_DIR/$u.h" | grep -qE "^/\* $u\.h \(marv "
done
echo "  [PASS] m r v a are each a .c with its own .h, and all four are intact!"
for u in m r v a; do
  $WCPL_BIN -q $WCPL_INC -I"$ROOT_DIR/" -c \
    -o "$TEST_WORK/wcpl_${u}.wo" "$ROOT_DIR/$u.c"
done
$WCPL_BIN -q $WCPL_INC -o "$TEST_WORK/wcpl_marv.wasm" \
  "$TEST_WORK/wcpl_m.wo" "$TEST_WORK/wcpl_r.wo" \
  "$TEST_WORK/wcpl_v.wo" "$TEST_WORK/wcpl_a.wo"
test -s "$TEST_WORK/wcpl_marv.wasm"
echo "  [PASS] All four compiler units compile and link with WCPL!"


echo ""
echo "9. Testing native stdio formatting and scanning regressions..."
cat > "$TEST_WORK/stdio-regression.mvs" <<'EOF'
(include system $stdio)
(module $stdio_regression
  (data $wide const align 1 "%600d\x00;")
  (data $small const align 1 "%hhd\x00;")
  (func $main (export "main") (result i32)
    (frame 16)
    (if (i32.ne (call $snprintf (frame.addr 0) (i32.const 16)
                                (ref.data $wide) (i32.const 42))
                (i32.const 600))
      (then (return (i32.const 1))))
    (if (i32.ne (i32.load8_u (frame.addr 15)) (i32.const 0))
      (then (return (i32.const 2))))
    (drop (call $snprintf (frame.addr 0) (i32.const 16)
                          (ref.data $small) (i32.const 257)))
    (if (i32.ne (i32.load8_u (frame.addr 0)) (i32.const 49))
      (then (return (i32.const 3))))
    (return (i32.const 0))))
EOF
"$MARV_BIN" -T target -t cc -o "$TEST_WORK/stdio-regression" "$TEST_WORK/stdio-regression.mvs"
"$TEST_WORK/stdio-regression"
$CC -std=c99 -O2 -Wall -Wextra -Werror -o "$TEST_WORK/native-stdio" \
  test/native-stdio.c lib/stdio.c lib/errno.c -lm
"$TEST_WORK/native-stdio"

echo ""
echo "10. Testing paths, precompiled dependencies, and failed-link cleanup..."
mkdir -p "$TEST_WORK/path with spaces/lib" "$TEST_WORK/path with spaces/tmp"
cp lib/*.mvh lib/*.c "$TEST_WORK/path with spaces/lib/"
"$MARV_BIN" -T target -t cc -c -o "$TEST_WORK/path with spaces/object file.c" examples/queens.mvs
(cd "$TEST_WORK" && "$MARV_BIN" -T "$ROOT_DIR/target" \
  -L "$TEST_WORK/path with spaces/lib" -t cc \
  -o "$TEST_WORK/path with spaces/program file" "$TEST_WORK/path with spaces/object file.c")
"$TEST_WORK/path with spaces/program file" > "$TEST_WORK/path-result"
diff -u "$TEST_WORK/gh_marv_queens_cc.out" "$TEST_WORK/path-result"
TMPDIR="$TEST_WORK/path with spaces/tmp" "$MARV_BIN" -T target -t cc \
  -o "$TEST_WORK/path with spaces/compiled source" examples/queens.mvs
"$TEST_WORK/path with spaces/compiled source" > "$TEST_WORK/path-result"
diff -u "$TEST_WORK/gh_marv_queens_cc.out" "$TEST_WORK/path-result"
if TMPDIR="$TEST_WORK/path with spaces/tmp" "$MARV_BIN" -T target -t cc \
  -o "$TEST_WORK/nonexistent-dir/out" examples/queens.mvs > /dev/null 2>&1; then
  echo "[FAIL] link to nonexistent directory unexpectedly succeeded" >&2; exit 1
fi
if [ -n "$(ls -A "$TEST_WORK/path with spaces/tmp")" ]; then
  echo "[FAIL] temporary objects survived a failed link" >&2; exit 1
fi
if TMPDIR="$TEST_WORK/path with spaces/tmp" "$MARV_BIN" -T target -t cc -o "$TEST_WORK/out;touch $TEST_WORK/injected" \
  examples/queens.mvs > /dev/null 2>&1; then
  echo "[FAIL] shell-sensitive path was accepted" >&2; exit 1
fi
test ! -e "$TEST_WORK/injected"
test -z "$(ls -A "$TEST_WORK/path with spaces/tmp")"
TMPDIR="$TEST_WORK/path with spaces/tmp" "$MARV_BIN" -n -T target -t cc \
  -o "$TEST_WORK/path with spaces/dry run" examples/queens.mvs > "$TEST_WORK/dry-run.sh"
bash "$TEST_WORK/dry-run.sh"
"$TEST_WORK/path with spaces/dry run" > "$TEST_WORK/path-result"
diff -u "$TEST_WORK/gh_marv_queens_cc.out" "$TEST_WORK/path-result"
test -z "$(ls -A "$TEST_WORK/path with spaces/tmp")"
printf '(module $empty)\n' > "$TEST_WORK/empty.mvs"
"$MARV_BIN" -T target -t cc -c -o "$TEST_WORK/empty.c" "$TEST_WORK/empty.mvs"
test -s "$TEST_WORK/empty.c"
echo "  [PASS] spaces, dependencies, invalid paths and failed-link cleanup!"

echo ""
echo "11. Testing grouped local and parameter declarations..."
# Compare with the equivalent individual declarations on every backend.
sed \
  -e 's/(local \$p \$q \$r ptr)/(local $p ptr) (local $q ptr) (local $r ptr)/' \
  -e 's/(local \$i \$j \$k i32)/(local $i i32) (local $j i32) (local $k i32)/' \
  -e 's/(local \$x \$y i64)/(local $x i64) (local $y i64)/' \
  -e 's/(local \$f \$g f32)/(local $f f32) (local $g f32)/' \
  -e 's/(local \$d \$e f64)/(local $d f64) (local $e f64)/' \
  -e 's/(param \$a \$b \$c i64)/(param $a i64) (param $b i64) (param $c i64)/' \
  -e 's/(param \$p \$q ptr)/(param $p ptr) (param $q ptr)/' \
  -e 's/(param \$x \$y f64)/(param $x f64) (param $y f64)/' \
  test/grouped-locals.mvs > "$TEST_WORK/grouped-expanded.mvs"
for t in cc wcpl rv64g; do
  case $t in cc) ext=c;; wcpl) ext=wo;; rv64g) ext=s;; esac
  "$MARV_BIN" -T target -t "$t" -c -o "$TEST_WORK/grouped.$ext" test/grouped-locals.mvs
  "$MARV_BIN" -T target -t "$t" -c -o "$TEST_WORK/expanded.$ext" "$TEST_WORK/grouped-expanded.mvs"
  diff -u "$TEST_WORK/expanded.$ext" "$TEST_WORK/grouped.$ext"
done
"$MARV_BIN" -T target -t cc -o "$TEST_WORK/grouped" test/grouped-locals.mvs
"$TEST_WORK/grouped"
$SKINT test/grouped-locals-rv64.scm "$TEST_WORK/grouped.s"
$WCPL_BIN -q $WCPL_INC -o "$TEST_WORK/grouped.wasm" "$TEST_WORK/grouped.wo"
test -s "$TEST_WORK/grouped.wasm"
cat > "$TEST_WORK/grouped-trampoline.mvs" <<'END'
(module $grouped_trampoline
  (trampoline_func $entry (result ptr)
    (local $x $y ptr)
    (return (local.get $y)))
  (func $main (export "main") (result i32)
    (if (ptr.eqz (call $entry))
      (then (return (i32.const 0)))
      (else (return (i32.const 1))))))
END
"$MARV_BIN" -T target -t cc -o "$TEST_WORK/grouped-trampoline" "$TEST_WORK/grouped-trampoline.mvs"
"$TEST_WORK/grouped-trampoline"
for local_decl in '(local)' '(local $x)' '(local $x unknown)' '(local $x 7 ptr)' '(local $x i32 $y)' \
                 '(param)' '(param $x)' '(param $x unknown)' '(param $x 7 i64)' '(param $x i64 $y)'; do
  printf '(module $invalid (func $main (result i32) %s (return (i32.const 0))))\n' "$local_decl" > "$TEST_WORK/invalid.mvs"
  if "$MARV_BIN" -T target -t cc -c -o "$TEST_WORK/invalid.c" "$TEST_WORK/invalid.mvs" > /dev/null 2>&1; then
    echo "[FAIL] accepted invalid declaration: $local_decl" >&2; exit 1
  fi
done
echo "  [PASS] grouped locals and params, macro typing, equivalence and diagnostics!"


echo ""
echo "12. Testing makefile-directory packaging..."
CC="$CC" WCPL_BIN="$WCPL_BIN" bash test/makefile.sh "$MARV_BIN"


echo ""
echo "13. Testing R7RS character literals, radix integers and name mangling..."
SKINT_BIN="$SKINT_BIN" SKINT_LIB="$SKINT_LIB" WCPL_BIN="$WCPL_BIN" WCPL_HOME="$WCPL_HOME" \
  bash test/characters.sh "$MARV_BIN"

echo ""
echo "14. Testing platform default targets..."
CC="$CC" bash test/default-target.sh "$MARV_BIN"

echo ""
echo "15. Testing target-native typed data initialization..."
CC="$CC" TEST_WASI_COMPILER="$TEST_WORK/wcpl_marv.wasm" bash test/native-data.sh "$MARV_BIN"
