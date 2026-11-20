#!/usr/bin/env bash
# Driver optimization-option tests.
set -euo pipefail
MARV=${1:?usage: bash test/optimization.sh /path/to/marv}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"
work=$(mktemp -d /tmp/marv-opt.XXXXXXXX)
trap 'rm -rf "$work"' EXIT
printf 'int main(void) { return 0; }\n' > "$work/input.c"
cat > "$work/cmd.mvt" <<'END'
(marv-target (type cc) (link-cmd "cc ~O0 ~O1 ~O2 ~O3 ~src -o ~exe"))
END
cat > "$work/flags.mvt" <<'END'
(marv-target (type cc) (cc "cc") (cflags ("~O0" "~O1" "~O2" "~O3")))
END
cat > "$work/fixed.mvt" <<'END'
(marv-target (type cc) (link-cmd "cc -O2 ~src -o ~exe"))
END
cat > "$work/embedded.mvt" <<'END'
(marv-target (type cc) (cc "cc") (cflags ("-DLEVELS=~O0,~O3"))
  (ldflags ("~O2")))
END
expect() {
  if [ "$1" != "$2" ]; then
    printf '[FAIL] expected: %s\n          got: %s\n' "$1" "$2" >&2
    exit 1
  fi
}
for target in cmd flags; do
  if [ "$target" = cmd ]; then
    base="cc LEVELS $work/input.c -o $work/out"
  else
    base="cc LEVELS -o $work/out $work/input.c"
  fi
  expect "${base/LEVELS/-O0 -O1 -O2 -O3}" \
    "$("$MARV" -n -t "$work/$target.mvt" -o "$work/out" "$work/input.c")"
  for level in 0 1 2 3; do
    expect "${base/LEVELS/-O$level -O$level -O$level -O$level}" \
      "$("$MARV" -n -t "$work/$target.mvt" -O"$level" -o "$work/out" "$work/input.c")"
  done
done
# Separated option arguments, short-option clusters and repeated overrides.
expect "cc -O3 -O3 -O3 -O3 $work/input.c -o $work/out" \
  "$("$MARV" -nO1 -O 3 -t "$work/cmd.mvt" -o "$work/out" "$work/input.c")"
# Placeholders are recognized anywhere in cflags strings, but not ldflags.
expect "cc -DLEVELS=-O1,-O1 ~O2 -o $work/out $work/input.c" \
  "$("$MARV" -n -O1 -t "$work/embedded.mvt" -o "$work/out" "$work/input.c")"
# A literal -O2 is not a template, even with an override.
expect "cc -O2 $work/input.c -o $work/out" \
  "$("$MARV" -n -O0 -t "$work/fixed.mvt" -o "$work/out" "$work/input.c")"
# The bundled Unix compiler targets use ~O2 and retain their default.
for target in cc gcc clang cl rv64g rv64g-cross; do
  default=$("$MARV" -n -T target -t "$target" -o "$work/out" "$work/input.c")
  override=$("$MARV" -n -T target -t "$target" -O1 -o "$work/out" "$work/input.c")
  case " $default " in *' -O2 '*) :;; *) echo "[FAIL] $target default: $default" >&2; exit 1;; esac
  case " $override " in *' -O1 '*) :;; *) echo "[FAIL] $target override: $override" >&2; exit 1;; esac
done
# MSVC receives the substitution saturated at 2, on both copies of the target
# file: there is no /O3, and cl answers an unknown /O letter with a warning
# and the default, so passing 3 through would compile SLOWER than asking for
# 2.  This checks MARV's command only.
for description in target/cl.mvt targets/cl.mvt; do
  [ -f "$description" ] || continue
  for level in 0 1 2; do
    expect "cl /nologo -O$level /Fe:$work/out $work/input.c" \
      "$("$MARV" -n -t "$ROOT/$description" -O"$level" -o "$work/out" "$work/input.c")"
  done
  expect "cl /nologo -O2 /Fe:$work/out $work/input.c" \
    "$("$MARV" -n -t "$ROOT/$description" -O3 -o "$work/out" "$work/input.c")"
  expect "cl /nologo -O2 /Fe:$work/out $work/input.c" \
    "$("$MARV" -n -t "$ROOT/$description" -o "$work/out" "$work/input.c")"
done
# No placeholder: WCPL ignores the override.
for target in wcpl; do
  default=$("$MARV" -n -T target -t "$target" -o "$work/out" "$work/input.c")
  override=$("$MARV" -n -T target -t "$target" -O3 -o "$work/out" "$work/input.c")
  expect "$default" "$override"
done
for bad in '' 4 -1 10 01 s 2x =2; do
  if "$MARV" "-O$bad" -t "$work/cmd.mvt" -o "$work/out" "$work/input.c" > "$work/log" 2>&1; then
    echo "[FAIL] accepted invalid -O value '$bad'" >&2; exit 1
  fi
  grep -q -- 'marv: -O' "$work/log"
done
if "$MARV" -O > "$work/log" 2>&1; then
  echo '[FAIL] accepted -O without an argument' >&2; exit 1
fi
# egetopt words a missing option argument the same way in every tool here
grep -q -- 'marv: option -O requires an argument' "$work/log"
# WCPL compile-only output is unchanged by the ignored optimization option.
"$MARV" -T target -t wcpl -c -o "$work/default.wo" test/grouped-locals.mvs
"$MARV" -T target -t wcpl -O3 -c -o "$work/opt.wo" test/grouped-locals.mvs
cmp "$work/default.wo" "$work/opt.wo"
# Compile-only generates identical MARV output for every optimization level.
"$MARV" -T target -t cc -c -o "$work/default.c" test/grouped-locals.mvs
for level in 0 1 2 3; do
  "$MARV" -T target -t cc -O"$level" -c -o "$work/opt.c" test/grouped-locals.mvs
  cmp "$work/default.c" "$work/opt.c"
  "$MARV" -T target -t cc -O"$level" -o "$work/prog" test/grouped-locals.mvs
  "$work/prog"
done
# -O3 on a non-cl cc target turns the splitting off unless -K says otherwise:
# the split walls each piece off from the inliner, which is the wrong trade
# at the level that asks for every optimization.  cl is exempt because it
# inlines across translation units.  Needs a module with more functions than
# the default split of 300, so the counts can differ.
pieces() {   # $1 = target, rest = options
  local t="$1"; shift
  "$MARV" -n -T target -t "$t" "$@" -o "$work/out" "$work/many.mvs" \
    | head -1 | grep -oE 'marv_[0-9]+_[0-9]+\.c' | wc -l | tr -d ' '
}
{
  echo "(module \$manyfuncs"
  i=0
  while [ "$i" -lt 400 ]; do
    echo "  (func \$f$i (result i32) (return (i32.const $i)))"
    i=$((i+1))
  done
  echo "  (func \$main (export \"main\") (result i32) (return (i32.const 0)))"
  echo ")"
} > "$work/many.mvs"
expect 2 "$(pieces cc)"                 # 401 functions, default split 300
expect 1 "$(pieces cc -O3)"             # -O3 implies no split
expect 2 "$(pieces cc -O3 -K300)"       # an explicit -K wins
expect 5 "$(pieces cc -O3 -K100)"       # and is honoured exactly
expect 2 "$(pieces cl -O3)"             # cl inlines across units: unchanged
echo '[PASS] -O defaults, overrides, fallback flags, targets and diagnostics'
