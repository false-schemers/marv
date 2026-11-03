#!/usr/bin/env bash
# Verify target defaults and explicit overrides without invoking MSVC.
set -euo pipefail
MARV=${1:?usage: bash test/default-target.sh /path/to/marv}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"
work=$(mktemp -d /tmp/marv-default-target.XXXXXXXX)
trap 'rm -rf "$work"' EXIT

case "$(uname -s)" in
  MINGW*|MSYS*) default=cl;;
  *) default=cc;;
esac
"$MARV" --help | grep -q 'default: cl on Windows, cc elsewhere'
implicit=$("$MARV" -T target -n -o "$work/out" test/native-stdio.c)
explicit=$("$MARV" -T target -t "$default" -n -o "$work/out" test/native-stdio.c)
[ "$implicit" = "$explicit" ]
"$MARV" -T target -v -c -o "$work/default.c" test/characters.mvs 2> "$work/verbose"
grep -q "marv -t $default -c" "$work/verbose"
"$MARV" -T target -t "$default" -c -o "$work/explicit.c" test/characters.mvs
cmp "$work/default.c" "$work/explicit.c"
"$MARV" -T target --target=wcpl -c -o "$work/override.wo" test/characters.mvs
grep -q '^(module ' "$work/override.wo"
if [ "$default" = cc ]; then
  "$MARV" -T target -M "$work/package" -o program test/characters.mvs
  test -f "$work/package/Makefile"
  grep -q '^[[:space:]]*cc ' "$work/package/Makefile"
  make -s -C "$work/package"
  "$work/package/program"
fi

# On Linux, compile the real Windows preprocessor branch with a small directory
# API shim. This checks selection and command generation, not MSVC execution.
if [ "$(uname -s)" = Linux ]; then
  cat > "$work/direct.h" <<'END'
#include <sys/stat.h>
#define _S_IFMT S_IFMT
#define _S_IFDIR S_IFDIR
static int _mkdir(const char *path) { return mkdir(path, 0777); }
END
  ${CC:-cc} -O2 -D_WIN32 -I"$work" -o "$work/windows-marv" \
    m.c r.c v.c a.c -lm
  implicit=$("$work/windows-marv" -T target -n -o out test/native-stdio.c)
  explicit=$("$work/windows-marv" -T target -t cl -n -o out test/native-stdio.c)
  [ "$implicit" = "$explicit" ]
  case "$implicit" in 'cl /nologo '*) :;; *) echo '[FAIL] Windows default is not cl' >&2; exit 1;; esac
  explicit=$("$work/windows-marv" -T target -t cc -n -o out test/native-stdio.c)
  case "$explicit" in 'cc '*) :;; *) echo '[FAIL] explicit Windows target did not override cl' >&2; exit 1;; esac
  "$work/windows-marv" -T target -v -c -o "$work/windows.c" test/characters.mvs 2> "$work/windows-verbose"
  grep -q 'marv -t cl -c' "$work/windows-verbose"
  "$work/windows-marv" -T target -M "$work/windows-package" -o program test/characters.mvs
  test -f "$work/windows-package/nmakefile"
  grep -q '^[[:space:]]*cl /nologo ' "$work/windows-package/nmakefile"
fi

echo '[PASS] platform default targets and explicit overrides'
