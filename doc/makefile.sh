#!/usr/bin/env bash
# End-to-end -M tests: packaged inputs are sufficient after sources move.
set -euo pipefail
MARV=${1:?usage: bash test/makefile.sh /path/to/marv}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"
work=$(mktemp -d /tmp/marv-make.XXXXXXXX)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/temp" "$work/src/sub" "$work/common" "$work/bin"
export TMPDIR="$work/temp" TMP="$work/temp" TEMP="$work/temp"

# A copied C file, recursively included headers, and a binary archive.
cat > "$work/src/main.c" <<'END'
#include"sub/values.h"
#include "shared.h"
int value(void);
int main(void) { return value() != EXPECTED || SHARED != 7; }
END
printf '#include "../answer.h"\n#define EXPECTED ANSWER\n' > "$work/src/sub/values.h"
printf '#define ANSWER 42\n' > "$work/src/answer.h"
printf '#define SHARED 7\n' > "$work/common/shared.h"
printf 'int value(void) { return 42; }\n' > "$work/src/value.c"
${CC:-cc} -c "$work/src/value.c" -o "$work/src/value.o"
${AR:-ar} cr "$work/src/value.a" "$work/src/value.o"
"$MARV" -T "$ROOT/target" -t cc -I "$work/common" --makefile-dir="$work/a directory" -o "$work/outside/my app" "$work/src/main.c" "$work/src/value.a"
test -f "$work/a directory/Makefile"
test ! -e "$work/outside"
test -z "$(ls -A "$work/temp")"
if grep -F "$work/src" "$work/a directory/Makefile"; then echo '[FAIL] external source in Makefile' >&2; exit 1; fi
mv "$work/src" "$work/hidden"
mv "$work/common" "$work/hidden-common"
make -s -C "$work/a directory"
"$work/a directory/my app"
touch "$work/a directory/keep.txt"
make -s -C "$work/a directory" clean
test ! -e "$work/a directory/my app"
test -f "$work/a directory/Makefile"
make -s -C "$work/a directory"
make -s -C "$work/a directory" cleanall
test "$(ls -A "$work/a directory")" = keep.txt

# Generated code, adapter dependencies, explicit optimization, and splitting.
"$MARV" -T "$ROOT/target" -t cc -O1 --split-funcs=1 -M "$work/generated" -o app examples/queens.mvs
test -z "$(ls -A "$work/temp")"
grep -q -- ' -O1 ' "$work/generated/Makefile"
if grep -F "$ROOT" "$work/generated/Makefile"; then echo '[FAIL] external repository path in Makefile' >&2; exit 1; fi
test "$(find "$work/generated" -maxdepth 1 -name '*.c' | wc -l)" -gt 2
mv "$work/generated" "$work/relocated"
make -s -C "$work/relocated"
"$work/relocated/app" > "$work/queens.out"
grep -q 'MARV 8-Queens' "$work/queens.out"
make -s -C "$work/relocated" cleanall
test -z "$(ls -A "$work/relocated")"
# A second generation into a cleanall'ed directory works.
"$MARV" -T "$ROOT/target" -t cc -M "$work/relocated" -o app test/grouped-locals.mvs
make -s -C "$work/relocated"; "$work/relocated/app"

# A precompiled MARV target file still contributes its recorded adapters.
"$MARV" -T "$ROOT/target" -t cc -c -o "$work/precompiled.c" examples/queens.mvs
"$MARV" -T "$ROOT/target" -t cc -M "$work/precompiled" -o queens "$work/precompiled.c"
rm "$work/precompiled.c"
make -s -C "$work/precompiled"
"$work/precompiled/queens" > "$work/precompiled.out"
cmp "$work/queens.out" "$work/precompiled.out"

# WCPL link mode: the command uses only packaged sources plus WCPL's own libs.
if [ -n "${WCPL_BIN:-}" ] && [ -x "$WCPL_BIN" ]; then
  ln -s "$WCPL_BIN" "$work/bin/wcpl"
  PATH="$work/bin:$PATH" "$MARV" -T "$ROOT/target" -t wcpl -O3 -M "$work/wasm" -o app.wasm test/grouped-locals.mvs
  PATH="$work/bin:$PATH" make -s -C "$work/wasm" > "$work/wcpl.log" 2>&1
  test -s "$work/wasm/app.wasm"
  "$MARV" -T "$ROOT/target" -t wcpl -c -o "$work/precompiled.wo" examples/queens.mvs
  PATH="$work/bin:$PATH" "$MARV" -T "$ROOT/target" -t wcpl -M "$work/wasm-deps" -o queens.wasm "$work/precompiled.wo"
  rm "$work/precompiled.wo"
  PATH="$work/bin:$PATH" make -s -C "$work/wasm-deps" > "$work/wcpl-deps.log" 2>&1
  test -s "$work/wasm-deps/queens.wasm"
fi

# RV64G doesn't require a cross compiler just to package.
"$MARV" -T "$ROOT/target" -t rv64g -M "$work/rv" -o app test/grouped-locals.mvs
test -f "$work/rv/Makefile"
find "$work/rv" -name '*.s' | grep -q .
# MSVC receives an nmake file, including .obj cleanup (no MSVC required here).
"$MARV" -T "$ROOT/target" -t cl -O1 -M "$work/msvc" -o app test/grouped-locals.mvs
test -f "$work/msvc/nmakefile"
grep -q 'cl /nologo -O1 /Fe:app.exe' "$work/msvc/nmakefile"
grep -q '\.obj' "$work/msvc/nmakefile"
grep -q '^cleanall: clean' "$work/msvc/nmakefile"

# Existing makefiles and conflicting modes must not clobber user files.
mkdir -p "$work/existing"; echo user > "$work/existing/Makefile"
if "$MARV" -T "$ROOT/target" -t cc -M "$work/existing" -o app test/grouped-locals.mvs > "$work/log" 2>&1; then exit 1; fi
test "$(cat "$work/existing/Makefile")" = user
test "$(ls -A "$work/existing")" = Makefile
for flag in -c -n --count; do
  if "$MARV" -T "$ROOT/target" -t cc "$flag" -M "$work/bad" -o app test/grouped-locals.mvs > "$work/log" 2>&1; then exit 1; fi
done
if "$MARV" -T "$ROOT/target" -t cc -M "$work/missing" -o app "$work/does-not-exist.c" > "$work/log" 2>&1; then exit 1; fi
test -z "$(ls -A "$work/missing")"
test -z "$(ls -A "$work/temp")"
echo '[PASS] relocatable makefile packaging, dependencies, cleanup, and no temporary files'
