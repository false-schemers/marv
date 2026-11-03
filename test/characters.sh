#!/usr/bin/env bash
# Reader and backend regression tests, independent of the historical src tree.
set -euo pipefail
MARV=${1:?usage: bash test/characters.sh /path/to/marv}
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"
work=$(mktemp -d /tmp/marv-chars.XXXXXXXX)
trap 'rm -rf "$work"' EXIT
cat > "$work/tokens" <<'END'
(#\A #\a #\x #\X #\é #\λ #\€ #\😀 #\x03bB #\X00003BB
 #\alarm #\backspace #\delete #\escape #\newline #\null #\return #\space #\tab
 #\( #\) #\; #\" #\\ #\  #\x0 #\xD7FF #\xE000 #\x10FFFF)
#u8(0 #xFF 32)
END
cat > "$work/expected" <<'END'
(65 97 120 88 233 955 8364 128512 955 955 7 8 127 27 10 0 13 32 9 40 41 59 34 92 32 0 55295 57344 1114111)
#u8(0 255 32)
END
"$MARV" --write "$work/tokens" > "$work/actual" 2> "$work/log"
diff -u "$work/expected" "$work/actual"
# Raw scalar boundaries and an actual whitespace character, rather than a name.
printf '#\\\302\200 #\\\340\240\200 #\\\360\220\200\200 #\\\364\217\277\277 #\\\000 #\\\n #\\\r\n' > "$work/boundaries"
printf '128\n2048\n65536\n1114111\n0\n10\n13\n' > "$work/boundary.expected"
"$MARV" --write "$work/boundaries" > "$work/boundary.actual" 2> "$work/log"
diff -u "$work/boundary.expected" "$work/boundary.actual"

reject() {
  if "$MARV" --write "$work/bad" > "$work/out" 2> "$work/log"; then
    echo "[FAIL] accepted invalid character input: $1" >&2; exit 1
  fi
  grep -q 'character\|UTF-8' "$work/log"
}
for literal in '#\' '#\Space' '#\unknown' '#\ab' '#\λx' '#\xg' '#\x110000' '#\xD800' '#\xDFFF' '#\xFFFFFFFFFFFFFFFFFFFFFFFF' '#\)a'; do
  printf '%s' "$literal" > "$work/bad"
  reject "$literal"
done
# Invalid UTF-8: stray continuation, overlong encoding, truncation, bad
# continuation, UTF-8 surrogate, above U+10FFFF, and embedded NUL in a name.
for kind in stray overlong truncated continuation surrogate toobig nul; do
  case "$kind" in
    stray)        printf '#\\\200 ' > "$work/bad";;
    overlong)     printf '#\\\300\200 ' > "$work/bad";;
    truncated)    printf '#\\\342\202' > "$work/bad";;
    continuation) printf '#\\\342\101\254 ' > "$work/bad";;
    surrogate)    printf '#\\\355\240\200 ' > "$work/bad";;
    toobig)       printf '#\\\364\220\200\200 ' > "$work/bad";;
    nul)          printf '#\\space\000x ' > "$work/bad";;
  esac
  reject "$kind"
done
# Even code points that fit in a byte are not legal bytevector elements.
for item in '#\A' '#\space' '#\x41' '#\null' '#\)'; do
  printf '#u8(0 %s 255)\n' "$item" > "$work/bad"
  reject "bytevector $item"
  grep -q 'not allowed in #u8' "$work/log"
done

# The restriction also applies in a module compiled by each backend.
printf '%s\n' '(module $bad (data $d const #u8(#\A)))' > "$work/invalid.mvs"
for target in cc wcpl rv64g; do
  if "$MARV" -T target -t "$target" -c -o "$work/bad-output" "$work/invalid.mvs" > "$work/out" 2> "$work/log"; then
    echo "[FAIL] $target accepted a character in #u8" >&2; exit 1
  fi
  grep -q 'not allowed in #u8' "$work/log"
done
# Included interfaces use the same reader and allow character-valued constants.
cat > "$work/char-header.mvh" <<'END'
(interface $character_header (const $LETTER i32 #\λ))
END
cat > "$work/include.mvs" <<'END'
(include $char-header)
(module $char_include
  (func $main (export "main") (result i32)
    (return (i32.sub (const $LETTER) (i32.const #x3bb)))))
END
"$MARV" -T target -I "$work" -t cc -o "$work/include" "$work/include.mvs"
"$work/include"

# The normal form emitted by --write uses numbers instead of character syntax.
"$MARV" --write test/characters.mvs > "$work/numeric.mvs" 2> "$work/log"
for target in cc wcpl rv64g; do
  case "$target" in cc) ext=c;; wcpl) ext=wo;; rv64g) ext=s;; esac
  "$MARV" -T target -t "$target" -c -o "$work/chars.$ext" test/characters.mvs
  "$MARV" -T target -t "$target" -c -o "$work/numeric.$ext" "$work/numeric.mvs"
  cmp "$work/chars.$ext" "$work/numeric.$ext"
done
"$MARV" -T target -t cc -o "$work/chars" test/characters.mvs
"$work/chars"
SKINT_BIN=${SKINT_BIN:-skint}
if [ -n "${SKINT_LIB:-}" ]; then
  "$SKINT_BIN" -I "$SKINT_LIB" -q test/characters-rv64.scm "$work/chars.s"
else
  "$SKINT_BIN" -q test/characters-rv64.scm "$work/chars.s"
fi
WCPL_BIN=${WCPL_BIN:-wcpl}
if [ -n "${WCPL_HOME:-}" ] && [ -d "$WCPL_HOME/lib/include" ]; then
  "$WCPL_BIN" -q -I"$WCPL_HOME/lib/include/" -L"$WCPL_HOME/lib/" -o "$work/chars.wasm" "$work/chars.wo"
else
  "$WCPL_BIN" -q -o "$work/chars.wasm" "$work/chars.wo"
fi
test -s "$work/chars.wasm"
echo '[PASS] character literals, UTF-8, diagnostics, bytevector restriction, and backend equivalence'
