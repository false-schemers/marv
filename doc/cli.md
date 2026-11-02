# MARV Command-Line Reference

`marv` compiles MARV module sources (`.mvs`) to one of its targets and, when
asked, drives the target's own compiler and linker to produce an executable.
The target is named by a target description file (`.mvt`); see
[the specification](README.md) 2.3 for how to write one.

## Synopsis

```sh
marv -t TARGDES [-c] [OPTION]... FILE...
```

## Options

| Option | Long Option | Argument | Description |
|---|---|---|---|
| `-t` | `--target=TARGDES` | `TARGDES` | **Required.** Target designation name or path mapped to a `.mvt` target description file. |
| `-T` | `--target-dir=DIR` | `DIR` | Add `DIR` to the `.mvt` target description lookup path (default: `("target/" "<PREFIX>/target")` or `("target/" "targets/")`). |
| `-c` | `--compile` | *(none)* | Compile one `.mvs` file into a target object file without linking. |
| `-o` | `--output=FILE` | `FILE` | Output pathname (optional override in `-c` mode; required executable name when `-c` is omitted). |
| `-O` | — | `0`, `1`, `2`, or `3` | Override optimization placeholders in the target compiler command (for example, `-O2` or `-O 2`). |
| `-L` | `--system-dir=DIR` | `DIR` | Add `DIR` to the system `.mvh` header and `.c` adapter lookup path (default: `("lib/" "<PREFIX>/lib")` or `("lib/" "libs/")`). |
| `-I` | `--include-dir=DIR` | `DIR` | Add `DIR` to the project/local `.mvh` header lookup path (default: `("./")`). |
| `-v` | `--verbose` | *(none)* | Print each compilation step line and the linker command line to `stderr` before executing. |
| `-n` | `--dry-run` | *(none)* | Print the target command and cleanup commands instead of executing them. MARV still generates the temporary target files. |
| `-M` | `--makefile-dir=DIR` | `DIR` | Generate a self-contained build directory with a makefile instead of running the backend compiler. |
| `-h` | `--help` | *(none)* | Display command-line usage and options. |
| | `--split-funcs=N` | `N` | In link mode on a `cc` target, cut the generated C into pieces of at most `N` function definitions, each compiled as its own translation unit (default `300`; `0` does not cut). One huge translation unit is what makes a host C compiler's memory grow on a large module. Ignored in `-c` mode, which always writes one file. |

Diagnostic modes `--count`, `--write`, `--stream`, `--3ac`, and `--3ac-write` inspect source input instead of compiling it; see `marv --help`.

Options follow standard POSIX syntax: short flags may be clustered (e.g. `-vc`), and option arguments may be passed either separately (`-t cc`, `-o app`) or attached (`-tcc`, `--target=cc`, `--output=app`).

---

## Operating Modes

### 1. Compile-Only Mode (`-c`)

When `-c` is given, `marv` accepts a single `.mvs` source file and compiles it to the object format specified by the target's `.mvt` file:

- `.wo` for `wcpl` targets
- `.s` for `rv64g` targets
- `.c` for `cc` targets

By default, the output file is written to the **same directory** as the input `.mvs` file with the same base name and the target's object extension. You can override the output location with `-o FILE`.

```sh
# Generates examples/queens.wo, examples/queens.c, and examples/queens.s
marv -T target -t wcpl  -c examples/queens.mvs
marv -T target -t cc    -c examples/queens.mvs
marv -T target -t rv64g -c examples/queens.mvs

# Override output file path with -o
marv -T target -t wcpl -c -o /tmp/queens.wo examples/queens.mvs
```

### 2. Compile-and-Link Mode (without `-c`)

When `-c` is omitted, `-o FILE` is required and names the output executable (a native binary for `cc`/`rv64g` targets, or a `.wasm` binary for `wcpl`).

Input files on the command line can be:
- One or more `.mvs` source files,
- Pre-compiled target object files (any file whose extension is not `.mvs`, such as `.wo`, `.c`, `.s`, or `.o`), or
- A mix of `.mvs` files and target object files.

In compile-and-link mode, `marv`:
1. Compiles each `.mvs` file on the command line to a temporary target object file in the system temporary directory (`TMPDIR`, `TMP`, or `TEMP`, falling back to `/tmp` on Unix-like systems or `.` on Windows).
2. Inspects the `(include system $...)` dependencies of the input modules, locates the corresponding system `.mvh` headers in the `-L` lookup directories (default `("lib/" "<PREFIX>/lib")` or `("lib/" "libs/")`), and pairs each with its `.c` adapter file in the same directory (`lib/<module>.c`).
3. Invokes the target compiler/linker defined in the `.mvt` file with `-o FILE`, the module object files, and the required system `.c` adapter files.
4. Deletes the temporary object files created during the run when the target command succeeds or fails. With `-n`, cleanup is printed instead of performed.

### 3. Makefile Directory (`-M DIR`)

In link mode, `-M DIR` (or `--makefile-dir=DIR`) generates the target files,
copies the required MARV adapters and other input files, and writes `Makefile`
in `DIR` instead of invoking the target compiler. MARV creates missing directory
components and does not use the temporary directory. The basename of `-o FILE`
names the executable **inside** `DIR`; any preceding directory is ignored.
For the `cl` target, MARV writes `nmakefile` instead and adds `.exe` if the
output name has no extension.

```sh
marv -T target -t cc -O2 -M build-marv -o app examples/queens.mvs
make -C build-marv                 # build the executable
make -C build-marv clean           # remove the executable and build products
make -C build-marv cleanall        # also remove MARV's copies and Makefile
```

On Windows with MSVC, use `nmake /f nmakefile` inside `DIR`, followed by
`nmake /f nmakefile clean` or `nmake /f nmakefile cleanall`. `cleanall` removes
only the files recorded by MARV, not unrelated files in the directory. MARV
refuses to overwrite an existing `Makefile` or `nmakefile` of the chosen kind;
run its `cleanall` first or choose a different directory. `-M` cannot be
combined with `-c`, `-n`, or diagnostic modes, and requires `-o`.

C inputs and adapters are copied along with recursively discovered literal
quoted `#include` dependencies; include names in the copies are rewritten to
local filenames. Toolchain-provided headers and libraries remain external.
Custom target commands and unusual dependencies—such as macro-computed C
includes, user headers written with angle brackets, or paths embedded in custom
compiler flags—are handled on a best-effort basis and may need manual changes.
User-supplied shared libraries may also need runtime loader configuration to
find their copies in `DIR`. An unresolved quoted include or nonliteral include
produces a warning. The generated
makefile uses the selected target command and `-O` substitution; it does not
invoke MARV when building.

### Backend optimization level

`-O0`, `-O1`, `-O2`, and `-O3` select the optimization level passed to the
backend compiler, rather than changing MARV's own code generation. The last
`-O` option wins. In a target's `link-cmd` or fallback `cflags`, a placeholder
`~O0`, `~O1`, `~O2`, or `~O3` becomes its corresponding `-O` flag by default,
or the requested `-O` flag when overridden. Ordinary minus-prefixed flags
such as `-O2` in `link-cmd` or `cflags` are passed as written: even an explicit
MARV `-O3` does not replace them. If a command contains both a literal flag
and a placeholder, both are passed to the backend, which determines how they
interact. If there is no placeholder, MARV's `-O` is silently ignored; the
bundled WCPL target has no placeholder. The MSVC (`cl`) target uses `~O2`,
so it receives the substituted flag literally. MSVC accepts `-` as an option
prefix and supports `-O1` (favor size) and `-O2` (favor speed), but does not
support `-O0` or `-O3`; it may warn and ignore those flags, or reject them with
`/options:strict`. MSVC uses `-Od` to disable optimization instead. See
[Microsoft's optimization options](https://learn.microsoft.com/en-us/cpp/build/reference/o-options-optimize-code?view=msvc-170).
In `-c` mode no backend compiler is invoked, so `-O` has no effect on the
emitted target file.

### Paths passed to the target command

MARV runs the `.mvt` link command through the host command interpreter. Each
object, adapter and output path is passed separately; paths containing spaces
are double-quoted. To keep the same simple convention on Unix shells and
Windows `cmd`, these paths are limited to ASCII letters and digits, spaces,
`_`, `-`, `.`, `/`, and `:`. Other characters cause a diagnostic rather than
being interpreted by the shell. On Windows, backslashes in these paths are
converted to forward slashes for the target command; Windows dry-run cleanup
uses `del`. The `.mvt` command template itself is trusted shell text and must
not surround `~src` or `~exe` with its own quotes. Paths used only by MARV,
such as a source compiled to a temporary object, are not subject to this
command-path restriction.

```sh
# Build a native executable from three .mvs modules
marv -T target -t cc -o app \
  examples/multimod/vec3.mvs examples/multimod/stats.mvs examples/multimod/app.mvs

# Build a WebAssembly (.wasm) binary mixing a pre-compiled .wo object and .mvs sources
marv -T target -t wcpl -o app.wasm \
  examples/multimod/vec3.wo examples/multimod/stats.mvs examples/multimod/app.mvs
```

---

## Lookup Directory Lists

`marv` maintains three configurable search path lists:

1. **Target Description Directories (`-T`)** — Defaults to `("target/" "<PREFIX>/target")` when the compiler is built with `-D PREFIX=...` (e.g. `/usr/local/share/marv/` via `Makefile`), or `("target/" "targets/")` if `-D PREFIX` was not given. Searched when resolving `-t TARGDES` to `<dir>/<TARGDES>.mvt` (after checking `<TARGDES>.mvt` directly).
2. **System Header & Adapter Directories (`-L`)** — Defaults to `("lib/" "<PREFIX>/lib")` when the compiler is built with `-D PREFIX=...`, or `("lib/" "libs/")` if `-D PREFIX` was not given. Searched when resolving `(include system $foo.bar)` to `<dir>/foo.bar.mvh` and its companion adapter `<dir>/foo.bar.c`.
3. **Local Header Directories (`-I`, default `("./")`)** — Searched (after the directory of the including file) when resolving `(include $foo.bar)` to `<dir>/foo.bar.mvh`.
