# MARV Test Suite (`test/`)

The `test/` directory contains the automated test suite for the MARV compiler, RV64G assembly simulator, standard library headers/adapters, and CLI driver:

- `test/test.scm` — Unit tests for `.mvh` header expansion, `.wo`/`cc`/`rv64g` code generation, `.mvt` target loading, and execution of generated RV64G assembly on the built-in RV64G LP64D simulator (`test/rv64-sim.scm`).
- `test/rv64-sim.scm` — R7RS RISC-V 64-bit (`RV64G` LP64D) assembly simulator used to verify register allocation, stack frames, tail calls, and standard library calls without requiring physical RISC-V hardware.
- `test/edge.mvs`, `test/edge.expected`, `test/edge-rv64.scm` — Instruction edge cases (shift-count masking, signed and unsigned division and remainder, `clz`/`ctz` of zero, sign extension, truncation, `nearest` ties-to-even, narrow loads with both sign extensions, `br_table` defaults). Each function returns one `i32`, so the `cc` build and the `rv64g` assembly can be compared with each other and with `test/edge.expected`, which holds what WebAssembly semantics require.
- `test/grouped-locals.mvs`, `test/grouped-locals-rv64.scm` — Grouped local declarations across scalar types, macro use, and RV64G simulation.
- `test/native-stdio.c` — Native adapter regressions for bounded formatting, length modifiers, bounded input, and stream scanning.
- `test/run-tests.sh` — End-to-end test runner that verifies:
  1. Single-source compilation of every `lib/*.c` adapter file under both `cc` and `wcpl -c`.
  2. All unit and RV64G simulation assertions in `test/test.scm`.
  3. CLI `-c` compile-only mode across `cc`, `wcpl`, and `rv64g`.
  4. CLI compile-and-link mode across single-module and 3-module examples (including mixed object + `.mvs` inputs and automatic standard library `lib/*.c` dependency resolution).
  5. SRFI-270 hexadecimal floating-point literals across all three targets.
  6. The edge cases above, on `cc` and on `rv64g`.
  7. `-D PREFIX` target and library lookup from an out-of-tree directory.
  8. That each compiler translation unit and header is present, and that the four units compile and link with WCPL.
  9. Native formatted I/O regressions.
  10. Linker paths with spaces, precompiled adapter dependencies, path rejection and failed-link cleanup.
  11. Grouped local declarations against separate declarations across the CC, WCPL and RV64G backends, plus invalid declarations.

## Running the Tests

From the repository root, run:

```sh
bash test/run-tests.sh
```

The runner copies the needed files to a private temporary directory and removes
that directory on exit. It does not delete generated-looking files in the source
tree.

Skint must be built with numerical-tower support (for example, `make
USE_TOWER=1` after a clean build).

Or run the Scheme unit and RV64G simulation test suite directly with `skint`:

```sh
skint test/test.scm
```

## Tool locations

`run-tests.sh` looks for `skint` and `wcpl` on `PATH`. Each tool and directory it needs can be overridden from the environment:

| Variable | Meaning | Default |
|---|---|---|
| `CC` | C compiler for the compiler itself and for the adapters | `cc` |
| `SKINT_BIN` | Skint executable | `skint` |
| `SKINT_LIB` | Skint library directory, passed as `-I` | `/home/user/skint/lib` if present |
| `WCPL_BIN` | WCPL executable | `wcpl` |
| `WCPL_HOME` | WCPL tree holding `lib/include` and `lib` | `/home/user/wcpl` |
| `MARV_BINDIR` | Optional directory to copy the freshly built binary into | unset; no copy |

For example:

```sh
CC=gcc WCPL_HOME=$HOME/wcpl SKINT_LIB=$HOME/skint/lib bash test/run-tests.sh
```
