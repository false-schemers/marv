# MARV

**MARV** is a portable high-level assembly and compiler intermediate language (IL) designed around the common subset of **RISC-V 64-bit (`RV64G`)** and **WebAssembly (`WASM`)**, with **Restricted C99 (`CC`)** as a third native compilation target.

MARV allows compilers and low-level systems code to emit a single, target-independent S-expression source module (`.mvs`) with modular interface headers (`.mvh`) and compile it to:

1. **WCPL (`.wo` / `.wasm`)** — Extended WebAssembly text objects linked into standalone WASI `.wasm` binaries by [WCPL](https://github.com/false-schemers/wcpl).
2. **RV64G (`.s`)** — RISC-V 64-bit LP64D assembly with automatic register allocation.
3. **CC (`.c`)** — Restricted C99 compiled by standard C compilers (`cc`, `gcc`, `clang`), free of undefined behavior except where the MARV program itself divides by zero or overflows a signed division, which `doc/README.md` 5.4 leaves undefined on every target.

---

## Core Concepts

### 1. Value Types & Uniform 64-Bit Memory Layout

MARV provides five fundamental scalar types:

| Type | Description | Register / Value Representation | Memory Slot Size |
|---|---|---|---|
| `i32` | 32-bit integer (signed/unsigned operations) | 32-bit integer | 1, 2, or 4 bytes (`i32.load8_*`, `i32.load16_*`, `i32.load`) |
| `i64` | 64-bit integer (signed/unsigned operations) | 64-bit integer | 1, 2, 4, or 8 bytes (`i64.load*`) |
| `f32` | 32-bit IEEE-754 floating-point | 32-bit float | 4 bytes (`f32.load`, `f32.store`) |
| `f64` | 64-bit IEEE-754 floating-point | 64-bit float | 8 bytes (`f64.load`, `f64.store`) |
| `ptr` | Opaque flat-memory address | 32-bit on `wcpl`, 64-bit on `rv64g`, pointer on `cc` | **Always 8 bytes** (`ptr.load`, `ptr.store`) |

Because `ptr` is an opaque type that always occupies an **8-byte slot in memory** across all targets (stored in the low 32 bits of an 8-byte zero-extended slot on 32-bit WebAssembly), record layouts, stack frames, and pointer arrays have identical byte offsets and sizes on 32-bit WebAssembly and 64-bit native targets.

Pointer operations are explicit:
- `(ptr.null)` — null pointer constant
- `(ptr.eqz p)`, `(ptr.eq p q)`, `(ptr.ne p q)`, `(ptr.lt_u p q)`, `(ptr.le_u p q)`, `(ptr.gt_u p q)`, `(ptr.ge_u p q)` — pointer comparisons
- `(ptr.add p i32_off)`, `(ptr.sub p i32_off)`, `(ptr.add64 p i64_off)` — byte-offset pointer arithmetic
- `(ptr.diff p q)` (`i32`), `(ptr.diff64 p q)` (`i64`) — byte difference between two pointers
- `(ptr.load [offset K] p)`, `(ptr.store [offset K] p val)` — 8-byte pointer slot load and store

### 2. Surface Syntax (`.mvs` and `.mvh`)

MARV source files (`.mvs`) and header files (`.mvh`) use a minimal subset of R7RS Scheme S-expressions extended with [SRFI-270](https://srfi.schemers.org/srfi-270/srfi-270.html) hexadecimal floating-point literals (such as `#x1.921fb54442d18p+1`).

Inside function bodies, instructions may be written in **folded** form, **flat** stack-machine form, or a mixture of both:

```scheme
;; Folded form
(local.set $hyp
  (f64.sqrt (f64.add (f64.mul (local.get $x) (local.get $x))
                     (f64.mul (local.get $y) (local.get $y)))))
```

### 3. Virtual Locals & Addressable Stack Frames (`(frame N)`)

MARV separates **virtual registers** (`(param $x T)` and `(local $y T)`) from **addressable stack memory**:

- Parameters and locals cannot have their address taken and are allocated to hardware registers on `rv64g` (or WebAssembly locals on `wcpl` and C locals on `cc`). Several locals of the same type can share a declaration, for example `(local $t1 $t2 $t3 ptr)`.
- Functions that need stack-allocated structs, buffers, out-parameters, or variadic argument areas declare a fixed-size 16-byte-aligned addressable frame with `(frame N)` and compute pointers into it with `(frame.addr K)`:

```scheme
(func $example (export "example") (result i32)
  (frame 32)
  (i32.store (frame.addr 0) (i32.const 42))
  (return (i32.load (frame.addr 0))))
```

Dynamic stack allocation (`alloca`) is intentionally excluded so frame sizes are statically known at compile time.

### 4. Structured Control Flow & Tail Calls

Control flow follows WebAssembly's structured control constructs:

- `(block $lbl [(result T)] ...)` — forward branch target (`(br $lbl)` exits the block)
- `(loop $lbl [(result T)] ...)` — backward branch target (`(br $lbl)` restarts the loop)
- `(if [(result T)] cond (then ...) [(else ...)])` — conditional execution
- `(br $lbl)`, `(br_if $lbl cond)`, `(br_table $l0 ... $default idx)` — structured branches
- `(return [val])` — function return
- `(return_call $fn arg...)`, `(return_call_indirect (type ...) arg... fn_ptr)` — proper tail calls (`musttail`) for same-prototype procedures, guaranteeing $O(1)$ bounded stack space even in functions with `(frame N)` stack frames and spilled registers.

### 5. Modular Interfaces (`.mvh`), Records, and Calling Conventions

Every MARV source file (`.mvs`) can include system headers (`(include system $stdio)`) and project headers (`(include $vec3)`) before its `(module $name ...)` definition.

Interface headers (`.mvh`) declare constants, records, functions, globals, and data segments using MARV's unified calling convention:

- **Compile-time constants:**
  ```scheme
  (const $O_CREAT i32 4096)
  ```
  Referenced in code via `(const $O_CREAT)`.
- **Records:**
  ```scheme
  (record $vec3
    (field $x f64)
    (field $y f64)
    (field $z f64))
  ```
  Queried and accessed via `(sizeof $vec3)`, `(alignof $vec3)`, `(offsetof $vec3 $y)`, `(field.get $vec3 $x ptr)`, `(field.set $vec3 $x ptr val)`, and `(field.addr $vec3 $x ptr)`.
- **By-value record returns (`rp$` convention):**
  Functions declared with `(result (record $R))` take a hidden first parameter `$rp$ : ptr` pointing to caller-allocated storage and return `void`. At call sites, the caller passes the destination pointer as the first argument:
  ```scheme
  ;; Interface declaration:
  (func $cross (param $a ptr) (param $b ptr) (result (record $vec3)))

  ;; Call site writing result to (frame.addr 0):
  (call $cross (frame.addr 0) (local.get $a) (local.get $b))
  ```
- **Variadic functions (`ap$` convention):**
  Functions declared with `(param ...)` use a trailing `$ap$ : ptr` parameter pointing to an array of 16-byte argument slots in the caller's stack frame, with `ap[-1]` (at byte offset `-16`) holding the variadic argument count. Call sites pass variadic arguments naturally — MARV automatically reserves frame space and populates the slot array:
  ```scheme
  (call $snprintf (frame.addr 0) (i32.const 64) (ref.data $fmt)
                  (i32.const 7) (f64.const 3.5))
  ```
  User-defined variadic functions in MARV read their variadic arguments with `(va.start)`, `(va.count)`, and `(va.arg $ap T)`.

For the complete language specification, instruction set reference, calling conventions, and library adapter guide, see [`doc/README.md`](doc/README.md).

---

## Building & Installation

The compiler is four C translation units in the repository root — `m.c`, `r.c`, `v.c` and `a.c`, each with its own header — and builds with any C99 compiler. It has no external dependencies beyond the C library and `-lm`.

### Unix-like Systems (Linux, macOS, BSD)

Optionally run `./configure` to set a custom installation `PREFIX` (defaults to `/usr/local`), then build the `marv` executable using `make`:

```sh
./configure --prefix=/usr/local   # optional; defaults to /usr/local
make
```

`Makefile` compiles the four units with `-D PREFIX=$(PREFIX)/share/marv/`, embedding `$(PREFIX)/share/marv/target` and `$(PREFIX)/share/marv/lib` into `marv`'s default target and library search paths alongside local `target/` and `lib/`.

You can also compile the four units directly with your C compiler (with or without `-D PREFIX`):

```sh
cc -O2 -D PREFIX=/usr/local/share/marv/ -o marv m.c r.c v.c a.c -lm
```

To install `marv` (along with the standard library in `$(PREFIX)/share/marv/lib` and target descriptions in `$(PREFIX)/share/marv/target`, defaulting to `PREFIX=/usr/local`):

```sh
sudo make install
```

You can customize the installation prefix via `./configure` or `make` variables:

```sh
./configure --prefix=$HOME/.local
make CC=clang install
```

### Windows (MSVC / `cl`)

Open a **Developer Command Prompt for Visual Studio** (x64 or x86 Native Tools Command Prompt) in the repository root and compile the four units with `cl`:

```bat
cl /nologo /O2 /Fe:marv.exe m.c r.c v.c a.c
```

To compile and link a MARV program into a native Windows `.exe` using the included `target/cl.mvt` target description (`cl` is the default on Windows; `cc` is the default elsewhere):

```bat
marv.exe -o queens.exe examples\queens.mvs
queens.exe
```

---

## Repository Layout

- `m.c`, `r.c`, `v.c`, `a.c` and `m.h`, `r.h`, `v.h`, `a.h` — The compiler and linker driver: the runtime library, the reader and front end, the three target writers, and the command-line driver.
- `configure` — Optional configuration script (`./configure --prefix=PATH`) that writes `config.mk`.
- `Makefile` — Build and installation rules for Unix-like systems.
- [`doc/`](doc/README.md) — The MARV specification: lexical structure, the three file formats, the type system, the instruction set, calling conventions, and how to write a `lib/` adapter.
- [`doc/cli.md`](doc/cli.md) — Command-line reference: options, the two operating modes, and the lookup paths.
- [`lib/`](lib/README.md) — Standard library `.mvh` headers and single-source `.c` adapters (`lib/*.mvh`, `lib/*.c`).
- [`srt/`](srt/README.md) — Runtime support for Scheme code compiled to MARV (`srt/*.mvh`, `srt/*.c`).
- [`target/`](target/README.md) — Target description files (`target/*.mvt`) for `wcpl`, `cc`, `gcc`, `clang`, `cl`, and `rv64g`.
- [`examples/`](examples/README.md) — Single-module and multi-module `.mvs`/`.mvh` example programs.
- [`test/`](test/README.md) — Unit, RV64G simulator, and end-to-end target test suite.
