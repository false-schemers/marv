# MARV Technical Specification & Backend Writer's Guide

This document specifies the **MARV** intermediate language: its source (`.mvs`), interface header (`.mvh`) and target description (`.mvt`) file formats, its type system and memory model, its instruction set, its calling conventions across every compilation target (`wcpl`, `cc` and `rv64g`), and how to write the single-source C adapters that back a `lib/` interface.

The compiler's own command line is in [the command-line reference](cli.md).

---

## Table of Contents

1. [Lexical Structure](#1-lexical-structure)
2. [File Formats (`.mvs`, `.mvh`, `.mvt`)](#2-file-formats-mvs-mvh-mvt)
3. [Type System & Memory Model](#3-type-system--memory-model)
4. [Module & Interface Declarations](#4-module--interface-declarations)
5. [Instruction Set Reference](#5-instruction-set-reference)
6. [Structured Control Flow & Calls](#6-structured-control-flow--calls)
7. [High-Level Forms (Constants, Records, Variadics)](#7-high-level-forms-constants-records-variadics)
8. [ABI & Calling Conventions](#8-abi--calling-conventions)
9. [Standard Library (`lib/`) & Writing C Adapters](#9-standard-library-lib--writing-c-adapters)

The compiler's options, operating modes and lookup paths: [command-line reference](cli.md).

---

## 1. Lexical Structure

MARV `.mvs` and `.mvh` files are sequences of S-expressions using a subset of R7RS Scheme lexical syntax extended with SRFI-270 hexadecimal floating-point literals:

- **Whitespace and Comments:**
  - Line comments start with `;` and extend to the end of the line. They are
    the only comment syntax.
- **Identifiers (Symbols):**
  - Module names, function names, globals, data labels, record names, field names, local variables, and block/loop labels are written with a leading `$` sigil (e.g. `$main`, `$vec3`, `$stdio:snprintf`, `$L_exit`).
  - Cross-module references use `$module:name` (where `module` may contain dots for nested namespaces, e.g. `$math.vec:cross`).
- **Integer Literals:**
  - Signed or unsigned decimal (`0`, `-42`, `1234567890123`) or hexadecimal (`#x73025164`, `#x-10`).
  - `i32.const` normalizes its immediate to a signed 32-bit two's-complement integer (`[-2147483648, 2147483647]`).
  - `i64.const` normalizes its immediate to a signed 64-bit two's-complement integer (`[-9223372036854775808, 9223372036854775807]`).
- **Floating-Point Literals:**
  - Standard decimal notation (`3.141592653589793`, `-0.5`, `1e-9`) and special IEEE-754 values (`+inf.0`, `-inf.0`, `+nan.0`, `-nan.0`).
  - **SRFI-270 Hexadecimal Floating-Point Literals:** `#x` followed by optional sign, hexadecimal significand (with optional `.`), and binary exponent `p` or `P` with signed decimal exponent:
    ```scheme
    (f64.const #x1.921fb54442d18p+1)
    (f32.const #x1.0p-1)
    ```
- **String Literals:**
  - Enclosed in double quotes `"..."`, used in `(export "...")`, `(import "..." "..." ...)`, and `(data ...)` byte segments.
  - Supported escapes: `\n` (`0x0A`), `\t` (`0x09`), `\r` (`0x0D`), `\\`, `\"`, and R7RS hex escapes `\x<hex>;` (e.g. `\x00;`, `\x41;`). There is no other escape form, so a byte
    without an escape of its own is written `\x<hex>;`.
- **Bytevector Literals:**
  - `#u8(<byte> ...)`, with decimal or hexadecimal byte values, is accepted wherever a string literal is, which in practice means inside a `(data ...)` segment: `#u8(65 66 0)` and `"AB\x00;"` are the same three bytes.

---

## 2. File Formats (`.mvs`, `.mvh`, `.mvt`)

### 2.1 Module Source Files (`.mvs`)

A `.mvs` file consists of zero or more `(include ...)` directives followed by a single `(module $<modname> ...)` form:

```scheme
(include system $stdio)
(include system $stdlib)
(include $my_header)

(module $my_mod
  ;; Data declarations
  ;; Global declarations
  ;; Explicit imports (optional; .mvh imports are synthesized on demand)
  ;; Function definitions
)
```

- `(include system $<mod>)` searches the system header directories (`-L`, default `("lib/" "<PREFIX>/lib")` when compiled with `-D PREFIX=...`, or `("lib/" "libs/")` otherwise) for `<mod>.mvh` (or `<slashed>.mvh` for dotted names) and records `<mod>` as a system adapter dependency if any symbol from `<mod>` is referenced by the module.
- `(include $<hdr>)` searches the directory of the current file first, followed by local include directories (`-I`, default `("./")`).
- Cyclic or duplicate `include` directives are automatically ignored.

### 2.2 Interface Header Files (`.mvh`)

A `.mvh` file consists of zero or more `(include ...)` directives followed by one or more `(interface $<modname> ...)` forms:

```scheme
(include system $sys.stat)

(interface $stdio
  (const $EOF i32 -1)
  (data $_iob var align 4 size 640)
  (const $stdout ptr (ptr.add (ref.data $stdio:_iob) (i32.const 32)))
  (func $fputs (param $s ptr) (param $stream ptr) (result i32))
  (func $snprintf (param $buf ptr) (param $n i32) (param $fmt ptr) (param ...) (result i32)))
```

Symbols declared in an `(interface $<mod> ...)` are visible both qualified (`$stdio:fputs`, `$stdio:EOF`) and unqualified (`$fputs`, `$EOF`). Referencing any `func`, `global`, or `data` symbol from a foreign interface (i.e., where `<mod>` differs from the current `(module $<self> ...)`) automatically synthesizes the corresponding `(import "<mod>" "<name>" ...)` declaration in the module. When a module includes its own `.mvh` header (`<mod>` equals `<self>`), self-imports are suppressed.

### 2.3 Target Description Files (`.mvt`)

A `.mvt` file defines a single `(marv-target ...)` S-expression used by `marv -t <targdes>` (searched in `-T` directories, which default to `("target/" "<PREFIX>/target")` when compiled with `-D PREFIX=...`, or `("target/" "targets/")` otherwise):

```scheme
(marv-target
  (name     "cc")
  (type     cc)          ;; Backend: wcpl | cc | rv64g
  (obj-ext  ".c")        ;; Object extension: ".wo" | ".c" | ".s"
  (cc       "cc")
  (cflags   ("-std=c99" "-O2"))
  (ldflags  ())
  (ldlibs   ("-lm"))
  (link-cmd "cc -std=c99 -O2 ~src -o ~exe -lm"))
```

---

## 3. Type System & Memory Model

### 3.1 Core Value Types

Every SSA/stack value, parameter, local, and global in MARV has one of five scalar types:

| Type | Width in Registers / Locals | Width in Memory | Description |
|---|---|---|---|
| `i32` | 32 bits | 1, 2, or 4 bytes | 32-bit signless integer (signedness is per-operator) |
| `i64` | 64 bits | 1, 2, 4, or 8 bytes | 64-bit signless integer (signedness is per-operator) |
| `f32` | 32 bits | 4 bytes | IEEE-754 binary32 floating-point |
| `f64` | 64 bits | 8 bytes | IEEE-754 binary64 floating-point |
| `ptr` | Target-dependent (`i32` on `wcpl`, 64-bit on `rv64g`, `void*` on `cc`) | **Always 8 bytes** | Opaque memory address / function pointer |

### 3.2 Uniform 8-Byte Pointer Slot Rule

In memory (inside stack frames, records, global arrays, and variadic argument slots), a `ptr` value **always occupies an 8-byte aligned slot** across all targets:

- On 64-bit targets (`rv64g`, 64-bit `cc`), `ptr.load` and `ptr.store` read and write all 8 bytes.
- On 32-bit targets (`wcpl`, 32-bit `cc`), `ptr.store` writes the 32-bit address to bytes `0..3` and zero-fills bytes `4..7` (`i64.extend_i32_u` + `i64.store`), while `ptr.load` reads the low 32 bits (`i32.load`).

Consequently, `sizeof`, `alignof`, and `offsetof` for every record are target-independent compile-time constants.

### 3.3 Virtual Registers vs. Addressable Stack Frames

MARV strictly separates virtual registers from addressable stack memory:

1. **Parameters and Locals (`(param $p T)`, `(local $v T)`):**
   - Pure virtual registers; their addresses can never be taken.
   - All locals are zero-initialized upon function entry (`0`, `0.0`, or `ptr.null`).
2. **Addressable Stack Frame (`(frame N)`):**
   - A function may declare at most one `(frame N)` clause (where `N` is a non-negative integer rounded up internally to a multiple of 16 bytes).
   - `(frame.addr K)` yields a `ptr` pointing `K` bytes from the base of the current invocation's addressable stack frame.
   - Dynamic stack allocation (`alloca`) is not supported; every function's frame size is statically constant.

---

## 4. Module & Interface Declarations

### 4.1 Static Data Segments (`data`)

Inside `(module ...)`:

```scheme
;; String-initialized read-only or mutable data
(data $msg const align 1 "Hello, MARV!\x00;")

;; Zero-initialized mutable buffer of N bytes
(data $buf var align 8 (size 256))

;; Structured byte/scalar/pointer array initializer
(data $table const align 8 (export "table")
  (i32 10 20 30 0)
  (f64 1.5 -2.25)
  (ptr $msg 0))
```

- Mutability keyword: `const` (read-only) or `var` (read-write).
- Alignment keyword: `align K` (power of 2; defaults to `1`).
- Optional `(export "external_name")`.
- Data items in structured initializers:
  - `"..."` — raw string bytes
  - `#u8(...)` — raw bytes, written as byte values
  - `(size N)` or `size N` — `N` zero bytes
  - `(i8 v...)` — 1-byte integers
  - `(i16 v...)` — 2-byte integers
  - `(i32 v...)` — 4-byte integers
  - `(i64 v...)` — 8-byte integers
  - `(f32 v...)` — 4-byte IEEE-754 floats
  - `(f64 v...)` — 8-byte IEEE-754 doubles
  - `(ptr sym-or-0 ...)` — 8-byte pointer slots initialized to a data symbol `$sym` or `0` (`ptr.null`)

Byte order and layout: typed items (`i16`/`i32`/`i64`/`f32`/`f64`/`ptr`)
are stored in the *native* byte order of the C target that compiles the
generated `.c` file, so the bytes in memory match what ordinary C code
linked with the program expects for the corresponding C type. The
generated code accesses them only through same-type loads and stores, so
a `.mvs` file that initializes data exclusively with typed items (and
addresses it with matching typed accesses) is endian-portable: the same
source produces correct values on little- and big-endian hosts alike.
Raw string bytes and `(size N)` regions have no inherent byte order, but
*reinterpreting* them — or any typed region — through a different-typed
or byte-wise access makes the program's meaning depend on the host byte
order. Targets that are not C (e.g. `rv64g`, `wcpl`) keep the byte order
of their own machine model.

Inside `(interface ...)`, external data segments are declared with `size N`:

```scheme
(data $_iob var align 4 size 640)
```

### 4.2 Global Variables (`global`)

Inside `(module ...)`:

```scheme
(global $counter (export "counter") (mut i32) (i32.const 0))
(global $pi f64 (f64.const #x1.921fb54442d18p+1))
(global $head (mut ptr) (ptr.null))
```

A mutable global's type is written `(mut T)`, as in WebAssembly; an immutable one is just `T`. The optional `(export "name")` clause comes before the type, and the initializer last.

Inside `(interface ...)`:

```scheme
(global $errno (mut i32))
```

### 4.3 Function Definitions (`func`, `trampoline_func`)

Inside `(module ...)`:

```scheme
(func $name [(export "external_name")]
  (param $p1 <type>) ...
  [(param ...)]                      ;; Marks function as variadic (see Section 7.3)
  [(result <type> | (record $R))]    ;; Optional single return value or record return
  [(frame <bytes>)]                  ;; Optional addressable stack frame size
  (local $l1 [$l2 ...] <type>) ...   ;; One or more zero-initialized registers
  <instr> ...)
```

A local declaration may name one or more variables followed by their shared
scalar type (`i32`, `i64`, `f32`, `f64`, or `ptr`). For example,
`(local $t1 $t2 $t3 ptr)` is equivalent to three separate declarations
`(local $t1 ptr)`, `(local $t2 ptr)`, and `(local $t3 ptr)`, in that order.
Each variable is independently initialized to zero (or `ptr.null`). The
existing single-name form remains valid. The declaration requires at least
one name; this shorthand applies to `local`, not `param` or a macro's
`(locals ...)` clause.

All `(param ...)`, `(result ...)`, `(frame ...)`, and `(local ...)` declarations must precede the instruction body.

A `trampoline_func` is declared and written exactly like a `func`, and is
where a chain of tail calls containing `trampoline_jump` (Section 6.3) is
entered and where it comes back to:

```scheme
(trampoline_func $run [(export "external_name")]
  (param $p1 <type>) ...
  (result ptr)                       ;; or i64: the chain's own return channel
  <instr> ...)                       ;; a body whose last act starts the chain
```

Its body makes the first transfer into the chain, by `call` or `return_call`,
and every `trampoline_jump` in that chain comes back to this function rather
than to its own caller. Calling a `trampoline_func` is an ordinary call: the
chain is entirely inside it, and it returns when the chain does.

A chain ends by returning the null pointer, so a `trampoline_func` is itself a
legal member of an enclosing chain and they nest. Because the return channel
carries the protocol, a chain that computes a result leaves it somewhere the
initiator owns — a `(frame N)` slot of the caller, a `global`, or a `data`
segment — rather than returning it.

Every signature reached by a `trampoline_jump` needs a `trampoline_func` that
enters it; a jump whose signature has none is an error.

### 4.4 Assembler Macros (`macro`)

A macro is an assembler-time template with a declared, machine-checked stack effect. Macros are defined inside `(interface ...)` headers (`.mvh`) or at module top level in a `.mvs` file, and are registered for every module that includes the header:

```scheme
;; Allocate a pair at the caller's $hp (which the template assigns
;; to), store its two fields, and leave the pair pointer on the stack.
(macro $cons ($a $d)
  (effect () -> (ptr))
  (local.set $hp (ptr.sub (local.get $hp) (i32.const 24)))
  (i64.store offset 8  (local.get $hp) $a)
  (i64.store offset 16 (local.get $hp) $d)
  (local.get $hp))
```

A macro is invoked with a `$`-symbol in head position, a shape no other MARV form uses, so macro names can never collide with instructions. The arguments are substituted for the parameter symbols in the template:

```scheme
($cons (local.get $x) (local.get $y))
```

An optional `(locals ($t <type>) ...)` clause (placed before the `(effect ...)`) declares macro-private locals, and an optional `(labels $L ...)` clause declares macro-private labels.

Semantics:

- Parameters are substituted syntactically into the template (after the arguments themselves have been macro-expanded); an argument referenced twice is expanded twice.
- `(locals ...)` and `(labels ...)` declare macro-private names. They are hygienically renamed on every expansion, and declared locals are allocated in the enclosing function. Any other `$name` in the template (e.g. a caller's `$hp` above) refers to the enclosing function's locals, which is what makes zero-overhead inline runtime helpers possible.
- The `(effect (ins) -> (outs))` clause is required and is **checked at every expansion**: the compiler simulates the template's stack effect at the invocation and rejects the invocation if it differs from the declared one. A macro that is defined but never invoked is not checked, so a header can carry a macro whose declared effect is wrong until some module uses it. Branches in a template may only target labels the macro itself declares.
- Redefinition follows the C rule: re-registering an identical macro (e.g. the same header included twice) is a no-op, while a different body under an existing name is a compile-time error — macros cannot shadow or override one another.

---

## 5. Instruction Set Reference

Instructions in a function body may be written in **folded S-expression form**, **flat stack-machine form**, or a combination of both:

```scheme
;; Folded form:
(local.set $z (i32.add (local.get $x) (i32.const 1)))

;; Equivalent flat form:
local.get $x
i32.const 1
i32.add
local.set $z
```

### 5.1 Constants, Addresses & Variable Access

| Instruction (Folded Syntax) | Stack Type | Description |
|---|---|---|
| `(i32.const <int>)` | `[] -> [i32]` | 32-bit integer constant |
| `(i64.const <int>)` | `[] -> [i64]` | 64-bit integer constant |
| `(f32.const <float>)` | `[] -> [f32]` | 32-bit IEEE-754 float constant (decimal or SRFI-270 `#x...p...`) |
| `(f64.const <float>)` | `[] -> [f64]` | 64-bit IEEE-754 double constant (decimal or SRFI-270 `#x...p...`) |
| `(ptr.null)` | `[] -> [ptr]` | Null pointer constant (`0`) |
| `(ref.data $sym)` | `[] -> [ptr]` | Address of module or imported data segment `$sym` |
| `(ref.func $sym)` | `[] -> [ptr]` | Address of module or imported function `$sym` |
| `(frame.addr <uimm>)` | `[] -> [ptr]` | Address at constant byte offset `<uimm>` in `(frame N)` |
| `(local.get $v)` | `[] -> [T]` | Read parameter or local `$v` |
| `(local.set $v <val>)` | `[T] -> []` | Write parameter or local `$v` |
| `(local.tee $v <val>)` | `[T] -> [T]` | Write parameter or local `$v` and return `<val>` |
| `(global.get $g)` | `[] -> [T]` | Read module or imported global `$g` |
| `(global.set $g <val>)` | `[T] -> []` | Write mutable global `$g` |

### 5.2 Memory Loads & Stores

Every load and store instruction accepts an optional unsigned byte offset `offset K` (`K >= 0`, default `0`) and optional alignment hint `align A` before its operand expressions. The offset may also be a scalar constant (`offset $CONST`):

```scheme
(i32.load offset 16 (local.get $p))
(ptr.store offset 8 (frame.addr 0) (local.get $cb))
```

| Instruction | Stack Type | Memory Bytes | Description |
|---|---|---|---|
| `(i32.load [offset K] [align A] <ptr>)` | `[ptr] -> [i32]` | 4 | Load 32-bit integer |
| `(i32.load8_s ... <ptr>)` | `[ptr] -> [i32]` | 1 | Load signed 8-bit integer, sign-extend to `i32` |
| `(i32.load8_u ... <ptr>)` | `[ptr] -> [i32]` | 1 | Load unsigned 8-bit integer, zero-extend to `i32` |
| `(i32.load16_s ... <ptr>)` | `[ptr] -> [i32]` | 2 | Load signed 16-bit integer, sign-extend to `i32` |
| `(i32.load16_u ... <ptr>)` | `[ptr] -> [i32]` | 2 | Load unsigned 16-bit integer, zero-extend to `i32` |
| `(i64.load ... <ptr>)` | `[ptr] -> [i64]` | 8 | Load 64-bit integer |
| `(i64.load8_s ... <ptr>)` | `[ptr] -> [i64]` | 1 | Load signed 8-bit integer, sign-extend to `i64` |
| `(i64.load8_u ... <ptr>)` | `[ptr] -> [i64]` | 1 | Load unsigned 8-bit integer, zero-extend to `i64` |
| `(i64.load16_s ... <ptr>)` | `[ptr] -> [i64]` | 2 | Load signed 16-bit integer, sign-extend to `i64` |
| `(i64.load16_u ... <ptr>)` | `[ptr] -> [i64]` | 2 | Load unsigned 16-bit integer, zero-extend to `i64` |
| `(i64.load32_s ... <ptr>)` | `[ptr] -> [i64]` | 4 | Load signed 32-bit integer, sign-extend to `i64` |
| `(i64.load32_u ... <ptr>)` | `[ptr] -> [i64]` | 4 | Load unsigned 32-bit integer, zero-extend to `i64` |
| `(f32.load ... <ptr>)` | `[ptr] -> [f32]` | 4 | Load 32-bit float |
| `(f64.load ... <ptr>)` | `[ptr] -> [f64]` | 8 | Load 64-bit double |
| `(ptr.load ... <ptr>)` | `[ptr] -> [ptr]` | 8 | Load pointer from 8-byte memory slot |
| `(i32.store ... <ptr> <i32>)` | `[ptr i32] -> []` | 4 | Store 32-bit integer |
| `(i32.store8 ... <ptr> <i32>)` | `[ptr i32] -> []` | 1 | Store low 8 bits of `i32` |
| `(i32.store16 ... <ptr> <i32>)` | `[ptr i32] -> []` | 2 | Store low 16 bits of `i32` |
| `(i64.store ... <ptr> <i64>)` | `[ptr i64] -> []` | 8 | Store 64-bit integer |
| `(i64.store8 ... <ptr> <i64>)` | `[ptr i64] -> []` | 1 | Store low 8 bits of `i64` |
| `(i64.store16 ... <ptr> <i64>)` | `[ptr i64] -> []` | 2 | Store low 16 bits of `i64` |
| `(i64.store32 ... <ptr> <i64>)` | `[ptr i64] -> []` | 4 | Store low 32 bits of `i64` |
| `(f32.store ... <ptr> <f32>)` | `[ptr f32] -> []` | 4 | Store 32-bit float |
| `(f64.store ... <ptr> <f64>)` | `[ptr f64] -> []` | 8 | Store 64-bit double |
| `(ptr.store ... <ptr> <ptr>)` | `[ptr ptr] -> []` | 8 | Store pointer into 8-byte memory slot |

#### Runs of Bytes

A whole run of bytes is moved or set by one instruction rather than by a loop,
so that every target can use the fastest means it has:

| Instruction | Stack Type | Description |
|---|---|---|
| `(memory.copy <dst_ptr> <src_ptr> <count_i32>)` | `[ptr ptr i32] -> []` | Copy `count` bytes from `src` to `dst` |
| `(memory.fill <dst_ptr> <byte_i32> <count_i32>)` | `[ptr i32 i32] -> []` | Set `count` bytes at `dst` to the low 8 bits of `byte` |

- The operands come in the order WebAssembly gives them: where, what, how many.
- `count` is read as an **unsigned** 32-bit number, so one run covers at most
  4294967295 bytes. A `count` of zero touches nothing and is not an error.
- The regions of a `memory.copy` **may overlap**. The result is as if the whole
  source were read before anything was written, which is what `memmove`
  guarantees and `memcpy` does not.
- Only the low 8 bits of a `memory.fill` byte are used; the rest are ignored.
- Whether a run reaches outside the memory the program owns is left undefined,
  exactly as it is for a single load or store.

### 5.3 Pointer Operations

| Instruction | Stack Type | Description |
|---|---|---|
| `(ptr.add <ptr> <i32>)` | `[ptr i32] -> [ptr]` | Advance pointer by signed 32-bit byte offset |
| `(ptr.add64 <ptr> <i64>)` | `[ptr i64] -> [ptr]` | Advance pointer by signed 64-bit byte offset |
| `(ptr.sub <ptr> <i32>)` | `[ptr i32] -> [ptr]` | Subtract signed 32-bit byte offset from pointer |
| `(ptr.diff <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | Byte difference `p1 - p2` as `i32` |
| `(ptr.diff64 <ptr> <ptr>)` | `[ptr ptr] -> [i64]` | Byte difference `p1 - p2` as `i64` |
| `(ptr.eqz <ptr>)` | `[ptr] -> [i32]` | `1` if pointer is null, else `0` |
| `(ptr.eq <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | `1` if pointers are equal, else `0` |
| `(ptr.ne <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | `1` if pointers are not equal, else `0` |
| `(ptr.lt_u <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | Unsigned address comparison `<` |
| `(ptr.gt_u <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | Unsigned address comparison `>` |
| `(ptr.le_u <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | Unsigned address comparison `<=` |
| `(ptr.ge_u <ptr> <ptr>)` | `[ptr ptr] -> [i32]` | Unsigned address comparison `>=` |
| `(ptr.of_i64 <i64>)` | `[i64] -> [ptr]` | Convert raw 64-bit integer to `ptr` |
| `(i64.of_ptr <ptr>)` | `[ptr] -> [i64]` | Convert `ptr` to raw 64-bit integer |

### 5.4 Integer Operations (`i32` and `i64`)

For both `T = i32` and `T = i64`:

- **Binary Arithmetic & Bitwise (`[T T] -> [T]`):**
  - `T.add`, `T.sub`, `T.mul` (two's-complement wraparound)
  - `T.div_s`, `T.div_u`, `T.rem_s`, `T.rem_u`
  - `T.and`, `T.or`, `T.xor`
  - `T.shl`, `T.shr_s`, `T.shr_u`, `T.rotl`, `T.rotr` (shift counts masked modulo 32 for `i32`, modulo 64 for `i64`)
- **Unary Bit Counting (`[T] -> [T]`):**
  - `T.clz` (count leading zeros), `T.ctz` (count trailing zeros), `T.popcnt` (population count)
- **Comparisons (`[T] -> [i32]` for `eqz`; `[T T] -> [i32]` for binary comparisons, yielding `1` or `0`):**
  - `T.eqz`, `T.eq`, `T.ne`, `T.lt_s`, `T.lt_u`, `T.gt_s`, `T.gt_u`, `T.le_s`, `T.le_u`, `T.ge_s`, `T.ge_u`

**Division is defined only where the targets agree.** A program must not divide by zero, and must not compute `T.div_s` with the most negative value as dividend and `-1` as divisor. The `cc` emitter lowers division to plain C `/` and `%` and does not guard either case, so a front end that needs defined behaviour emits the test itself. Measured:

| Case | `rv64g` | `cc` | `wcpl` |
|---|---|---|---|
| `div_s(INT_MIN, -1)` | `INT_MIN`: `divw` defines the overflow | undefined behaviour in C (one build returned `INT_MIN`, another aborted) | traps, "divide result unrepresentable" |
| `rem_s(INT_MIN, -1)` | `0` | undefined behaviour in C (observed `0`) | `0`, defined |
| division by zero | `divw`/`remw`, which RISC-V defines without trapping | undefined behaviour in C (observed `SIGILL`) | traps |

The `rv64g` column is what the emitted instruction is specified to do; it has not yet been run on hardware.

### 5.5 Floating-Point Operations (`f32` and `f64`)

For both `F = f32` and `F = f64`:

- **Binary Operations (`[F F] -> [F]`):**
  - `F.add`, `F.sub`, `F.mul`, `F.div`, `F.min`, `F.max`, `F.copysign`
- **Unary Operations (`[F] -> [F]`):**
  - `F.abs`, `F.neg`, `F.sqrt`, `F.ceil`, `F.floor`, `F.trunc`, `F.nearest` (round to nearest, ties to even)
- **Comparisons (`[F F] -> [i32]`, yielding `1` or `0`):**
  - `F.eq`, `F.ne`, `F.lt`, `F.gt`, `F.le`, `F.ge`

`F.min` and `F.max` agree on signed zero across the targets: `min(-0.0, +0.0)` is `-0.0` on all three. They do **not** agree when an operand is NaN, and a program must not depend on what they do with one. Measured with `min(NaN, 1.0)`: `wcpl` returns NaN, because WebAssembly propagates it, while `cc` returns `1.0`, because it lowers to C `fmin`, which returns the other operand. `rv64g` emits `fmin.d`, which RISC-V also specifies to return the other operand.

### 5.6 Conversions & Reinterpretations

| Instruction | Stack Type | Description |
|---|---|---|
| `i32.wrap_i64` | `[i64] -> [i32]` | Low 32 bits of `i64` |
| `i64.extend_i32_s`, `i64.extend_i32_u` | `[i32] -> [i64]` | Sign- or zero-extend `i32` to `i64` |
| `i32.extend8_s`, `i32.extend16_s` | `[i32] -> [i32]` | Sign-extend low 8 or 16 bits to `i32` |
| `i64.extend8_s`, `i64.extend16_s`, `i64.extend32_s` | `[i64] -> [i64]` | Sign-extend low 8, 16, or 32 bits to `i64` |
| `i32.trunc_f32_s`, `i32.trunc_f32_u` | `[f32] -> [i32]` | Truncate `f32` toward zero to signed/unsigned `i32` |
| `i32.trunc_f64_s`, `i32.trunc_f64_u` | `[f64] -> [i32]` | Truncate `f64` toward zero to signed/unsigned `i32` |
| `i64.trunc_f32_s`, `i64.trunc_f32_u` | `[f32] -> [i64]` | Truncate `f32` toward zero to signed/unsigned `i64` |
| `i64.trunc_f64_s`, `i64.trunc_f64_u` | `[f64] -> [i64]` | Truncate `f64` toward zero to signed/unsigned `i64` |
| `f32.convert_i32_s`, `f32.convert_i32_u` | `[i32] -> [f32]` | Convert signed/unsigned `i32` to `f32` |
| `f32.convert_i64_s`, `f32.convert_i64_u` | `[i64] -> [f32]` | Convert signed/unsigned `i64` to `f32` |
| `f64.convert_i32_s`, `f64.convert_i32_u` | `[i32] -> [f64]` | Convert signed/unsigned `i32` to `f64` |
| `f64.convert_i64_s`, `f64.convert_i64_u` | `[i64] -> [f64]` | Convert signed/unsigned `i64` to `f64` |
| `f32.demote_f64` | `[f64] -> [f32]` | Narrow `f64` to `f32` |
| `f64.promote_f32` | `[f32] -> [f64]` | Widen `f32` to `f64` |
| `i32.reinterpret_f32` | `[f32] -> [i32]` | Bitwise reinterpretation (`f32 -> i32`) |
| `f32.reinterpret_i32` | `[i32] -> [f32]` | Bitwise reinterpretation (`i32 -> f32`) |
| `i64.reinterpret_f64` | `[f64] -> [i64]` | Bitwise reinterpretation (`f64 -> i64`) |
| `f64.reinterpret_i64` | `[i64] -> [f64]` | Bitwise reinterpretation (`i64 -> f64`) |

### 5.7 Stack & Miscellaneous Instructions

- `(drop <val>)` — `[T] -> []`: Discard value.
- `(select <val_true> <val_false> <cond_i32>)` — `[T T i32] -> [T]`: Return `<val_true>` if `<cond_i32> != 0`, else `<val_false>`.
- `(nop)` — `[] -> []`: No operation.
- `(unreachable)` — Trap / unreachable code point.

---

## 6. Structured Control Flow & Calls

### 6.1 `block`, `loop`, and `if`

MARV uses WebAssembly-style structured control flow (no arbitrary `goto`):

```scheme
;; Forward-branching block: (br $exit) jumps to the end of the block
(block $exit [(result T)]
  <body> ...)

;; Backward-branching loop: (br $again) jumps to the beginning of the loop
(loop $again [(result T)]
  <body> ...)

;; Conditional (folded syntax):
(if [(result T)] <cond_i32>
  (then <then_body> ...)
  [(else <else_body> ...)])
```

Flat syntax `block [$lbl] [(result T)] ... end [$lbl]`, `loop [$lbl] [(result T)] ... end [$lbl]`, and `if [(result T)] ... [else ...] end` is also accepted.

### 6.2 Branches & Returns

- `(br $label)` — Unconditional branch to enclosing `$label`.
- `(br_if $label <cond_i32>)` — Conditional branch to `$label` if `<cond_i32> != 0`.
- `(br_table $l0 $l1 ... $default <idx_i32>)` — Indexed jump table branching to `$l_k` for `0 <= idx < N`, or `$default` otherwise.
- `(return [<val>])` — Return from current function.

### 6.3 Direct, Indirect, and Tail Calls

- **Direct Call:**
  ```scheme
  (call $fn <arg1> <arg2> ...)
  ```
- **Indirect Call (via `ptr` from `ref.func`):**
  ```scheme
  ;; Folded syntax: callee pointer is the LAST operand
  (call_indirect (param T1 T2 ...) [(result R)] <arg1> <arg2> ... <callee_ptr>)
  ```
- **Proper Tail Calls (`return_call`, `return_call_indirect`):**
  ```scheme
  (return_call $fn <arg1> <arg2> ...)
  (return_call_indirect (param T1 T2 ...) [(result R)] <arg1> <arg2> ... <callee_ptr>)
  ```
  Tail calls tear down the current activation frame (including `(frame N)` and saved registers) before transferring control, guaranteeing $O(1)$ stack usage.
- **Resetting Tail Call (`trampoline_jump`):**
  ```scheme
  (trampoline_jump $fn <arg1> <arg2> ...)
  ```
  A tail call that also lets the chain containing it return to a fixed depth.
  It transfers control exactly as `return_call` does, and the function it
  names is entered with the arguments given.

  A `trampoline_jump` is **not** what a tail call compiles to. It is what a
  chain of tail calls does once in a while: a stretch of `return_call`s, then
  one `trampoline_jump`, then the next stretch. Two rules make it well formed:

  - The enclosing function returns `ptr` or `i64`.
  - The function it names has a signature that some `(trampoline_func ...)` in
    the program enters, and is reached only by a transfer within the chain —
    never by an ordinary `call`.

  Where the chain runs is a decision for whatever emits the MARV module, and
  one `trampoline_jump` on every loop back edge and every transfer between
  procedures is enough. MARV does not check that the decision was made: a
  chain with no `trampoline_jump` in a cycle is accepted, and on a target whose
  compiler guarantees a jump for every tail call it runs in bounded stack, so
  the omission shows only where that guarantee is absent.

---

## 7. High-Level Forms (Constants, Records, Variadics)

Before backend code generation, MARV lowers `.mvh`/`.mvs` constants, records, and variadic operations into core MARV instructions.

### 7.1 Constants (`const`)

Declared in `(interface ...)` or `(module ...)`:

```scheme
(const $O_CREAT i32 4096)
(const $PI      f64 #x1.921fb54442d18p+1)
(const $stdout  ptr (ptr.add (ref.data $stdio:_iob) (i32.const 32)))
```

Referenced in expressions via `(const $O_CREAT)` or `(const $stdio:O_CREAT)`, which expands inline to `(i32.const 4096)` (or the constant's expression).

### 7.2 Records (`record`)

Declared in `(interface ...)` or `(module ...)`:

```scheme
(record $stat
  (field $st_dev   u64)
  (field $st_ino   u64)
  (field $st_mode  u32)
  (field $st_nlink u64)
  (field $st_size  i64))
```

Supported field types (`<ftype>`):
- Scalars: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `f32`, `f64`, `ptr` (8-byte slot)
- Fixed arrays: `(array <ftype> <count>)`
- Nested records: `(record $OtherRecord)`
- Explicit layout overrides (optional): `(size <bytes>)`, `(align <bytes>)`

Each field is aligned to its natural alignment, and the record's total size is padded to a multiple of its maximum field alignment.

Record operations in function bodies:

| Form | Lowered Core MARV Instruction |
|---|---|
| `(sizeof $R)` | `(i32.const <size_of_R>)` |
| `(alignof $R)` | `(i32.const <align_of_R>)` |
| `(offsetof $R $f)` | `(i32.const <byte_offset_of_f>)` |
| `(field.addr $R $f <base_ptr>)` | `(ptr.add <base_ptr> (i32.const <offset>))` (or `<base_ptr>` when offset is `0`) |
| `(field.get $R $f <base_ptr>)` | `(<load_op> offset <offset> <base_ptr>)` (using signed/unsigned width of `$f`, e.g. `i32.load8_s` for `i8`, `i32.load8_u` for `u8`) |
| `(field.set $R $f <base_ptr> <val>)` | `(<store_op> offset <offset> <base_ptr> <val>)` |

### 7.3 Variadic Functions (`va.start`, `va.count`, `va.arg`)

A function is variadic if its parameter list ends with `(param ...)`:

```scheme
(func $sum_ints (param $tag i32) (param ...) (result i32)
  (local $ap ptr)
  (local $n i32)
  (local.set $ap (va.start))
  (local.set $n  (va.count))
  ;; Read next argument of type T from $ap and advance $ap by 16 bytes:
  ;; (va.arg $ap i32)
  ...)
```

- `(va.start)` — `[] -> [ptr]`: Returns the incoming `$ap$` pointer to the first variadic argument slot.
- `(va.count)` — `[] -> [i32]`: Returns the number of variadic arguments passed by the caller (`i32.load` at `(ptr.sub (local.get $ap$) (i32.const 16))`).
- `(va.arg $ap_local <type>)` — `[] -> [<type>]`: Loads a value of `<type>` (`i32`, `i64`, `f32`, `f64`, or `ptr`) from offset `0` of `$ap_local` and advances `$ap_local` by `16` bytes (`(local.set $ap_local (ptr.add (local.get $ap_local) (i32.const 16)))`). Note: `f32` variadic arguments are promoted to `f64` in the 16-byte slot and demoted via `f32.demote_f64` by `(va.arg $ap f32)`.

---

## 8. ABI & Calling Conventions

### 8.1 Symbol Naming Across Targets

Given module `$mod` (where dots in nested module names such as `math.vec` are replaced by `_` on `cc` and `rv64g`) and symbol `$name`:

| Construct | `wcpl` (`.wo`) | `cc` (`.c`) | `rv64g` (`.s`) |
|---|---|---|---|
| Internal function/data/global | `$mod:name` | `static` `<mod>_<name>` | Local `<mod>_<name>` |
| Exported function/data/global `(export "foo")` | `$mod:foo` | `<mod>_foo` | `.globl <mod>_foo` |
| Exported `(export "main")` | `$mod:main` (plus `_start` wrapper) | `int main(int argc, char **argv)` wrapper calling `<mod>_main` | `.globl main` |
| Imported `("imod" "iname" ...)` | `$imod:iname` | `extern` `<imod>_<iname>` | `<imod>_<iname>` |

### 8.2 By-Value Record Returns (`rp$` Convention)

When a function is declared with `(result (record $R))`:

1. **Signature Lowering:** A hidden first parameter `(param $rp$ ptr)` is prepended to the function's parameter list, and the function's return type becomes `void` (no result).
2. **Call Site:** The caller passes a `ptr` to destination storage (typically `(frame.addr K)`) as the first argument of `(call $fn <dest_ptr> <arg1> ...)`.

This matches WCPL's structure-return ABI (`rp$`) as well as C adapter functions of the form `void mod_fn(record_t *rp, ...)`.

### 8.3 Variadic Calls (`ap$` Convention)

When calling a function declared with `(param ...)`:

1. **Slot Layout:** Each variadic argument occupies a **16-byte slot** (`va_arg_t`), preceded by a 16-byte header slot at `ap - 16`:
   - `*(i32 *)(base + 0) = N` (the variadic argument count `va_count`, where `ap = base + 16`).
   - Slot `k` (`0 <= k < N`) is stored at `ap + 16 * k` (`base + 16 * (k + 1)`):
     - `i32` -> `i32.store` at `ap + 16*k`
     - `i64` -> `i64.store` at `ap + 16*k`
     - `f64` -> `f64.store` at `ap + 16*k`
     - `f32` -> promoted via `f64.promote_f32` and stored with `f64.store` at `ap + 16*k`
     - `ptr` -> `ptr.store` (8-byte uniform pointer slot) at `ap + 16*k`
2. **Caller Frame Allocation:** MARV automatically extends the caller's `(frame ...)` size by `16 * (1 + max_varargs_in_func)` bytes, populates the header and argument slots before the call, and passes `(frame.addr <ap_offset>)` as the final `$ap$ : ptr` argument to the callee.

---

## 9. Standard Library (`lib/`) & Writing C Adapters

### 9.1 How `lib/` Files Are Organized and Linked

Each system library module `<mod>` consists of two files in `lib/`:

1. **`lib/<mod>.mvh`** — The MARV interface header declaring the module's constants, records, globals, data segments, and function signatures.
2. **`lib/<mod>.c`** — A **single-source C adapter** that compiles under both:
   - `wcpl -c lib/<mod>.c -o <mod>.wo` (for the `wcpl` WebAssembly target), and
   - Standard C compilers (`cc`, `gcc`, `clang`, `cl`, `riscv64-linux-gnu-gcc`) for the `cc` and `rv64g` targets.

When `marv` compiles a `.mvs` file, it records every referenced system module in a header comment at line 1 of the generated object file:

```c
/* marv-sys-deps: stdlib stdio */
```

When `marv` is invoked in compile-and-link mode (without `-c`), it reads `marv-sys-deps` from the input modules/objects, locates the corresponding `lib/<mod>.c` files in the `-L` system directories (default `("lib/" "<PREFIX>/lib")` or `("lib/" "libs/")`), and passes them to the target compiler/linker automatically.

### 9.2 Structure of a Single-Source `lib/<mod>.c` Adapter

A `lib/<mod>.c` file has two conditional sections separated by `#if defined(__WCPL__)`:

1. **`#if defined(__WCPL__)` Section:**
   - Starts with `#pragma module "<mod>"` at column 0.
   - Uses WCPL's C dialect and WASI system calls (`<wasi/api.h>`).
   - Because MARV's `.mvh` constants, record layouts, `rp$` struct returns, and `ap$` 16-byte `va_arg_t` variadic slots are identical to WCPL's ABI, the WCPL section exports the functions directly under their unmangled names within `#pragma module "<mod>"`.
2. **`#else` Section (Native `cc` and `rv64g` Targets):**
   - Defines the standard MARV scalar typedefs (`marv_i32`, `marv_u32`, `marv_i64`, `marv_u64`, `marv_f32`, `marv_f64`, `marv_ptr`) and `marv_va_arg_t`.
   - Defines functions named `<mod>_<func>` (with `.` in `<mod>` replaced by `_`, e.g. `math_vec_cross` for `$math.vec:cross`) that adapt between MARV's ABI (8-byte pointer slots, portable record layouts, WASI constant values, and `marv_va_arg_t` variadic arrays) and the host OS / C runtime (`POSIX` or `_WIN32`).

### 9.3 Adapter Implementation Patterns

#### Pattern 1: Simple Scalar & Record Functions (`lib/math.vec.c`)

When a record contains only 64-bit naturally aligned fields (`f64`, `i64`) and no pointers, its C `struct` layout is identical across 32-bit WCPL and 64-bit C/RV64G. A single implementation can be shared by aliasing the function names in the `#else` branch:

```c
/* lib/math.vec.mvh:
 * (interface $math.vec
 *   (record $vec3 (field $x f64) (field $y f64) (field $z f64))
 *   (func $dot   (param $a ptr) (param $b ptr) (result f64))
 *   (func $cross (param $a ptr) (param $b ptr) (result (record $vec3))))
 */

#if defined(__WCPL__)
#pragma module "math.vec"
#include <math.h>
#else
#include <math.h>
#define dot   math_vec_dot
#define cross math_vec_cross
#endif

typedef struct marv_vec3 {
  double x;
  double y;
  double z;
} marv_vec3_t;

double dot(const marv_vec3_t *a, const marv_vec3_t *b) {
  return a->x * b->x + a->y * b->y + a->z * b->z;
}

/* By-value record return (rp$ convention): destination pointer `rp` is first parameter */
void cross(marv_vec3_t *rp, const marv_vec3_t *a, const marv_vec3_t *b) {
  double ax = a->x, ay = a->y, az = a->z;
  double bx = b->x, by = b->y, bz = b->z;
  rp->x = ay * bz - az * by;
  rp->y = az * bx - ax * bz;
  rp->z = ax * by - ay * bx;
}
```

#### Pattern 2: Pointer Arrays & 8-Byte Pointer Slots (e.g., `scandir`)

Whenever an adapter reads or writes a `ptr` inside memory shared with MARV code (such as an out-parameter `marv_ptr *namelist_out` or an array of pointers), use `marv_ptr_load` and `marv_ptr_store` (8 bytes per pointer slot):

```c
static inline marv_ptr marv_ptr_load(marv_ptr base, marv_i64 offset) {
  uint64_t slot = 0;
  memcpy(&slot, (const uint8_t*)base + offset, 8);
  return (marv_ptr)(uintptr_t)slot;
}

static inline void marv_ptr_store(marv_ptr base, marv_i64 offset, marv_ptr val) {
  uint64_t slot = (uint64_t)(uintptr_t)val;
  memcpy((uint8_t*)base + offset, &slot, 8);
}
```

#### Pattern 3: Variadic Functions (`ap$` Convention)

In the `#else` branch of an adapter, a MARV variadic function `(func $snprintf (param $buf ptr) (param $n i32) (param $fmt ptr) (param ...) (result i32))` receives a trailing `marv_ptr ap` parameter pointing to an array of 16-byte `marv_va_arg_t` slots:

```c
typedef union marv_va_arg {
  marv_i32 va_int;
  marv_u32 va_uint;
  marv_i64 va_llong;
  marv_u64 va_ullong;
  marv_f64 va_double;
  uint64_t va_ptr_slot;
  uint64_t _pad[2]; /* 16-byte stride */
} marv_va_arg_t;

#define MARV_VA_COUNT(ap) \
  (((const marv_va_arg_t *)(uintptr_t)(ap))[-1].va_int)

#define MARV_VA_PTR(slot_ptr) \
  ((marv_ptr)(uintptr_t)((slot_ptr)->va_ptr_slot))

marv_i32 stdio_snprintf(marv_ptr buf, marv_i32 n, marv_ptr fmt, marv_ptr ap) {
  const marv_va_arg_t *args = (const marv_va_arg_t *)(uintptr_t)ap;
  marv_i32 argc = MARV_VA_COUNT(ap);
  /* Read slot k (0 <= k < argc) via args[k].va_int, args[k].va_llong,
     args[k].va_double, or MARV_VA_PTR(&args[k]) */
  ...
}
```

#### Pattern 4: Function Pointer Callbacks (e.g., `qsort`, `bsearch`, `scandir`)

Because MARV functions use the same scalar calling convention as C functions (`(marv_ptr, marv_ptr) -> marv_i32`), a callback passed via `(ref.func $cmp)` is cast directly to a C function pointer in the `#else` branch:

```c
typedef marv_i32 (*marv_cmp_fn)(marv_ptr, marv_ptr);

void stdlib_qsort(marv_ptr base, marv_i32 nmemb, marv_i32 size, marv_ptr compar) {
  marv_cmp_fn cmp = (marv_cmp_fn)(uintptr_t)compar;
  ...
}
```

#### Pattern 5: Translating OS Constants & Structs (`errno`, `fcntl`, `sys.stat`, `dirent`)

All constants in `lib/*.mvh` use the canonical WCPL/WASI constant values (for example, `$O_CREAT = 4096`, `$EDOM = 18`, `$DT_REG = 4`).
- On `wcpl`, zero translation is performed.
- On `cc` and `rv64g`, the `#else` branch of `lib/fcntl.c`, `lib/errno.c`, `lib/sys.stat.c`, and `lib/dirent.c` translates between MARV's portable constants/struct layouts and the host OS's native constants/structs.
