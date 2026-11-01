;; =============================================================================
;; MARV: High-Level Assembly & Compiler IL Prototype (R7RS Scheme for Skint)
;; Targets:
;;   1. wcpl .wo  (Extended WAT with symbolic relocations, 32-bit pointers)
;;   2. rv64g     (RISC-V 64-bit assembly, LP64D ABI, with Linear-Scan RA)
;;   3. c99       (Restricted UB-free C99 using marv.h)
;; =============================================================================

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme char)
        (scheme cxr)
        (scheme inexact)
        (scheme time)
        (scheme process-context)
        (only (skint) box unbox)
        (skint fl)
        (skint format)
        (skint getopt)
        (skint hidden))

;; -----------------------------------------------------------------------------
;; 1. Utility & String/Symbol Helpers
;; -----------------------------------------------------------------------------

(define (string-prefix? pref str)
  (let ((plen (string-length pref))
        (slen (string-length str)))
    (and (<= plen slen)
         (string=? pref (substring str 0 plen)))))

(define (string-index str ch)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond ((= i len) #f)
            ((char=? (string-ref str i) ch) i)
            (else (loop (+ i 1)))))))

(define (strip-dollar sym-or-str)
  (let ((s (if (symbol? sym-or-str) (symbol->string sym-or-str) sym-or-str)))
    (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\$))
        (substring s 1 (string-length s))
        s)))

;; Convert MARV symbol ($foo or $mod:foo) to a valid C identifier
(define (c-ident sym-or-str)
  (let* ((s (strip-dollar sym-or-str))
         (len (string-length s))
         (out (make-string len #\_)))
    (let loop ((i 0))
      (if (= i len)
          (if (and (> len 0) (char-numeric? (string-ref out 0)))
              (string-append "_" out)
              out)
          (let ((c (string-ref s i)))
            (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))
                (string-set! out i c)
                (string-set! out i #\_))
            (loop (+ i 1)))))))

;; Convert MARV symbol ($foo or $mod:foo) to a valid RV64G assembly symbol
(define (asm-ident modname sym-or-str)
  (let* ((raw (strip-dollar sym-or-str))
         (qualified (if (string-index raw #\:)
                        raw
                        (string-append (strip-dollar modname) ":" raw))))
    (c-ident qualified)))

;; Convert MARV symbol ($foo or $mod:foo) to WCPL .wo $mod:name symbol
(define (wo-ident modname sym-or-str)
  (let ((raw (strip-dollar sym-or-str)))
    (if (string-index raw #\:)
        (string-append "$" raw)
        (string-append "$" (strip-dollar modname) ":" raw))))

(define (align-up n align)
  (let ((rem (modulo n align)))
    (if (= rem 0) n (+ n (- align rem)))))

(define (filter pred lst)
  (let loop ((l lst) (acc '()))
    (cond ((null? l) (reverse acc))
          ((pred (car l)) (loop (cdr l) (cons (car l) acc)))
          (else (loop (cdr l) acc)))))

(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))

(define (every pred lst)
  (cond ((null? lst) #t)
        ((not (pred (car lst))) #f)
        (else (every pred (cdr lst)))))

(define (take n lst)
  (let loop ((k n) (l lst) (acc '()))
    (if (or (<= k 0) (null? l))
        (reverse acc)
        (loop (- k 1) (cdr l) (cons (car l) acc)))))

(define (drop-n n lst)
  (let loop ((k n) (l lst))
    (if (or (<= k 0) (null? l))
        l
        (loop (- k 1) (cdr l)))))

(define (iota count)
  (let loop ((i 0) (acc '()))
    (if (= i count) (reverse acc) (loop (+ i 1) (cons i acc)))))

;; -----------------------------------------------------------------------------
;; 2. IEEE-754 Hexadecimal Floating-Point & Binary Encoding (SRFI-270 compatible)
;; -----------------------------------------------------------------------------

;; Format a flonum as a 64-bit C99 / WCPL .wo hex float literal (13 fractional hex digits)
(define (flonum->f64-hex-literal x)
  (let* ((uval (flonum->ieee64-bits x))
         (dneg (= (quotient uval (expt 2 63)) 1))
         (rem1 (modulo uval (expt 2 63)))
         (exp2 (quotient rem1 (expt 2 52)))
         (mant (modulo rem1 (expt 2 52))))
    (cond
      ((= exp2 #x7ff)
       (if (not (= mant 0))
           (if dneg "-nan" "+nan")
           (if dneg "-inf" "+inf")))
      (else
       (let* ((val (if (= exp2 0) (if (= mant 0) 0 -1022) (- exp2 1023)))
              (hex (string-downcase (number->string mant 16)))
              (padded (string-append (make-string (max 0 (- 13 (string-length hex))) #\0) hex))
              (ldig (if (= exp2 0) "0" "1")))
         (string-append
           (if dneg "-0x" "0x")
           ldig "." padded "p"
           (if (>= val 0) "+" "")
           (number->string val)))))))

;; Format a flonum as a 32-bit C99 / WCPL .wo hex float literal (6 fractional hex digits)
(define (flonum->f32-hex-literal x)
  (let* ((uval (flonum->ieee32-bits x))
         (dneg (= (quotient uval (expt 2 31)) 1))
         (rem1 (modulo uval (expt 2 31)))
         (exp2 (quotient rem1 (expt 2 23)))
         (mant (modulo rem1 (expt 2 23))))
    (cond
      ((= exp2 #xff)
       (if (not (= mant 0))
           (if dneg "-nan" "+nan")
           (if dneg "-inf" "+inf")))
      (else
       (let* ((val (if (= exp2 0) (if (= mant 0) 0 -126) (- exp2 127)))
              (hex (string-downcase (number->string (* mant 2) 16)))
              (padded (string-append (make-string (max 0 (- 6 (string-length hex))) #\0) hex))
              (ldig (if (= exp2 0) "0" "1")))
         (string-append
           (if dneg "-0x" "0x")
           ldig "." padded "p"
           (if (>= val 0) "+" "")
           (number->string val)))))))

(define (flonum->hex-literal x)
  (flonum->f64-hex-literal x))

;; Exact 64-bit IEEE-754 unsigned integer representation of a flonum
(define (flonum->ieee64-bits x)
  (let* ((v (inexact x))
         (s (flsign-bit v)))
    (cond
      ((flnan? v) #x7ff8000000000000)
      ((flinfinite? v) (+ (* s (expt 2 63)) #x7ff0000000000000))
      ((flzero? v) (* s (expt 2 63)))
      (else
        (let* ((eb (box 0))
               (fr (flfrexp (flabs v) eb))
               (e (- (unbox eb) 1)))
          (if (>= e -1022)
              (let ((be (+ e 1023))
                    (mant (exact (flround (* (- (* fr 2.0) 1.0) (expt 2.0 52.0))))))
                (+ (* s (expt 2 63)) (* be (expt 2 52)) mant))
              (let ((mant (exact (flround (* (flabs v) (expt 2.0 1074.0))))))
                (+ (* s (expt 2 63)) mant))))))))

;; Exact 32-bit IEEE-754 unsigned integer representation of a single-precision flonum
(define (flonum->ieee32-bits x)
  (let* ((v (inexact x))
         (s (flsign-bit v)))
    (cond
      ((flnan? v) #x7fc00000)
      ((flinfinite? v) (+ (* s (expt 2 31)) #x7f800000))
      ((flzero? v) (* s (expt 2 31)))
      (else
        (let* ((eb (box 0))
               (fr (flfrexp (flabs v) eb))
               (e (- (unbox eb) 1)))
          (cond
            ((> e 127) (+ (* s (expt 2 31)) #x7f800000))
            ((>= e -126)
             (let* ((be (+ e 127))
                    (mant (exact (flround (* (- (* fr 2.0) 1.0) (expt 2.0 23.0)))))
                    ;; handle rounding overflow into exponent
                    (be2 (if (>= mant (expt 2 23)) (+ be 1) be))
                    (mant2 (if (>= mant (expt 2 23)) 0 mant)))
               (+ (* s (expt 2 31)) (* be2 (expt 2 23)) mant2)))
            (else
             (let ((mant (exact (flround (* (flabs v) (expt 2.0 149.0))))))
               (+ (* s (expt 2 31)) mant)))))))))

(define (int->le-bytes val nbytes)
  (let ((u (if (< val 0) (+ val (expt 2 (* 8 nbytes))) val)))
    (let loop ((i 0) (cur u) (acc '()))
      (if (= i nbytes)
          (reverse acc)
          (loop (+ i 1) (quotient cur 256) (cons (modulo cur 256) acc))))))

(define (normalize-i32 v)
  (let ((m (modulo v 4294967296)))
    (if (>= m 2147483648) (- m 4294967296) m)))

(define (normalize-i64 v)
  (let ((m (modulo v 18446744073709551616)))
    (if (>= m 9223372036854775808) (- m 18446744073709551616) m)))

;; -----------------------------------------------------------------------------
;; 3. MARV Instruction Table & Signatures
;; -----------------------------------------------------------------------------

;; Each entry: (op-sym kind imm-count pop-types push-type natural-align)
;; Kinds:
;;   const, local-get, local-set, local-tee, global-get, global-set,
;;   frame-addr, ref-data, ref-func, load, store, unary, binary,
;;   compare, convert, select, drop, nop, unreachable,
;;   call, call-indirect, br, br-if, br-table, return
(define *instr-table*
  '(;; Constants & References
    (i32.const          const         1 ()          i32 4)
    (i64.const          const         1 ()          i64 8)
    (f32.const          const         1 ()          f32 4)
    (f64.const          const         1 ()          f64 8)
    (ptr.null           const         0 ()          ptr 8)
    (ref.data           ref-data      1 ()          ptr 8)
    (ref.func           ref-func      1 ()          ptr 8)
    (frame.addr         frame-addr    1 ()          ptr 8)

    ;; Locals & Globals
    (local.get          local-get     1 ()          any 0)
    (local.set          local-set     1 (any)       #f  0)
    (local.tee          local-tee     1 (any)       any 0)
    (global.get         global-get    1 ()          any 0)
    (global.set         global-set    1 (any)       #f  0)

    ;; Stack & Control
    (nop                nop           0 ()          #f  0)
    (unreachable        unreachable   0 ()          #f  0)
    (drop               drop          0 (any)       #f  0)
    (select             select        0 (any any i32) any 0)
    (br                 br            1 ()          #f  0)
    (br_if              br-if         1 (i32)       #f  0)
    (br_table           br-table      2 (i32)       #f  0)
    (return             return               0 ()          #f  0)
    (call               call                 1 ()          any 0)
    (call_indirect      call-indirect        1 ()          any 0)
    (return_call        return-call          1 ()          #f  0)
    (return_call_indirect return-call-indirect 1 ()        #f  0)

    ;; Pointer Operations (Opaque ptr, 8-byte memory slot)
    (ptr.add            binary        0 (ptr i32)   ptr 8)
    (ptr.add64          binary        0 (ptr i64)   ptr 8)
    (ptr.sub            binary        0 (ptr i32)   ptr 8)
    (ptr.diff           binary        0 (ptr ptr)   i32 4)
    (ptr.diff64         binary        0 (ptr ptr)   i64 8)
    (ptr.eqz            unary         0 (ptr)       i32 4)
    (ptr.eq             compare       0 (ptr ptr)   i32 4)
    (ptr.ne             compare       0 (ptr ptr)   i32 4)
    (ptr.lt_u           compare       0 (ptr ptr)   i32 4)
    (ptr.gt_u           compare       0 (ptr ptr)   i32 4)
    (ptr.le_u           compare       0 (ptr ptr)   i32 4)
    (ptr.ge_u           compare       0 (ptr ptr)   i32 4)
    (ptr.of_i64         convert       0 (i64)       ptr 8)
    (i64.of_ptr         convert       0 (ptr)       i64 8)
    (ptr.load           load          2 (ptr)       ptr 8)
    (ptr.store          store         2 (ptr ptr)   #f  8)

    ;; Memory Loads
    (i32.load           load          2 (ptr)       i32 4)
    (i32.load8_s        load          2 (ptr)       i32 1)
    (i32.load8_u        load          2 (ptr)       i32 1)
    (i32.load16_s       load          2 (ptr)       i32 2)
    (i32.load16_u       load          2 (ptr)       i32 2)
    (i64.load           load          2 (ptr)       i64 8)
    (i64.load8_s        load          2 (ptr)       i64 1)
    (i64.load8_u        load          2 (ptr)       i64 1)
    (i64.load16_s       load          2 (ptr)       i64 2)
    (i64.load16_u       load          2 (ptr)       i64 2)
    (i64.load32_s       load          2 (ptr)       i64 4)
    (i64.load32_u       load          2 (ptr)       i64 4)
    (f32.load           load          2 (ptr)       f32 4)
    (f64.load           load          2 (ptr)       f64 8)

    ;; Memory Stores
    (i32.store          store         2 (ptr i32)   #f  4)
    (i32.store8         store         2 (ptr i32)   #f  1)
    (i32.store16        store         2 (ptr i32)   #f  2)
    (i64.store          store         2 (ptr i64)   #f  8)
    (i64.store8         store         2 (ptr i64)   #f  1)
    (i64.store16        store         2 (ptr i64)   #f  2)
    (i64.store32        store         2 (ptr i64)   #f  4)
    (f32.store          store         2 (ptr f32)   #f  4)
    (f64.store          store         2 (ptr f64)   #f  8)

    ;; i32 Arithmetic & Bitwise
    (i32.add            binary        0 (i32 i32)   i32 4)
    (i32.sub            binary        0 (i32 i32)   i32 4)
    (i32.mul            binary        0 (i32 i32)   i32 4)
    (i32.div_s          binary        0 (i32 i32)   i32 4)
    (i32.div_u          binary        0 (i32 i32)   i32 4)
    (i32.rem_s          binary        0 (i32 i32)   i32 4)
    (i32.rem_u          binary        0 (i32 i32)   i32 4)
    (i32.and            binary        0 (i32 i32)   i32 4)
    (i32.or             binary        0 (i32 i32)   i32 4)
    (i32.xor            binary        0 (i32 i32)   i32 4)
    (i32.shl            binary        0 (i32 i32)   i32 4)
    (i32.shr_s          binary        0 (i32 i32)   i32 4)
    (i32.shr_u          binary        0 (i32 i32)   i32 4)
    (i32.rotl           binary        0 (i32 i32)   i32 4)
    (i32.rotr           binary        0 (i32 i32)   i32 4)
    (i32.clz            unary         0 (i32)       i32 4)
    (i32.ctz            unary         0 (i32)       i32 4)
    (i32.popcnt         unary         0 (i32)       i32 4)

    ;; i32 Comparisons
    (i32.eqz            unary         0 (i32)       i32 4)
    (i32.eq             compare       0 (i32 i32)   i32 4)
    (i32.ne             compare       0 (i32 i32)   i32 4)
    (i32.lt_s           compare       0 (i32 i32)   i32 4)
    (i32.lt_u           compare       0 (i32 i32)   i32 4)
    (i32.gt_s           compare       0 (i32 i32)   i32 4)
    (i32.gt_u           compare       0 (i32 i32)   i32 4)
    (i32.le_s           compare       0 (i32 i32)   i32 4)
    (i32.le_u           compare       0 (i32 i32)   i32 4)
    (i32.ge_s           compare       0 (i32 i32)   i32 4)
    (i32.ge_u           compare       0 (i32 i32)   i32 4)

    ;; i64 Arithmetic & Bitwise
    (i64.add            binary        0 (i64 i64)   i64 8)
    (i64.sub            binary        0 (i64 i64)   i64 8)
    (i64.mul            binary        0 (i64 i64)   i64 8)
    (i64.div_s          binary        0 (i64 i64)   i64 8)
    (i64.div_u          binary        0 (i64 i64)   i64 8)
    (i64.rem_s          binary        0 (i64 i64)   i64 8)
    (i64.rem_u          binary        0 (i64 i64)   i64 8)
    (i64.and            binary        0 (i64 i64)   i64 8)
    (i64.or             binary        0 (i64 i64)   i64 8)
    (i64.xor            binary        0 (i64 i64)   i64 8)
    (i64.shl            binary        0 (i64 i64)   i64 8)
    (i64.shr_s          binary        0 (i64 i64)   i64 8)
    (i64.shr_u          binary        0 (i64 i64)   i64 8)
    (i64.rotl           binary        0 (i64 i64)   i64 8)
    (i64.rotr           binary        0 (i64 i64)   i64 8)
    (i64.clz            unary         0 (i64)       i64 8)
    (i64.ctz            unary         0 (i64)       i64 8)
    (i64.popcnt         unary         0 (i64)       i64 8)

    ;; i64 Comparisons
    (i64.eqz            unary         0 (i64)       i32 4)
    (i64.eq             compare       0 (i64 i64)   i32 4)
    (i64.ne             compare       0 (i64 i64)   i32 4)
    (i64.lt_s           compare       0 (i64 i64)   i32 4)
    (i64.lt_u           compare       0 (i64 i64)   i32 4)
    (i64.gt_s           compare       0 (i64 i64)   i32 4)
    (i64.gt_u           compare       0 (i64 i64)   i32 4)
    (i64.le_s           compare       0 (i64 i64)   i32 4)
    (i64.le_u           compare       0 (i64 i64)   i32 4)
    (i64.ge_s           compare       0 (i64 i64)   i32 4)
    (i64.ge_u           compare       0 (i64 i64)   i32 4)

    ;; f32 Operations
    (f32.add            binary        0 (f32 f32)   f32 4)
    (f32.sub            binary        0 (f32 f32)   f32 4)
    (f32.mul            binary        0 (f32 f32)   f32 4)
    (f32.div            binary        0 (f32 f32)   f32 4)
    (f32.min            binary        0 (f32 f32)   f32 4)
    (f32.max            binary        0 (f32 f32)   f32 4)
    (f32.copysign       binary        0 (f32 f32)   f32 4)
    (f32.abs            unary         0 (f32)       f32 4)
    (f32.neg            unary         0 (f32)       f32 4)
    (f32.sqrt           unary         0 (f32)       f32 4)
    (f32.ceil           unary         0 (f32)       f32 4)
    (f32.floor          unary         0 (f32)       f32 4)
    (f32.trunc          unary         0 (f32)       f32 4)
    (f32.nearest        unary         0 (f32)       f32 4)
    (f32.eq             compare       0 (f32 f32)   i32 4)
    (f32.ne             compare       0 (f32 f32)   i32 4)
    (f32.lt             compare       0 (f32 f32)   i32 4)
    (f32.gt             compare       0 (f32 f32)   i32 4)
    (f32.le             compare       0 (f32 f32)   i32 4)
    (f32.ge             compare       0 (f32 f32)   i32 4)

    ;; f64 Operations
    (f64.add            binary        0 (f64 f64)   f64 8)
    (f64.sub            binary        0 (f64 f64)   f64 8)
    (f64.mul            binary        0 (f64 f64)   f64 8)
    (f64.div            binary        0 (f64 f64)   f64 8)
    (f64.min            binary        0 (f64 f64)   f64 8)
    (f64.max            binary        0 (f64 f64)   f64 8)
    (f64.copysign       binary        0 (f64 f64)   f64 8)
    (f64.abs            unary         0 (f64)       f64 8)
    (f64.neg            unary         0 (f64)       f64 8)
    (f64.sqrt           unary         0 (f64)       f64 8)
    (f64.ceil           unary         0 (f64)       f64 8)
    (f64.floor          unary         0 (f64)       f64 8)
    (f64.trunc          unary         0 (f64)       f64 8)
    (f64.nearest        unary         0 (f64)       f64 8)
    (f64.eq             compare       0 (f64 f64)   i32 4)
    (f64.ne             compare       0 (f64 f64)   i32 4)
    (f64.lt             compare       0 (f64 f64)   i32 4)
    (f64.gt             compare       0 (f64 f64)   i32 4)
    (f64.le             compare       0 (f64 f64)   i32 4)
    (f64.ge             compare       0 (f64 f64)   i32 4)

    ;; Conversions & Casts
    (i32.wrap_i64       convert       0 (i64)       i32 4)
    (i64.extend_i32_s   convert       0 (i32)       i64 8)
    (i64.extend_i32_u   convert       0 (i32)       i64 8)
    (i32.extend8_s      convert       0 (i32)       i32 4)
    (i32.extend16_s     convert       0 (i32)       i32 4)
    (i64.extend8_s      convert       0 (i64)       i64 8)
    (i64.extend16_s     convert       0 (i64)       i64 8)
    (i64.extend32_s     convert       0 (i64)       i64 8)
    (i32.trunc_f32_s    convert       0 (f32)       i32 4)
    (i32.trunc_f32_u    convert       0 (f32)       i32 4)
    (i32.trunc_f64_s    convert       0 (f64)       i32 4)
    (i32.trunc_f64_u    convert       0 (f64)       i32 4)
    (i64.trunc_f32_s    convert       0 (f32)       i64 8)
    (i64.trunc_f32_u    convert       0 (f32)       i64 8)
    (i64.trunc_f64_s    convert       0 (f64)       i64 8)
    (i64.trunc_f64_u    convert       0 (f64)       i64 8)
    (f32.convert_i32_s  convert       0 (i32)       f32 4)
    (f32.convert_i32_u  convert       0 (i32)       f32 4)
    (f32.convert_i64_s  convert       0 (i64)       f32 4)
    (f32.convert_i64_u  convert       0 (i64)       f32 4)
    (f32.demote_f64     convert       0 (f64)       f32 4)
    (f64.convert_i32_s  convert       0 (i32)       f64 8)
    (f64.convert_i32_u  convert       0 (i32)       f64 8)
    (f64.convert_i64_s  convert       0 (i64)       f64 8)
    (f64.convert_i64_u  convert       0 (i64)       f64 8)
    (f64.promote_f32    convert       0 (f32)       f64 8)
    (i32.reinterpret_f32 convert      0 (f32)       i32 4)
    (i64.reinterpret_f64 convert      0 (f64)       i64 8)
    (f32.reinterpret_i32 convert      0 (i32)       f32 4)
    (f64.reinterpret_i64 convert      0 (i64)       f64 8)))

(define (lookup-instr op)
  (assq op *instr-table*))

(define (instr-kind info)      (list-ref info 1))
(define (instr-imm-count info) (list-ref info 2))
(define (instr-pop-types info) (list-ref info 3))
(define (instr-push-type info) (list-ref info 4))
(define (instr-nat-align info) (list-ref info 5))

(define (float-type? t)
  (or (eq? t 'f32) (eq? t 'f64)))

(define (int-or-ptr-type? t)
  (or (eq? t 'i32) (eq? t 'i64) (eq? t 'ptr)))

;; -----------------------------------------------------------------------------
;; 4. Flattening Pass (Supports Both Flat/Postfix and Folded S-Expressions)
;; -----------------------------------------------------------------------------

;; Parse optional `offset N` / `align N` attributes on load/store/ref.data
;; Two-element attributes: `offset 16`, `align 8`, `size 640`.
;; The glued WAT-style forms (offset=16) are gone -- gluing a value
;; onto a symbol with #\= was a bad idea inherited from WAT.  When
;; TOKS starts with KEY followed by a number, returns (value . rest);
;; #f otherwise.  A stray token containing #\= in attribute position
;; is reported rather than silently misparsed.
(define (attr-value toks key)
  (cond ((and (pair? toks) (pair? (cdr toks))
              (eq? (car toks) key) (number? (cadr toks)))
         (cons (cadr toks) (cddr toks)))
        ((and (pair? toks) (symbol? (car toks))
              (string-index (symbol->string (car toks)) #\=))
         (error "glued key=value attributes are not supported; use the two-element form, e.g. (offset 16)"
                (car toks)))
        (else #f)))

;; Normalize a sequence of flat tokens, parenthesized flat instructions, or folded S-expressions
;; into a canonical list of postfix instructions with structured nodes:
;;   (*block* label result-type flat-body)
;;   (*loop*  label result-type flat-body)
;;   (*if*    result-type flat-then flat-else)

(define *anon-label-counter* 0)
(define (fresh-label prefix)
  (set! *anon-label-counter* (+ *anon-label-counter* 1))
  (string->symbol (string-append "$" prefix (number->string *anon-label-counter*))))

(define (result-clause? x)
  (and (pair? x) (eq? (car x) 'result)))

(define (param-clause? x)
  (and (pair? x) (eq? (car x) 'param)))

(define (then-clause? x)
  (and (pair? x) (eq? (car x) 'then)))

(define (else-clause? x)
  (and (pair? x) (eq? (car x) 'else)))

(define (folded-expr? x)
  (or (and (symbol? x) (lookup-instr x))
      (and (pair? x)
           (symbol? (car x))
           (or (lookup-instr (car x))
               (memq (car x) '(block loop if))))))

(define (label-symbol? x)
  (and (symbol? x)
       (not (lookup-instr x))
       (not (memq x '(block loop if else end then result param)))))

;; Check if a token in a flat stream is an `end` or `(end [$lbl])`
(define (end-token? x)
  (or (eq? x 'end)
      (and (pair? x) (eq? (car x) 'end))))

;; Check if a token in a flat stream is `else` or `(else)`
(define (flat-else-token? x)
  (or (eq? x 'else)
      (and (pair? x) (eq? (car x) 'else) (null? (cdr x)))))

;; Check if a token is a flat block/loop/if opener
(define (opener-token? x)
  (or (memq x '(block loop if))
      (and (pair? x)
           (or (and (memq (car x) '(block loop))
                    ;; Flat parenthesized (block [$lbl] [(result T)]) has no body forms inside
                    (every (lambda (sub) (or (label-symbol? sub) (result-clause? sub))) (cdr x)))
               (and (eq? (car x) 'if)
                    (every result-clause? (cdr x)))))))

;; Collect tokens from `rest` up to the matching `end` (accounting for nested flat openers)
;; Returns: (values body-tokens remaining-after-end)
(define (collect-until-end rest)
  (let loop ((r rest) (depth 0) (acc '()))
    (cond
      ((null? r)
       (error "missing matching 'end' in flat block/loop/if"))
      ((end-token? (car r))
       (let* ((after-end (if (and (eq? (car r) 'end)
                                  (pair? (cdr r))
                                  (label-symbol? (cadr r)))
                             (cddr r)
                             (cdr r))))
         (if (= depth 0)
             (values (reverse acc) after-end)
             (loop after-end (- depth 1) (cons '(end) acc)))))
      ((opener-token? (car r))
       (loop (cdr r) (+ depth 1) (cons (car r) acc)))
      (else
       (loop (cdr r) depth (cons (car r) acc))))))

;; Split top-level flat `if` tokens into (then-tokens . else-tokens) at depth 0 `else`
(define (split-flat-if-branches tokens)
  (let loop ((r tokens) (depth 0) (then-acc '()))
    (cond
      ((null? r)
       (cons (reverse then-acc) '()))
      ((and (= depth 0) (flat-else-token? (car r)))
       (cons (reverse then-acc) (cdr r)))
      ((end-token? (car r))
       (let ((after-end (if (and (eq? (car r) 'end)
                                 (pair? (cdr r))
                                 (label-symbol? (cadr r)))
                            (cddr r)
                            (cdr r))))
         (loop after-end (- depth 1) (cons '(end) then-acc))))
      ((opener-token? (car r))
       (loop (cdr r) (+ depth 1) (cons (car r) then-acc)))
      (else
       (loop (cdr r) depth (cons (car r) then-acc))))))

(define (flatten-seq forms)
  (let loop ((rest forms) (acc '()))
    (if (null? rest)
        (apply append (reverse acc))
        (let ((f (car rest)))
          (cond
            ;; 1. Unparenthesized flat `block` or `loop`: block [$lbl] [(result T)] ... end [$lbl]
            ((memq f '(block loop))
             (let* ((r1 (cdr rest))
                    (has-lbl (and (pair? r1) (label-symbol? (car r1))))
                    (lbl (if has-lbl (car r1) (fresh-label (symbol->string f))))
                    (r2 (if has-lbl (cdr r1) r1))
                    (has-res (and (pair? r2) (result-clause? (car r2))))
                    (rt (if has-res (cadar r2) #f))
                    (r3 (if has-res (cdr r2) r2))
                    (tag (if (eq? f 'block) '*block* '*loop*)))
               (let-values (((body-toks after-end) (collect-until-end r3)))
                 (loop after-end
                       (cons (list (list tag lbl rt (flatten-seq body-toks))) acc)))))

            ;; 2. Unparenthesized flat `if`: if [(result T)] ... [else ...] end
            ((eq? f 'if)
             (let* ((r1 (cdr rest))
                    (has-res (and (pair? r1) (result-clause? (car r1))))
                    (rt (if has-res (cadar r1) #f))
                    (r2 (if has-res (cdr r1) r1)))
               (let-values (((body-toks after-end) (collect-until-end r2)))
                 (let* ((branches (split-flat-if-branches body-toks))
                        (flat-then (flatten-seq (car branches)))
                        (flat-else (flatten-seq (cdr branches))))
                   (loop after-end
                         (cons (list (list '*if* rt flat-then flat-else)) acc))))))

            ;; 3. Unparenthesized bare instruction symbol in flat stream
            ((symbol? f)
             (let ((info (lookup-instr f)))
               (if (not info)
                   (if (label-symbol? f)
                       (loop (cdr rest) (cons (list (list 'local.get f)) acc))
                       (error "unknown symbol in instruction stream" f))
                   (let ((kind (instr-kind info))
                         (k (instr-imm-count info)))
                     (cond
                       ((memq kind '(load store))
                        ;; Consume optional `offset N` and `align N` tokens
                        (let mloop ((r (cdr rest)) (off #f) (aln #f))
                          (cond
                            ((attr-value r 'offset)
                             => (lambda (p) (mloop (cdr p) (car p) aln)))
                            ((attr-value r 'align)
                             => (lambda (p) (mloop (cdr p) off (car p))))
                            (else
                             (loop r (cons (list (list f (or off 0) (or aln (instr-nat-align info)))) acc))))))
                       ((eq? kind 'ref-data)
                        (let* ((r1 (cdr rest))
                               (sym (car r1))
                               (p (attr-value (cdr r1) 'offset))
                               (off (if p (car p) 0))
                               (r3 (if p (cdr p) (cdr r1))))
                          (loop r3 (cons (list (list 'ref.data sym off)) acc))))
                       ((memq kind '(call-indirect return-call-indirect))
                        (let ciloop ((r (cdr rest)) (ptypes '()) (rtype #f))
                          (cond
                            ((and (pair? r) (param-clause? (car r)))
                             (ciloop (cdr r) (append ptypes (cdar r)) rtype))
                            ((and (pair? r) (result-clause? (car r)))
                             (ciloop (cdr r) ptypes (if (pair? (cdar r)) (cadar r) #f)))
                            (else
                             (loop r (cons (list (list f ptypes rtype)) acc))))))
                       ((eq? kind 'br-table)
                        (let bloop ((r (cdr rest)) (lbls '()))
                          (if (and (pair? r) (label-symbol? (car r)))
                              (bloop (cdr r) (cons (car r) lbls))
                              (let* ((all (reverse lbls))
                                     (def-lbl (car (reverse all)))
                                     (targets (reverse (cdr (reverse all)))))
                                (loop r (cons (list (list 'br_table targets def-lbl)) acc))))))
                       ((= k 0)
                        (loop (cdr rest) (cons (list (list f)) acc)))
                       ((= k 1)
                        (let* ((imm (cadr rest))
                               (norm-imm (cond ((eq? f 'i32.const) (normalize-i32 imm))
                                               ((eq? f 'i64.const) (normalize-i64 imm))
                                               (else imm))))
                          (loop (cddr rest) (cons (list (list f norm-imm)) acc))))
                       (else
                        (loop (cdr rest) (cons (flatten-one (list f)) acc))))))))

            ;; 4. Parenthesized flat opener: (block [$lbl] [(result T)]) ... (end [$lbl])
            ((and (pair? f) (opener-token? f))
             (let ((op (car f))
                   (args (cdr f)))
               (if (eq? op 'if)
                   (let ((rt (if (and (pair? args) (result-clause? (car args))) (cadar args) #f)))
                     (let-values (((body-toks after-end) (collect-until-end (cdr rest))))
                       (let* ((branches (split-flat-if-branches body-toks))
                              (flat-then (flatten-seq (car branches)))
                              (flat-else (flatten-seq (cdr branches))))
                         (loop after-end
                               (cons (list (list '*if* rt flat-then flat-else)) acc)))))
                   (let* ((has-lbl (and (pair? args) (label-symbol? (car args))))
                          (lbl (if has-lbl (car args) (fresh-label (symbol->string op))))
                          (rem (if has-lbl (cdr args) args))
                          (rt (if (and (pair? rem) (result-clause? (car rem))) (cadar rem) #f))
                          (tag (if (eq? op 'block) '*block* '*loop*)))
                     (let-values (((body-toks after-end) (collect-until-end (cdr rest))))
                       (loop after-end
                             (cons (list (list tag lbl rt (flatten-seq body-toks))) acc)))))))

            ;; 5. Standard folded or parenthesized instruction: (op ...)
            ((pair? f)
             (loop (cdr rest) (cons (flatten-one f) acc)))

            (else
             (error "invalid instruction form in body" f)))))))

(define (flatten-one form)
  (let ((op (car form))
        (args (cdr form)))
    (case op
      ((seq begin)
       (flatten-seq args))

      ((block loop)
       (let* ((has-label (and (pair? args) (symbol? (car args))
                              (not (lookup-instr (car args)))))
              (lbl (if has-label (car args) (fresh-label (symbol->string op))))
              (rem1 (if has-label (cdr args) args))
              (has-res (and (pair? rem1) (result-clause? (car rem1))))
              (res-type (if has-res (cadr (car rem1)) #f))
              (body-forms (if has-res (cdr rem1) rem1))
              (flat-body (flatten-seq body-forms))
              (tag (if (eq? op 'block) '*block* '*loop*)))
         (list (list tag lbl res-type flat-body))))

      ((if)
       (let* ((has-res (and (pair? args) (result-clause? (car args))))
              (res-type (if has-res (cadr (car args)) #f))
              (rem (if has-res (cdr args) args)))
         ;; Collect any folded condition expressions appearing before (then ...)
         (let collect-cond ((r rem) (cond-forms '()))
           (cond
             ((null? r)
              (error "malformed (if ...): missing (then ...) clause" form))
             ((then-clause? (car r))
              (let* ((then-forms (cdar r))
                     (after-then (cdr r))
                     (else-forms (if (and (pair? after-then) (else-clause? (car after-then)))
                                     (cdar after-then)
                                     '()))
                     (flat-cond (flatten-seq (reverse cond-forms)))
                     (flat-then (flatten-seq then-forms))
                     (flat-else (flatten-seq else-forms)))
                (append flat-cond
                        (list (list '*if* res-type flat-then flat-else)))))
             (else
              (collect-cond (cdr r) (cons (car r) cond-forms)))))))

      (else
       (let ((info (lookup-instr op)))
         (if (not info)
             (error "unknown MARV instruction" op form)
             (let ((kind (instr-kind info))
                   (nat-align (instr-nat-align info)))
               (case kind
                 ;; Memory loads & stores: optional [offset [align]] numbers
                 ;; or the two-element forms `offset N` / `align N`
                 ((load store)
                  (let parse-mem ((r args) (off #f) (aln #f))
                    (cond
                      ((and (pair? r) (number? (car r)) (not off))
                       (parse-mem (cdr r) (car r) aln))
                      ((and (pair? r) (number? (car r)) (not aln))
                       (parse-mem (cdr r) off (car r)))
                      ((attr-value r 'offset)
                       => (lambda (p) (parse-mem (cdr p) (car p) aln)))
                      ((attr-value r 'align)
                       => (lambda (p) (parse-mem (cdr p) off (car p))))
                      (else
                       (let ((final-off (or off 0))
                             (final-aln (or aln nat-align))
                             (folded-operands (flatten-seq r)))
                         (append folded-operands
                                 (list (list op final-off final-aln))))))))

                 ;; ref.data $sym [offset | offset N]
                 ((ref-data)
                  (let* ((sym (car args))
                         (r (cdr args))
                         (off (cond ((and (pair? r) (number? (car r))) (car r))
                                    ((attr-value r 'offset) => (lambda (p) (car p)))
                                    (else 0))))
                    (list (list 'ref.data sym off))))

                 ;; call_indirect / return_call_indirect (param ...) [(result ...)] [folded-args...]
                 ((call-indirect return-call-indirect)
                  (let parse-ci ((r args) (ptypes '()) (rtype #f))
                    (cond
                      ((and (pair? r) (param-clause? (car r)))
                       (parse-ci (cdr r) (append ptypes (cdar r)) rtype))
                      ((and (pair? r) (result-clause? (car r)))
                       (parse-ci (cdr r) ptypes (if (pair? (cdar r)) (cadar r) #f)))
                      (else
                       (let ((folded-operands (flatten-seq r)))
                         (append folded-operands
                                 (list (list op ptypes rtype))))))))

                 ;; br_table ($l0 $l1 ...) $default [folded-cond...]
                 ((br-table)
                  (if (and (pair? args) (pair? (car args)) (not (folded-expr? (car args))))
                      ;; Form 1: (br_table ($l0 $l1 ...) $default [cond])
                      (let* ((labels (car args))
                             (def-lbl (cadr args))
                             (folded-operands (flatten-seq (cddr args))))
                        (append folded-operands
                                (list (list 'br_table labels def-lbl))))
                      ;; Form 2: (br_table $l0 $l1 ... $default [cond])
                      (let collect-lbls ((r args) (lbls '()))
                        (if (and (pair? r) (symbol? (car r)) (not (lookup-instr (car r))))
                            (collect-lbls (cdr r) (cons (car r) lbls))
                            (let* ((all (reverse lbls))
                                   (def-lbl (car (reverse all)))
                                   (targets (reverse (cdr (reverse all))))
                                   (folded-operands (flatten-seq r)))
                              (append folded-operands
                                      (list (list 'br_table targets def-lbl))))))))

                 ;; General instruction with K immediates followed by 0+ folded operands
                 (else
                  (let* ((k (instr-imm-count info))
                         (raw-imms (take k args))
                         (imms (cond ((and (eq? op 'i32.const) (pair? raw-imms))
                                      (list (normalize-i32 (car raw-imms))))
                                     ((and (eq? op 'i64.const) (pair? raw-imms))
                                      (list (normalize-i64 (car raw-imms))))
                                     (else raw-imms)))
                         (rest-operands (drop-n k args))
                         (folded-operands (flatten-seq rest-operands)))
                    (append folded-operands
                            (list (cons op imms)))))))))))))

;; -----------------------------------------------------------------------------
;; 5. Module Parser (`parse-module`)
;; -----------------------------------------------------------------------------

;; ---- native-endian static data (c99 target) ----------------------------
;; Typed data items ((i16/i32/i64/f32/f64) ...) are abstract numbers:
;; the c99 target materializes them as typed C arrays in host-native
;; format, so generated code (native memcpy loads/stores throughout)
;; is endian-correct on any host and links naturally with C data.
;; Byte items (strings, (i8 ...), (size ...)) stay raw bytes.  The wo
;; and rv64g targets keep little-endian byte emission: those machines
;; are little-endian, so native == little-endian there too.
(define (c99-run-val i32? v)
  (c99-format-opnd (list 'imm (if i32? 'i32 'i64) v) #f #f))

;; Static data materialization (cc target).  C gives no way to place
;; per-run initializers at byte offsets inside one object (a union
;; initializer selects a single member), so a multi-run segment is an
;; aliased chain: the segment object itself carries run 0, each later
;; run is an anchor typedef + alias pointer at its byte offset, and a
;; generated constructor memcpy's every anchor's typed initializer --
;; numbers as typed scalars, hence stored in host-native format on
;; any host -- into the segment image.  Single-run segments are just
;; the union object with a designated initializer (no constructor).
;; Byte runs (strings, (i8 ...), (size ...)) stay raw bytes from the
;; little-endian byte image; MARV_DS_TAIL pads the last link's view.
(define (c99-data-run-type r)
  (case (car r)
    ((raw bytes) "uint8_t")
    ((i16) "int16_t")   ((u16) "uint16_t")
    ((i32) "int32_t")   ((u32) "uint32_t")
    ((i64) "int64_t")   ((u64) "uint64_t")
    ((f32) "marv_f32")  ((f64) "marv_f64")
    ((ptr) "marv_ptr")
    (else (error "c99-data-run-type: bad run" r))))

;; scalar initializer strings for a run, or #f for byte/ptr runs
(define (c99-data-run-vals r)
  (case (car r)
    ((i16 u16 i32 u32) (map (lambda (v) (c99-run-val #t v)) (cdr r)))
    ((i64 u64)         (map (lambda (v) (c99-run-val #f v)) (cdr r)))
    ((f32) (map (lambda (v)
                  (string-append "((marv_f32)" (flonum->hex-literal v) ")"))
                (cdr r)))
    ((f64) (map flonum->hex-literal (cdr r)))
    (else #f)))

;; item size in bytes (typed scalar or 1 for bytes/size runs)
(define (c99-run-elem-size r)
  (case (car r)
    ((i16 u16) 2)
    ((i32 u32 f32) 4)
    ((i64 u64 f64 ptr) 8)
    (else 1)))

;; number of elements in a run (a raw run stores its byte count)
(define (c99-run-count r)
  (if (eq? (car r) 'raw) (cadr r) (length (cdr r))))

;; Re-derive the run structure of a data segment from its source items
;; (data record slot 7).  Mirrors expand-data-items offset accounting,
;; including (ptr ...) slot lists; relocations are recomputed by the
;; caller of the cc emitter from slot 6, so they are not returned here.
(define (expand-data-runs items)
  (let loop ((rest items) (runs '()) (cur-off 0))
    (if (null? rest)
        (reverse runs)
        (let ((it (car rest)))
          (cond
            ((string? it)
             (loop (cdr rest)
                   (cons (list 'raw (string-length it)) runs)
                   (+ cur-off (string-length it))))
            ((bytevector? it)
             (loop (cdr rest)
                   (cons (list 'raw (bytevector-length it)) runs)
                   (+ cur-off (bytevector-length it))))
            ((and (pair? it) (memq (car it) '(size zero)))
             (loop (cdr rest)
                   (cons (list 'raw (cadr it)) runs)
                   (+ cur-off (cadr it))))
            ((and (pair? it) (memq (car it) '(i8 u8)))
             (loop (cdr rest)
                   (cons (cons 'bytes (cdr it)) runs)
                   (+ cur-off (length (cdr it)))))
            ((and (pair? it) (memq (car it) '(i16 u16 i32 u32 i64 u64 f32 f64)))
             (loop (cdr rest)
                   (cons it runs)
                   (+ cur-off (* (length (cdr it))
                                 (case (car it)
                                   ((i16 u16) 2) ((i32 u32 f32) 4)
                                   (else 8))))))
            ((and (pair? it)
                  (or (eq? (car it) 'ref.data) (eq? (car it) 'ref)
                      (eq? (car it) 'ref.func)))
             (loop (cdr rest)
                   (cons (list 'ptr 1) runs)
                   (+ cur-off 8)))
            ((and (pair? it) (eq? (car it) 'ptr))
             (loop (cdr rest)
                   (cons (list 'ptr (length (cdr it))) runs)
                   (+ cur-off (* 8 (length (cdr it))))))
            (else (error "unsupported item in (data ...)" it)))))))

;; Expand data items into (bytes-list . relocs-list)
;; where each reloc is: (kind sym sym-offset byte-offset-in-segment)
;; Note: (ref.data $sym [off]) and (ref.func $sym) always occupy an 8-byte slot!
(define (expand-data-items items)
  (let loop ((rest items) (bytes '()) (relocs '()) (cur-off 0))
    (if (null? rest)
        (cons (apply append (reverse bytes)) (reverse relocs))
        (let ((it (car rest)))
          (cond
            ((string? it)
             (let* ((len (string-length it))
                    (bs (let sloop ((i 0) (acc '()))
                          (if (= i len)
                              (reverse acc)
                              (sloop (+ i 1) (cons (char->integer (string-ref it i)) acc))))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off len))))

            ((bytevector? it)
             (let* ((len (bytevector-length it))
                    (bs (let bloop ((i 0) (acc '()))
                          (if (= i len)
                              (reverse acc)
                              (bloop (+ i 1) (cons (bytevector-u8-ref it i) acc))))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off len))))

            ((and (pair? it) (memq (car it) '(size zero)))
             (let ((sz (cadr it)))
               (loop (cdr rest) (cons (make-list sz 0) bytes) relocs (+ cur-off sz))))

            ((and (pair? it) (memq (car it) '(i8 u8 bytes)))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 1)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'i16))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 2)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'u16))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 2)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'i32))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 4)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'u32))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 4)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'i64))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 8)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'u64))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes v 8)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'f32))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes (flonum->ieee32-bits v) 4)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ((and (pair? it) (eq? (car it) 'f64))
             (let* ((vals (cdr it))
                    (bs (apply append (map (lambda (v) (int->le-bytes (flonum->ieee64-bits v) 8)) vals))))
               (loop (cdr rest) (cons bs bytes) relocs (+ cur-off (length bs)))))

            ;; (ptr v ...): 8-byte pointer slots; each v is 0, a data
            ;; symbol, (ref.data $sym [off]) or (ref.func $sym)
            ((and (pair? it) (eq? (car it) 'ptr))
             (let ploop2 ((vs (cdr it)) (bs '()) (rl '()) (off cur-off))
               (cond
                 ((null? vs)
                  (loop (cdr rest) (cons (apply append (reverse bs)) bytes)
                        (append rl relocs) off))
                 ((or (eq? (car vs) 0)
                      (and (pair? (car vs)) (eq? (caar vs) 'ptr.null)))
                  (ploop2 (cdr vs) (cons (make-list 8 0) bs) rl (+ off 8)))
                 ((symbol? (car vs))
                  (ploop2 (cdr vs) (cons (make-list 8 0) bs)
                          (cons (list 'ref.data (car vs) 0 off) rl) (+ off 8)))
                 ((and (pair? (car vs)) (eq? (caar vs) 'ref.data))
                  (ploop2 (cdr vs) (cons (make-list 8 0) bs)
                          (cons (list 'ref.data (cadar vs)
                                      (if (pair? (cddar vs)) (caddar vs) 0)
                                      off)
                                rl)
                          (+ off 8)))
                 ((and (pair? (car vs)) (eq? (caar vs) 'ref.func))
                  (ploop2 (cdr vs) (cons (make-list 8 0) bs)
                          (cons (list 'ref.func (cadar vs) 0 off) rl)
                                (+ off 8)))
                 (else (error "unsupported value in (ptr ...) data item"
                              (car vs))))))

            ;; Uniform 8-byte pointer slot in static data:
            ((and (pair? it) (or (eq? (car it) 'ref.data) (eq? (car it) 'ref)))
             (let ((sym (cadr it))
                   (soff (if (pair? (cddr it)) (caddr it) 0)))
               (loop (cdr rest)
                     (cons (make-list 8 0) bytes)
                     (cons (list 'ref.data sym soff cur-off) relocs)
                     (+ cur-off 8))))

            ((and (pair? it) (eq? (car it) 'ref.func))
             (let ((sym (cadr it)))
               (loop (cdr rest)
                     (cons (make-list 8 0) bytes)
                     (cons (list 'ref.func sym 0 cur-off) relocs)
                     (+ cur-off 8))))

            (else
             (error "unsupported item in (data ...)" it)))))))

;; Expand a shared-type declaration to ordered (name . type) entries.
(define (parse-local-declaration item)
  (if (or (not (list? item)) (< (length item) 3))
      (error "malformed (local $name ... type)" item)
      (let* ((reversed (reverse (cdr item)))
             (type (car reversed))
             (names (reverse (cdr reversed))))
        (if (not (memq type '(i32 i64 f32 f64 ptr)))
            (error "invalid local type" item))
        (map (lambda (name)
               (if (and (symbol? name) (string-prefix? "$" (symbol->string name)))
                   (cons name type)
                   (error "invalid local name" item)))
             names))))

;; Normalize before macro expansion and lowering, so their type environments
;; see every local exactly as if it had its own declaration.
(define (normalize-func-locals form)
  (append (list (car form) (cadr form))
          (apply append
            (map (lambda (clause)
                   (if (and (pair? clause) (eq? (car clause) 'local))
                       (map (lambda (entry) (list 'local (car entry) (cdr entry)))
                            (parse-local-declaration clause))
                       (list clause)))
                 (cddr form)))))

;; Parse a (func ...) form
(define (parse-func form)
  (let* ((name (cadr form))
         (rest0 (cddr form))
         (has-exp (and (pair? rest0) (pair? (car rest0)) (eq? (caar rest0) 'export)))
         (export-name (if has-exp (cadar rest0) #f))
         (rest1 (if has-exp (cdr rest0) rest0)))
    (let loop ((r rest1)
               (params '())
               (res-type #f)
               (frame-size 0)
               (locals '())
               (body-forms '())
               (pcount 0))
      (if (null? r)
          (list 'func
                name
                export-name
                (reverse params)
                res-type
                (align-up frame-size 16)
                (reverse locals)
                (flatten-seq (reverse body-forms)))
          (let ((item (car r)))
            (if (pair? item)
                (case (car item)
                  ((param)
                   ;; Supports (param $x i32) or anonymous (param i32 i64 ...)
                   (if (and (= (length item) 3) (symbol? (cadr item)))
                       (loop (cdr r)
                             (cons (cons (cadr item) (caddr item)) params)
                             res-type frame-size locals body-forms (+ pcount 1))
                       (let ploop ((ts (cdr item)) (ps params) (pc pcount))
                         (if (null? ts)
                             (loop (cdr r) ps res-type frame-size locals body-forms pc)
                             (let ((pname (string->symbol (string-append "$p" (number->string pc)))))
                               (ploop (cdr ts) (cons (cons pname (car ts)) ps) (+ pc 1)))))))
                  ((result)
                   (loop (cdr r) params (cadr item) frame-size locals body-forms pcount))
                  ((frame)
                   (loop (cdr r) params res-type (cadr item) locals body-forms pcount))
                  ((local)
                   (loop (cdr r) params res-type frame-size
                         (append (reverse (parse-local-declaration item)) locals)
                         body-forms pcount))
                  ((body)
                   (loop (cdr r) params res-type frame-size locals
                         (append (reverse (cdr item)) body-forms) pcount))
                  (else
                   (loop (cdr r) params res-type frame-size locals
                         (cons item body-forms) pcount)))
                ;; Bare symbol instruction in function body
                (loop (cdr r) params res-type frame-size locals
                      (cons item body-forms) pcount)))))))

;; Parse a (data ...) form
(define (parse-data form)
  (let* ((name (cadr form))
         (r0 (cddr form))
         (has-exp (and (pair? r0) (pair? (car r0)) (eq? (caar r0) 'export)))
         (export-name (if has-exp (cadar r0) #f))
         (r1 (if has-exp (cdr r0) r0))
         (mut (if (and (pair? r1) (memq (car r1) '(const var))) (car r1) 'var))
         (r2 (if (and (pair? r1) (memq (car r1) '(const var))) (cdr r1) r1))
         (align (cond ((and (pair? r2) (pair? (car r2)) (eq? (caar r2) 'align))
                       (cadar r2))
                      ((attr-value r2 'align) => (lambda (p) (car p)))
                      (else 8)))
         (r3 (cond ((and (pair? r2) (pair? (car r2)) (eq? (caar r2) 'align))
                    (cdr r2))
                   ((attr-value r2 'align) => (lambda (p) (cdr p)))
                   (else r2)))
         (expanded (expand-data-items r3)))
    (list 'data name export-name mut align (car expanded) (cdr expanded) r3)))

;; Parse a (global ...) form
;;   (global $name [(export "n")] T|(mut T) [<const expr>])
;; The shape is checked here: an unknown type used to travel on as a type and
;; fail much later, and a missing initializer used to default to (i32.const 0)
;; whatever the type was, which put an i32 constant in an i64 global.  [cco]
(define (parse-global form)
  (let* ((name (cadr form))
         (r0 (cddr form))
         (has-exp (and (pair? r0) (pair? (car r0)) (eq? (caar r0) 'export)))
         (export-name (if has-exp (cadar r0) #f))
         (r1 (if has-exp (cdr r0) r0)))
    (if (not (pair? r1))
        (error "malformed (global ...): type expected" form))
    (let* ((type-spec (car r1))
           (mut? (and (pair? type-spec) (eq? (car type-spec) 'mut)))
           (vtype (if mut?
                      (if (and (pair? (cdr type-spec)) (null? (cddr type-spec)))
                          (cadr type-spec)
                          (error "malformed (global ...): (mut T) expected" form))
                      type-spec))
           (rest (cdr r1)))
      (if (not (memq vtype '(i32 i64 f32 f64 ptr)))
          (error "malformed (global ...): type must be i32, i64, f32, f64 or ptr, alone or as (mut T)" form))
      (if (and (pair? rest) (pair? (cdr rest)))
          (error "malformed (global ...): at most one initializer expected" form))
      (if (and (pair? rest) (not (pair? (car rest))))
          (error "malformed (global ...): initializer must be a constant expression" form))
      (let ((init-expr
              (cond ((pair? rest) (car rest))
                    ((eq? vtype 'i32) '(i32.const 0))
                    ((eq? vtype 'i64) '(i64.const 0))
                    ((eq? vtype 'f32) '(f32.const 0.0))
                    ((eq? vtype 'f64) '(f64.const 0.0))
                    (else '(ptr.null)))))
        (list 'global name export-name mut? vtype init-expr)))))

;; Parse an (import "mod" "name" <desc>) form
(define (parse-import form)
  (let ((imod (cadr form))
        (iname (caddr form))
        (desc (cadddr form)))
    (case (car desc)
      ((func)
       (let ((fname (cadr desc)))
         (let loop ((r (cddr desc)) (ptypes '()) (rtype #f))
           (cond
             ((null? r)
              (list 'import-func imod iname fname (reverse ptypes) rtype))
             ((param-clause? (car r))
              (let ((ps (cdar r)))
                ;; handle (param $x T) or (param T1 T2 ...)
                (if (and (= (length ps) 2) (symbol? (car ps))
                         (not (memq (car ps) '(i32 i64 f32 f64 ptr)))
                         (memq (cadr ps) '(i32 i64 f32 f64 ptr)))
                    (loop (cdr r) (cons (cadr ps) ptypes) rtype)
                    (loop (cdr r) (append (reverse ps) ptypes) rtype))))
             ((result-clause? (car r))
              (loop (cdr r) ptypes (cadar r)))
             (else
              (error "invalid clause in import func" (car r)))))))
      ((global)
       (let* ((gname (cadr desc))
              (tspec (caddr desc))
              (mut? (and (pair? tspec) (eq? (car tspec) 'mut)))
              (vtype (if mut? (cadr tspec) tspec)))
         (list 'import-global imod iname gname mut? vtype)))
      ((data)
       (let* ((dname (cadr desc))
              (mut (if (and (pair? (cddr desc)) (memq (caddr desc) '(const var)))
                       (caddr desc)
                       'var))
              (rest (if (and (pair? (cddr desc)) (memq (caddr desc) '(const var)))
                        (cdddr desc)
                        (cddr desc))))
         (let dloop ((r rest) (align 8) (sz 8))
           (cond
             ((null? r)
              (list 'import-data imod iname dname mut align sz))
             ((and (pair? (car r)) (eq? (caar r) 'align))
              (dloop (cdr r) (cadar r) sz))
             ((and (pair? (car r)) (eq? (caar r) 'size))
              (dloop (cdr r) align (cadar r)))
             ((attr-value r 'align)
              => (lambda (p) (dloop (cdr p) (car p) sz)))
             ((attr-value r 'size)
              => (lambda (p) (dloop (cdr p) align (car p))))
             (else
              (dloop (cdr r) align sz))))))
      ((memory)
       (let ((mname (cadr desc))
             (min-pages (caddr desc)))
         (list 'import-memory imod iname mname min-pages)))
      (else
       (error "unsupported import descriptor" desc)))))

;; Parse top-level (module $name ...)
(define (parse-module sexp)
  (if (not (and (pair? sexp) (eq? (car sexp) 'module)))
      (error "expected (module $name ...)" sexp)
      (let* ((has-name (and (pair? (cdr sexp)) (symbol? (cadr sexp))))
             (modname (if has-name (cadr sexp) '$marv_mod))
             (items (if has-name (cddr sexp) (cdr sexp))))
        (let loop ((r items)
                   (imports '())
                   (globals '())
                   (datas '())
                   (tables '())
                   (funcs '()))
          (if (null? r)
              (list 'module
                    modname
                    (reverse imports)
                    (reverse globals)
                    (reverse datas)
                    (reverse tables)
                    (reverse funcs))
              (let ((it (car r)))
                (case (car it)
                  ((import)
                   (loop (cdr r) (cons (parse-import it) imports) globals datas tables funcs))
                  ((global)
                   (loop (cdr r) imports (cons (parse-global it) globals) datas tables funcs))
                  ((data)
                   (loop (cdr r) imports globals (cons (parse-data it) datas) tables funcs))
                  ((table)
                   (loop (cdr r) imports globals datas (cons (list 'table (cadr it) (caddr it)) tables) funcs))
                  ((func)
                   (loop (cdr r) imports globals datas tables (cons (parse-func it) funcs)))
                  ((include)
                   (error "(include ...) belongs before the (module ...) form, not inside it" it))
                  (else
                   (error "unknown top-level form in module" it)))))))))

(define (mod-name m)    (list-ref m 1))
(define (mod-imports m) (list-ref m 2))
(define (mod-globals m) (list-ref m 3))
(define (mod-datas m)   (list-ref m 4))
(define (mod-tables m)  (list-ref m 5))
(define (mod-funcs m)   (list-ref m 6))

(define (func-name f)       (list-ref f 1))
(define (func-export f)     (list-ref f 2))
(define (func-params f)     (list-ref f 3))
(define (func-result f)     (list-ref f 4))
(define (func-frame-size f) (list-ref f 5))
(define (func-locals f)     (list-ref f 6))
(define (func-body f)       (list-ref f 7))

;; Build symbol lookup tables for functions and globals in a module
(define (build-func-sig-table mod)
  (let ((imp-sigs
          (map (lambda (imp)
                 (list (list-ref imp 3) (list-ref imp 4) (list-ref imp 5)
                       (list-ref imp 1) (list-ref imp 2)))
               (filter (lambda (x) (eq? (car x) 'import-func)) (mod-imports mod))))
        (def-sigs
          (map (lambda (fn)
                 (list (func-name fn)
                       (map cdr (func-params fn))
                       (func-result fn)
                       #f
                       (func-export fn)))
               (mod-funcs mod))))
    (append imp-sigs def-sigs)))

(define (build-global-type-table mod)
  (let ((imp-globs
          (map (lambda (imp)
                 (cons (list-ref imp 3) (list-ref imp 5)))
               (filter (lambda (x) (eq? (car x) 'import-global)) (mod-imports mod))))
        (def-globs
          (map (lambda (g)
                 (cons (list-ref g 1) (list-ref g 4)))
               (mod-globals mod))))
    (append imp-globs def-globs)))

;; Match symbols either by exact match ($foo) or stripped base name
(define (lookup-sym-entry sym alist)
  (or (assq sym alist)
      (let ((raw (strip-dollar sym)))
        (let loop ((l alist))
          (cond ((null? l) #f)
                ((string=? raw (strip-dollar (caar l))) (car l))
                (else (loop (cdr l))))))))

;; =============================================================================
;; 6. Target 1: WCPL `.wo` Emitter
;; =============================================================================

(define (wo-type t)
  (if (eq? t 'ptr) "i32" (symbol->string t)))

(define (wo-escape-bytes bytes)
  (let ((p (open-output-string)))
    (write-char #\" p)
    (for-each
      (lambda (b)
        (cond
          ((= b #x09) (display "\\t" p))
          ((= b #x0A) (display "\\n" p))
          ((= b #x0D) (display "\\r" p))
          ((= b #x22) (display "\\\"" p))
          ((= b #x27) (display "\\'" p))
          ((= b #x5C) (display "\\\\" p))
          ((and (>= b 32) (< b 127)) (write-char (integer->char b) p))
          (else
           (let ((hi (quotient b 16))
                 (lo (modulo b 16)))
             (write-char #\\ p)
             (display (number->string hi 16) p)
             (display (number->string lo 16) p)))))
      bytes)
    (write-char #\" p)
    (get-output-string p)))

(define (emit-wo mod port)
  (let* ((mname (mod-name mod))
         (imports (mod-imports mod))
         (globals (mod-globals mod))
         (datas (mod-datas mod))
         (tables (mod-tables mod))
         (funcs (mod-funcs mod))
         (fsigs (build-func-sig-table mod))
         (has-sp-import?
           (any (lambda (imp)
                  (and (eq? (car imp) 'import-global)
                       (string=? (list-ref imp 1) "crt")
                       (string=? (list-ref imp 2) "sp$")))
                imports))
         (has-mem-import?
           (any (lambda (imp) (eq? (car imp) 'import-memory)) imports))
         (main-fn
           (let loop ((fs funcs))
             (cond ((null? fs) #f)
                   ((and (func-export (car fs))
                         (string=? (func-export (car fs)) "main"))
                    (car fs))
                   (else (loop (cdr fs))))))
         (has-start?
           (any (lambda (fn)
                  (and (func-export fn) (string=? (func-export fn) "_start")))
                funcs)))

    ;; Resolve a function or data symbol to $mod:id
    (define (resolve-wo-sym sym)
      (let ((raw (strip-dollar sym)))
        (if (string-index raw #\:)
            (string-append "$" raw)
            ;; check if imported under another module
            (let ((fentry (lookup-sym-entry sym fsigs)))
              (if (and fentry (list-ref fentry 3))
                  (string-append "$" (list-ref fentry 3) ":" (list-ref fentry 4))
                  (wo-ident mname sym))))))

    (fprintf port "(module ~a~%" ( ->>-modname mname))
    ;; Always emit crt:sp$ and crt:memory if not already imported
    (when (not has-sp-import?)
      (fprintf port "  (import \"crt\" \"sp$\" (global $crt:sp$ (mut i32)))~%"))
    (when (not has-mem-import?)
      (fprintf port "  (import \"crt\" \"memory\" (memory $crt:memory 0))~%"))
    (when (and main-fn (not has-start?))
      (fprintf port "  (import \"crt\" \"initialize\" (func $crt:initialize))~%")
      (fprintf port "  (import \"crt\" \"terminate\" (func $crt:terminate (param i32)))~%"))

    ;; User imports
    (for-each
      (lambda (imp)
        (case (car imp)
          ((import-func)
           (let ((imod (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (ptypes (list-ref imp 4))
                 (rtype (list-ref imp 5)))
             (fprintf port "  (import \"~a\" \"~a\" (func $~a:~a" imod iname imod iname)
             (for-each (lambda (pt) (fprintf port " (param ~a)" (wo-type pt))) ptypes)
             (when rtype (fprintf port " (result ~a)" (wo-type rtype)))
             (fprintf port "))~%")))
          ((import-global)
           (let ((imod (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (mut? (list-ref imp 4))
                 (vtype (list-ref imp 5)))
             (if mut?
                 (fprintf port "  (import \"~a\" \"~a\" (global $~a:~a (mut ~a)))~%"
                          imod iname imod iname (wo-type vtype))
                 (fprintf port "  (import \"~a\" \"~a\" (global $~a:~a ~a))~%"
                          imod iname imod iname (wo-type vtype)))))
          ((import-data)
           (let ((imod (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (mut (list-ref imp 4))
                 (align (list-ref imp 5))
                 (sz (list-ref imp 6)))
             (fprintf port "  (import \"~a\" \"~a\" (data $~a:~a ~a align=~a size=~a))~%"
                      imod iname imod iname mut align sz)))
          ((import-memory)
           (let ((imod (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (minp (list-ref imp 4)))
             (fprintf port "  (import \"~a\" \"~a\" (memory $~a:~a ~a))~%"
                      imod iname imod iname minp)))))
      imports)

    ;; Globals
    (for-each
      (lambda (g)
        (let ((gname (list-ref g 1))
              (exp (list-ref g 2))
              (mut? (list-ref g 3))
              (vtype (list-ref g 4))
              (init (list-ref g 5)))
          (fprintf port "  (global ~a" (wo-ident mname gname))
          (when exp (fprintf port " (export \"~a\")" exp))
          (if mut?
              (fprintf port " (mut ~a)" (wo-type vtype))
              (fprintf port " ~a" (wo-type vtype)))
          (fprintf port " (~a))~%" (format-wo-const init))))
      globals)

    ;; Data segments (with 8-byte slot relocation support!)
    (for-each
      (lambda (d)
        (let* ((dname (list-ref d 1))
               (exp (list-ref d 2))
               (mut (list-ref d 3))
               (align (min 16 (list-ref d 4)))
               (bytes (list-ref d 5))
               (relocs (list-ref d 6))
               (all-zero? (every (lambda (b) (= b 0)) bytes)))
          (fprintf port "  (data ~a" (wo-ident mname dname))
          (when exp (fprintf port " (export \"~a\")" exp))
          (fprintf port " ~a align=~a" mut align)
          (if (and all-zero? (> (length bytes) 0))
              (fprintf port " size=~a" (length bytes))
              (fprintf port " ~a" (wo-escape-bytes bytes)))
          (if (null? relocs)
              (fprintf port ")~%")
              (begin
                (fprintf port "~%")
                (let rloop ((rs relocs) (first? #t))
                  (when (pair? rs)
                    (let* ((rel (car rs))
                           (rkind (list-ref rel 0))
                           (rsym (list-ref rel 1))
                           (soff (list-ref rel 2))
                           (boff (list-ref rel 3))
                           (last? (null? (cdr rs))))
                      (fprintf port "    ~a~a ~a"
                               (if first? "(" " ")
                               (if (eq? rkind 'ref.func) "ref.func" "ref.data")
                               (resolve-wo-sym rsym))
                      (when (and (eq? rkind 'ref.data) (not (= soff 0)))
                        (fprintf port " offset=~a" soff))
                      (fprintf port "~%     data.put_ref offset=~a~a~%"
                               boff (if last? "))" ""))
                      (rloop (cdr rs) #f))))))))
      datas)

    ;; Tables
    (for-each
      (lambda (t)
        (let* ((syms (list-ref t 2))
               (tlen (+ (length syms) 1)))
          (fprintf port "  (table ~a ~a funcref)~%" tlen tlen)
          (fprintf port "  (elem (i32.const 1)")
          (for-each (lambda (s) (fprintf port " ~a" (resolve-wo-sym s))) syms)
          (fprintf port ")~%")))
      tables)

    ;; Functions
    (for-each
      (lambda (fn)
        (emit-wo-func mname fn resolve-wo-sym port))
      funcs)

    ;; Auto-emit _start if main is exported
    (when (and main-fn (not has-start?))
      (fprintf port "  (func ~a (export \"_start\")~%" (wo-ident mname '$_start))
      (fprintf port "    call $crt:initialize~%")
      (for-each
        (lambda (p)
          (fprintf port "    ~a.const 0~%" (wo-type (cdr p))))
        (func-params main-fn))
      (fprintf port "    call ~a~%" (wo-ident mname (func-name main-fn)))
      (if (func-result main-fn)
          (fprintf port "    call $crt:terminate~%")
          (begin
            (fprintf port "    i32.const 0~%")
            (fprintf port "    call $crt:terminate~%")))
      (fprintf port "  )~%"))

    (fprintf port ")~%")))

(define (->>-modname mname)
  (string-append "$" (strip-dollar mname)))

(define (format-wo-const expr)
  (let ((op (car expr))
        (val (if (pair? (cdr expr)) (cadr expr) 0)))
    (case op
      ((ptr.null) "i32.const 0")
      ((i32.const i64.const)
       (format #f "~a ~a" op val))
      ((f32.const f64.const)
       (format #f "~a ~a" op (flonum->hex-literal val)))
      (else "i32.const 0"))))

(define (emit-wo-func mname fn resolve-wo-sym port)
  (let ((fname (func-name fn))
        (exp (func-export fn))
        (params (func-params fn))
        (res-type (func-result fn))
        (frame-sz (func-frame-size fn))
        (locals (func-locals fn))
        (body (func-body fn)))
    (fprintf port "  (func ~a" (wo-ident mname fname))
    (when exp (fprintf port " (export \"~a\")" exp))
    (fprintf port "~%")

    ;; Params & result
    (when (or (pair? params) res-type)
      (fprintf port "    ")
      (for-each
        (lambda (p)
          (fprintf port "(param $~a ~a) " (strip-dollar (car p)) (wo-type (cdr p))))
        params)
      (when res-type
        (fprintf port "(result ~a) " (wo-type res-type)))
      (fprintf port "~%"))

    ;; Locals (+ $bp$ and $fp$ if frame-sz > 0)
    (let ((all-locals (if (> frame-sz 0)
                          (append locals '(($bp$ . i32) ($fp$ . i32)))
                          locals)))
      (when (pair? all-locals)
        (fprintf port "    ")
        (for-each
          (lambda (l)
            (fprintf port "(local $~a ~a) " (strip-dollar (car l)) (wo-type (cdr l))))
          all-locals)
        (fprintf port "~%")))

    ;; Frame prologue
    (when (> frame-sz 0)
      (fprintf port "    global.get $crt:sp$~%")
      (fprintf port "    local.set $bp$~%")
      (fprintf port "    global.get $crt:sp$~%")
      (fprintf port "    i32.const ~a~%" frame-sz)
      (fprintf port "    i32.sub~%")
      (fprintf port "    local.tee $fp$~%")
      (fprintf port "    global.set $crt:sp$~%"))

    ;; Instructions
    (let emit-seq ((seq body))
      (for-each
        (lambda (ins)
          (let ((op (car ins)))
            (case op
              ((*block*)
               (let ((lbl (list-ref ins 1))
                     (rt (list-ref ins 2))
                     (bseq (list-ref ins 3)))
                 (if rt
                     (fprintf port "    block $~a (result ~a)~%" (strip-dollar lbl) (wo-type rt))
                     (fprintf port "    block $~a~%" (strip-dollar lbl)))
                 (emit-seq bseq)
                 (fprintf port "    end $~a~%" (strip-dollar lbl))))

              ((*loop*)
               (let ((lbl (list-ref ins 1))
                     (rt (list-ref ins 2))
                     (bseq (list-ref ins 3)))
                 (if rt
                     (fprintf port "    loop $~a (result ~a)~%" (strip-dollar lbl) (wo-type rt))
                     (fprintf port "    loop $~a~%" (strip-dollar lbl)))
                 (emit-seq bseq)
                 (fprintf port "    end $~a~%" (strip-dollar lbl))))

              ((*if*)
               (let ((rt (list-ref ins 1))
                     (tseq (list-ref ins 2))
                     (eseq (list-ref ins 3)))
                 (if rt
                     (fprintf port "    if (result ~a)~%" (wo-type rt))
                     (fprintf port "    if~%"))
                 (emit-seq tseq)
                 (when (pair? eseq)
                   (fprintf port "    else~%")
                   (emit-seq eseq))
                 (fprintf port "    end~%")))

              ((frame.addr)
               (let ((off (cadr ins)))
                 (fprintf port "    local.get $fp$~%")
                 (when (not (= off 0))
                   (fprintf port "    i32.const ~a~%" off)
                   (fprintf port "    i32.add~%"))))

              ((ptr.null)
               (fprintf port "    i32.const 0~%"))

              (else
               (emit-wo-simple-instr mname ins frame-sz resolve-wo-sym port)))))
        seq))

    ;; Epilogue at fallthrough end of function (if last instr is not a terminator)
    (if (not (seq-terminates? body))
        (when (> frame-sz 0)
          (fprintf port "    local.get $bp$~%")
          (fprintf port "    global.set $crt:sp$~%"))
        ;; If the function ends with an `if ... else ... end` where both arms return,
        ;; WASM binary validation resets stack polymorphism at `end`, so emit `unreachable`.
        (when (and res-type
                   (pair? body)
                   (eq? (caar (reverse body)) '*if*))
          (fprintf port "    unreachable~%")))

    (fprintf port "  )~%")))

(define (seq-terminates? seq)
  (and (pair? seq)
       (let ((last-ins (car (reverse seq))))
         (or (memq (car last-ins) '(return return_call return_call_indirect br br_table unreachable))
             (and (eq? (car last-ins) '*if*)
                  (seq-terminates? (list-ref last-ins 2))
                  (seq-terminates? (list-ref last-ins 3)))))))

(define (emit-wo-simple-instr mname ins frame-sz resolve-wo-sym port)
  (let ((op (car ins)))
    (case op
      ((ptr.add)    (fprintf port "    i32.add~%"))
      ((ptr.add64)  (fprintf port "    i32.wrap_i64~%    i32.add~%"))
      ((ptr.sub)    (fprintf port "    i32.sub~%"))
      ((ptr.diff)   (fprintf port "    i32.sub~%"))
      ((ptr.diff64) (fprintf port "    i32.sub~%    i64.extend_i32_s~%"))
      ((ptr.eqz)    (fprintf port "    i32.eqz~%"))
      ((ptr.eq)     (fprintf port "    i32.eq~%"))
      ((ptr.ne)     (fprintf port "    i32.ne~%"))
      ((ptr.lt_u)   (fprintf port "    i32.lt_u~%"))
      ((ptr.gt_u)   (fprintf port "    i32.gt_u~%"))
      ((ptr.le_u)   (fprintf port "    i32.le_u~%"))
      ((ptr.ge_u)   (fprintf port "    i32.ge_u~%"))
      ((ptr.of_i64) (fprintf port "    i32.wrap_i64~%"))
      ((i64.of_ptr) (fprintf port "    i64.extend_i32_u~%"))

      ;; Uniform 8-byte pointer slot load/store on 32-bit WASM:
      ;; ptr.load loads the low 32 bits from offset;
      ;; ptr.store zero-extends the 32-bit pointer to 64 bits so the full 8-byte slot is written!
      ((ptr.load)
       (let ((off (cadr ins))
             (aln (min 4 (caddr ins))))
         (fprintf port "    i32.load offset=~a align=~a~%" off aln)))
      ((ptr.store)
       (let ((off (cadr ins))
             (aln (min 8 (caddr ins))))
         (fprintf port "    i64.extend_i32_u~%")
         (fprintf port "    i64.store offset=~a align=~a~%" off aln)))

      ((local.get local.set local.tee)
       (fprintf port "    ~a $~a~%" op (strip-dollar (cadr ins))))

      ((global.get global.set)
       (fprintf port "    ~a ~a~%" op (resolve-wo-sym (cadr ins))))

      ((i32.const i64.const)
       (fprintf port "    ~a ~a~%" op (cadr ins)))

      ((f32.const)
       (let ((v (cadr ins)))
         (fprintf port "    f32.const ~a (; ~a ;)~%" (flonum->f32-hex-literal v) (inexact v))))

      ((f64.const)
       (let ((v (cadr ins)))
         (fprintf port "    f64.const ~a (; ~a ;)~%" (flonum->f64-hex-literal v) (inexact v))))

      ((ref.data)
       (let ((sym (cadr ins))
             (off (caddr ins)))
         (if (= off 0)
             (fprintf port "    ref.data ~a~%" (resolve-wo-sym sym))
             (fprintf port "    ref.data ~a offset=~a~%" (resolve-wo-sym sym) off))))

      ((ref.func)
       (fprintf port "    ref.func ~a~%" (resolve-wo-sym (cadr ins))))

      ((call)
       (fprintf port "    call ~a~%" (resolve-wo-sym (cadr ins))))

      ((call_indirect)
       (let ((ptypes (cadr ins))
             (rtype (caddr ins)))
         (fprintf port "    call_indirect (param")
         (for-each (lambda (pt) (fprintf port " ~a" (wo-type pt))) ptypes)
         (fprintf port ") (result")
         (when rtype (fprintf port " ~a" (wo-type rtype)))
         (fprintf port ")~%")))

      ((return_call)
       (when (> frame-sz 0)
         (fprintf port "    local.get $bp$~%")
         (fprintf port "    global.set $crt:sp$~%"))
       (fprintf port "    return_call ~a~%" (resolve-wo-sym (cadr ins))))

      ((return_call_indirect)
       (when (> frame-sz 0)
         (fprintf port "    local.get $bp$~%")
         (fprintf port "    global.set $crt:sp$~%"))
       (let ((ptypes (cadr ins))
             (rtype (caddr ins)))
         (fprintf port "    return_call_indirect (param")
         (for-each (lambda (pt) (fprintf port " ~a" (wo-type pt))) ptypes)
         (fprintf port ") (result")
         (when rtype (fprintf port " ~a" (wo-type rtype)))
         (fprintf port ")~%")))

      ((br br_if)
       (fprintf port "    ~a $~a~%" op (strip-dollar (cadr ins))))

      ((br_table)
       (let ((targets (cadr ins))
             (def-lbl (caddr ins)))
         (fprintf port "    br_table")
         (for-each (lambda (l) (fprintf port " $~a" (strip-dollar l))) targets)
         (fprintf port " $~a~%" (strip-dollar def-lbl))))

      ((return)
       (when (> frame-sz 0)
         (fprintf port "    local.get $bp$~%")
         (fprintf port "    global.set $crt:sp$~%"))
       (fprintf port "    return~%"))

      (else
       (let ((info (lookup-instr op)))
         (if (memq (instr-kind info) '(load store))
             (fprintf port "    ~a offset=~a align=~a~%" op (cadr ins) (caddr ins))
             (fprintf port "    ~a~%" op)))))))

;; =============================================================================
;; 7. Abstract-Stack Unstacker -> Three-Address IR (3AC)
;; =============================================================================

;; Operand constructors & accessors:
;;   (imm type val)
;;   (frame-addr offset)
;;   (ref-data sym offset)
;;   (ref-func sym)
;;   (vreg sym type)

(define (opnd-kind x) (car x))

(define (opnd-type x)
  (case (car x)
    ((imm)        (cadr x))
    ((frame-addr) 'ptr)
    ((ref-data)   'ptr)
    ((ref-func)   'ptr)
    ((vreg)       (caddr x))
    (else (error "unknown operand" x))))

(define (vreg? x)
  (and (pair? x) (eq? (car x) 'vreg)))

(define (vreg-name x) (cadr x))
(define (vreg-type x) (caddr x))

(define (imm? x)
  (and (pair? x) (eq? (car x) 'imm)))

(define (frame-addr? x)
  (and (pair? x) (eq? (car x) 'frame-addr)))

;; Unstack a parsed function into:
;;   (list 'ir-func name export params res-type frame-size all-vregs ir-instrs)
;; where all-vregs is an alist of (vreg-sym . type) for params + locals + temps.
(define (unstack-func mod fn)
  (let* ((fname (func-name fn))
         (exp (func-export fn))
         (params (func-params fn))
         (res-type (func-result fn))
         (frame-sz (func-frame-size fn))
         (locals (func-locals fn))
         (body (func-body fn))
         (fsigs (build-func-sig-table mod))
         (gtypes (build-global-type-table mod))
         (var-types (append params locals))
         (temps '())
         (temp-counter 0)
         (lbl-counter 0)
         (ir-rev '()))

    (define (emit! ins)
      (set! ir-rev (cons ins ir-rev)))

    (define (new-temp! type)
      (let* ((idx temp-counter)
             (sym (string->symbol (string-append "$_t" (number->string idx)))))
        (set! temp-counter (+ idx 1))
        (set! temps (cons (cons sym type) temps))
        (list 'vreg sym type)))

    (define (new-label! prefix)
      (let ((idx lbl-counter))
        (set! lbl-counter (+ idx 1))
        (string-append "L_" (c-ident (mod-name mod)) "_" (c-ident fname) "_" (c-ident prefix) "_" (number->string idx))))

    (define (lookup-var-type vname)
      (let ((entry (lookup-sym-entry vname var-types)))
        (if entry
            (cdr entry)
            (error "undeclared local/param variable" vname fname))))

    (define (lookup-global-type gname)
      (let ((entry (lookup-sym-entry gname gtypes)))
        (if entry (cdr entry) 'i32)))

    (define (lookup-func-sig callee)
      (let ((entry (lookup-sym-entry callee fsigs)))
        (if entry
            (cons (list-ref entry 1) (list-ref entry 2))
            (error "unknown function in call" callee fname))))

    ;; Before overwriting local $x, materialize any reference to $x on the operand stack
    (define (preserve-var-on-stack! vname stk)
      (map (lambda (opnd)
             (if (and (vreg? opnd) (eq? (vreg-name opnd) vname))
                 (let ((tmp (new-temp! (vreg-type opnd))))
                   (emit! (list 'mov tmp opnd))
                   tmp)
                 opnd))
           stk))

    ;; Materialize non-trivial stack items before entering a loop or branch
    (define (materialize-stack! stk)
      (map (lambda (opnd)
             (if (vreg? opnd)
                 opnd
                 (let ((tmp (new-temp! (opnd-type opnd))))
                   (emit! (list 'mov tmp opnd))
                   tmp)))
           stk))

    (define (terminating-seq? seq)
      (seq-terminates? seq))

    ;; ctrl-env entries: (list label-sym kind target-lbl-str res-vreg)
    ;; where kind is 'block or 'loop
    (define (lookup-ctrl lbl ctrl-env)
      (let ((raw (strip-dollar lbl)))
        (let loop ((env ctrl-env))
          (cond ((null? env) (error "unknown branch label" lbl fname))
                ((string=? raw (strip-dollar (caar env))) (car env))
                (else (loop (cdr env)))))))

    (define (unstack-seq seq stk ctrl-env)
      (let loop ((rest seq) (s stk))
        (if (null? rest)
            s
            (let* ((ins (car rest))
                   (op (car ins)))
              (case op
                ((*block*)
                 (let* ((lbl (list-ref ins 1))
                        (rt (list-ref ins 2))
                        (bseq (list-ref ins 3))
                        (end-lbl (new-label! (string-append "blk_" (strip-dollar lbl))))
                        (res-vreg (if rt (new-temp! rt) #f))
                        (new-env (cons (list lbl 'block end-lbl res-vreg) ctrl-env))
                        (s-after (unstack-seq bseq s new-env)))
                   (when (and rt (not (terminating-seq? bseq)) (pair? s-after))
                     (emit! (list 'mov res-vreg (car s-after))))
                   (emit! (list 'label end-lbl))
                   (loop (cdr rest) (if rt (cons res-vreg s) s))))

                ((*loop*)
                 (let* ((lbl (list-ref ins 1))
                        (rt (list-ref ins 2))
                        (bseq (list-ref ins 3))
                        (s-mat (materialize-stack! s))
                        (start-lbl (new-label! (string-append "loop_" (strip-dollar lbl))))
                        (end-lbl (new-label! (string-append "loop_end_" (strip-dollar lbl))))
                        (res-vreg (if rt (new-temp! rt) #f))
                        (new-env (cons (list lbl 'loop start-lbl res-vreg) ctrl-env)))
                   (emit! (list 'loop-begin lbl start-lbl))
                   (let ((s-after (unstack-seq bseq s-mat new-env)))
                     (when (and rt (not (terminating-seq? bseq)) (pair? s-after))
                       (emit! (list 'mov res-vreg (car s-after))))
                     (emit! (list 'loop-end lbl end-lbl))
                     (loop (cdr rest) (if rt (cons res-vreg s-mat) s-mat)))))

                ((*if*)
                 (let* ((rt (list-ref ins 1))
                        (tseq (list-ref ins 2))
                        (eseq (list-ref ins 3))
                        (cond-opnd (car s))
                        (s-base (cdr s))
                        (else-lbl (new-label! "else"))
                        (end-lbl (new-label! "endif"))
                        (res-vreg (if rt (new-temp! rt) #f)))
                   (if (null? eseq)
                       (begin
                         (emit! (list 'br-false cond-opnd end-lbl))
                         (let ((s-then (unstack-seq tseq s-base ctrl-env)))
                           (when (and rt (not (terminating-seq? tseq)) (pair? s-then))
                             (emit! (list 'mov res-vreg (car s-then))))
                           (emit! (list 'label end-lbl))
                           (loop (cdr rest) (if rt (cons res-vreg s-base) s-base))))
                       (begin
                         (emit! (list 'br-false cond-opnd else-lbl))
                         (let ((s-then (unstack-seq tseq s-base ctrl-env)))
                           (when (and rt (not (terminating-seq? tseq)) (pair? s-then))
                             (emit! (list 'mov res-vreg (car s-then))))
                           (when (not (terminating-seq? tseq))
                             (emit! (list 'jump end-lbl)))
                           (emit! (list 'label else-lbl))
                           (let ((s-else (unstack-seq eseq s-base ctrl-env)))
                             (when (and rt (not (terminating-seq? eseq)) (pair? s-else))
                               (emit! (list 'mov res-vreg (car s-else))))
                             (emit! (list 'label end-lbl))
                             (loop (cdr rest) (if rt (cons res-vreg s-base) s-base))))))))

                (else
                 (let ((s-next (unstack-simple-instr ins s ctrl-env)))
                   (loop (cdr rest) s-next))))))))

    (define (unstack-simple-instr ins s ctrl-env)
      (let* ((op (car ins))
             (info (lookup-instr op))
             (kind (instr-kind info)))
        (case kind
          ((nop) s)
          ((unreachable)
           (emit! '(unreachable))
           s)
          ((drop)
           (cdr s))
          ((const)
           (if (eq? op 'ptr.null)
               (cons '(imm ptr 0) s)
               (cons (list 'imm (instr-push-type info) (cadr ins)) s)))
          ((frame-addr)
           (cons (list 'frame-addr (cadr ins)) s))
          ((ref-data)
           (cons (list 'ref-data (cadr ins) (caddr ins)) s))
          ((ref-func)
           (cons (list 'ref-func (cadr ins)) s))
          ((local-get)
           (let* ((vname (cadr ins))
                  (vt (lookup-var-type vname)))
             (cons (list 'vreg vname vt) s)))
          ((local-set)
           (let* ((vname (cadr ins))
                  (vt (lookup-var-type vname))
                  (val (car s))
                  (s-rem (preserve-var-on-stack! vname (cdr s))))
             (emit! (list 'mov (list 'vreg vname vt) val))
             s-rem))
          ((local-tee)
           (let* ((vname (cadr ins))
                  (vt (lookup-var-type vname))
                  (val (car s))
                  (s-rem (preserve-var-on-stack! vname (cdr s)))
                  (dst (list 'vreg vname vt)))
             (emit! (list 'mov dst val))
             (cons dst s-rem)))
          ((global-get)
           (let* ((gname (cadr ins))
                  (gt (lookup-global-type gname))
                  (dst (new-temp! gt)))
             (emit! (list 'global-get dst gname gt))
             (cons dst s)))
          ((global-set)
           (let* ((gname (cadr ins))
                  (gt (lookup-global-type gname))
                  (val (car s)))
             (emit! (list 'global-set gname gt val))
             (cdr s)))
          ((select)
           (let* ((cond-opnd (car s))
                  (vfalse (cadr s))
                  (vtrue (caddr s))
                  (s-rem (cdddr s))
                  (dst (new-temp! (opnd-type vtrue))))
             (emit! (list 'select dst vtrue vfalse cond-opnd))
             (cons dst s-rem)))
          ((unary)
           (let* ((x (car s))
                  (s-rem (cdr s))
                  (dst (new-temp! (instr-push-type info))))
             (emit! (list 'unary dst op x))
             (cons dst s-rem)))
          ((binary compare)
           (let* ((rhs (car s))
                  (lhs (cadr s))
                  (s-rem (cddr s)))
             ;; Peephole: fold (frame-addr K) +/- (imm i32 C) directly into (frame-addr K+-C)
             (cond
               ((and (eq? op 'ptr.add) (frame-addr? lhs) (imm? rhs))
                (cons (list 'frame-addr (+ (cadr lhs) (caddr rhs))) s-rem))
               ((and (eq? op 'ptr.sub) (frame-addr? lhs) (imm? rhs))
                (cons (list 'frame-addr (- (cadr lhs) (caddr rhs))) s-rem))
               (else
                (let ((dst (new-temp! (instr-push-type info))))
                  (emit! (list 'binary dst op lhs rhs))
                  (cons dst s-rem))))))
          ((convert)
           (let* ((x (car s))
                  (s-rem (cdr s))
                  (dst (new-temp! (instr-push-type info))))
             (emit! (list 'unary dst op x))
             (cons dst s-rem)))
          ((load)
           (let* ((off (cadr ins))
                  (aln (caddr ins))
                  (addr (car s))
                  (s-rem (cdr s))
                  (dst (new-temp! (instr-push-type info))))
             (if (frame-addr? addr)
                 (emit! (list 'load dst op '(frame-addr 0) (+ (cadr addr) off) aln))
                 (emit! (list 'load dst op addr off aln)))
             (cons dst s-rem)))
          ((store)
           (let* ((off (cadr ins))
                  (aln (caddr ins))
                  (val (car s))
                  (addr (cadr s))
                  (s-rem (cddr s)))
             (if (frame-addr? addr)
                 (emit! (list 'store op '(frame-addr 0) (+ (cadr addr) off) aln val))
                 (emit! (list 'store op addr off aln val)))
             s-rem))
          ((call)
           (let* ((callee (cadr ins))
                  (sig (lookup-func-sig callee))
                  (ptypes (car sig))
                  (rtype (cdr sig))
                  (nargs (length ptypes))
                  (args-rev (take nargs s))
                  (s-rem (drop-n nargs s))
                  (args (reverse args-rev))
                  (dst (if rtype (new-temp! rtype) #f)))
             (emit! (list 'call dst callee args))
             (if dst (cons dst s-rem) s-rem)))
          ((call-indirect)
           (let* ((ptypes (cadr ins))
                  (rtype (caddr ins))
                  (callee-opnd (car s))
                  (s1 (cdr s))
                  (nargs (length ptypes))
                  (args (reverse (take nargs s1)))
                  (s-rem (drop-n nargs s1))
                  (dst (if rtype (new-temp! rtype) #f)))
             (emit! (list 'call-indirect dst ptypes rtype callee-opnd args))
             (if dst (cons dst s-rem) s-rem)))
          ((return-call)
           (let* ((callee (cadr ins))
                  (sig (lookup-func-sig callee))
                  (ptypes (car sig))
                  (rtype (cdr sig))
                  (nargs (length ptypes))
                  (args (reverse (take nargs s)))
                  (s-rem (drop-n nargs s)))
             (when (any frame-addr? args)
               (error "return_call cannot pass escaping frame.addr pointer" fname callee))
             (emit! (list 'return-call rtype callee args))
             s-rem))
          ((return-call-indirect)
           (let* ((ptypes (cadr ins))
                  (rtype (caddr ins))
                  (callee-opnd (car s))
                  (s1 (cdr s))
                  (nargs (length ptypes))
                  (args (reverse (take nargs s1)))
                  (s-rem (drop-n nargs s1)))
             (when (or (frame-addr? callee-opnd) (any frame-addr? args))
               (error "return_call_indirect cannot pass escaping frame.addr pointer" fname))
             (emit! (list 'return-call-indirect ptypes rtype callee-opnd args))
             s-rem))
          ((br)
           (let* ((lbl (cadr ins))
                  (target (lookup-ctrl lbl ctrl-env))
                  (tkind (list-ref target 1))
                  (tlbl (list-ref target 2))
                  (res-vreg (list-ref target 3)))
             (when (and (eq? tkind 'block) res-vreg (pair? s))
               (emit! (list 'mov res-vreg (car s))))
             (emit! (list 'jump tlbl))
             (if (and (eq? tkind 'block) res-vreg (pair? s)) (cdr s) s)))
          ((br-if)
           (let* ((lbl (cadr ins))
                  (cond-opnd (car s))
                  (s-rem (cdr s))
                  (target (lookup-ctrl lbl ctrl-env))
                  (tkind (list-ref target 1))
                  (tlbl (list-ref target 2))
                  (res-vreg (list-ref target 3)))
             (when (and (eq? tkind 'block) res-vreg (pair? s-rem))
               (emit! (list 'mov res-vreg (car s-rem))))
             (emit! (list 'br-true cond-opnd tlbl))
             s-rem))
          ((br-table)
           (let* ((targets (cadr ins))
                  (def-lbl (caddr ins))
                  (idx-opnd (car s))
                  (s-rem (cdr s))
                  (tlbls (map (lambda (l) (list-ref (lookup-ctrl l ctrl-env) 2)) targets))
                  (dlbl (list-ref (lookup-ctrl def-lbl ctrl-env) 2)))
             (emit! (list 'br-table idx-opnd tlbls dlbl))
             s-rem))
          ((return)
           (if res-type
               (let ((ret-val (car s)))
                 (emit! (list 'return ret-val))
                 (cdr s))
               (begin
                 (emit! '(return #f))
                 s)))
          (else
           (error "unhandled instruction kind in unstacker" kind ins)))))

    (let ((final-stk (unstack-seq body '() '())))
      ;; If function has a return value and body didn't end with an explicit return,
      ;; emit an implicit return of top-of-stack
      (when (not (terminating-seq? body))
        (if (and res-type (pair? final-stk))
            (emit! (list 'return (car final-stk)))
            (emit! '(return #f))))
      (list 'ir-func
            fname
            exp
            params
            res-type
            frame-sz
            (append params locals (reverse temps))
            (reverse ir-rev)))))

(define (ir-func-name f)       (list-ref f 1))
(define (ir-func-export f)     (list-ref f 2))
(define (ir-func-params f)     (list-ref f 3))
(define (ir-func-result f)     (list-ref f 4))
(define (ir-func-frame-size f) (list-ref f 5))
(define (ir-func-vregs f)      (list-ref f 6))
(define (ir-func-instrs f)     (list-ref f 7))

;; =============================================================================
;; 8. Target 2: Restricted C99 Emitter (`emit-c99`)
;; =============================================================================

(define (c99-type t)
  (case t
    ((i32) "marv_i32")
    ((i64) "marv_i64")
    ((f32) "marv_f32")
    ((f64) "marv_f64")
    ((ptr) "marv_ptr")
    ((#f)  "void")
    (else  "marv_i32")))

(define (c99-func-cname mname fn-sym export-name)
  (if export-name
      (if (string=? export-name "main")
          "MARV_MAIN_FN"
          (import-c-sym (strip-dollar mname) export-name))
      (asm-ident mname fn-sym)))

;; The C entry point is stricter than MARV's (i32, ptr) shape: hosted
;; C99 (and Clang in particular) requires `int main(int, char **)`.
;; When a function is exported as "main", a ptr parameter is therefore
;; declared `char **` (marv_i32 is int32_t == int, so the rest of the
;; signature already conforms).
(define (c99-param-type exp ptype)
  (if (and exp (string=? exp "main") (eq? ptype 'ptr))
      "char **"
      (c99-type ptype)))

(define (import-c-sym imod iname)
  (let* ((cm (c-ident imod))
         (cn (c-ident iname))
         (prefix (string-append cm "_")))
    (if (string-prefix? prefix cn)
        cn
        (string-append prefix cn))))

(define (sym-matches-import? sym dsym iname)
  (or (eq? sym dsym)
      (string=? (strip-dollar sym) (strip-dollar dsym))
      (string=? (strip-dollar sym) iname)))


(define *marv-c99-header*
"/* MARV Restricted C99 Runtime & Intrinsics */
#ifndef MARV_H_
#define MARV_H_

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <math.h>

typedef int32_t  marv_i32;
typedef uint32_t marv_u32;
typedef int64_t  marv_i64;
typedef uint64_t marv_u64;
typedef float    marv_f32;
typedef double   marv_f64;
typedef void*    marv_ptr;

#define MARV_NULL ((marv_ptr)0)

typedef union marv_va_arg {
  marv_i32 va_int;
  marv_u32 va_uint;
  marv_i64 va_llong;
  marv_u64 va_ullong;
  marv_f64 va_double;
  uint64_t va_ptr_slot;
  uint64_t _pad[2];
} marv_va_arg_t;

#define MARV_VA_COUNT(ap) \\
  (((const marv_va_arg_t *)(uintptr_t)(ap))[-1].va_int)

#define MARV_VA_PTR(slot_ptr) \\
  ((marv_ptr)(uintptr_t)((slot_ptr)->va_ptr_slot))

#if defined(_MSC_VER) && !defined(__cplusplus) && (_MSC_VER < 1900)
#define inline __inline
#endif

#if defined(__GNUC__) || defined(__clang__)
#define MARV_ALIGNED(n) __attribute__((aligned(n)))
#define MARV_UNUSED __attribute__((unused))
#elif defined(_MSC_VER)
#define MARV_ALIGNED(n) __declspec(align(n))
#define MARV_UNUSED
#else
#define MARV_ALIGNED(n)
#define MARV_UNUSED
#endif

/* Static data segments are unions of per-run arrays (see the data
   section of the emitter): MARV_DSZ(n) is the run array bound.
   A union of arrays needs a positive bound, so an empty run still
   occupies one element; the surplus bytes sit past every offset the
   program can compute and are never addressed.  (On ANSI compilers
   without zero-length-array extensions this is also the portable
   way to say 'possibly empty'.) */
#define MARV_DSZ(n) ((n) > 0 ? (n) : 1)

/* static-data anchor tail: the final anchor's byte view runs out to
   the end of its own object (255 is just an upper bound on the view;
   an anchor union's size is the max of its members, and anchors are
   memcpy sources, never sized by the view).  The segment object
   itself is sized by its own byte member, which spans the whole
   segment; do NOT cap it by this value. */
#define MARV_DS_TAIL 255

#if defined(__has_attribute)
#  if __has_attribute(musttail) && (defined(MARV_NO_TRAMPOLINE) || !(defined(__powerpc64__) && (!defined(_CALL_ELF) || (_CALL_ELF == 1))))
#    define MARV_MUSTTAIL __attribute__((musttail))
#    define MARV_TAILCALL_GUARANTEE 1
#  else
#    define MARV_MUSTTAIL
#  endif
#else
#  define MARV_MUSTTAIL
#endif

/* Chained code (the converters emit modules whose functions transfer
   control with MARV_MUSTTAIL returns) needs those returns to become
   jumps, or the C stack grows by one frame per transfer until it
   overflows.  Three configurations, Skint-style:
     MARV_TAILCALL_GUARANTEE -- musttail is honoured; plain chain.
     MARV_TRAMPOLINE (default when there is no guarantee) -- unit
       transfers are fuel-checked; when the fuel runs out the transfer
       stashes the chain registers in the marv_tc_* shadows below and
       returns, unwinding to a driver loop at main that re-enters the
       target.  The C stack is then bounded by MARV_TC_FUEL frames.
     MARV_NO_TRAMPOLINE (user-defined) -- trust the compiler's plain
       sibling-call heuristics; historical shape, no checks.
   The guarantee is withheld on PowerPC64 ELFv1 (function descriptors
   with a per-function TOC in r2) even when musttail exists: clang
   refuses indirect musttail calls on PPC outright, and GCC before 15
   has no musttail and silently declines to sibling-call indirect or
   cross-TU calls there.  Trampoline mode is the safe default for
   that ABI; ELFv2 has no per-function TOC and keeps the guarantee.
   Defining MARV_NO_TRAMPOLINE on ELFv1 re-enables musttail there:
   the compiler then either honours it or refuses to compile (clang),
   never silently miscompiles. */
#if !defined(MARV_TAILCALL_GUARANTEE) && !defined(MARV_NO_TRAMPOLINE)
#  define MARV_TRAMPOLINE 1
#endif

#ifdef MARV_TRAMPOLINE
#  ifndef MARV_TC_FUEL
#    define MARV_TC_FUEL 512
#  endif
#  ifndef MARV_TC_STATE_DEFINED
/* Resume state (shadow chain registers): a transfer that finds the
   fuel exhausted stores its target and arguments here and returns;
   the driver loop at main reloads and re-enters.  Generated code
   refers only to the marv_tc_* names. */
/* The resume slots are a signature-agnostic array: slot 0 is the
   target function pointer bit-cast to an integer, slot 1 the pending
   flag, the remaining slots the callee's arguments in order, each
   zero-extended to 64 bits (pointers via uintptr_t).  Two chain
   signatures are supported (the two converters' unit shapes):
   DS  (i64 i64 i64 i64 ptr ptr i32) -> ptr   -- uses slots 3..9
   CPS (ptr i64 ptr ptr i32) -> ptr           -- uses slots 3..7
   The driver dispatches on the number of stashed arguments. */
/* Definitions live in exactly one translation unit of a program:
   in link mode the driver keeps them in the first .mvs module (or
   in a raw C input that already carries them, in which case every
   .mvs module gets the externs above); split pieces k>0 also get
   externs. */
extern marv_i64 marv_tc_fuel;
extern marv_i64 marv_tc_state[16];
#  endif
#  define MARV_MAIN_FN marv_main_impl
#else
#  define MARV_MAIN_FN main
#endif

/* Macro form of the MARV C99 intrinsics.  The expression macros use
   GNU statement expressions where a helper needs a temporary, so each
   argument is evaluated exactly once, just as with the old inline
   functions.  The fallback path for other compilers is below. */
#if defined(__GNUC__) || defined(__clang__)
#define marv_ptr_add(p, off) ((marv_ptr)((uint8_t*)(p) + (intptr_t)(off)))
#define marv_ptr_add64(p, off) ((marv_ptr)((uint8_t*)(p) + (intptr_t)(off)))
#define marv_ptr_sub(p, off) ((marv_ptr)((uint8_t*)(p) - (intptr_t)(off)))
#define marv_ptr_diff(p1, p2) ((marv_i32)((const uint8_t*)(p1) - (const uint8_t*)(p2)))
#define marv_ptr_diff64(p1, p2) ((marv_i64)((const uint8_t*)(p1) - (const uint8_t*)(p2)))
#define marv_ptr_eqz(p) ((p) == NULL ? 1 : 0)
#define marv_ptr_eq(a, b) ((a) == (b) ? 1 : 0)
#define marv_ptr_ne(a, b) ((a) != (b) ? 1 : 0)
#define marv_ptr_lt_u(a, b) ((uintptr_t)(a) < (uintptr_t)(b) ? 1 : 0)
#define marv_ptr_gt_u(a, b) ((uintptr_t)(a) > (uintptr_t)(b) ? 1 : 0)
#define marv_ptr_le_u(a, b) ((uintptr_t)(a) <= (uintptr_t)(b) ? 1 : 0)
#define marv_ptr_ge_u(a, b) ((uintptr_t)(a) >= (uintptr_t)(b) ? 1 : 0)
#define marv_ptr_of_i64(v) ((marv_ptr)(uintptr_t)(uint64_t)(v))
#define marv_i64_of_ptr(p) ((marv_i64)(uint64_t)(uintptr_t)(p))
#define marv_ptr_load(base, offset) __extension__({ marv_ptr _b = (base); marv_i64 _o = (offset); uint64_t _s = 0; memcpy(&_s, (const uint8_t*)_b + _o, 8); (marv_ptr)(uintptr_t)_s; })
#define marv_ptr_store(base, offset, val) do { marv_ptr _b = (base); marv_i64 _o = (offset); uint64_t _s = (uint64_t)(uintptr_t)(val); memcpy((uint8_t*)_b + _o, &_s, 8); } while (0)
#define marv_i32_load(base, off) __extension__({ marv_i32 _v; memcpy(&_v, (const uint8_t*)(base) + (off), 4); _v; })
#define marv_i32_load8_s(base, off) __extension__({ int8_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 1); (marv_i32)_v; })
#define marv_i32_load8_u(base, off) __extension__({ uint8_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 1); (marv_i32)_v; })
#define marv_i32_load16_s(base, off) __extension__({ int16_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 2); (marv_i32)_v; })
#define marv_i32_load16_u(base, off) __extension__({ uint16_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 2); (marv_i32)_v; })
#define marv_i64_load(base, off) __extension__({ marv_i64 _v; memcpy(&_v, (const uint8_t*)(base) + (off), 8); _v; })
#define marv_i64_load8_s(base, off) __extension__({ int8_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 1); (marv_i64)_v; })
#define marv_i64_load8_u(base, off) __extension__({ uint8_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 1); (marv_i64)_v; })
#define marv_i64_load16_s(base, off) __extension__({ int16_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 2); (marv_i64)_v; })
#define marv_i64_load16_u(base, off) __extension__({ uint16_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 2); (marv_i64)_v; })
#define marv_i64_load32_s(base, off) __extension__({ int32_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 4); (marv_i64)_v; })
#define marv_i64_load32_u(base, off) __extension__({ uint32_t _v; memcpy(&_v, (const uint8_t*)(base) + (off), 4); (marv_i64)_v; })
#define marv_f32_load(base, off) __extension__({ marv_f32 _v; memcpy(&_v, (const uint8_t*)(base) + (off), 4); _v; })
#define marv_f64_load(base, off) __extension__({ marv_f64 _v; memcpy(&_v, (const uint8_t*)(base) + (off), 8); _v; })
#define marv_i32_store(base, off, v) do { marv_i32 _v = (v); memcpy((uint8_t*)(base) + (off), &_v, 4); } while (0)
#define marv_i32_store8(base, off, v) do { uint8_t _v = (uint8_t)(v); memcpy((uint8_t*)(base) + (off), &_v, 1); } while (0)
#define marv_i32_store16(base, off, v) do { uint16_t _v = (uint16_t)(v); memcpy((uint8_t*)(base) + (off), &_v, 2); } while (0)
#define marv_i64_store(base, off, v) do { marv_i64 _v = (v); memcpy((uint8_t*)(base) + (off), &_v, 8); } while (0)
#define marv_i64_store8(base, off, v) do { uint8_t _v = (uint8_t)(v); memcpy((uint8_t*)(base) + (off), &_v, 1); } while (0)
#define marv_i64_store16(base, off, v) do { uint16_t _v = (uint16_t)(v); memcpy((uint8_t*)(base) + (off), &_v, 2); } while (0)
#define marv_i64_store32(base, off, v) do { uint32_t _v = (uint32_t)(v); memcpy((uint8_t*)(base) + (off), &_v, 4); } while (0)
#define marv_f32_store(base, off, v) do { marv_f32 _v = (v); memcpy((uint8_t*)(base) + (off), &_v, 4); } while (0)
#define marv_f64_store(base, off, v) do { marv_f64 _v = (v); memcpy((uint8_t*)(base) + (off), &_v, 8); } while (0)
#define marv_i32_add(a, b) ((marv_i32)((marv_u32)(a) + (marv_u32)(b)))
#define marv_i32_sub(a, b) ((marv_i32)((marv_u32)(a) - (marv_u32)(b)))
#define marv_i32_mul(a, b) ((marv_i32)((marv_u32)(a) * (marv_u32)(b)))
#define marv_i32_div_s(a, b) ((a) / (b))
#define marv_i32_div_u(a, b) ((marv_i32)((marv_u32)(a) / (marv_u32)(b)))
#define marv_i32_rem_s(a, b) ((a) % (b))
#define marv_i32_rem_u(a, b) ((marv_i32)((marv_u32)(a) % (marv_u32)(b)))
#define marv_i32_and(a, b) ((a) & (b))
#define marv_i32_or(a, b) ((a) | (b))
#define marv_i32_xor(a, b) ((a) ^ (b))
#define marv_i32_shl(a, b) ((marv_i32)((marv_u32)(a) << ((b) & 31)))
#define marv_i32_shr_s(a, b) ((a) >> ((b) & 31))
#define marv_i32_shr_u(a, b) ((marv_i32)((marv_u32)(a) >> ((b) & 31)))
#define marv_i32_rotl(a, b) __extension__({ marv_u32 _u = (marv_u32)(a); marv_u32 _k = (marv_u32)(b) & 31u; (marv_i32)((_u << _k) | (_u >> ((32u - _k) & 31u))); })
#define marv_i32_rotr(a, b) __extension__({ marv_u32 _u = (marv_u32)(a); marv_u32 _k = (marv_u32)(b) & 31u; (marv_i32)((_u >> _k) | (_u << ((32u - _k) & 31u))); })
#define marv_i32_clz(a) __extension__({ marv_i32 _a = (a); _a == 0 ? 32 : (marv_i32)__builtin_clz((marv_u32)_a); })
#define marv_i32_ctz(a) __extension__({ marv_i32 _a = (a); _a == 0 ? 32 : (marv_i32)__builtin_ctz((marv_u32)_a); })
#define marv_i32_popcnt(a) ((marv_i32)__builtin_popcount((marv_u32)(a)))
#define marv_i32_eqz(a) ((a) == 0 ? 1 : 0)
#define marv_i32_eq(a, b) ((a) == (b) ? 1 : 0)
#define marv_i32_ne(a, b) ((a) != (b) ? 1 : 0)
#define marv_i32_lt_s(a, b) ((a) < (b) ? 1 : 0)
#define marv_i32_lt_u(a, b) ((marv_u32)(a) < (marv_u32)(b) ? 1 : 0)
#define marv_i32_gt_s(a, b) ((a) > (b) ? 1 : 0)
#define marv_i32_gt_u(a, b) ((marv_u32)(a) > (marv_u32)(b) ? 1 : 0)
#define marv_i32_le_s(a, b) ((a) <= (b) ? 1 : 0)
#define marv_i32_le_u(a, b) ((marv_u32)(a) <= (marv_u32)(b) ? 1 : 0)
#define marv_i32_ge_s(a, b) ((a) >= (b) ? 1 : 0)
#define marv_i32_ge_u(a, b) ((marv_u32)(a) >= (marv_u32)(b) ? 1 : 0)
#define marv_i64_add(a, b) ((marv_i64)((marv_u64)(a) + (marv_u64)(b)))
#define marv_i64_sub(a, b) ((marv_i64)((marv_u64)(a) - (marv_u64)(b)))
#define marv_i64_mul(a, b) ((marv_i64)((marv_u64)(a) * (marv_u64)(b)))
#define marv_i64_div_s(a, b) ((a) / (b))
#define marv_i64_div_u(a, b) ((marv_i64)((marv_u64)(a) / (marv_u64)(b)))
#define marv_i64_rem_s(a, b) ((a) % (b))
#define marv_i64_rem_u(a, b) ((marv_i64)((marv_u64)(a) % (marv_u64)(b)))
#define marv_i64_and(a, b) ((a) & (b))
#define marv_i64_or(a, b) ((a) | (b))
#define marv_i64_xor(a, b) ((a) ^ (b))
#define marv_i64_shl(a, b) ((marv_i64)((marv_u64)(a) << ((b) & 63)))
#define marv_i64_shr_s(a, b) ((a) >> ((b) & 63))
#define marv_i64_shr_u(a, b) ((marv_i64)((marv_u64)(a) >> ((b) & 63)))
#define marv_i64_rotl(a, b) __extension__({ marv_u64 _u = (marv_u64)(a); marv_u64 _k = (marv_u64)(b) & 63u; (marv_i64)((_u << _k) | (_u >> ((64u - _k) & 63u))); })
#define marv_i64_rotr(a, b) __extension__({ marv_u64 _u = (marv_u64)(a); marv_u64 _k = (marv_u64)(b) & 63u; (marv_i64)((_u >> _k) | (_u << ((64u - _k) & 63u))); })
#define marv_i64_clz(a) __extension__({ marv_i64 _a = (a); _a == 0 ? 64 : (marv_i64)__builtin_clzll((marv_u64)_a); })
#define marv_i64_ctz(a) __extension__({ marv_i64 _a = (a); _a == 0 ? 64 : (marv_i64)__builtin_ctzll((marv_u64)_a); })
#define marv_i64_popcnt(a) ((marv_i64)__builtin_popcountll((marv_u64)(a)))
#define marv_i64_eqz(a) ((marv_i32)((a) == 0 ? 1 : 0))
#define marv_i64_eq(a, b) ((a) == (b) ? 1 : 0)
#define marv_i64_ne(a, b) ((a) != (b) ? 1 : 0)
#define marv_i64_lt_s(a, b) ((a) < (b) ? 1 : 0)
#define marv_i64_lt_u(a, b) ((marv_u64)(a) < (marv_u64)(b) ? 1 : 0)
#define marv_i64_gt_s(a, b) ((a) > (b) ? 1 : 0)
#define marv_i64_gt_u(a, b) ((marv_u64)(a) > (marv_u64)(b) ? 1 : 0)
#define marv_i64_le_s(a, b) ((a) <= (b) ? 1 : 0)
#define marv_i64_le_u(a, b) ((marv_u64)(a) <= (marv_u64)(b) ? 1 : 0)
#define marv_i64_ge_s(a, b) ((a) >= (b) ? 1 : 0)
#define marv_i64_ge_u(a, b) ((marv_u64)(a) >= (marv_u64)(b) ? 1 : 0)
#define marv_f32_add(a, b) ((a) + (b))
#define marv_f32_sub(a, b) ((a) - (b))
#define marv_f32_mul(a, b) ((a) * (b))
#define marv_f32_div(a, b) ((a) / (b))
#define marv_f32_sqrt(a) (sqrtf(a))
#define marv_f32_abs(a) (fabsf(a))
#define marv_f32_neg(a) (-(a))
#define marv_f32_copysign(a, b) (copysignf((a), (b)))
#define marv_f32_ceil(a) (ceilf(a))
#define marv_f32_floor(a) (floorf(a))
#define marv_f32_trunc(a) (truncf(a))
#define marv_f32_nearest(a) (rintf(a))
#define marv_f32_min(a, b) (fminf((a), (b)))
#define marv_f32_max(a, b) (fmaxf((a), (b)))
#define marv_f32_eq(a, b) ((a) == (b) ? 1 : 0)
#define marv_f32_ne(a, b) ((a) != (b) ? 1 : 0)
#define marv_f32_lt(a, b) ((a) < (b) ? 1 : 0)
#define marv_f32_gt(a, b) ((a) > (b) ? 1 : 0)
#define marv_f32_le(a, b) ((a) <= (b) ? 1 : 0)
#define marv_f32_ge(a, b) ((a) >= (b) ? 1 : 0)
#define marv_f64_add(a, b) ((a) + (b))
#define marv_f64_sub(a, b) ((a) - (b))
#define marv_f64_mul(a, b) ((a) * (b))
#define marv_f64_div(a, b) ((a) / (b))
#define marv_f64_sqrt(a) (sqrt(a))
#define marv_f64_abs(a) (fabs(a))
#define marv_f64_neg(a) (-(a))
#define marv_f64_copysign(a, b) (copysign((a), (b)))
#define marv_f64_ceil(a) (ceil(a))
#define marv_f64_floor(a) (floor(a))
#define marv_f64_trunc(a) (trunc(a))
#define marv_f64_nearest(a) (rint(a))
#define marv_f64_min(a, b) (fmin((a), (b)))
#define marv_f64_max(a, b) (fmax((a), (b)))
#define marv_f64_eq(a, b) ((a) == (b) ? 1 : 0)
#define marv_f64_ne(a, b) ((a) != (b) ? 1 : 0)
#define marv_f64_lt(a, b) ((a) < (b) ? 1 : 0)
#define marv_f64_gt(a, b) ((a) > (b) ? 1 : 0)
#define marv_f64_le(a, b) ((a) <= (b) ? 1 : 0)
#define marv_f64_ge(a, b) ((a) >= (b) ? 1 : 0)
#define marv_i32_wrap_i64(a) ((marv_i32)(marv_u32)(marv_u64)(a))
#define marv_i64_extend_i32_s(a) ((marv_i64)(a))
#define marv_i64_extend_i32_u(a) ((marv_i64)(marv_u32)(a))
#define marv_i32_extend8_s(a) ((marv_i32)(int8_t)(a))
#define marv_i32_extend16_s(a) ((marv_i32)(int16_t)(a))
#define marv_i64_extend8_s(a) ((marv_i64)(int8_t)(a))
#define marv_i64_extend16_s(a) ((marv_i64)(int16_t)(a))
#define marv_i64_extend32_s(a) ((marv_i64)(int32_t)(a))
#define marv_i32_trunc_f32_s(a) ((marv_i32)(a))
#define marv_i32_trunc_f32_u(a) ((marv_i32)(marv_u32)(a))
#define marv_i32_trunc_f64_s(a) ((marv_i32)(a))
#define marv_i32_trunc_f64_u(a) ((marv_i32)(marv_u32)(a))
#define marv_i64_trunc_f32_s(a) ((marv_i64)(a))
#define marv_i64_trunc_f32_u(a) ((marv_i64)(marv_u64)(a))
#define marv_i64_trunc_f64_s(a) ((marv_i64)(a))
#define marv_i64_trunc_f64_u(a) ((marv_i64)(marv_u64)(a))
#define marv_f32_convert_i32_s(a) ((marv_f32)(a))
#define marv_f32_convert_i32_u(a) ((marv_f32)(marv_u32)(a))
#define marv_f32_convert_i64_s(a) ((marv_f32)(a))
#define marv_f32_convert_i64_u(a) ((marv_f32)(marv_u64)(a))
#define marv_f32_demote_f64(a) ((marv_f32)(a))
#define marv_f64_convert_i32_s(a) ((marv_f64)(a))
#define marv_f64_convert_i32_u(a) ((marv_f64)(marv_u32)(a))
#define marv_f64_convert_i64_s(a) ((marv_f64)(a))
#define marv_f64_convert_i64_u(a) ((marv_f64)(marv_u64)(a))
#define marv_f64_promote_f32(a) ((marv_f64)(a))
#define marv_i32_reinterpret_f32(a) __extension__({ marv_f32 _a = (a); marv_i32 _r; memcpy(&_r, &_a, 4); _r; })
#define marv_i64_reinterpret_f64(a) __extension__({ marv_f64 _a = (a); marv_i64 _r; memcpy(&_r, &_a, 8); _r; })
#define marv_f32_reinterpret_i32(a) __extension__({ marv_i32 _a = (a); marv_f32 _r; memcpy(&_r, &_a, 4); _r; })
#define marv_f64_reinterpret_i64(a) __extension__({ marv_i64 _a = (a); marv_f64 _r; memcpy(&_r, &_a, 8); _r; })
#else

static inline marv_ptr marv_ptr_add(marv_ptr p, marv_i32 off) {
  return (marv_ptr)((uint8_t*)p + (intptr_t)off);
}
static inline marv_ptr marv_ptr_add64(marv_ptr p, marv_i64 off) {
  return (marv_ptr)((uint8_t*)p + (intptr_t)off);
}
static inline marv_ptr marv_ptr_sub(marv_ptr p, marv_i32 off) {
  return (marv_ptr)((uint8_t*)p - (intptr_t)off);
}
static inline marv_i32 marv_ptr_diff(marv_ptr p1, marv_ptr p2) {
  return (marv_i32)((const uint8_t*)p1 - (const uint8_t*)p2);
}
static inline marv_i64 marv_ptr_diff64(marv_ptr p1, marv_ptr p2) {
  return (marv_i64)((const uint8_t*)p1 - (const uint8_t*)p2);
}

static inline marv_i32 marv_ptr_eqz(marv_ptr p) { return p == NULL ? 1 : 0; }
static inline marv_i32 marv_ptr_eq(marv_ptr a, marv_ptr b)   { return a == b ? 1 : 0; }
static inline marv_i32 marv_ptr_ne(marv_ptr a, marv_ptr b)   { return a != b ? 1 : 0; }
static inline marv_i32 marv_ptr_lt_u(marv_ptr a, marv_ptr b) { return (uintptr_t)a <  (uintptr_t)b ? 1 : 0; }
static inline marv_i32 marv_ptr_gt_u(marv_ptr a, marv_ptr b) { return (uintptr_t)a >  (uintptr_t)b ? 1 : 0; }
static inline marv_i32 marv_ptr_le_u(marv_ptr a, marv_ptr b) { return (uintptr_t)a <= (uintptr_t)b ? 1 : 0; }
static inline marv_i32 marv_ptr_ge_u(marv_ptr a, marv_ptr b) { return (uintptr_t)a >= (uintptr_t)b ? 1 : 0; }

static inline marv_ptr marv_ptr_of_i64(marv_i64 v) { return (marv_ptr)(uintptr_t)(uint64_t)v; }
static inline marv_i64 marv_i64_of_ptr(marv_ptr p) { return (marv_i64)(uint64_t)(uintptr_t)p; }

static inline marv_ptr marv_ptr_load(marv_ptr base, marv_i64 offset) {
  uint64_t slot = 0;
  memcpy(&slot, (const uint8_t*)base + offset, 8);
  return (marv_ptr)(uintptr_t)slot;
}
static inline void marv_ptr_store(marv_ptr base, marv_i64 offset, marv_ptr val) {
  uint64_t slot = (uint64_t)(uintptr_t)val;
  memcpy((uint8_t*)base + offset, &slot, 8);
}

static inline marv_i32 marv_i32_load(marv_ptr base, marv_i64 off) {
  marv_i32 v; memcpy(&v, (const uint8_t*)base + off, 4); return v;
}
static inline marv_i32 marv_i32_load8_s(marv_ptr base, marv_i64 off) {
  int8_t v; memcpy(&v, (const uint8_t*)base + off, 1); return (marv_i32)v;
}
static inline marv_i32 marv_i32_load8_u(marv_ptr base, marv_i64 off) {
  uint8_t v; memcpy(&v, (const uint8_t*)base + off, 1); return (marv_i32)v;
}
static inline marv_i32 marv_i32_load16_s(marv_ptr base, marv_i64 off) {
  int16_t v; memcpy(&v, (const uint8_t*)base + off, 2); return (marv_i32)v;
}
static inline marv_i32 marv_i32_load16_u(marv_ptr base, marv_i64 off) {
  uint16_t v; memcpy(&v, (const uint8_t*)base + off, 2); return (marv_i32)v;
}

static inline marv_i64 marv_i64_load(marv_ptr base, marv_i64 off) {
  marv_i64 v; memcpy(&v, (const uint8_t*)base + off, 8); return v;
}
static inline marv_i64 marv_i64_load8_s(marv_ptr base, marv_i64 off) {
  int8_t v; memcpy(&v, (const uint8_t*)base + off, 1); return (marv_i64)v;
}
static inline marv_i64 marv_i64_load8_u(marv_ptr base, marv_i64 off) {
  uint8_t v; memcpy(&v, (const uint8_t*)base + off, 1); return (marv_i64)v;
}
static inline marv_i64 marv_i64_load16_s(marv_ptr base, marv_i64 off) {
  int16_t v; memcpy(&v, (const uint8_t*)base + off, 2); return (marv_i64)v;
}
static inline marv_i64 marv_i64_load16_u(marv_ptr base, marv_i64 off) {
  uint16_t v; memcpy(&v, (const uint8_t*)base + off, 2); return (marv_i64)v;
}
static inline marv_i64 marv_i64_load32_s(marv_ptr base, marv_i64 off) {
  int32_t v; memcpy(&v, (const uint8_t*)base + off, 4); return (marv_i64)v;
}
static inline marv_i64 marv_i64_load32_u(marv_ptr base, marv_i64 off) {
  uint32_t v; memcpy(&v, (const uint8_t*)base + off, 4); return (marv_i64)v;
}

static inline marv_f32 marv_f32_load(marv_ptr base, marv_i64 off) {
  marv_f32 v; memcpy(&v, (const uint8_t*)base + off, 4); return v;
}
static inline marv_f64 marv_f64_load(marv_ptr base, marv_i64 off) {
  marv_f64 v; memcpy(&v, (const uint8_t*)base + off, 8); return v;
}

static inline void marv_i32_store(marv_ptr base, marv_i64 off, marv_i32 v) {
  memcpy((uint8_t*)base + off, &v, 4);
}
static inline void marv_i32_store8(marv_ptr base, marv_i64 off, marv_i32 v) {
  uint8_t b = (uint8_t)v; memcpy((uint8_t*)base + off, &b, 1);
}
static inline void marv_i32_store16(marv_ptr base, marv_i64 off, marv_i32 v) {
  uint16_t h = (uint16_t)v; memcpy((uint8_t*)base + off, &h, 2);
}

static inline void marv_i64_store(marv_ptr base, marv_i64 off, marv_i64 v) {
  memcpy((uint8_t*)base + off, &v, 8);
}
static inline void marv_i64_store8(marv_ptr base, marv_i64 off, marv_i64 v) {
  uint8_t b = (uint8_t)v; memcpy((uint8_t*)base + off, &b, 1);
}
static inline void marv_i64_store16(marv_ptr base, marv_i64 off, marv_i64 v) {
  uint16_t h = (uint16_t)v; memcpy((uint8_t*)base + off, &h, 2);
}
static inline void marv_i64_store32(marv_ptr base, marv_i64 off, marv_i64 v) {
  uint32_t w = (uint32_t)v; memcpy((uint8_t*)base + off, &w, 4);
}

static inline void marv_f32_store(marv_ptr base, marv_i64 off, marv_f32 v) {
  memcpy((uint8_t*)base + off, &v, 4);
}
static inline void marv_f64_store(marv_ptr base, marv_i64 off, marv_f64 v) {
  memcpy((uint8_t*)base + off, &v, 8);
}

static inline marv_i32 marv_i32_add(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a + (marv_u32)b); }
static inline marv_i32 marv_i32_sub(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a - (marv_u32)b); }
static inline marv_i32 marv_i32_mul(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a * (marv_u32)b); }
static inline marv_i32 marv_i32_div_s(marv_i32 a, marv_i32 b) { return a / b; }
static inline marv_i32 marv_i32_div_u(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a / (marv_u32)b); }
static inline marv_i32 marv_i32_rem_s(marv_i32 a, marv_i32 b) { return a % b; }
static inline marv_i32 marv_i32_rem_u(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a % (marv_u32)b); }
static inline marv_i32 marv_i32_and(marv_i32 a, marv_i32 b) { return a & b; }
static inline marv_i32 marv_i32_or(marv_i32 a, marv_i32 b)  { return a | b; }
static inline marv_i32 marv_i32_xor(marv_i32 a, marv_i32 b) { return a ^ b; }
static inline marv_i32 marv_i32_shl(marv_i32 a, marv_i32 b)   { return (marv_i32)((marv_u32)a << (b & 31)); }
static inline marv_i32 marv_i32_shr_s(marv_i32 a, marv_i32 b) { return a >> (b & 31); }
static inline marv_i32 marv_i32_shr_u(marv_i32 a, marv_i32 b) { return (marv_i32)((marv_u32)a >> (b & 31)); }
static inline marv_i32 marv_i32_rotl(marv_i32 a, marv_i32 b) {
  marv_u32 u = (marv_u32)a, k = (marv_u32)b & 31u;
  return (marv_i32)((u << k) | (u >> ((32u - k) & 31u)));
}
static inline marv_i32 marv_i32_rotr(marv_i32 a, marv_i32 b) {
  marv_u32 u = (marv_u32)a, k = (marv_u32)b & 31u;
  return (marv_i32)((u >> k) | (u << ((32u - k) & 31u)));
}
static inline marv_i32 marv_i32_clz(marv_i32 a) {
#if defined(__GNUC__) || defined(__clang__)
  return a == 0 ? 32 : (marv_i32)__builtin_clz((marv_u32)a);
#else
  marv_u32 u = (marv_u32)a; marv_i32 n = 0;
  if (u == 0) return 32;
  while ((u & 0x80000000u) == 0) { ++n; u <<= 1; }
  return n;
#endif
}
static inline marv_i32 marv_i32_ctz(marv_i32 a) {
#if defined(__GNUC__) || defined(__clang__)
  return a == 0 ? 32 : (marv_i32)__builtin_ctz((marv_u32)a);
#else
  marv_u32 u = (marv_u32)a; marv_i32 n = 0;
  if (u == 0) return 32;
  while ((u & 1u) == 0) { ++n; u >>= 1; }
  return n;
#endif
}
static inline marv_i32 marv_i32_popcnt(marv_i32 a) {
#if defined(__GNUC__) || defined(__clang__)
  return (marv_i32)__builtin_popcount((marv_u32)a);
#else
  marv_u32 u = (marv_u32)a; marv_i32 n = 0;
  while (u) { u &= u - 1u; ++n; }
  return n;
#endif
}

static inline marv_i32 marv_i32_eqz(marv_i32 a) { return a == 0 ? 1 : 0; }
static inline marv_i32 marv_i32_eq(marv_i32 a, marv_i32 b)   { return a == b ? 1 : 0; }
static inline marv_i32 marv_i32_ne(marv_i32 a, marv_i32 b)   { return a != b ? 1 : 0; }
static inline marv_i32 marv_i32_lt_s(marv_i32 a, marv_i32 b) { return a <  b ? 1 : 0; }
static inline marv_i32 marv_i32_lt_u(marv_i32 a, marv_i32 b) { return (marv_u32)a <  (marv_u32)b ? 1 : 0; }
static inline marv_i32 marv_i32_gt_s(marv_i32 a, marv_i32 b) { return a >  b ? 1 : 0; }
static inline marv_i32 marv_i32_gt_u(marv_i32 a, marv_i32 b) { return (marv_u32)a >  (marv_u32)b ? 1 : 0; }
static inline marv_i32 marv_i32_le_s(marv_i32 a, marv_i32 b) { return a <= b ? 1 : 0; }
static inline marv_i32 marv_i32_le_u(marv_i32 a, marv_i32 b) { return (marv_u32)a <= (marv_u32)b ? 1 : 0; }
static inline marv_i32 marv_i32_ge_s(marv_i32 a, marv_i32 b) { return a >= b ? 1 : 0; }
static inline marv_i32 marv_i32_ge_u(marv_i32 a, marv_i32 b) { return (marv_u32)a >= (marv_u32)b ? 1 : 0; }

static inline marv_i64 marv_i64_add(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a + (marv_u64)b); }
static inline marv_i64 marv_i64_sub(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a - (marv_u64)b); }
static inline marv_i64 marv_i64_mul(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a * (marv_u64)b); }
static inline marv_i64 marv_i64_div_s(marv_i64 a, marv_i64 b) { return a / b; }
static inline marv_i64 marv_i64_div_u(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a / (marv_u64)b); }
static inline marv_i64 marv_i64_rem_s(marv_i64 a, marv_i64 b) { return a % b; }
static inline marv_i64 marv_i64_rem_u(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a % (marv_u64)b); }
static inline marv_i64 marv_i64_and(marv_i64 a, marv_i64 b) { return a & b; }
static inline marv_i64 marv_i64_or(marv_i64 a, marv_i64 b)  { return a | b; }
static inline marv_i64 marv_i64_xor(marv_i64 a, marv_i64 b) { return a ^ b; }
static inline marv_i64 marv_i64_shl(marv_i64 a, marv_i64 b)   { return (marv_i64)((marv_u64)a << (b & 63)); }
static inline marv_i64 marv_i64_shr_s(marv_i64 a, marv_i64 b) { return a >> (b & 63); }
static inline marv_i64 marv_i64_shr_u(marv_i64 a, marv_i64 b) { return (marv_i64)((marv_u64)a >> (b & 63)); }
static inline marv_i64 marv_i64_rotl(marv_i64 a, marv_i64 b) {
  marv_u64 u = (marv_u64)a, k = (marv_u64)b & 63u;
  return (marv_i64)((u << k) | (u >> ((64u - k) & 63u)));
}
static inline marv_i64 marv_i64_rotr(marv_i64 a, marv_i64 b) {
  marv_u64 u = (marv_u64)a, k = (marv_u64)b & 63u;
  return (marv_i64)((u >> k) | (u << ((64u - k) & 63u)));
}
static inline marv_i64 marv_i64_clz(marv_i64 a) {
#if defined(__GNUC__) || defined(__clang__)
  return a == 0 ? 64 : (marv_i64)__builtin_clzll((marv_u64)a);
#else
  marv_u64 u = (marv_u64)a; marv_i64 n = 0;
  if (u == 0) return 64;
  while ((u & (1ULL << 63)) == 0) { ++n; u <<= 1; }
  return n;
#endif
}
static inline marv_i64 marv_i64_ctz(marv_i64 a) {
#if defined(__GNUC__) || defined(__clang__)
  return a == 0 ? 64 : (marv_i64)__builtin_ctzll((marv_u64)a);
#else
  marv_u64 u = (marv_u64)a; marv_i64 n = 0;
  if (u == 0) return 64;
  while ((u & 1ULL) == 0) { ++n; u >>= 1; }
  return n;
#endif
}
static inline marv_i64 marv_i64_popcnt(marv_i64 a) {
#if defined(__GNUC__) || defined(__clang__)
  return (marv_i64)__builtin_popcountll((marv_u64)a);
#else
  marv_u64 u = (marv_u64)a; marv_i64 n = 0;
  while (u) { u &= u - 1ULL; ++n; }
  return n;
#endif
}

static inline marv_i32 marv_i64_eqz(marv_i64 a) { return a == 0 ? 1 : 0; }
static inline marv_i32 marv_i64_eq(marv_i64 a, marv_i64 b)   { return a == b ? 1 : 0; }
static inline marv_i32 marv_i64_ne(marv_i64 a, marv_i64 b)   { return a != b ? 1 : 0; }
static inline marv_i32 marv_i64_lt_s(marv_i64 a, marv_i64 b) { return a <  b ? 1 : 0; }
static inline marv_i32 marv_i64_lt_u(marv_i64 a, marv_i64 b) { return (marv_u64)a <  (marv_u64)b ? 1 : 0; }
static inline marv_i32 marv_i64_gt_s(marv_i64 a, marv_i64 b) { return a >  b ? 1 : 0; }
static inline marv_i32 marv_i64_gt_u(marv_i64 a, marv_i64 b) { return (marv_u64)a >  (marv_u64)b ? 1 : 0; }
static inline marv_i32 marv_i64_le_s(marv_i64 a, marv_i64 b) { return a <= b ? 1 : 0; }
static inline marv_i32 marv_i64_le_u(marv_i64 a, marv_i64 b) { return (marv_u64)a <= (marv_u64)b ? 1 : 0; }
static inline marv_i32 marv_i64_ge_s(marv_i64 a, marv_i64 b) { return a >= b ? 1 : 0; }
static inline marv_i32 marv_i64_ge_u(marv_i64 a, marv_i64 b) { return (marv_u64)a >= (marv_u64)b ? 1 : 0; }

static inline marv_f32 marv_f32_add(marv_f32 a, marv_f32 b) { return a + b; }
static inline marv_f32 marv_f32_sub(marv_f32 a, marv_f32 b) { return a - b; }
static inline marv_f32 marv_f32_mul(marv_f32 a, marv_f32 b) { return a * b; }
static inline marv_f32 marv_f32_div(marv_f32 a, marv_f32 b) { return a / b; }
static inline marv_f32 marv_f32_sqrt(marv_f32 a)            { return sqrtf(a); }
static inline marv_f32 marv_f32_abs(marv_f32 a)             { return fabsf(a); }
static inline marv_f32 marv_f32_neg(marv_f32 a)             { return -a; }
static inline marv_f32 marv_f32_copysign(marv_f32 a, marv_f32 b) { return copysignf(a, b); }
static inline marv_f32 marv_f32_ceil(marv_f32 a)            { return ceilf(a); }
static inline marv_f32 marv_f32_floor(marv_f32 a)           { return floorf(a); }
static inline marv_f32 marv_f32_trunc(marv_f32 a)           { return truncf(a); }
static inline marv_f32 marv_f32_nearest(marv_f32 a)         { return rintf(a); }
static inline marv_f32 marv_f32_min(marv_f32 a, marv_f32 b) { return fminf(a, b); }
static inline marv_f32 marv_f32_max(marv_f32 a, marv_f32 b) { return fmaxf(a, b); }

static inline marv_i32 marv_f32_eq(marv_f32 a, marv_f32 b) { return a == b ? 1 : 0; }
static inline marv_i32 marv_f32_ne(marv_f32 a, marv_f32 b) { return a != b ? 1 : 0; }
static inline marv_i32 marv_f32_lt(marv_f32 a, marv_f32 b) { return a <  b ? 1 : 0; }
static inline marv_i32 marv_f32_gt(marv_f32 a, marv_f32 b) { return a >  b ? 1 : 0; }
static inline marv_i32 marv_f32_le(marv_f32 a, marv_f32 b) { return a <= b ? 1 : 0; }
static inline marv_i32 marv_f32_ge(marv_f32 a, marv_f32 b) { return a >= b ? 1 : 0; }

static inline marv_f64 marv_f64_add(marv_f64 a, marv_f64 b) { return a + b; }
static inline marv_f64 marv_f64_sub(marv_f64 a, marv_f64 b) { return a - b; }
static inline marv_f64 marv_f64_mul(marv_f64 a, marv_f64 b) { return a * b; }
static inline marv_f64 marv_f64_div(marv_f64 a, marv_f64 b) { return a / b; }
static inline marv_f64 marv_f64_sqrt(marv_f64 a)            { return sqrt(a); }
static inline marv_f64 marv_f64_abs(marv_f64 a)             { return fabs(a); }
static inline marv_f64 marv_f64_neg(marv_f64 a)             { return -a; }
static inline marv_f64 marv_f64_copysign(marv_f64 a, marv_f64 b) { return copysign(a, b); }
static inline marv_f64 marv_f64_ceil(marv_f64 a)            { return ceil(a); }
static inline marv_f64 marv_f64_floor(marv_f64 a)           { return floor(a); }
static inline marv_f64 marv_f64_trunc(marv_f64 a)           { return trunc(a); }
static inline marv_f64 marv_f64_nearest(marv_f64 a)         { return rint(a); }
static inline marv_f64 marv_f64_min(marv_f64 a, marv_f64 b) { return fmin(a, b); }
static inline marv_f64 marv_f64_max(marv_f64 a, marv_f64 b) { return fmax(a, b); }

static inline marv_i32 marv_f64_eq(marv_f64 a, marv_f64 b) { return a == b ? 1 : 0; }
static inline marv_i32 marv_f64_ne(marv_f64 a, marv_f64 b) { return a != b ? 1 : 0; }
static inline marv_i32 marv_f64_lt(marv_f64 a, marv_f64 b) { return a <  b ? 1 : 0; }
static inline marv_i32 marv_f64_gt(marv_f64 a, marv_f64 b) { return a >  b ? 1 : 0; }
static inline marv_i32 marv_f64_le(marv_f64 a, marv_f64 b) { return a <= b ? 1 : 0; }
static inline marv_i32 marv_f64_ge(marv_f64 a, marv_f64 b) { return a >= b ? 1 : 0; }

static inline marv_i32 marv_i32_wrap_i64(marv_i64 a)       { return (marv_i32)(marv_u32)(marv_u64)a; }
static inline marv_i64 marv_i64_extend_i32_s(marv_i32 a)   { return (marv_i64)a; }
static inline marv_i64 marv_i64_extend_i32_u(marv_i32 a)   { return (marv_i64)(marv_u32)a; }
static inline marv_i32 marv_i32_extend8_s(marv_i32 a)      { return (marv_i32)(int8_t)a; }
static inline marv_i32 marv_i32_extend16_s(marv_i32 a)     { return (marv_i32)(int16_t)a; }
static inline marv_i64 marv_i64_extend8_s(marv_i64 a)      { return (marv_i64)(int8_t)a; }
static inline marv_i64 marv_i64_extend16_s(marv_i64 a)     { return (marv_i64)(int16_t)a; }
static inline marv_i64 marv_i64_extend32_s(marv_i64 a)     { return (marv_i64)(int32_t)a; }

static inline marv_i32 marv_i32_trunc_f32_s(marv_f32 a)    { return (marv_i32)a; }
static inline marv_i32 marv_i32_trunc_f32_u(marv_f32 a)    { return (marv_i32)(marv_u32)a; }
static inline marv_i32 marv_i32_trunc_f64_s(marv_f64 a)    { return (marv_i32)a; }
static inline marv_i32 marv_i32_trunc_f64_u(marv_f64 a)    { return (marv_i32)(marv_u32)a; }
static inline marv_i64 marv_i64_trunc_f32_s(marv_f32 a)    { return (marv_i64)a; }
static inline marv_i64 marv_i64_trunc_f32_u(marv_f32 a)    { return (marv_i64)(marv_u64)a; }
static inline marv_i64 marv_i64_trunc_f64_s(marv_f64 a)    { return (marv_i64)a; }
static inline marv_i64 marv_i64_trunc_f64_u(marv_f64 a)    { return (marv_i64)(marv_u64)a; }

static inline marv_f32 marv_f32_convert_i32_s(marv_i32 a)  { return (marv_f32)a; }
static inline marv_f32 marv_f32_convert_i32_u(marv_i32 a)  { return (marv_f32)(marv_u32)a; }
static inline marv_f32 marv_f32_convert_i64_s(marv_i64 a)  { return (marv_f32)a; }
static inline marv_f32 marv_f32_convert_i64_u(marv_i64 a)  { return (marv_f32)(marv_u64)a; }
static inline marv_f32 marv_f32_demote_f64(marv_f64 a)     { return (marv_f32)a; }

static inline marv_f64 marv_f64_convert_i32_s(marv_i32 a)  { return (marv_f64)a; }
static inline marv_f64 marv_f64_convert_i32_u(marv_i32 a)  { return (marv_f64)(marv_u32)a; }
static inline marv_f64 marv_f64_convert_i64_s(marv_i64 a)  { return (marv_f64)a; }
static inline marv_f64 marv_f64_convert_i64_u(marv_i64 a)  { return (marv_f64)(marv_u64)a; }
static inline marv_f64 marv_f64_promote_f32(marv_f32 a)    { return (marv_f64)a; }

static inline marv_i32 marv_i32_reinterpret_f32(marv_f32 a) {
  marv_i32 r; memcpy(&r, &a, 4); return r;
}
static inline marv_i64 marv_i64_reinterpret_f64(marv_f64 a) {
  marv_i64 r; memcpy(&r, &a, 8); return r;
}
static inline marv_f32 marv_f32_reinterpret_i32(marv_i32 a) {
  marv_f32 r; memcpy(&r, &a, 4); return r;
}
static inline marv_f64 marv_f64_reinterpret_i64(marv_i64 a) {
  marv_f64 r; memcpy(&r, &a, 8); return r;
}

#endif

#endif /* MARV_H_ */

")

(define (emit-c99 mod port)
  (let* ((mname (mod-name mod))
         (imports (mod-imports mod))
         (globals (mod-globals mod))
         (datas (mod-datas mod))
         (funcs (mod-funcs mod))
         (fsigs (build-func-sig-table mod)))

    (define (resolve-c-func-sym sym)
      (let ((entry (lookup-sym-entry sym fsigs)))
        (if entry
            (let ((imod (list-ref entry 3))
                  (iname (list-ref entry 4)))
              (if imod
                  (import-c-sym imod iname)
                  (if iname
                      (if (string=? iname "main")
                          "main"
                          (import-c-sym (strip-dollar mname) iname))
                      (asm-ident mname (list-ref entry 0)))))
            (asm-ident mname sym))))

    (define (resolve-c-data-sym sym)
      (let iloop ((imps imports))
        (cond
          ((null? imps)
           (let dloop ((ds datas))
             (cond
               ((null? ds) (asm-ident mname sym))
               ((and (sym-matches-import? sym (list-ref (car ds) 1) "")
                     (list-ref (car ds) 2))
                (import-c-sym (strip-dollar mname) (list-ref (car ds) 2)))
               (else (dloop (cdr ds))))))
          ((and (eq? (caar imps) 'import-data)
                (sym-matches-import? sym (list-ref (car imps) 3) (list-ref (car imps) 2)))
           (import-c-sym (list-ref (car imps) 1) (list-ref (car imps) 2)))
          (else (iloop (cdr imps))))))

    (define (resolve-c-global-sym sym)
      (let iloop ((imps imports))
        (cond
          ((null? imps)
           (let gloop ((gs globals))
             (cond
               ((null? gs) (asm-ident mname sym))
               ((and (sym-matches-import? sym (list-ref (car gs) 1) "")
                     (list-ref (car gs) 2))
                (import-c-sym (strip-dollar mname) (list-ref (car gs) 2)))
               (else (gloop (cdr gs))))))
          ((and (eq? (caar imps) 'import-global)
                (sym-matches-import? sym (list-ref (car imps) 3) (list-ref (car imps) 2)))
           (import-c-sym (list-ref (car imps) 1) (list-ref (car imps) 2)))
          (else (iloop (cdr imps))))))

    (fprintf port "/* Generated by MARV Compiler (Restricted C99 Target) */~%")
    ;; Trampoline resume state is program-global (one definition per
    ;; program, owned by the first link-mode module; -c output never
    ;; owns it and refers to the extern declarations in the header).
    ;; The driver (a.sf) sets the bit above before compiling the
    ;; first .mvs of a link; reset it here, after marking one module.
    (display *marv-c99-header* port)

    ;; Trampoline resume state definitions (declared in the header).
    ;; Emitted as module-global-shaped definitions so split output
    ;; shares them like globals: definition in piece 0, extern
    ;; declarations in the other pieces.
    ;; Trampoline fuel definition (declared in the header): one per
    ;; program: one definition per program is kept by the link-mode
    ;; driver (module 0); marv_tc_state is zero-initialised (BSS).
    (fprintf port "#ifdef MARV_TRAMPOLINE~%")
    (fprintf port "marv_i64 marv_tc_fuel = MARV_TC_FUEL;~%")
    (fprintf port "marv_i64 marv_tc_state[16];~%")
    (fprintf port "#endif~%")

    ;; 1. Imported declarations (all follow MARV-ABI `<mod>_<name>` convention)
    (for-each
      (lambda (imp)
        (case (car imp)
          ((import-func)
           (let ((imod   (list-ref imp 1))
                 (iname  (list-ref imp 2))
                 (ptypes (list-ref imp 4))
                 (rtype  (list-ref imp 5)))
             (fprintf port "extern ~a ~a(" (c99-type rtype) (import-c-sym imod iname))
             (if (null? ptypes)
                 (fprintf port "void")
                 (let ploop ((ps ptypes) (first? #t))
                   (when (pair? ps)
                     (when (not first?) (fprintf port ", "))
                     (fprintf port "~a" (c99-type (car ps)))
                     (ploop (cdr ps) #f))))
             (fprintf port ");~%")))
          ((import-global)
           (let ((imod  (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (vtype (list-ref imp 5)))
             (fprintf port "extern ~a ~a;~%" (c99-type vtype) (import-c-sym imod iname))))
          ((import-data)
           (let ((imod  (list-ref imp 1))
                 (iname (list-ref imp 2))
                 (sz    (list-ref imp 6)))
             (fprintf port "extern uint8_t ~a[];~%" (import-c-sym imod iname))))))
      imports)

    ;; 2. Forward declarations for module functions
    (for-each
      (lambda (fn)
        (let* ((fname (func-name fn))
               (exp (func-export fn))
               (params (func-params fn))
               (rtype (func-result fn))
               (cname (c99-func-cname mname fname exp)))
          (when (not exp) (fprintf port "static "))
          (fprintf port "~a ~a(" (c99-type rtype) cname)
          (if (null? params)
              (fprintf port "void")
              (let ploop ((ps params) (first? #t))
                (when (pair? ps)
                  (when (not first?) (fprintf port ", "))
                  (fprintf port "~a ~a" (c99-param-type exp (cdar ps)) (c-ident (caar ps)))
                  (ploop (cdr ps) #f))))
          (fprintf port ");~%")
          (when (and exp (not (string=? exp "main")))
            (let ((bname (c-ident exp)))
              (when (not (string=? bname cname))
                (fprintf port "#if defined(__GNUC__) || defined(__clang__)~%")
                (fprintf port "__attribute__((weak))~%")
                (fprintf port "#endif~%")
                (fprintf port "~a ~a(" (c99-type rtype) bname)
                (if (null? params)
                    (fprintf port "void")
                    (let ploop ((ps params) (first? #t))
                      (when (pair? ps)
                        (when (not first?) (fprintf port ", "))
                        (fprintf port "~a ~a" (c99-param-type exp (cdar ps)) (c-ident (caar ps)))
                        (ploop (cdr ps) #f))))
                (fprintf port ");~%"))))))
      funcs)
    (fprintf port "~%")

    ;; 3. Globals
    (for-each
      (lambda (g)
        (let* ((gname (list-ref g 1))
               (exp (list-ref g 2))
               (vtype (list-ref g 4))
               (init (list-ref g 5))
               (cname (resolve-c-global-sym gname)))
          (when (not exp) (fprintf port "static "))
          (fprintf port "~a ~a = ~a;~%"
                   (c99-type vtype) cname
                   (c99-format-opnd (list 'imm vtype (if (pair? (cdr init)) (cadr init) 0))
                                    resolve-c-data-sym resolve-c-func-sym))))
      globals)

    ;; 4. Static Data Segments
    ;; Each run is a chain link: a union whose _t member holds the
    ;; run's initializer -- numbers as typed scalars, hence stored in
    ;; host-native format on any host -- and whose _b byte view
    ;; continues the segment image for the next link (see c99-data-run
    ;; helpers).  Byte/zero runs stay raw bytes; pointer slots are
    ;; filled by the reloc constructor below.  One segment symbol and
    ;; unchanged byte offsets throughout.
    (for-each
      (lambda (d)
        (let* ((dname (list-ref d 1))
               (exp (list-ref d 2))
               (align (max 8 (list-ref d 4)))
               (bytes (list-ref d 5))
               (cname (resolve-c-data-sym dname)))
          (when (not exp) (fprintf port "static "))
          (if (and (pair? bytes) (every (lambda (b) (= b 0)) bytes))
              ;; all-zero segment: C zero-initializes static storage
              (fprintf port "MARV_ALIGNED(~a) uint8_t ~a[~a];~%"
                       align cname (length bytes))
              (let ((runs (expand-data-runs (list-ref d 7)))
                     (total (length bytes)))
                (define (emit-run-init r bs)
                  (let ((vals (c99-data-run-vals r)))
                    (if vals
                        (let vloop ((vs vals) (f2? #t))
                          (when (pair? vs)
                            (when (not f2?) (fprintf port ","))
                            (fprintf port "~a" (car vs))
                            (vloop (cdr vs) #f)))
                        (let ((nbytes (* (c99-run-count r)
                                         (c99-run-elem-size r))))
                          (let wloop ((m 0) (f2? #t))
                            (when (< m nbytes)
                              (when (not f2?) (fprintf port ","))
                              (fprintf port "~a" (list-ref bs m))
                              (wloop (+ m 1) #f)))))))
                (define (run-size r)
                  (* (c99-run-count r) (c99-run-elem-size r)))
                (fprintf port
                  "MARV_ALIGNED(~a) union { uint8_t _b[MARV_DSZ(~a)]; ~a _t[MARV_DSZ(~a)]; } ~a = {._t={"
                  align
                  (number->string total)
                  (c99-data-run-type (car runs))
                  (c99-run-count (car runs))
                  cname)
                (emit-run-init (car runs) bytes)
                (fprintf port "}};~%")
                (when (pair? (cdr runs))
                  (let rloop ((rs (cdr runs))
                              (bs (list-tail bytes (run-size (car runs))))
                              (off (run-size (car runs)))
                              (k 1))
                    (when (pair? rs)
                      (let ((r (car rs)))
                        (fprintf port
                          "typedef union { uint8_t _b[MARV_DSZ(~a)]; ~a _t[MARV_DSZ(~a)]; } ~a_a~a_t;~%"
                          (if (null? (cdr rs)) "MARV_DS_TAIL"
                              (number->string (- total (+ off (run-size r)))))
                          (c99-data-run-type r) (c99-run-count r) cname k)
                        (fprintf port
                          "static ~a_a~a_t ~a_a~a = {._t={"
                          cname k cname k)
                        (emit-run-init r bs)
                        (fprintf port "}};~%")
                        (fprintf port
                          "static ~a_a~a_t *const ~a_~a = (~a_a~a_t*)((uint8_t*)&~a + ~a);~%"
                          cname k cname k cname k cname off)
                        (rloop (cdr rs) (list-tail bs (run-size r))
                               (+ off (run-size r)) (+ k 1)))))
                  (fprintf port "#if defined(__GNUC__) || defined(__clang__)~%")
                  (fprintf port "__attribute__((constructor))~%")
                  (fprintf port "#endif~%")
                  (fprintf port "static void marv_dinit_~a(void);~%" cname)
                  (fprintf port "static void marv_dinit_~a(void) {~%" cname)
                  (let rloop ((rs (cdr runs)) (k 1))
                    (when (pair? rs)
                      (fprintf port "  memcpy(~a_~a, &~a_a~a, ~a);~%"
                               cname k cname k (run-size (car rs)))
                      (rloop (cdr rs) (+ k 1))))
                  (fprintf port "}~%")))))
)
      datas)

    ;; If any data segment has pointer relocations (ref.data / ref.func),
    ;; emit a constructor that initializes the 8-byte pointer slots at startup!
    (let ((has-relocs? (any (lambda (d) (pair? (list-ref d 6))) datas)))
      (when has-relocs?
        (fprintf port "#if defined(__GNUC__) || defined(__clang__)~%")
        (fprintf port "__attribute__((constructor))~%")
        (fprintf port "#endif~%")
        (fprintf port "static void _marv_init_data_relocs(void) {~%")
        ;; multi-run segments first: their anchors must be in place
        ;; before any pointer slot (possibly inside one) is stored
        (for-each
          (lambda (d)
            (when (pair? (cdr (expand-data-runs (list-ref d 7))))
              (fprintf port "  marv_dinit_~a();~%"
                       (resolve-c-data-sym (list-ref d 1)))))
          datas)
        (for-each
          (lambda (d)
            (let ((dname (resolve-c-data-sym (list-ref d 1)))
                  (relocs (list-ref d 6)))
              (for-each
                (lambda (rel)
                  (let ((rkind (list-ref rel 0))
                        (rsym (list-ref rel 1))
                        (soff (list-ref rel 2))
                        (boff (list-ref rel 3)))
                    (if (eq? rkind 'ref.func)
                        (fprintf port "  marv_ptr_store((marv_ptr)(void *)&~a, ~a, (marv_ptr)&~a);~%"
                                 dname boff (resolve-c-func-sym rsym))
                        (fprintf port "  marv_ptr_store((marv_ptr)(void *)&~a, ~a, marv_ptr_add((marv_ptr)(void *)&~a, ~a));~%"
                                 dname boff (resolve-c-data-sym rsym) soff))))
                relocs)))
          datas)
        (fprintf port "}~%")))
    (fprintf port "~%")

    ;; 5. Function Definitions
    ;; unstack one function at a time and emit it immediately, so
    ;; no function IR outlives its own emission (with 2,600+ funcs,
    ;; holding every IR at once dominates MARV's own memory use)
    (for-each
      (lambda (fn)
        (emit-c99-func mname (unstack-func mod fn) resolve-c-data-sym resolve-c-func-sym resolve-c-global-sym port))
      funcs)))

(define (c99-format-opnd opnd resolve-c-data-sym resolve-c-func-sym)
  (case (opnd-kind opnd)
    ((imm)
     (let ((t (cadr opnd))
           (v (caddr opnd)))
       (case t
         ((ptr) "((marv_ptr)0)")
         ((i32) (if (= v -2147483648) "(-2147483647 - 1)" (number->string v)))
         ((i64) (string-append (number->string v) "LL"))
         ((f32) (string-append "((marv_f32)" (flonum->hex-literal v) ")"))
         ((f64) (flonum->hex-literal v)))))
    ((frame-addr)
     (let ((off (cadr opnd)))
       (if (= off 0)
           "((marv_ptr)_frame)"
           (format #f "((marv_ptr)(_frame + ~a))" off))))
    ((ref-data)
     (let ((sym (cadr opnd))
           (off (caddr opnd)))
       (if (= off 0)
           (format #f "((marv_ptr)(void *)&~a)" (resolve-c-data-sym sym))
           (format #f "marv_ptr_add((marv_ptr)(void *)&~a, ~a)" (resolve-c-data-sym sym) off))))
    ((ref-func)
     (format #f "((marv_ptr)&~a)" (resolve-c-func-sym (cadr opnd))))
    ((vreg)
     (c-ident (vreg-name opnd)))))

(define (c99-op-fn op)
  (string-append "marv_" (c-ident op)))

(define (emit-c99-func mname irf resolve-c-data-sym resolve-c-func-sym resolve-c-global-sym port)
  (let* ((fname (ir-func-name irf))
         (exp (ir-func-export irf))
         (params (ir-func-params irf))
         (rtype (ir-func-result irf))
         (frame-sz (ir-func-frame-size irf))
         (vregs (ir-func-vregs irf))
         (non-param-vregs (drop-n (length params) vregs))
         (instrs (ir-func-instrs irf))
         (cname (c99-func-cname mname fname exp)))

    (define (fmt opnd)
      (c99-format-opnd opnd resolve-c-data-sym resolve-c-func-sym))

    (when (not exp) (fprintf port "static "))
    (fprintf port "~a ~a(" (c99-type rtype) cname)
    (if (null? params)
        (fprintf port "void")
        (let ploop ((ps params) (first? #t))
          (when (pair? ps)
            (when (not first?) (fprintf port ", "))
            (fprintf port "MARV_UNUSED ~a ~a" (c99-param-type exp (cdar ps)) (c-ident (caar ps)))
            (ploop (cdr ps) #f))))
    (fprintf port ") {~%")

    ;; Declare all local variables & temporaries (zero-initialized per WASM/MARV semantics)
    (for-each
      (lambda (vr)
        (let ((vname (c-ident (car vr)))
              (vt (cdr vr)))
          (fprintf port "  MARV_UNUSED ~a ~a = ~a;~%"
                   (c99-type vt) vname
                   (if (eq? vt 'ptr) "((marv_ptr)0)" "0"))))
      non-param-vregs)

    (let ((has-tail-calls? (any (lambda (ins)
                                  (memq (car ins) '(return-call return-call-indirect)))
                                instrs))
          (deferred-tails '())
          (tail-idx 0))

      ;; Trampoline fuel check (Skint-style bounded C stack): in
      ;; MARV_TRAMPOLINE mode a chained transfer whose fuel is
      ;; exhausted stashes its target and arguments in the marv_tc_*
      ;; shadows and returns, unwinding to the driver loop at main.
      ;; Only chain-signature transfers (chain-sig?, the converters'
      ;; unit shapes) can be resumed this way; other tail calls keep
      ;; the plain heuristic shape in every mode.
      ;; chain signatures the trampoline can resume: the two
      ;; converters' unit shapes.  Arguments are stashed by position
      ;; into the generic marv_tc_state slots (see the header), so a
      ;; resume needs nothing but the callee signature, dispatched
      ;; on the argument count in the driver.
      (define (chain-sig? types)
        (or (equal? types '(i64 i64 i64 i64 ptr ptr i32))
            (equal? types '(ptr i64 ptr ptr i32))))

      (define (emit-c99-fuel-check fn-expr arg-types args)
        (fprintf port "#ifdef MARV_TRAMPOLINE~%")
        (fprintf port
          "  if (--marv_tc_fuel <= 0) { marv_tc_state[0] = (marv_i64)(uintptr_t)(~a); "
          fn-expr)
        (let sloop ((ts arg-types) (as args) (k 3))
          (when (pair? as)
            (fprintf port "marv_tc_state[~a] = ~a; " k
                     (if (eq? (car ts) 'ptr)
                         (format #f "(marv_i64)(uintptr_t)(~a)"
                                 (fmt (car as)))
                         (format #f "(marv_i64)(~a)" (fmt (car as)))))
            (sloop (cdr ts) (cdr as) (+ k 1))))
        (fprintf port
          "marv_tc_state[2] = ~a; marv_tc_state[1] = 1; return ((marv_ptr)0); }~%"
          (length args))
        (fprintf port "#endif~%"))

      (define (emit-c99-tail-stmt ins)
        (case (car ins)
          ((return-call)
           (let ((rt (cadr ins))
                 (callee (caddr ins))
                 (args (cadddr ins)))
             (when (and rt (eq? rt 'ptr)
                        (chain-sig? (map cdr params))
                        (= (length args) (length params)))
               (emit-c99-fuel-check
                 (format #f "(marv_ptr)(void *)&~a"
                         (resolve-c-func-sym callee))
                 (map cdr params)
                 args))
             (if rt
                 (fprintf port "  MARV_MUSTTAIL return ~a(" (resolve-c-func-sym callee))
                 (fprintf port "  ~a(" (resolve-c-func-sym callee)))
             (let aloop ((as args) (first? #t))
               (when (pair? as)
                 (when (not first?) (fprintf port ", "))
                 (fprintf port "~a" (fmt (car as)))
                 (aloop (cdr as) #f)))
             (if rt
                 (fprintf port ");~%")
                 (fprintf port "); return;~%"))))
          ((return-call-indirect)
           (let ((ptypes (cadr ins))
                 (rt (caddr ins))
                 (callee-opnd (cadddr ins))
                 (args (list-ref ins 4)))
             (when (and rt (eq? rt 'ptr) (chain-sig? ptypes)
                        (= (length args) (length ptypes)))
               (emit-c99-fuel-check
                 (format #f "(marv_ptr)(~a)" (fmt callee-opnd))
                 ptypes
                 args))
             (if rt
                 (fprintf port "  MARV_MUSTTAIL return ((~a (*)(" (c99-type rt))
                 (fprintf port "  ((~a (*)(" (c99-type rt)))
             (if (null? ptypes)
                 (fprintf port "void")
                 (let ploop ((ps ptypes) (first? #t))
                   (when (pair? ps)
                     (when (not first?) (fprintf port ", "))
                     (fprintf port "~a" (c99-type (car ps)))
                     (ploop (cdr ps) #f))))
             (fprintf port "))(~a))(" (fmt callee-opnd))
             (let aloop ((as args) (first? #t))
               (when (pair? as)
                 (when (not first?) (fprintf port ", "))
                 (fprintf port "~a" (fmt (car as)))
                 (aloop (cdr as) #f)))
             (if rt
                 (fprintf port ");~%")
                 (fprintf port "); return;~%"))))))

      ;; Static addressable frame buffer (when frame-sz > 0).
      ;; When the function also contains tail calls, scope `_frame` in an inner lexical block
      ;; that closes before the `MARV_MUSTTAIL return` statements so C compilers prove `_frame`'s
      ;; lifetime has ended before the sibling call!
      (when (> frame-sz 0)
        (when has-tail-calls?
          (fprintf port "  {~%"))
        (fprintf port "  MARV_ALIGNED(16) uint8_t _frame[~a];~%" frame-sz))

      ;; Emit 3AC instructions
      (for-each
        (lambda (ins)
          (case (car ins)
            ((mov)
             (fprintf port "  ~a = ~a;~%" (fmt (cadr ins)) (fmt (caddr ins))))
            ((unary)
             (let ((dst (cadr ins))
                   (op (caddr ins))
                   (src (cadddr ins)))
               (fprintf port "  ~a = ~a(~a);~%" (fmt dst) (c99-op-fn op) (fmt src))))
            ((binary)
             (let ((dst (cadr ins))
                   (op (caddr ins))
                   (lhs (cadddr ins))
                   (rhs (list-ref ins 4)))
               (fprintf port "  ~a = ~a(~a, ~a);~%" (fmt dst) (c99-op-fn op) (fmt lhs) (fmt rhs))))
            ((select)
             (let ((dst (cadr ins))
                   (vt (caddr ins))
                   (vf (cadddr ins))
                   (cnd (list-ref ins 4)))
               (fprintf port "  ~a = (~a != 0) ? ~a : ~a;~%" (fmt dst) (fmt cnd) (fmt vt) (fmt vf))))
            ((load)
             (let ((dst (cadr ins))
                   (op (caddr ins))
                   (addr (cadddr ins))
                   (off (list-ref ins 4)))
               (fprintf port "  ~a = ~a(~a, ~a);~%" (fmt dst) (c99-op-fn op) (fmt addr) off)))
            ((store)
             (let ((op (cadr ins))
                   (addr (caddr ins))
                   (off (cadddr ins))
                   (val (list-ref ins 5)))
               (fprintf port "  ~a(~a, ~a, ~a);~%" (c99-op-fn op) (fmt addr) off (fmt val))))
            ((global-get)
             (let ((dst (cadr ins))
                   (gsym (caddr ins)))
               (fprintf port "  ~a = ~a;~%" (fmt dst) (resolve-c-global-sym gsym))))
            ((global-set)
             (let ((gsym (cadr ins))
                   (val (cadddr ins)))
               (fprintf port "  ~a = ~a;~%" (resolve-c-global-sym gsym) (fmt val))))
            ((call)
             (let ((dst (cadr ins))
                   (callee (caddr ins))
                   (args (cadddr ins)))
               (if dst
                   (fprintf port "  ~a = ~a(" (fmt dst) (resolve-c-func-sym callee))
                   (fprintf port "  ~a(" (resolve-c-func-sym callee)))
               (let aloop ((as args) (first? #t))
                 (when (pair? as)
                   (when (not first?) (fprintf port ", "))
                   (fprintf port "~a" (fmt (car as)))
                   (aloop (cdr as) #f)))
               (fprintf port ");~%")))
            ((call-indirect)
             (let ((dst (cadr ins))
                   (ptypes (caddr ins))
                   (rt (cadddr ins))
                   (callee-opnd (list-ref ins 4))
                   (args (list-ref ins 5)))
               (fprintf port "  ")
               (when dst (fprintf port "~a = " (fmt dst)))
               (fprintf port "((~a (*)(" (c99-type rt))
               (if (null? ptypes)
                   (fprintf port "void")
                   (let ploop ((ps ptypes) (first? #t))
                     (when (pair? ps)
                       (when (not first?) (fprintf port ", "))
                       (fprintf port "~a" (c99-type (car ps)))
                       (ploop (cdr ps) #f))))
               (fprintf port "))(~a))(" (fmt callee-opnd))
               (let aloop ((as args) (first? #t))
                 (when (pair? as)
                   (when (not first?) (fprintf port ", "))
                   (fprintf port "~a" (fmt (car as)))
                   (aloop (cdr as) #f)))
               (fprintf port ");~%")))
            ((return-call return-call-indirect)
             (if (> frame-sz 0)
                 (let ((tlbl (format #f "_marv_tail_~a_~a" cname tail-idx)))
                   (set! tail-idx (+ tail-idx 1))
                   (set! deferred-tails (cons (cons tlbl ins) deferred-tails))
                   (fprintf port "  goto ~a;~%" tlbl))
                 (emit-c99-tail-stmt ins)))
            ((label)
             (fprintf port "~a: ;~%" (cadr ins)))
            ((loop-begin)
             (fprintf port "~a: ;~%" (caddr ins)))
            ((loop-end)
             ;; Liveness marker only; no C label needed
             #f)
            ((jump)
             (fprintf port "  goto ~a;~%" (cadr ins)))
            ((br-true)
             (fprintf port "  if (~a != 0) goto ~a;~%" (fmt (cadr ins)) (caddr ins)))
            ((br-false)
             (fprintf port "  if (~a == 0) goto ~a;~%" (fmt (cadr ins)) (caddr ins)))
            ((br-table)
             (let ((idx (cadr ins))
                   (tlbls (caddr ins))
                   (dlbl (cadddr ins)))
               (fprintf port "  switch ((uint32_t)(~a)) {~%" (fmt idx))
               (let tloop ((ls tlbls) (i 0))
                 (when (pair? ls)
                   (fprintf port "    case ~a: goto ~a;~%" i (car ls))
                   (tloop (cdr ls) (+ i 1))))
               (fprintf port "    default: goto ~a;~%  }~%" dlbl)))
            ((return)
             (let ((val (cadr ins)))
               (if val
                   (fprintf port "  return ~a;~%" (fmt val))
                   (fprintf port "  return;~%"))))
            ((unreachable)
             (fprintf port "  __builtin_trap();~%"))))
        instrs)

      (when (and (> frame-sz 0) has-tail-calls?)
        (fprintf port "  }~%")
        (for-each
          (lambda (dt)
            (fprintf port "~a:~%" (car dt))
            (emit-c99-tail-stmt (cdr dt)))
          (reverse deferred-tails))))

    (fprintf port "}~%")
    (when (and exp (not (string=? exp "main")))
      (let ((bname (c-ident exp)))
        (when (not (string=? bname cname))
          (fprintf port "#if defined(__GNUC__) || defined(__clang__)~%")
          (fprintf port "__attribute__((weak))~%")
          (fprintf port "#endif~%")
          (fprintf port "~a ~a(" (c99-type rtype) bname)
          (if (null? params)
              (fprintf port "void")
              (let ploop ((ps params) (first? #t))
                (when (pair? ps)
                  (when (not first?) (fprintf port ", "))
                  (fprintf port "~a ~a" (c99-param-type exp (cdar ps)) (c-ident (caar ps)))
                  (ploop (cdr ps) #f))))
          (fprintf port ") { ")
          (if rtype
              (fprintf port "return ~a(" cname)
              (fprintf port "~a(" cname))
          (let aloop ((ps params) (first? #t))
            (when (pair? ps)
              (when (not first?) (fprintf port ", "))
              (fprintf port "~a" (c-ident (caar ps)))
              (aloop (cdr ps) #f)))
          (if rtype
              (fprintf port "); }~%")
              (fprintf port "); return; }~%")))))
    ;; In trampoline mode the exported main was emitted as
    ;; MARV_MAIN_FN (marv_main_impl); emit the real main as the
    ;; Skint-style driver loop that re-enters the chain whenever a
    ;; fuel-exhausted transfer unwound to it (marv_tc_pending).
    (when (and exp (string=? exp "main"))
      (fprintf port "#ifdef MARV_TRAMPOLINE~%")
      (fprintf port "marv_i32 main(")
      (if (null? params)
          (fprintf port "void")
          (let ploop ((ps params) (first? #t))
            (when (pair? ps)
              (when (not first?) (fprintf port ", "))
              (fprintf port "~a ~a" (c99-param-type exp (cdar ps))
                       (c-ident (caar ps)))
              (ploop (cdr ps) #f))))
      (fprintf port ") {~%")
      (when rtype
        (fprintf port "  ~a _rc;~%" (c99-type rtype)))
      (fprintf port "  marv_tc_fuel = MARV_TC_FUEL;~%")
      (fprintf port "  marv_tc_state[1] = 0;~%")
      (fprintf port "  ")
      (when rtype (fprintf port "_rc = "))
      (fprintf port "MARV_MAIN_FN(")
      (let aloop ((ps params) (first? #t))
        (when (pair? ps)
          (when (not first?) (fprintf port ", "))
          (fprintf port "~a" (c-ident (caar ps)))
          (aloop (cdr ps) #f)))
      (fprintf port ");~%")
      (fprintf port "  while (marv_tc_state[1]) {~%")
      (fprintf port "    marv_tc_state[1] = 0;~%")
      (fprintf port "    marv_tc_fuel = MARV_TC_FUEL;~%")
      (fprintf port "    if (marv_tc_state[2] == 7)~%")
      (fprintf port "      (void)((marv_ptr (*)(marv_i64, marv_i64, marv_i64, marv_i64, marv_ptr, marv_ptr, marv_i32))((marv_ptr)(uintptr_t)marv_tc_state[0]))(marv_tc_state[3], marv_tc_state[4], marv_tc_state[5], marv_tc_state[6], (marv_ptr)(uintptr_t)marv_tc_state[7], (marv_ptr)(uintptr_t)marv_tc_state[8], (marv_i32)marv_tc_state[9]);~%")
      (fprintf port "    else~%")
      (fprintf port "      (void)((marv_ptr (*)(marv_ptr, marv_i64, marv_ptr, marv_ptr, marv_i32))((marv_ptr)(uintptr_t)marv_tc_state[0]))((marv_ptr)(uintptr_t)marv_tc_state[3], marv_tc_state[4], (marv_ptr)(uintptr_t)marv_tc_state[5], (marv_ptr)(uintptr_t)marv_tc_state[6], (marv_i32)marv_tc_state[7]);~%")
      (fprintf port "  }~%")
      (if rtype
          (fprintf port "  return (marv_i32)_rc;~%")
          (fprintf port "  return 0;~%"))
      (fprintf port "}~%")
      (fprintf port "#endif~%"))
    (fprintf port "~%")))

;; =============================================================================
;; 9. Target 3: RV64G Register Allocator & Assembly Emitter (`emit-rv64g`)
;; =============================================================================

;; Extract vreg symbols defined and used by a 3AC instruction: returns (defs . uses)
(define (ir-instr-defs-uses ins)
  (define (opnd-vregs opnd)
    (if (vreg? opnd) (list (vreg-name opnd)) '()))
  (case (car ins)
    ((mov)
     (cons (opnd-vregs (cadr ins))
           (opnd-vregs (caddr ins))))
    ((unary)
     (cons (opnd-vregs (cadr ins))
           (opnd-vregs (cadddr ins))))
    ((binary)
     (cons (opnd-vregs (cadr ins))
           (append (opnd-vregs (cadddr ins))
                   (opnd-vregs (list-ref ins 4)))))
    ((select)
     (cons (opnd-vregs (cadr ins))
           (append (opnd-vregs (caddr ins))
                   (opnd-vregs (cadddr ins))
                   (opnd-vregs (list-ref ins 4)))))
    ((load)
     (cons (opnd-vregs (cadr ins))
           (opnd-vregs (cadddr ins))))
    ((store)
     (cons '()
           (append (opnd-vregs (caddr ins))
                   (opnd-vregs (list-ref ins 5)))))
    ((global-get)
     (cons (opnd-vregs (cadr ins)) '()))
    ((global-set)
     (cons '() (opnd-vregs (cadddr ins))))
    ((call)
     (cons (if (cadr ins) (opnd-vregs (cadr ins)) '())
           (apply append (map opnd-vregs (cadddr ins)))))
    ((call-indirect)
     (cons (if (cadr ins) (opnd-vregs (cadr ins)) '())
           (append (opnd-vregs (list-ref ins 4))
                   (apply append (map opnd-vregs (list-ref ins 5))))))
    ((return-call)
     (cons '()
           (apply append (map opnd-vregs (cadddr ins)))))
    ((return-call-indirect)
     (cons '()
           (append (opnd-vregs (cadddr ins))
                   (apply append (map opnd-vregs (list-ref ins 4))))))
    ((br-true br-false)
     (cons '() (opnd-vregs (cadr ins))))
    ((br-table)
     (cons '() (opnd-vregs (cadr ins))))
    ((return)
     (cons '() (if (cadr ins) (opnd-vregs (cadr ins)) '())))
    (else
     (cons '() '()))))

;; Interval record: #(sym type first-def last-use crosses-call? location)
;; where location becomes either ('reg . "s0") or ('spill . slot-index)
(define (make-interval sym type fdef luse crosses?)
  (vector sym type fdef luse crosses? #f))
(define (iv-sym iv)       (vector-ref iv 0))
(define (iv-type iv)      (vector-ref iv 1))
(define (iv-start iv)     (vector-ref iv 2))
(define (iv-end iv)       (vector-ref iv 3))
(define (iv-crosses? iv)  (vector-ref iv 4))
(define (iv-loc iv)       (vector-ref iv 5))
(define (iv-set-start! iv v)   (vector-set! iv 2 v))
(define (iv-set-end! iv v)     (vector-set! iv 3 v))
(define (iv-set-crosses! iv v) (vector-set! iv 4 v))
(define (iv-set-loc! iv v)     (vector-set! iv 5 v))

;; Insertion sort for intervals by start position
(define (sort-intervals-by-start ivs)
  (let loop ((rest ivs) (sorted '()))
    (if (null? rest)
        sorted
        (let ((x (car rest)))
          (let insert ((s sorted))
            (cond ((null? s) (loop (cdr rest) (list x)))
                  ((<= (iv-start x) (iv-start (car s)))
                   (loop (cdr rest) (cons x s)))
                  (else
                   (let iloop ((prev (list (car s))) (cur (cdr s)))
                     (cond ((null? cur)
                            (loop (cdr rest) (append (reverse prev) (list x))))
                           ((<= (iv-start x) (iv-start (car cur)))
                            (loop (cdr rest) (append (reverse prev) (cons x cur))))
                           (else
                            (iloop (cons (car cur) prev) (cdr cur))))))))))))

;; Compute exact live intervals and perform Split-Pool Linear-Scan Register Allocation
;; Returns: (list alloc-table used-callee-gp used-callee-fp num-spill-slots has-calls?)
(define (allocate-registers-rv64g irf)
  (let* ((params (ir-func-params irf))
         (vregs (ir-func-vregs irf))
         (instrs (ir-func-instrs irf))
         (n-instrs (length instrs))
         ;; Create initial interval table for all vregs
         (iv-alist
           (map (lambda (vr)
                  (let* ((sym (car vr))
                         (vt (cdr vr))
                         (is-param? (assq sym params)))
                    (cons sym (make-interval sym vt
                                             (if is-param? 0 (+ n-instrs 1))
                                             (if is-param? 0 -1)
                                             #f))))
                vregs))
         (loop-starts '())
         (loop-ranges '())
         (call-sites '()))

    ;; 1. Linear scan over instructions to record defs, uses, loops, and call sites
    (let iloop ((rest instrs) (idx 1))
      (when (pair? rest)
        (let* ((ins (car rest))
               (op (car ins))
               (du (ir-instr-defs-uses ins))
               (defs (car du))
               (uses (cdr du)))
          (when (eq? op 'loop-begin)
            (set! loop-starts (cons (cons (cadr ins) idx) loop-starts)))
          (when (eq? op 'loop-end)
            (let ((entry (assq (cadr ins) loop-starts)))
              (when entry
                (set! loop-ranges (cons (cons (cdr entry) idx) loop-ranges)))))
          (when (memq op '(call call-indirect))
            (set! call-sites (cons idx call-sites)))
          ;; Check uses BEFORE defs so a local used before its first def gets start=0
          (for-each
            (lambda (u)
              (let ((iv (cdr (assq u iv-alist))))
                (if (> (iv-start iv) n-instrs)
                    ;; Used before any def: live from function entry (zero-initialized)!
                    (begin
                      (iv-set-start! iv 0)
                      (when (> idx (iv-end iv)) (iv-set-end! iv idx)))
                    (begin
                      (when (< idx (iv-start iv)) (iv-set-start! iv idx))
                      (when (> idx (iv-end iv))   (iv-set-end! iv idx))))))
            uses)
          (for-each
            (lambda (d)
              (let ((iv (cdr (assq d iv-alist))))
                (when (< idx (iv-start iv)) (iv-set-start! iv idx))
                (when (> idx (iv-end iv))   (iv-set-end! iv idx))))
            defs)
          (iloop (cdr rest) (+ idx 1)))))

    ;; 2. Extend live intervals across enclosing structured loops (run twice for nested loops)
    (let pass ((k 0))
      (when (< k 3)
        (for-each
          (lambda (lr)
            (let ((lstart (car lr))
                  (lend (cdr lr)))
              (for-each
                (lambda (pair)
                  (let ((iv (cdr pair)))
                    (when (and (< (iv-start iv) lstart)
                               (>= (iv-end iv) lstart)
                               (< (iv-end iv) lend))
                      (iv-set-end! iv lend))))
                iv-alist)))
          loop-ranges)
        (pass (+ k 1))))

    ;; 3. Classify call-crossing intervals
    (for-each
      (lambda (pair)
        (let ((iv (cdr pair)))
          (when (any (lambda (cs) (and (< (iv-start iv) cs) (> (iv-end iv) cs)))
                     call-sites)
            (iv-set-crosses! iv #t))))
      iv-alist)

    ;; 4. Filter live intervals and run Split-Pool Linear Scan for GP and FP
    (let* ((live-ivs
             (filter (lambda (iv) (>= (iv-end iv) (iv-start iv)))
                     (map cdr iv-alist)))
           (gp-ivs (sort-intervals-by-start
                     (filter (lambda (iv) (int-or-ptr-type? (iv-type iv))) live-ivs)))
           (fp-ivs (sort-intervals-by-start
                     (filter (lambda (iv) (float-type? (iv-type iv))) live-ivs)))
           (spill-count 0)
           (used-s-regs '())
           (used-fs-regs '()))

      (define (alloc-spill-slot!)
        (let ((s spill-count))
          (set! spill-count (+ s 1))
          (cons 'spill s)))

      (define (record-callee-reg! r is-fp?)
        (if is-fp?
            (when (not (member r used-fs-regs))
              (set! used-fs-regs (append used-fs-regs (list r))))
            (when (not (member r used-s-regs))
              (set! used-s-regs (append used-s-regs (list r))))))

      ;; Linear scan over one bank (GP or FP)
      (define (run-linear-scan! ivs caller-pool-init callee-pool-init is-fp?)
        (let ((caller-free caller-pool-init)
              (callee-free callee-pool-init)
              (active '())) ;; list of (iv reg from-callee?)

          (define (expire-old! cur-start)
            (let loop ((act active) (kept '()))
              (if (null? act)
                  (set! active (reverse kept))
                  (let* ((entry (car act))
                         (a-iv (car entry))
                         (a-reg (cadr entry))
                         (from-callee? (caddr entry)))
                    (if (< (iv-end a-iv) cur-start)
                        (begin
                          (if from-callee?
                              (set! callee-free (cons a-reg callee-free))
                              (set! caller-free (cons a-reg caller-free)))
                          (loop (cdr act) kept))
                        (loop (cdr act) (cons entry kept)))))))

          ;; Spill the longest-ending active interval in matching pool (or cur-iv)
          (define (spill-at-interval! cur-iv require-callee?)
            (let ((candidates
                    (if require-callee?
                        (filter (lambda (e) (caddr e)) active)
                        active)))
              (if (null? candidates)
                  (iv-set-loc! cur-iv (alloc-spill-slot!))
                  ;; find candidate with largest iv-end
                  (let find-max ((cs (cdr candidates)) (best (car candidates)))
                    (if (null? cs)
                        (let ((best-iv (car best))
                              (best-reg (cadr best))
                              (best-callee? (caddr best)))
                          (if (> (iv-end best-iv) (iv-end cur-iv))
                              (begin
                                (iv-set-loc! cur-iv (cons 'reg best-reg))
                                (iv-set-loc! best-iv (alloc-spill-slot!))
                                (set! active
                                  (cons (list cur-iv best-reg best-callee?)
                                        (filter (lambda (e) (not (eq? (car e) best-iv))) active))))
                              (iv-set-loc! cur-iv (alloc-spill-slot!))))
                        (if (> (iv-end (caar cs)) (iv-end (car best)))
                            (find-max (cdr cs) (car cs))
                            (find-max (cdr cs) best)))))))

          (for-each
            (lambda (iv)
              (expire-old! (iv-start iv))
              (if (iv-crosses? iv)
                  ;; Must use callee-saved register (or spill)
                  (if (pair? callee-free)
                      (let ((r (car callee-free)))
                        (set! callee-free (cdr callee-free))
                        (record-callee-reg! r is-fp?)
                        (iv-set-loc! iv (cons 'reg r))
                        (set! active (cons (list iv r #t) active)))
                      (spill-at-interval! iv #t))
                  ;; Non-call-crossing: prefer caller-saved register, fallback to callee-saved
                  (cond
                    ((pair? caller-free)
                     (let ((r (car caller-free)))
                       (set! caller-free (cdr caller-free))
                       (iv-set-loc! iv (cons 'reg r))
                       (set! active (cons (list iv r #f) active))))
                    ((pair? callee-free)
                     (let ((r (car callee-free)))
                       (set! callee-free (cdr callee-free))
                       (record-callee-reg! r is-fp?)
                       (iv-set-loc! iv (cons 'reg r))
                       (set! active (cons (list iv r #t) active))))
                    (else
                     (spill-at-interval! iv #f)))))
            ivs)))

      ;; Reserve t5, t6 for scratch reloads/immediates; use t0..t4 plus any a2..a6
      ;; strictly above the function's max incoming/outgoing GP argument arity.
      (let* ((count-gp (lambda (ts)
                         (let lp ((l ts) (c 0))
                           (if (null? l) c
                               (lp (cdr l) (if (float-type? (car l)) c (+ c 1)))))))
             (max-gp-arity
               (let ilp ((is instrs) (mx (count-gp (map cdr params))))
                 (if (null? is)
                     mx
                     (let* ((ins (car is))
                            (cargs (case (car ins)
                                     ((call return-call) (cadddr ins))
                                     ((call-indirect)    (list-ref ins 5))
                                     ((return-call-indirect) (list-ref ins 4))
                                     (else '()))))
                       (ilp (cdr is)
                            (if (null? cargs)
                                mx
                                (max mx (count-gp (map opnd-type cargs)))))))))
             (extra-a-regs
               (let alp ((k (max 2 max-gp-arity)) (acc '()))
                 (if (> k 6)
                     (reverse acc)
                     (alp (+ k 1) (cons (string-append "a" (number->string k)) acc))))))
        (run-linear-scan! gp-ivs
                          (append '("t0" "t1" "t2" "t3" "t4") extra-a-regs)
                          '("s0" "s1" "s2" "s3" "s4" "s5" "s6" "s7" "s8" "s9" "s10" "s11")
                          #f))
      ;; Reserve ft10, ft11 for scratch FP reloads; use ft0..ft9 for caller-saved FP
      (run-linear-scan! fp-ivs
                        '("ft0" "ft1" "ft2" "ft3" "ft4" "ft5" "ft6" "ft7" "ft8" "ft9")
                        '("fs0" "fs1" "fs2" "fs3" "fs4" "fs5" "fs6" "fs7" "fs8" "fs9" "fs10" "fs11")
                        #t)

      ;; Any dead vreg (never used) gets assigned a dummy scratch register "t5" / "ft10"
      (for-each
        (lambda (pair)
          (let ((iv (cdr pair)))
            (when (not (iv-loc iv))
              (iv-set-loc! iv (cons 'reg (if (float-type? (iv-type iv)) "ft10" "t5"))))))
        iv-alist)

      (list (map (lambda (pair) (cons (car pair) (iv-loc (cdr pair)))) iv-alist)
            used-s-regs
            used-fs-regs
            spill-count
            (pair? call-sites)
            (map (lambda (iv) (iv-sym iv))
                 (filter (lambda (iv)
                           (and (not (assq (iv-sym iv) params))
                                (= (iv-start iv) 0)
                                (>= (iv-end iv) 0)))
                         (map cdr iv-alist)))))))

;; Check if an integer fits in RV64G signed 12-bit immediate [-2048, 2047]
(define (rv-simm12? n)
  (and (exact-integer? n) (>= n -2048) (<= n 2047)))

(define (emit-rv64g mod port)
  (let* ((mname (mod-name mod))
         (imports (mod-imports mod))
         (globals (mod-globals mod))
         (datas (mod-datas mod))
         (funcs (mod-funcs mod))
         (fsigs (build-func-sig-table mod)))

    (define (resolve-asm-func-sym sym)
      (let ((entry (lookup-sym-entry sym fsigs)))
        (if entry
            (let ((imod (list-ref entry 3))
                  (iname (list-ref entry 4)))
              (if imod
                  (import-c-sym imod iname)
                  (if iname
                      (if (string=? iname "main")
                          "main"
                          (import-c-sym (strip-dollar mname) iname))
                      (asm-ident mname (list-ref entry 0)))))
            (asm-ident mname sym))))

    (define (resolve-asm-data-sym sym)
      (let iloop ((imps imports))
        (cond
          ((null? imps)
           (let dloop ((ds datas))
             (cond
               ((null? ds) (asm-ident mname sym))
               ((and (sym-matches-import? sym (list-ref (car ds) 1) "")
                     (list-ref (car ds) 2))
                (import-c-sym (strip-dollar mname) (list-ref (car ds) 2)))
               (else (dloop (cdr ds))))))
          ((and (eq? (caar imps) 'import-data)
                (sym-matches-import? sym (list-ref (car imps) 3) (list-ref (car imps) 2)))
           (import-c-sym (list-ref (car imps) 1) (list-ref (car imps) 2)))
          (else (iloop (cdr imps))))))

    (define (resolve-asm-global-sym sym)
      (let iloop ((imps imports))
        (cond
          ((null? imps)
           (let gloop ((gs globals))
             (cond
               ((null? gs) (asm-ident mname sym))
               ((and (sym-matches-import? sym (list-ref (car gs) 1) "")
                     (list-ref (car gs) 2))
                (import-c-sym (strip-dollar mname) (list-ref (car gs) 2)))
               (else (gloop (cdr gs))))))
          ((and (eq? (caar imps) 'import-global)
                (sym-matches-import? sym (list-ref (car imps) 3) (list-ref (car imps) 2)))
           (import-c-sym (list-ref (car imps) 1) (list-ref (car imps) 2)))
          (else (iloop (cdr imps))))))

    (fprintf port "# Generated by MARV Compiler (RV64G LP64D Target)~%")
    (fprintf port "  .option pic~%~%")

    ;; 1. Globals
    (when (pair? globals)
      (fprintf port "  .data~%")
      (for-each
        (lambda (g)
          (let* ((gname (list-ref g 1))
                 (exp (list-ref g 2))
                 (vtype (list-ref g 4))
                 (init (list-ref g 5))
                 (val (if (pair? (cdr init)) (cadr init) 0))
                 (asym (resolve-asm-global-sym gname)))
            (when exp
              (fprintf port "  .globl ~a~%" asym)
              (let ((bname (c-ident exp)))
                (when (not (string=? bname asym))
                  (fprintf port "  .weak ~a~%" bname))))
            (fprintf port "  .p2align 3~%")
            (fprintf port "~a:~%" asym)
            (when exp
              (let ((bname (c-ident exp)))
                (when (not (string=? bname asym))
                  (fprintf port "~a:~%" bname))))
            (case vtype
              ((i32) (fprintf port "  .word ~a~%" val))
              ((i64 ptr) (fprintf port "  .dword ~a~%" val))
              ((f32) (fprintf port "  .word ~a~%" (flonum->ieee32-bits val)))
              ((f64) (fprintf port "  .dword ~a~%" (flonum->ieee64-bits val))))))
        globals)
      (fprintf port "~%"))

    ;; 2. Data Segments (with 8-byte pointer relocations `.dword sym + off`)
    (for-each
      (lambda (d)
        (let* ((dname (list-ref d 1))
               (exp (list-ref d 2))
               (mut (list-ref d 3))
               (align (list-ref d 4))
               (bytes (list-ref d 5))
               (relocs (list-ref d 6))
               (asym (resolve-asm-data-sym dname))
               (p2 (cond ((>= align 16) 4)
                         ((>= align 8)  3)
                         ((>= align 4)  2)
                         ((>= align 2)  1)
                         (else          0))))
          (if (and (eq? mut 'const) (null? relocs))
              (fprintf port "  .section .rodata~%")
              (fprintf port "  .data~%"))
          (when exp
            (fprintf port "  .globl ~a~%" asym)
            (let ((bname (c-ident exp)))
              (when (not (string=? bname asym))
                (fprintf port "  .weak ~a~%" bname))))
          (fprintf port "  .p2align ~a~%" p2)
          (fprintf port "~a:~%" asym)
          (when exp
            (let ((bname (c-ident exp)))
              (when (not (string=? bname asym))
                (fprintf port "~a:~%" bname))))
          (if (and (null? relocs) (every (lambda (b) (= b 0)) bytes) (> (length bytes) 0))
              (fprintf port "  .zero ~a~%" (length bytes))
              ;; Emit bytes, substituting .dword relocation at reloc byte offsets
              (let bloop ((bs bytes) (idx 0))
                (when (pair? bs)
                  (let ((rel (let rfind ((rs relocs))
                               (cond ((null? rs) #f)
                                     ((= (list-ref (car rs) 3) idx) (car rs))
                                     (else (rfind (cdr rs)))))))
                    (if rel
                        (let ((rkind (list-ref rel 0))
                              (rsym (list-ref rel 1))
                              (soff (list-ref rel 2)))
                          (if (eq? rkind 'ref.func)
                              (fprintf port "  .dword ~a~%" (resolve-asm-func-sym rsym))
                              (if (= soff 0)
                                  (fprintf port "  .dword ~a~%" (resolve-asm-data-sym rsym))
                                  (fprintf port "  .dword ~a + ~a~%" (resolve-asm-data-sym rsym) soff)))
                          (bloop (drop-n 8 bs) (+ idx 8)))
                        (begin
                          (fprintf port "  .byte ~a~%" (car bs))
                          (bloop (cdr bs) (+ idx 1))))))))))
      datas)
    (fprintf port "~%")

    ;; 3. Functions
    (fprintf port "  .text~%")
    (for-each
      (lambda (fn)
        (emit-rv64g-func mname (unstack-func mod fn) resolve-asm-data-sym resolve-asm-func-sym resolve-asm-global-sym port))
      funcs)
    (fprintf port "  .section .note.GNU-stack,\"\",@progbits~%")))

(define (emit-rv64g-func mname irf resolve-asm-data-sym resolve-asm-func-sym resolve-asm-global-sym port)
  (let* ((fname (ir-func-name irf))
         (exp (ir-func-export irf))
         (params (ir-func-params irf))
         (rtype (ir-func-result irf))
         (frame-sz (ir-func-frame-size irf))
         (instrs (ir-func-instrs irf))
         (asym (if exp (c-ident exp) (asm-ident mname fname)))
         (ra-info (allocate-registers-rv64g irf))
         (alloc-table (list-ref ra-info 0))
         (used-s (list-ref ra-info 1))
         (used-fs (list-ref ra-info 2))
         (num-spills (list-ref ra-info 3))
         (has-calls? (list-ref ra-info 4))
         (zero-init-syms (list-ref ra-info 5))
         (num-saved (+ (if has-calls? 1 0) (length used-s) (length used-fs)))
         (max-stack-args
           (let ilp ((is instrs) (mx 0))
             (if (null? is)
                 mx
                 (let* ((ins (car is))
                        (cargs (cond ((eq? (car ins) 'call) (cadddr ins))
                                     ((eq? (car ins) 'call-indirect) (list-ref ins 5))
                                     (else '()))))
                   (if (null? cargs)
                       (ilp (cdr is) mx)
                       (let alp ((as cargs) (gi 0) (fi 0) (st 0))
                         (if (null? as)
                             (ilp (cdr is) (max mx st))
                             (if (float-type? (opnd-type (car as)))
                                 (alp (cdr as) gi (+ fi 1) (if (>= fi 8) (+ st 1) st))
                                 (alp (cdr as) (+ gi 1) fi (if (>= gi 8) (+ st 1) st))))))))))
         (frame-base (align-up (* max-stack-args 8) 16))
         (spill-base (+ frame-base frame-sz))
         (save-base (+ spill-base (* num-spills 8)))
         (total-frame (align-up (+ save-base (* num-saved 8)) 16)))

    (define (vreg-loc sym)
      (let ((e (assq sym alloc-table)))
        (if e (cdr e) '(reg . "t5"))))

    (define (spill-offset slot)
      (+ spill-base (* slot 8)))

    ;; Emit load of operand `opnd` into register `scratch-reg` (or return its physical register directly)
    (define (get-opnd-reg opnd scratch-reg)
      (case (opnd-kind opnd)
        ((vreg)
         (let* ((sym (vreg-name opnd))
                (vt (vreg-type opnd))
                (loc (vreg-loc sym)))
           (if (eq? (car loc) 'reg)
               (cdr loc)
               (let ((off (spill-offset (cdr loc))))
                 (case vt
                   ((i32)     (fprintf port "  lw ~a, ~a(sp)~%" scratch-reg off))
                   ((i64 ptr) (fprintf port "  ld ~a, ~a(sp)~%" scratch-reg off))
                   ((f32)     (fprintf port "  flw ~a, ~a(sp)~%" scratch-reg off))
                   ((f64)     (fprintf port "  fld ~a, ~a(sp)~%" scratch-reg off)))
                 scratch-reg))))
        ((imm)
         (let ((vt (cadr opnd))
               (val (caddr opnd)))
           (case vt
             ((i32 i64 ptr)
              (if (= val 0)
                  "zero"
                  (begin
                    (fprintf port "  li ~a, ~a~%" scratch-reg val)
                    scratch-reg)))
             ((f32)
              (let ((bits (flonum->ieee32-bits val)))
                (if (= bits 0)
                    (fprintf port "  fmv.w.x ~a, zero~%" scratch-reg)
                    (begin
                      (fprintf port "  li t6, ~a~%" bits)
                      (fprintf port "  fmv.w.x ~a, t6~%" scratch-reg)))
                scratch-reg))
             ((f64)
              (let ((bits (flonum->ieee64-bits val)))
                (if (= bits 0)
                    (fprintf port "  fmv.d.x ~a, zero~%" scratch-reg)
                    (begin
                      (fprintf port "  li t6, ~a~%" bits)
                      (fprintf port "  fmv.d.x ~a, t6~%" scratch-reg)))
                scratch-reg)))))
        ((frame-addr)
         (let ((off (+ frame-base (cadr opnd))))
           (if (= off 0)
               "sp"
               (begin
                 (if (rv-simm12? off)
                     (fprintf port "  addi ~a, sp, ~a~%" scratch-reg off)
                     (begin
                       (fprintf port "  li ~a, ~a~%" scratch-reg off)
                       (fprintf port "  add ~a, sp, ~a~%" scratch-reg scratch-reg)))
                 scratch-reg))))
        ((ref-data)
         (let ((sym (resolve-asm-data-sym (cadr opnd)))
               (off (caddr opnd)))
           (if (= off 0)
               (fprintf port "  la ~a, ~a~%" scratch-reg sym)
               (fprintf port "  la ~a, ~a + ~a~%" scratch-reg sym off))
           scratch-reg))
        ((ref-func)
         (let ((sym (resolve-asm-func-sym (cadr opnd))))
           (fprintf port "  la ~a, ~a~%" scratch-reg sym)
           scratch-reg))))

    ;; Destination register for `dst-vreg` (returns target register, and a finish thunk if spilled)
    (define ( get-dst-reg dst-vreg scratch-reg)
      (let ((loc (vreg-loc (vreg-name dst-vreg))))
        (if (eq? (car loc) 'reg)
            (cdr loc)
            scratch-reg)))

    (define (commit-dst! dst-vreg computed-reg)
      (let* ((sym (vreg-name dst-vreg))
             (vt (vreg-type dst-vreg))
             (loc (vreg-loc sym)))
        (if (eq? (car loc) 'reg)
            (let ((phys (cdr loc)))
              (when (not (string=? phys computed-reg))
                (case vt
                  ((f32) (fprintf port "  fmv.s ~a, ~a~%" phys computed-reg))
                  ((f64) (fprintf port "  fmv.d ~a, ~a~%" phys computed-reg))
                  (else  (fprintf port "  mv ~a, ~a~%" phys computed-reg)))))
            (let ((off (spill-offset (cdr loc))))
              (case vt
                ((i32)     (fprintf port "  sw ~a, ~a(sp)~%" computed-reg off))
                ((i64 ptr) (fprintf port "  sd ~a, ~a(sp)~%" computed-reg off))
                ((f32)     (fprintf port "  fsw ~a, ~a(sp)~%" computed-reg off))
                ((f64)     (fprintf port "  fsd ~a, ~a(sp)~%" computed-reg off)))))))

    (define (emit-frame-teardown!)
      (when (> total-frame 0)
        (let ((off save-base))
          (when has-calls?
            (fprintf port "  ld ra, ~a(sp)~%" off)
            (set! off (+ off 8)))
          (for-each
            (lambda (sr)
              (fprintf port "  ld ~a, ~a(sp)~%" sr off)
              (set! off (+ off 8)))
            used-s)
          (for-each
            (lambda (fsr)
              (fprintf port "  fld ~a, ~a(sp)~%" fsr off)
              (set! off (+ off 8)))
            used-fs)
          (fprintf port "  addi sp, sp, ~a~%" total-frame))))

    (define (emit-epilogue!)
      (emit-frame-teardown!)
      (fprintf port "  ret~%"))

    (define (emit-call-args! args)
      (let aloop ((as args) (gi 0) (fi 0) (st 0))
        (when (pair? as)
          (let* ((a (car as))
                 (at (opnd-type a)))
            (if (float-type? at)
                (if (< fi 8)
                    (let ((areg (string-append "fa" (number->string fi)))
                          (sreg (get-opnd-reg a "ft11")))
                      (when (not (string=? areg sreg))
                        (if (eq? at 'f32)
                            (fprintf port "  fmv.s ~a, ~a~%" areg sreg)
                            (fprintf port "  fmv.d ~a, ~a~%" areg sreg)))
                      (aloop (cdr as) gi (+ fi 1) st))
                    (let ((sreg (get-opnd-reg a "ft11"))
                          (off (* st 8)))
                      (if (eq? at 'f32)
                          (fprintf port "  fsw ~a, ~a(sp)~%" sreg off)
                          (fprintf port "  fsd ~a, ~a(sp)~%" sreg off))
                      (aloop (cdr as) gi (+ fi 1) (+ st 1))))
                (if (< gi 8)
                    (let ((areg (string-append "a" (number->string gi)))
                          (sreg (get-opnd-reg a "t6")))
                      (when (not (string=? areg sreg))
                        (fprintf port "  mv ~a, ~a~%" areg sreg))
                      (aloop (cdr as) (+ gi 1) fi st))
                    (let ((sreg (get-opnd-reg a "t6"))
                          (off (* st 8)))
                      (fprintf port "  sd ~a, ~a(sp)~%" sreg off)
                      (aloop (cdr as) (+ gi 1) fi (+ st 1)))))))))

    ;; Function header & register allocation comment
    (fprintf port "# -- Function ~a (frame=~a, spills=~a, total_sp=~a, s_regs=~a, fs_regs=~a) --~%"
             asym frame-sz num-spills total-frame used-s used-fs)
    (when exp
      (fprintf port "  .globl ~a~%" asym)
      (when (not (string=? exp "main"))
        (let ((qname (import-c-sym (strip-dollar mname) exp)))
          (when (not (string=? qname asym))
            (fprintf port "  .globl ~a~%" qname)))))
    (fprintf port "  .p2align 2~%")
    (fprintf port "~a:~%" asym)
    (when (and exp (not (string=? exp "main")))
      (let ((qname (import-c-sym (strip-dollar mname) exp)))
        (when (not (string=? qname asym))
          (fprintf port "~a:~%" qname))))

    ;; Prologue
    (when (> total-frame 0)
      (fprintf port "  addi sp, sp, -~a~%" total-frame)
      (let ((off save-base))
        (when has-calls?
          (fprintf port "  sd ra, ~a(sp)~%" off)
          (set! off (+ off 8)))
        (for-each
          (lambda (sr)
            (fprintf port "  sd ~a, ~a(sp)~%" sr off)
            (set! off (+ off 8)))
          used-s)
        (for-each
          (lambda (fsr)
            (fprintf port "  fsd ~a, ~a(sp)~%" fsr off)
            (set! off (+ off 8)))
          used-fs)))

    ;; Move incoming ABI parameter registers (a0..a7 / fa0..fa7 and caller stack slots) into allocated locations
    (let ploop ((ps params) (gi 0) (fi 0) (st 0))
      (when (pair? ps)
        (let* ((p (car ps))
               (psym (car p))
               (pt (cdr p))
               (vreg (list 'vreg psym pt)))
          (if (float-type? pt)
              (if (< fi 8)
                  (let ((areg (string-append "fa" (number->string fi))))
                    (commit-dst! vreg areg)
                    (ploop (cdr ps) gi (+ fi 1) st))
                  (let ((off (+ total-frame (* st 8))))
                    (if (eq? pt 'f32)
                        (fprintf port "  flw ft11, ~a(sp)~%" off)
                        (fprintf port "  fld ft11, ~a(sp)~%" off))
                    (commit-dst! vreg "ft11")
                    (ploop (cdr ps) gi (+ fi 1) (+ st 1))))
              (if (< gi 8)
                  (let ((areg (string-append "a" (number->string gi))))
                    (commit-dst! vreg areg)
                    (ploop (cdr ps) (+ gi 1) fi st))
                  (let ((off (+ total-frame (* st 8))))
                    (if (eq? pt 'i32)
                        (fprintf port "  lw t6, ~a(sp)~%" off)
                        (fprintf port "  ld t6, ~a(sp)~%" off))
                    (commit-dst! vreg "t6")
                    (ploop (cdr ps) (+ gi 1) fi (+ st 1))))))))

    ;; Zero-initialize only non-parameter locals that are used before defined
    (for-each
      (lambda (vr)
        (let ((sym (car vr))
              (vt (cdr vr)))
          (when (memq sym zero-init-syms)
            (let ((vreg (list 'vreg sym vt)))
              (case vt
                ((f32 f64) (commit-dst! vreg "zero"))
                (else      (commit-dst! vreg "zero")))))))
      (ir-func-vregs irf))

    ;; Emit instructions
    (for-each
      (lambda (ins)
        (case (car ins)
          ((mov)
           (let* ((dst (cadr ins))
                  (src (caddr ins))
                  (dt (vreg-type dst))
                  (dreg (get-dst-reg dst (if (float-type? dt) "ft11" "t5"))))
             (if (and (imm? src) (int-or-ptr-type? dt))
                 (begin
                   (fprintf port "  li ~a, ~a~%" dreg (caddr src))
                   (commit-dst! dst dreg))
                 (let ((sreg (get-opnd-reg src (if (float-type? dt) "ft11" "t5"))))
                   (commit-dst! dst sreg)))))

          ((unary)
           (let* ((dst (cadr ins))
                  (op (caddr ins))
                  (src (cadddr ins))
                  (dt (vreg-type dst))
                  (sreg (get-opnd-reg src (if (float-type? (opnd-type src)) "ft10" "t5")))
                  (dreg (get-dst-reg dst (if (float-type? dt) "ft11" "t6"))))
             (emit-rv64g-unary op dreg sreg port)
             (commit-dst! dst dreg)))

          ((binary)
           (let* ((dst (cadr ins))
                  (op (caddr ins))
                  (lhs (cadddr ins))
                  (rhs (list-ref ins 4))
                  (dt (vreg-type dst))
                  (dreg (get-dst-reg dst (if (float-type? dt) "ft11" "t6"))))
             ;; Immediate folding for add/sub/and/or/xor/shl/shr/lt when rhs is simm12
             (if (and (imm? rhs)
                      (rv-simm12? (caddr rhs))
                      (memq op '(i32.add i32.sub i64.add i64.sub ptr.add ptr.add64 ptr.sub
                                 i32.and i32.or i32.xor i64.and i64.or i64.xor
                                 i32.shl i32.shr_s i32.shr_u i64.shl i64.shr_s i64.shr_u
                                 i32.lt_s i32.lt_u i64.lt_s i64.lt_u)))
                 (let ((lreg (get-opnd-reg lhs "t5"))
                       (immv (caddr rhs)))
                   (emit-rv64g-binary-imm op dreg lreg immv port)
                   (commit-dst! dst dreg))
                 (let* ((lreg (get-opnd-reg lhs (if (float-type? (opnd-type lhs)) "ft10" "t5")))
                        (rreg (get-opnd-reg rhs (if (float-type? (opnd-type rhs)) "ft11" "t6"))))
                   (emit-rv64g-binary op dreg lreg rreg port)
                   (commit-dst! dst dreg)))))

          ((select)
           (let* ((dst (cadr ins))
                  (vt (caddr ins))
                  (vf (cadddr ins))
                  (cnd (list-ref ins 4))
                  (dt (vreg-type dst))
                  (creg (get-opnd-reg cnd "t5"))
                  (treg (get-opnd-reg vt (if (float-type? dt) "ft10" "t6")))
                  (freg (get-opnd-reg vf (if (float-type? dt) "ft11" "a7")))
                  (dreg (get-dst-reg dst (if (float-type? dt) "ft11" "t6")))
                  (lbl (c-ident (symbol->string (fresh-label "sel")))))
             (commit-dst! dst treg)
             (fprintf port "  bnez ~a, ~a~%" creg lbl)
             (commit-dst! dst freg)
             (fprintf port "~a:~%" lbl)))

          ((load)
           (let* ((dst (cadr ins))
                  (op (caddr ins))
                  (addr (cadddr ins))
                  (off (if (frame-addr? addr)
                           (+ frame-base (cadr addr) (list-ref ins 4))
                           (list-ref ins 4)))
                  (dt (vreg-type dst))
                  (dreg (get-dst-reg dst (if (float-type? dt) "ft11" "t6")))
                  (breg (if (frame-addr? addr)
                            "sp"
                            (get-opnd-reg addr "t5")))
                  (mnem (rv64g-load-mnem op)))
             (if (rv-simm12? off)
                 (fprintf port "  ~a ~a, ~a(~a)~%" mnem dreg off breg)
                 (begin
                   (fprintf port "  li t6, ~a~%" off)
                   (fprintf port "  add t5, ~a, t6~%" breg)
                   (fprintf port "  ~a ~a, 0(t5)~%" mnem dreg)))
             (commit-dst! dst dreg)))

          ((store)
           (let* ((op (cadr ins))
                  (addr (caddr ins))
                  (off (if (frame-addr? addr)
                           (+ frame-base (cadr addr) (cadddr ins))
                           (cadddr ins)))
                  (val (list-ref ins 5))
                  (vt (opnd-type val))
                  (vreg (get-opnd-reg val (if (float-type? vt) "ft11" "t6")))
                  (breg (if (frame-addr? addr)
                            "sp"
                            (get-opnd-reg addr "t5")))
                  (mnem (rv64g-store-mnem op)))
             (if (rv-simm12? off)
                 (fprintf port "  ~a ~a, ~a(~a)~%" mnem vreg off breg)
                 (begin
                   (fprintf port "  li a7, ~a~%" off)
                   (fprintf port "  add t5, ~a, a7~%" breg)
                   (fprintf port "  ~a ~a, 0(t5)~%" mnem vreg)))))

          ((global-get)
           (let* ((dst (cadr ins))
                  (gsym (resolve-asm-global-sym (caddr ins)))
                  (gt (cadddr ins))
                  (dreg (get-dst-reg dst (if (float-type? gt) "ft11" "t6"))))
             (fprintf port "  la t5, ~a~%" gsym)
             (case gt
               ((i32)     (fprintf port "  lw ~a, 0(t5)~%" dreg))
               ((i64 ptr) (fprintf port "  ld ~a, 0(t5)~%" dreg))
               ((f32)     (fprintf port "  flw ~a, 0(t5)~%" dreg))
               ((f64)     (fprintf port "  fld ~a, 0(t5)~%" dreg)))
             (commit-dst! dst dreg)))

          ((global-set)
           (let* ((gsym (resolve-asm-global-sym (cadr ins)))
                  (gt (caddr ins))
                  (val (cadddr ins))
                  (vreg (get-opnd-reg val (if (float-type? gt) "ft11" "t6"))))
             (fprintf port "  la t5, ~a~%" gsym)
             (case gt
               ((i32)     (fprintf port "  sw ~a, 0(t5)~%" vreg))
               ((i64 ptr) (fprintf port "  sd ~a, 0(t5)~%" vreg))
               ((f32)     (fprintf port "  fsw ~a, 0(t5)~%" vreg))
               ((f64)     (fprintf port "  fsd ~a, 0(t5)~%" vreg)))))

          ((call)
           (let* ((dst (cadr ins))
                  (callee (resolve-asm-func-sym (caddr ins)))
                  (args (cadddr ins)))
             (emit-call-args! args)
             (fprintf port "  call ~a~%" callee)
             (when dst
               (commit-dst! dst (if (float-type? (vreg-type dst)) "fa0" "a0")))))

          ((call-indirect)
           (let* ((dst (cadr ins))
                  (callee-opnd (list-ref ins 4))
                  (args (list-ref ins 5)))
             (emit-call-args! args)
             (let ((creg (get-opnd-reg callee-opnd "t5")))
               (when (not (string=? creg "t5"))
                 (fprintf port "  mv t5, ~a~%" creg)))
             (fprintf port "  jalr ra, 0(t5)~%")
             (when dst
               (commit-dst! dst (if (float-type? (vreg-type dst)) "fa0" "a0")))))

          ((return-call)
           (let* ((callee (resolve-asm-func-sym (caddr ins)))
                  (args (cadddr ins)))
             (emit-call-args! args)
             (emit-frame-teardown!)
             (fprintf port "  tail ~a~%" callee)))

          ((return-call-indirect)
           (let* ((callee-opnd (cadddr ins))
                  (args (list-ref ins 4)))
             (emit-call-args! args)
             (let ((creg (get-opnd-reg callee-opnd "t5")))
               (when (not (string=? creg "t5"))
                 (fprintf port "  mv t5, ~a~%" creg)))
             (emit-frame-teardown!)
             (fprintf port "  jr t5~%")))

          ((label)
           (fprintf port "~a:~%" (cadr ins)))
          ((loop-begin)
           (fprintf port "~a:~%" (caddr ins)))
          ((loop-end)
           (fprintf port "~a:~%" (caddr ins)))
          ((jump)
           (fprintf port "  j ~a~%" (cadr ins)))
          ((br-true)
           (let ((creg (get-opnd-reg (cadr ins) "t5")))
             (fprintf port "  bnez ~a, ~a~%" creg (caddr ins))))
          ((br-false)
           (let ((creg (get-opnd-reg (cadr ins) "t5")))
             (fprintf port "  beqz ~a, ~a~%" creg (caddr ins))))
          ((br-table)
           (let ((ireg (get-opnd-reg (cadr ins) "t5"))
                 (tlbls (caddr ins))
                 (dlbl (cadddr ins)))
             (let tloop ((ls tlbls) (i 0))
               (when (pair? ls)
                 (fprintf port "  li t6, ~a~%" i)
                 (fprintf port "  beq ~a, t6, ~a~%" ireg (car ls))
                 (tloop (cdr ls) (+ i 1))))
             (fprintf port "  j ~a~%" dlbl)))
          ((return)
           (let ((val (cadr ins)))
             (when val
               (let ((vt (opnd-type val)))
                 (if (float-type? vt)
                     (let ((sreg (get-opnd-reg val "fa0")))
                       (when (not (string=? sreg "fa0"))
                         (if (eq? vt 'f32)
                             (fprintf port "  fmv.s fa0, ~a~%" sreg)
                             (fprintf port "  fmv.d fa0, ~a~%" sreg))))
                     (let ((sreg (get-opnd-reg val "a0")))
                       (when (not (string=? sreg "a0"))
                         (fprintf port "  mv a0, ~a~%" sreg))))))
             (emit-epilogue!)))
          ((unreachable)
           (fprintf port "  ebreak~%"))))
      instrs)
    (fprintf port "~%")))

(define (rv64g-load-mnem op)
  (case op
    ((i32.load i64.load32_s)   "lw")
    ((i64.load32_u)            "lwu")
    ((i32.load8_s i64.load8_s) "lb")
    ((i32.load8_u i64.load8_u) "lbu")
    ((i32.load16_s i64.load16_s) "lh")
    ((i32.load16_u i64.load16_u) "lhu")
    ((i64.load ptr.load)       "ld")
    ((f32.load)                "flw")
    ((f64.load)                "fld")
    (else "ld")))

(define (rv64g-store-mnem op)
  (case op
    ((i32.store i64.store32)   "sw")
    ((i32.store8 i64.store8)   "sb")
    ((i32.store16 i64.store16) "sh")
    ((i64.store ptr.store)     "sd")
    ((f32.store)               "fsw")
    ((f64.store)               "fsd")
    (else "sd")))

(define (emit-rv64g-binary-imm op dreg lreg immv port)
  (case op
    ((i32.add)   (fprintf port "  addiw ~a, ~a, ~a~%" dreg lreg immv))
    ((i32.sub)   (fprintf port "  addiw ~a, ~a, ~a~%" dreg lreg (- immv)))
    ((i64.add ptr.add ptr.add64) (fprintf port "  addi ~a, ~a, ~a~%" dreg lreg immv))
    ((i64.sub ptr.sub)           (fprintf port "  addi ~a, ~a, ~a~%" dreg lreg (- immv)))
    ((i32.and i64.and) (fprintf port "  andi ~a, ~a, ~a~%" dreg lreg immv))
    ((i32.or  i64.or)  (fprintf port "  ori ~a, ~a, ~a~%" dreg lreg immv))
    ((i32.xor i64.xor) (fprintf port "  xori ~a, ~a, ~a~%" dreg lreg immv))
    ((i32.shl)   (fprintf port "  slliw ~a, ~a, ~a~%" dreg lreg (modulo immv 32)))
    ((i32.shr_s) (fprintf port "  sraiw ~a, ~a, ~a~%" dreg lreg (modulo immv 32)))
    ((i32.shr_u) (fprintf port "  srliw ~a, ~a, ~a~%" dreg lreg (modulo immv 32)))
    ((i64.shl)   (fprintf port "  slli ~a, ~a, ~a~%" dreg lreg (modulo immv 64)))
    ((i64.shr_s) (fprintf port "  srai ~a, ~a, ~a~%" dreg lreg (modulo immv 64)))
    ((i64.shr_u) (fprintf port "  srli ~a, ~a, ~a~%" dreg lreg (modulo immv 64)))
    ((i32.lt_s i64.lt_s) (fprintf port "  slti ~a, ~a, ~a~%" dreg lreg immv))
    ((i32.lt_u i64.lt_u) (fprintf port "  sltiu ~a, ~a, ~a~%" dreg lreg immv))))

(define (emit-rv64g-binary op dreg lreg rreg port)
  (case op
    ;; i32 arithmetic
    ((i32.add)   (fprintf port "  addw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.sub)   (fprintf port "  subw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.mul)   (fprintf port "  mulw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.div_s) (fprintf port "  divw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.div_u) (fprintf port "  divuw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.rem_s) (fprintf port "  remw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.rem_u) (fprintf port "  remuw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.and)   (fprintf port "  and ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.or)    (fprintf port "  or ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.xor)   (fprintf port "  xor ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.shl)   (fprintf port "  sllw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.shr_s) (fprintf port "  sraw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.shr_u) (fprintf port "  srlw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.rotl)
     (fprintf port "  negw a7, ~a~%" rreg)
     (fprintf port "  srlw a7, ~a, a7~%" lreg)
     (fprintf port "  sllw ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  or ~a, ~a, a7~%" dreg dreg))
    ((i32.rotr)
     (fprintf port "  negw a7, ~a~%" rreg)
     (fprintf port "  sllw a7, ~a, a7~%" lreg)
     (fprintf port "  srlw ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  or ~a, ~a, a7~%" dreg dreg))

    ;; i64 & ptr arithmetic
    ((i64.add ptr.add ptr.add64) (fprintf port "  add ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.sub ptr.sub ptr.diff64) (fprintf port "  sub ~a, ~a, ~a~%" dreg lreg rreg))
    ((ptr.diff)  (fprintf port "  subw ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.mul)   (fprintf port "  mul ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.div_s) (fprintf port "  div ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.div_u) (fprintf port "  divu ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.rem_s) (fprintf port "  rem ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.rem_u) (fprintf port "  remu ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.and)   (fprintf port "  and ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.or)    (fprintf port "  or ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.xor)   (fprintf port "  xor ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.shl)   (fprintf port "  sll ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.shr_s) (fprintf port "  sra ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.shr_u) (fprintf port "  srl ~a, ~a, ~a~%" dreg lreg rreg))
    ((i64.rotl)
     (fprintf port "  neg a7, ~a~%" rreg)
     (fprintf port "  srl a7, ~a, a7~%" lreg)
     (fprintf port "  sll ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  or ~a, ~a, a7~%" dreg dreg))
    ((i64.rotr)
     (fprintf port "  neg a7, ~a~%" rreg)
     (fprintf port "  sll a7, ~a, a7~%" lreg)
     (fprintf port "  srl ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  or ~a, ~a, a7~%" dreg dreg))

    ;; Comparisons (i32, i64, ptr)
    ((i32.eq i64.eq ptr.eq)
     (fprintf port "  sub ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  seqz ~a, ~a~%" dreg dreg))
    ((i32.ne i64.ne ptr.ne)
     (fprintf port "  sub ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  snez ~a, ~a~%" dreg dreg))
    ((i32.lt_s i64.lt_s)
     (fprintf port "  slt ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.lt_u i64.lt_u ptr.lt_u)
     (fprintf port "  sltu ~a, ~a, ~a~%" dreg lreg rreg))
    ((i32.gt_s i64.gt_s)
     (fprintf port "  slt ~a, ~a, ~a~%" dreg rreg lreg))
    ((i32.gt_u i64.gt_u ptr.gt_u)
     (fprintf port "  sltu ~a, ~a, ~a~%" dreg rreg lreg))
    ((i32.le_s i64.le_s)
     (fprintf port "  slt ~a, ~a, ~a~%" dreg rreg lreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))
    ((i32.le_u i64.le_u ptr.le_u)
     (fprintf port "  sltu ~a, ~a, ~a~%" dreg rreg lreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))
    ((i32.ge_s i64.ge_s)
     (fprintf port "  slt ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))
    ((i32.ge_u i64.ge_u ptr.ge_u)
     (fprintf port "  sltu ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))

    ;; f32 operations
    ((f32.add)      (fprintf port "  fadd.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.sub)      (fprintf port "  fsub.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.mul)      (fprintf port "  fmul.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.div)      (fprintf port "  fdiv.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.min)      (fprintf port "  fmin.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.max)      (fprintf port "  fmax.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.copysign) (fprintf port "  fsgnj.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.eq)       (fprintf port "  feq.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.ne)
     (fprintf port "  feq.s ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))
    ((f32.lt)       (fprintf port "  flt.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.gt)       (fprintf port "  flt.s ~a, ~a, ~a~%" dreg rreg lreg))
    ((f32.le)       (fprintf port "  fle.s ~a, ~a, ~a~%" dreg lreg rreg))
    ((f32.ge)       (fprintf port "  fle.s ~a, ~a, ~a~%" dreg rreg lreg))

    ;; f64 operations
    ((f64.add)      (fprintf port "  fadd.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.sub)      (fprintf port "  fsub.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.mul)      (fprintf port "  fmul.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.div)      (fprintf port "  fdiv.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.min)      (fprintf port "  fmin.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.max)      (fprintf port "  fmax.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.copysign) (fprintf port "  fsgnj.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.eq)       (fprintf port "  feq.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.ne)
     (fprintf port "  feq.d ~a, ~a, ~a~%" dreg lreg rreg)
     (fprintf port "  xori ~a, ~a, 1~%" dreg dreg))
    ((f64.lt)       (fprintf port "  flt.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.gt)       (fprintf port "  flt.d ~a, ~a, ~a~%" dreg rreg lreg))
    ((f64.le)       (fprintf port "  fle.d ~a, ~a, ~a~%" dreg lreg rreg))
    ((f64.ge)       (fprintf port "  fle.d ~a, ~a, ~a~%" dreg rreg lreg))
    (else
     (fprintf port "  # unhandled binary ~a ~a, ~a, ~a~%" op dreg lreg rreg))))

(define (emit-rv64g-unary op dreg sreg port)
  (case op
    ((i32.eqz i64.eqz ptr.eqz)
     (fprintf port "  seqz ~a, ~a~%" dreg sreg))
    ((f32.abs)  (fprintf port "  fsgnjx.s ~a, ~a, ~a~%" dreg sreg sreg))
    ((f32.neg)  (fprintf port "  fsgnjn.s ~a, ~a, ~a~%" dreg sreg sreg))
    ((f32.sqrt) (fprintf port "  fsqrt.s ~a, ~a~%" dreg sreg))
    ((f64.abs)  (fprintf port "  fsgnjx.d ~a, ~a, ~a~%" dreg sreg sreg))
    ((f64.neg)  (fprintf port "  fsgnjn.d ~a, ~a, ~a~%" dreg sreg sreg))
    ((f64.sqrt) (fprintf port "  fsqrt.d ~a, ~a~%" dreg sreg))

    ;; Float rounding via fcvt with explicit rounding mode (rtz, rdn, rup, rne)
    ((f64.trunc)
     (fprintf port "  fcvt.l.d a7, ~a, rtz~%" sreg)
     (fprintf port "  fcvt.d.l ~a, a7~%" dreg)
     (fprintf port "  fsgnj.d ~a, ~a, ~a~%" dreg dreg sreg))
    ((f64.floor)
     (fprintf port "  fcvt.l.d a7, ~a, rdn~%" sreg)
     (fprintf port "  fcvt.d.l ~a, a7~%" dreg)
     (fprintf port "  fsgnj.d ~a, ~a, ~a~%" dreg dreg sreg))
    ((f64.ceil)
     (fprintf port "  fcvt.l.d a7, ~a, rup~%" sreg)
     (fprintf port "  fcvt.d.l ~a, a7~%" dreg)
     (fprintf port "  fsgnj.d ~a, ~a, ~a~%" dreg dreg sreg))
    ((f64.nearest)
     (fprintf port "  fcvt.l.d a7, ~a, rne~%" sreg)
     (fprintf port "  fcvt.d.l ~a, a7~%" dreg)
     (fprintf port "  fsgnj.d ~a, ~a, ~a~%" dreg dreg sreg))

    ((f32.trunc)
     (fprintf port "  fcvt.w.s a7, ~a, rtz~%" sreg)
     (fprintf port "  fcvt.s.w ~a, a7~%" dreg)
     (fprintf port "  fsgnj.s ~a, ~a, ~a~%" dreg dreg sreg))
    ((f32.floor)
     (fprintf port "  fcvt.w.s a7, ~a, rdn~%" sreg)
     (fprintf port "  fcvt.s.w ~a, a7~%" dreg)
     (fprintf port "  fsgnj.s ~a, ~a, ~a~%" dreg dreg sreg))
    ((f32.ceil)
     (fprintf port "  fcvt.w.s a7, ~a, rup~%" sreg)
     (fprintf port "  fcvt.s.w ~a, a7~%" dreg)
     (fprintf port "  fsgnj.s ~a, ~a, ~a~%" dreg dreg sreg))
    ((f32.nearest)
     (fprintf port "  fcvt.w.s a7, ~a, rne~%" sreg)
     (fprintf port "  fcvt.s.w ~a, a7~%" dreg)
     (fprintf port "  fsgnj.s ~a, ~a, ~a~%" dreg dreg sreg))

    ;; Integer/pointer conversions
    ((i32.wrap_i64 i64.extend_i32_s i64.extend32_s)
     (fprintf port "  addiw ~a, ~a, 0~%" dreg sreg))
    ((i64.extend_i32_u)
     (fprintf port "  slli ~a, ~a, 32~%" dreg sreg)
     (fprintf port "  srli ~a, ~a, 32~%" dreg dreg))
    ((i32.extend8_s i64.extend8_s)
     (fprintf port "  slli ~a, ~a, 56~%" dreg sreg)
     (fprintf port "  srai ~a, ~a, 56~%" dreg dreg))
    ((i32.extend16_s i64.extend16_s)
     (fprintf port "  slli ~a, ~a, 48~%" dreg sreg)
     (fprintf port "  srai ~a, ~a, 48~%" dreg dreg))
    ((ptr.of_i64 i64.of_ptr)
     (when (not (string=? dreg sreg))
       (fprintf port "  mv ~a, ~a~%" dreg sreg)))

    ;; Float <-> Int conversions
    ((i32.trunc_f32_s)   (fprintf port "  fcvt.w.s ~a, ~a, rtz~%" dreg sreg))
    ((i32.trunc_f32_u)   (fprintf port "  fcvt.wu.s ~a, ~a, rtz~%" dreg sreg))
    ((i32.trunc_f64_s)   (fprintf port "  fcvt.w.d ~a, ~a, rtz~%" dreg sreg))
    ((i32.trunc_f64_u)   (fprintf port "  fcvt.wu.d ~a, ~a, rtz~%" dreg sreg))
    ((i64.trunc_f32_s)   (fprintf port "  fcvt.l.s ~a, ~a, rtz~%" dreg sreg))
    ((i64.trunc_f32_u)   (fprintf port "  fcvt.lu.s ~a, ~a, rtz~%" dreg sreg))
    ((i64.trunc_f64_s)   (fprintf port "  fcvt.l.d ~a, ~a, rtz~%" dreg sreg))
    ((i64.trunc_f64_u)   (fprintf port "  fcvt.lu.d ~a, ~a, rtz~%" dreg sreg))
    ((f32.convert_i32_s) (fprintf port "  fcvt.s.w ~a, ~a~%" dreg sreg))
    ((f32.convert_i32_u) (fprintf port "  fcvt.s.wu ~a, ~a~%" dreg sreg))
    ((f32.convert_i64_s) (fprintf port "  fcvt.s.l ~a, ~a~%" dreg sreg))
    ((f32.convert_i64_u) (fprintf port "  fcvt.s.lu ~a, ~a~%" dreg sreg))
    ((f64.convert_i32_s) (fprintf port "  fcvt.d.w ~a, ~a~%" dreg sreg))
    ((f64.convert_i32_u) (fprintf port "  fcvt.d.wu ~a, ~a~%" dreg sreg))
    ((f64.convert_i64_s) (fprintf port "  fcvt.d.l ~a, ~a~%" dreg sreg))
    ((f64.convert_i64_u) (fprintf port "  fcvt.d.lu ~a, ~a~%" dreg sreg))
    ((f32.demote_f64)    (fprintf port "  fcvt.s.d ~a, ~a~%" dreg sreg))
    ((f64.promote_f32)   (fprintf port "  fcvt.d.s ~a, ~a~%" dreg sreg))

    ;; Bitwise reinterpretations (int <-> fp register moves)
    ((i32.reinterpret_f32) (fprintf port "  fmv.x.w ~a, ~a~%" dreg sreg))
    ((i64.reinterpret_f64) (fprintf port "  fmv.x.d ~a, ~a~%" dreg sreg))
    ((f32.reinterpret_i32) (fprintf port "  fmv.w.x ~a, ~a~%" dreg sreg))
    ((f64.reinterpret_i64) (fprintf port "  fmv.d.x ~a, ~a~%" dreg sreg))

    ;; Base RV64I bit-counting sequences (no Zbb extension required)
    ((i32.popcnt i64.popcnt)
     (let ((l-loop (c-ident (symbol->string (fresh-label "pop_lp"))))
           (l-done (c-ident (symbol->string (fresh-label "pop_dn")))))
       (if (eq? op 'i32.popcnt)
           (begin
             (fprintf port "  slli a7, ~a, 32~%" sreg)
             (fprintf port "  srli a7, a7, 32~%" ))
           (fprintf port "  mv a7, ~a~%" sreg))
       (fprintf port "  li ~a, 0~%" dreg)
       (fprintf port "  beqz a7, ~a~%" l-done)
       (fprintf port "~a:~%" l-loop)
       (fprintf port "  addi t6, a7, -1~%")
       (fprintf port "  and a7, a7, t6~%")
       (fprintf port "  addiw ~a, ~a, 1~%" dreg dreg)
       (fprintf port "  bnez a7, ~a~%" l-loop)
       (fprintf port "~a:~%" l-done)))

    ((i32.clz i64.clz)
     (let ((l-loop (c-ident (symbol->string (fresh-label "clz_lp"))))
           (l-done (c-ident (symbol->string (fresh-label "clz_dn"))))
           (width (if (eq? op 'i32.clz) 32 64)))
       (if (eq? op 'i32.clz)
           (fprintf port "  slli a7, ~a, 32~%" sreg)
           (fprintf port "  mv a7, ~a~%" sreg))
       (fprintf port "  li ~a, ~a~%" dreg width)
       (fprintf port "  beqz a7, ~a~%" l-done)
       (fprintf port "  li ~a, 0~%" dreg)
       (fprintf port "~a:~%" l-loop)
       (fprintf port "  bltz a7, ~a~%" l-done)
       (fprintf port "  slli a7, a7, 1~%")
       (fprintf port "  addiw ~a, ~a, 1~%" dreg dreg)
       (fprintf port "  j ~a~%" l-loop)
       (fprintf port "~a:~%" l-done)))

    ((i32.ctz i64.ctz)
     (let ((l-loop (c-ident (symbol->string (fresh-label "ctz_lp"))))
           (l-done (c-ident (symbol->string (fresh-label "ctz_dn"))))
           (width (if (eq? op 'i32.ctz) 32 64)))
       (if (eq? op 'i32.ctz)
           (begin
             (fprintf port "  slli a7, ~a, 32~%" sreg)
             (fprintf port "  srli a7, a7, 32~%"))
           (fprintf port "  mv a7, ~a~%" sreg))
       (fprintf port "  li ~a, ~a~%" dreg width)
       (fprintf port "  beqz a7, ~a~%" l-done)
       (fprintf port "  li ~a, 0~%" dreg)
       (fprintf port "~a:~%" l-loop)
       (fprintf port "  andi t6, a7, 1~%")
       (fprintf port "  bnez t6, ~a~%" l-done)
       (fprintf port "  srli a7, a7, 1~%")
       (fprintf port "  addiw ~a, ~a, 1~%" dreg dreg)
       (fprintf port "  j ~a~%" l-loop)
       (fprintf port "~a:~%" l-done)))
    (else
     (fprintf port "  # unhandled unary ~a ~a, ~a~%" op dreg sreg))))

;; =============================================================================
;; 10. MARV-ABI `.mvh` Interface Loader, Record & Vararg Lowering, and CLI
;;
;; All modules linked together follow MARV calling conventions:
;;   - No `c-` prefixes (`interface`, `func`, `const`, `record`, `global`, `data`).
;;   - A module described by `foo.bar.mvh` corresponds to `lib/foo/bar.c`.
;;   - Constants are taken from the WASM (WCPL) libraries so zero constant
;;     remapping is needed on `.wo`.
;;   - Variadic functions (`(param ...)`) follow WCPL's `ap$` convention: the
;;     caller fills 16-byte `va_arg_t` slots in its stack frame (with `ap[-1]`
;;     holding `va_count`) and passes `$ap$ : ptr` as the final parameter.
;;   - By-value record returns (`(result (record $R))`) follow WCPL's `rp$`
;;     convention: the caller passes destination pointer `$rp$ : ptr` as the
;;     first parameter and the function returns void.
;; =============================================================================

(define *mstd-target-dirs* '("target/" "targets/"))
(define *mstd-system-dirs* '("lib/" "libs/"))
(define *mstd-local-dirs*  '("./"))

;; Link-mode C splitting: maximum number of function definitions per
;; generated C piece when linking (see marv-split-c-temp below).
;; 0 disables splitting.  Only the driver set!s this (from
;; --split-funcs); -c mode ignores it and always emits one file.
(define *marv-split-funcs* 300)

;; Recorded during the most recent `expand-mvs-forms` call:
(define *last-expanded-sys-mvh-paths* '())
(define *last-expanded-sys-mods*      '())

(define (marv-join-path dir rel)
  (let ((dlen (string-length dir)))
    (cond
      ((or (= dlen 0) (string=? dir ".")) rel)
      ((char=? (string-ref dir (- dlen 1)) #\/)
       (string-append dir rel))
      (else
       (string-append dir "/" rel)))))

(define (marv-has-suffix? str suf)
  (let ((slen (string-length str))
        (xlen (string-length suf)))
    (and (>= slen xlen)
         (string=? (substring str (- slen xlen) slen) suf))))

(define (marv-replace-ext path new-ext)
  (let ((len (string-length path)))
    (let loop ((i (- len 1)))
      (cond
        ((< i 0) (string-append path new-ext))
        ((or (char=? (string-ref path i) #\/)
             (char=? (string-ref path i) #\\))
         (string-append path new-ext))
        ((char=? (string-ref path i) #\.)
         (string-append (substring path 0 i) new-ext))
        (else (loop (- i 1)))))))

(define (mvh-sym->relpaths sym)
  (let* ((s (strip-dollar sym))
         (len (string-length s))
         (slashed
           (let loop ((i 0) (acc '()))
             (if (= i len)
                 (list->string (reverse acc))
                 (let ((c (string-ref s i)))
                   (loop (+ i 1) (cons (if (char=? c #\.) #\/ c) acc)))))))
    (if (string=? s slashed)
        (list (string-append s ".mvh"))
        (list (string-append s ".mvh")
              (string-append slashed ".mvh")))))

(define (find-mvh-file hsym dirs)
  (let ((relpaths (mvh-sym->relpaths hsym)))
    (let dloop ((ds dirs))
      (if (null? ds)
          #f
          (let* ((d (car ds))
                 (found
                   (let rloop ((rs relpaths))
                     (if (null? rs)
                         #f
                         (let ((p (marv-join-path d (car rs))))
                           (if (file-exists? p) p (rloop (cdr rs))))))))
            (if found found (dloop (cdr ds))))))))

;; Given a system `.mvh` file path (e.g. "lib/stdio.mvh" or "lib/math.vec.mvh"),
;; return the corresponding system adapter `.c` file in the same directory.
(define (mvh-path->adapter-c-path mvh-path)
  (let ((c-direct (marv-replace-ext mvh-path ".c")))
    (if (file-exists? c-direct)
        c-direct
        (let* ((dir (marv-dirname mvh-path))
               (base-len (- (string-length mvh-path) 4))
               (dlen (if (string=? dir ".") 0 (+ (string-length dir) 1)))
               (stem (substring mvh-path dlen base-len))
               (slashed
                 (let loop ((i 0) (acc '()))
                   (if (= i (string-length stem))
                       (list->string (reverse acc))
                       (let ((ch (string-ref stem i)))
                         (loop (+ i 1) (cons (if (char=? ch #\.) #\/ ch) acc))))))
               (c-slashed (marv-join-path dir (string-append slashed ".c"))))
          (if (file-exists? c-slashed)
              c-slashed
              c-direct)))))

(define (read-all-sexps filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((acc '()))
        (let ((x (read port)))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(define (sexp-mentions-sym? x sym)
  (cond
    ((eq? x sym) #t)
    ((pair? x) (or (sexp-mentions-sym? (car x) sym)
                   (sexp-mentions-sym? (cdr x) sym)))
    (else #f)))

(define (qualify-mod-sym mod-str sym)
  (let ((raw (strip-dollar sym)))
    (if (string-index raw #\:)
        (string->symbol (string-append "$" raw))
        (string->symbol (string-append "$" mod-str ":" raw)))))

(define (unqualify-sym sym)
  (let* ((raw (strip-dollar sym))
         (idx (string-index raw #\:)))
    (if idx
        (string->symbol (string-append "$" (substring raw (+ idx 1) (string-length raw))))
        (string->symbol (string-append "$" raw)))))

(define (raw-unqualified-name sym)
  (let* ((raw (strip-dollar sym))
         (idx (string-index raw #\:)))
    (if idx
        (substring raw (+ idx 1) (string-length raw))
        raw)))

(define (normalize-marv-vtype t)
  (case t
    ((i8 u8 i16 u16 i32 u32) 'i32)
    ((i64 u64) 'i64)
    ((f32) 'f32)
    ((f64) 'f64)
    ((ptr) 'ptr)
    (else 'ptr)))

(define (record-field-metrics ftype record-env)
  ;; Returns (list mtype size align load-op store-op)
  (cond
    ((symbol? ftype)
     (case ftype
       ((i8)  '(i32 1 1 i32.load8_s  i32.store8))
       ((u8)  '(i32 1 1 i32.load8_u  i32.store8))
       ((i16) '(i32 2 2 i32.load16_s i32.store16))
       ((u16) '(i32 2 2 i32.load16_u i32.store16))
       ((i32 u32) '(i32 4 4 i32.load i32.store))
       ((i64 u64) '(i64 8 8 i64.load i64.store))
       ((f32) '(f32 4 4 f32.load f32.store))
       ((f64) '(f64 8 8 f64.load f64.store))
       ((ptr) '(ptr 8 8 ptr.load ptr.store))
       (else  '(i32 4 4 i32.load i32.store))))
    ((and (pair? ftype) (eq? (car ftype) 'array))
     (let* ((et (cadr ftype))
            (cnt (if (and (pair? (cddr ftype)) (number? (caddr ftype)))
                     (caddr ftype)
                     1))
            (em (record-field-metrics et record-env))
            (esz (list-ref em 1))
            (eal (list-ref em 2)))
       (list 'ptr (* esz cnt) eal #f #f)))
    ((and (pair? ftype) (memq (car ftype) '(record struct)))
     (let* ((rsym (cadr ftype))
            (rentry (assq rsym record-env)))
       (if rentry
           (let ((rinfo (cdr rentry)))
             (list 'ptr (list-ref rinfo 1) (list-ref rinfo 2) #f #f))
           '(ptr 8 8 #f #f))))
    (else '(i32 4 4 i32.load i32.store))))

(define (parse-record-def rform record-env)
  ;; (record $R (field $f1 <type>) ...)
  ;; Returns (list rname total-size max-align fields)
  ;; where each field is (list fname ftype mtype offset size align load-op store-op)
  (let ((rname (cadr rform))
        (clauses (cddr rform)))
    (let loop ((cs clauses) (off 0) (max-al 1) (explicit-sz #f) (explicit-al #f) (flds '()))
      (if (null? cs)
          (let* ((al (or explicit-al max-al))
                 (sz (or explicit-sz (align-up off al))))
            (list rname sz al (reverse flds)))
          (let ((c (car cs)))
            (cond
              ((and (pair? c) (eq? (car c) 'size))
               (loop (cdr cs) off max-al (cadr c) explicit-al flds))
              ((and (pair? c) (eq? (car c) 'align))
               (loop (cdr cs) off max-al explicit-sz (cadr c) flds))
              ((and (pair? c) (eq? (car c) 'field))
               (let* ((fname (cadr c))
                      (ftype (caddr c))
                      (metrics (record-field-metrics ftype record-env))
                      (mtype (list-ref metrics 0))
                      (fsz   (list-ref metrics 1))
                      (fal   (list-ref metrics 2))
                      (lop   (list-ref metrics 3))
                      (sop   (list-ref metrics 4))
                      (foff  (align-up off fal))
                      (fentry (list fname ftype mtype foff fsz fal lop sop)))
                 (loop (cdr cs) (+ foff fsz) (max max-al fal) explicit-sz explicit-al
                       (cons fentry flds))))
              (else
               (loop (cdr cs) off max-al explicit-sz explicit-al flds))))))))

(define (marv-dirname path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path i) #\/)
               (char=? (string-ref path i) #\\))
           (if (= i 0) "/" (substring path 0 i)))
          (else (loop (- i 1))))))

;; Load `.mvh` headers and lower interfaces, records, constants, and varargs
;; into a standard core MARV `(module ...)` S-expression.
(define (expand-mvs-forms forms base-dir)
  (let* ((mod-form
           (let mfind ((fs forms))
             (cond ((null? fs) #f)
                   ((and (pair? (car fs)) (eq? (caar fs) 'module)) (car fs))
                   (else (mfind (cdr fs))))))
         (self-mod-str
           (if (and mod-form (pair? (cdr mod-form)) (symbol? (cadr mod-form)))
               (strip-dollar (cadr mod-form))
               ""))
         (included '())
         (included-sys '())
         (sys-mvh-paths '())
         (sys-mods '())
         (const-env '())    ;; alist: ($SYM . ('scalar type . val)) or ($SYM . ('expr . sexp))
        (macro-env '())    ;; alist: ($SYM . macrodef) -- see register-macro!
         (record-env '())   ;; alist: ($R . (rname size align fields))
         (ifunc-env '())    ;; alist: ($fn . (canon-sym fixed-ptypes rtype variadic? ret-rec imp-sexp))
         (iglobal-env '())  ;; alist: ($g  . (canon-sym vtype imp-sexp))
         (idata-env '())    ;; alist: ($d  . (canon-sym imp-sexp))
         (used-imps '())    ;; list of canonical import symbols referenced by module
         (raw-imps '()))    ;; list of explicit raw `(import ...)` forms from headers

    (define (mark-used-imp! canon-sym)
      (when (not (memq canon-sym used-imps))
        (set! used-imps (cons canon-sym used-imps))))

    (define (register-dual! env-setter mod-str sym val)
      (let ((u (unqualify-sym sym))
            (q (qualify-mod-sym mod-str sym)))
        (env-setter u val)
        (when (not (eq? u q))
          (env-setter q val))))

    (define (add-const! k v)  (set! const-env (cons (cons k v) const-env)))
    (define (add-record! k v) (set! record-env (cons (cons k v) record-env)))
    (define (add-ifunc! k v)  (set! ifunc-env (cons (cons k v) ifunc-env)))
    (define (add-iglobal! k v) (set! iglobal-env (cons (cons k v) iglobal-env)))
    (define (add-idata! k v)  (set! idata-env (cons (cons k v) idata-env)))

    ;; Macros (see register-macro! / expand-func-macros below).
    ;; Redefinition with a different body is an error (no shadowing);
    ;; an identical redefinition (same header reached twice) is a no-op,
    ;; as in C.
    (define (add-macro! k v)
      (let ((old (assq k macro-env)))
        (cond ((not old) (set! macro-env (cons (cons k v) macro-env)))
              ((equal? (cdr old) v) #f)
              (else
               (error "macro redefined with a different body (shadowing is not allowed)" k)))))

    (define (parse-interface-func! item iface-str wo-mod)
      ;; (func $fn (param ...) ... [(result ...)])
      (let ((fsym (cadr item))
            (clauses (cddr item)))
        (let loop ((cs clauses)
                   (fixed-params '())
                   (variadic? #f)
                   (rtype #f)
                   (ret-rec #f))
          (if (null? cs)
              (let* ((raw-name (raw-unqualified-name fsym))
                     (canon-sym (qualify-mod-sym wo-mod fsym))
                     (core-params
                       (append
                         (if ret-rec (list '(param ptr)) '())
                         (map (lambda (pt) (list 'param pt)) (reverse fixed-params))
                         (if variadic? (list '(param ptr)) '())))
                     (core-ptypes
                       (append
                         (if ret-rec '(ptr) '())
                         (reverse fixed-params)
                         (if variadic? '(ptr) '())))
                     (core-ret (if ret-rec #f rtype))
                     (imp-sexp
                       `(import ,wo-mod ,raw-name
                          (func ,canon-sym
                            ,@core-params
                            ,@(if core-ret (list (list 'result core-ret)) '()))))
                     ;; Fixed arity BEFORE the trailing $ap$ parameter:
                     ;; if ret-rec is present, caller passes $rp$ as first fixed arg!
                     (fixed-arity (+ (if ret-rec 1 0) (length fixed-params)))
                     (finfo (list canon-sym core-ptypes core-ret variadic? ret-rec imp-sexp fixed-arity)))
                (add-ifunc! canon-sym finfo)
                (register-dual! add-ifunc! iface-str fsym finfo)
                (when (not (string=? iface-str wo-mod))
                  (register-dual! add-ifunc! wo-mod fsym finfo)))
              (let ((c (car cs)))
                (cond
                  ((and (pair? c) (eq? (car c) 'param))
                   (let ((ps (cdr c)))
                     (cond
                       ((null? ps)
                        (loop (cdr cs) fixed-params variadic? rtype ret-rec))
                       ;; (param ...) variadic marker
                       ((and (= (length ps) 1) (eq? (car ps) '...))
                        (loop (cdr cs) fixed-params #t rtype ret-rec))
                       ;; (param $x <type>)
                       ((and (= (length ps) 2)
                             (symbol? (car ps))
                             (char=? (string-ref (symbol->string (car ps)) 0) #\$))
                        (loop (cdr cs)
                              (cons (normalize-marv-vtype (cadr ps)) fixed-params)
                              variadic? rtype ret-rec))
                       ;; (param t1 t2 ... [...])
                       (else
                        (let plp ((ts ps) (fps fixed-params) (va? variadic?))
                          (cond
                            ((null? ts)
                             (loop (cdr cs) fps va? rtype ret-rec))
                            ((eq? (car ts) '...)
                             (plp (cdr ts) fps #t))
                            (else
                             (plp (cdr ts) (cons (normalize-marv-vtype (car ts)) fps) va?))))))))
                  ((and (pair? c) (eq? (car c) 'result))
                   (if (null? (cdr c))
                       (loop (cdr cs) fixed-params variadic? #f ret-rec)
                       (let ((rt (cadr c)))
                         (if (and (pair? rt) (memq (car rt) '(record struct)))
                             (loop (cdr cs) fixed-params variadic? #f (cadr rt))
                             (loop (cdr cs) fixed-params variadic? (normalize-marv-vtype rt) ret-rec)))))
                  (else
                   (loop (cdr cs) fixed-params variadic? rtype ret-rec))))))))

    (define (mx-distinct? xs)
      (cond ((null? xs) #t)
            ((memq (car xs) (cdr xs)) #f)
            (else (mx-distinct? (cdr xs)))))

    ;; (macro $name (param ...)
    ;;   [(locals ($x type) ...)] [(labels $L ...)]
    ;;   (effect (tin ...) -> (tout ...))
    ;;   template ...)
    ;; Parses, validates, and registers a macro definition in macro-env.
    ;; The (effect ...) clause is required; the declared stack effect is
    ;; checked on every expansion (see mx-check-effect).  A definition is
    ;; stored as (macro name params locals labels eff-in eff-out template)
    ;; with locals an alist (sym . type).  Macro definitions may appear in
    ;; .mvh interfaces and as module items; (include ...) inside an
    ;; interface registers that header's macros too.
    (define (register-macro! item iface-str wo-mod)
      (let ((mname (cadr item))
            (rest (cddr item)))
        (when (not (and (pair? rest) (list? (car rest))))
          (error "malformed (macro ...): missing parameter list" item))
        (let loop ((cs (cdr rest)) (locals '()) (labels '()) (eff #f))
          (if (and (pair? cs) (pair? (car cs))
                   (memq (caar cs) '(locals labels effect)))
              (case (caar cs)
                ((locals)
                 (loop (cdr cs)
                       (append locals
                               (map (lambda (lt)
                                      (cons (car lt)
                                            (normalize-marv-vtype (cadr lt))))
                                    (cdar cs)))
                       labels eff))
                ((labels)
                 (loop (cdr cs) locals (append labels (cdar cs)) eff))
                ((effect)
                 (let ((e (car cs)))
                   (when (not (and (= (length e) 4) (list? (cadr e))
                                   (eq? (caddr e) '->) (list? (cadddr e))))
                     (error "malformed (effect (tin ...) -> (tout ...)) in macro definition"
                            mname e))
                   (loop (cdr cs) locals labels
                         (cons (map normalize-marv-vtype (cadr e))
                               (map normalize-marv-vtype (cadddr e)))))))
              (begin
                (when (not eff)
                  (error "macro definition requires an (effect ...) clause" mname item))
                (when (not (pair? cs))
                  (error "macro definition has no template body" mname item))
                (let* ((params (car rest))
                       (template (if (null? (cdr cs)) (car cs) (cons 'seq cs)))
                       (def (list 'macro mname params locals labels
                                  (car eff) (cdr eff) template))
                       (lnames (map car locals)))
                  (for-each
                    (lambda (p)
                      (when (not (symbol? p))
                        (error "macro parameter is not a symbol" mname p)))
                    params)
                  (when (not (mx-distinct? params))
                    (error "duplicate macro parameter" mname params))
                  (when (not (mx-distinct? (append lnames labels)))
                    (error "duplicate macro local/label name" mname))
                  (for-each
                    (lambda (p)
                      (when (memq p (append lnames labels))
                        (error "macro parameter collides with a declared local/label"
                               mname p)))
                    params)
                  (register-dual! add-macro! iface-str mname def)
                  (when (not (string=? iface-str wo-mod))
                    (register-dual! add-macro! wo-mod mname def))
                  (add-macro! (qualify-mod-sym wo-mod mname) def)))))))

    (define (process-interface-item! item iface-str wo-mod cur-dir)
      (let ((self? (or (string=? iface-str self-mod-str)
                       (string=? wo-mod self-mod-str))))
        (when (pair? item)
          (case (car item)
            ((include)
             (process-include! item cur-dir))
            ((const c-const)
             (let ((cname (cadr item)))
               (if (and (pair? (cdddr item)) (symbol? (caddr item)))
                   ;; (const $NAME <type> <val>)
                   (let ((ctype (normalize-marv-vtype (caddr item)))
                         (cval  (cadddr item)))
                     (register-dual! add-const! iface-str cname (cons 'scalar (cons ctype cval)))
                     (when (not (string=? iface-str wo-mod))
                       (register-dual! add-const! wo-mod cname (cons 'scalar (cons ctype cval)))))
                   ;; (const $NAME <expr>)
                   (let ((cexpr (caddr item)))
                     (register-dual! add-const! iface-str cname (cons 'expr cexpr))
                     (when (not (string=? iface-str wo-mod))
                       (register-dual! add-const! wo-mod cname (cons 'expr cexpr)))))))
            ((record struct)
             (let ((rinfo (parse-record-def item record-env))
                   (rname (cadr item)))
               (register-dual! add-record! iface-str rname rinfo)
               (when (not (string=? iface-str wo-mod))
                 (register-dual! add-record! wo-mod rname rinfo))))
            ((macro)
             (register-macro! item iface-str wo-mod))
            ((func)
             (when (not self?)
               (parse-interface-func! item iface-str wo-mod)))
            ((global)
             (when (not self?)
               (let* ((gsym (cadr item))
                      (tspec (caddr item))
                      (vtype (if (and (pair? tspec) (eq? (car tspec) 'mut))
                                 (normalize-marv-vtype (cadr tspec))
                                 (normalize-marv-vtype tspec)))
                      (raw-name (raw-unqualified-name gsym))
                      (canon-sym (qualify-mod-sym wo-mod gsym))
                      (imp-sexp `(import ,wo-mod ,raw-name (global ,canon-sym ,tspec)))
                      (ginfo (list canon-sym vtype imp-sexp)))
                 (add-iglobal! canon-sym ginfo)
                 (register-dual! add-iglobal! iface-str gsym ginfo)
                 (when (not (string=? iface-str wo-mod))
                   (register-dual! add-iglobal! wo-mod gsym ginfo)))))
            ((data)
             (when (not self?)
               (let* ((dsym (cadr item))
                      (raw-name (raw-unqualified-name dsym))
                      (canon-sym (qualify-mod-sym wo-mod dsym))
                      (imp-sexp `(import ,wo-mod ,raw-name (data ,canon-sym ,@(cddr item))))
                      (dinfo (list canon-sym imp-sexp)))
                 (add-idata! canon-sym dinfo)
                 (register-dual! add-idata! iface-str dsym dinfo)
                 (when (not (string=? iface-str wo-mod))
                   (register-dual! add-idata! wo-mod dsym dinfo)))))
            ((import)
             (when (not self?)
               (set! raw-imps (append raw-imps (list item)))))))))

    (define (process-header-item! item cur-dir)
      (when (pair? item)
        (case (car item)
          ((include)
           (process-include! item cur-dir))
          ((interface mvh-header c-interface)
           (let* ((isym (cadr item))
                  (iface-str (strip-dollar isym))
                  (subitems (cddr item))
                  (wo-mod
                    (let mloop ((ss subitems))
                      (cond
                        ((null? ss) iface-str)
                        ((and (pair? (car ss)) (eq? (caar ss) 'module) (string? (cadar ss)))
                         (cadar ss))
                        (else (mloop (cdr ss)))))))
             (for-each (lambda (sub)
                         (process-interface-item! sub iface-str wo-mod cur-dir))
                       subitems)))
          (else
           (process-interface-item! item "global" "global" cur-dir)))))

    (define (process-include! inc-form cur-dir)
      (let* ((rest (cdr inc-form))
             (sys? (and (= (length rest) 2) (eq? (car rest) 'system)))
             (hsym (if sys? (cadr rest) (car rest)))
             (mname (strip-dollar hsym)))
        (when (not (and sys? (member mname included-sys)))
          (let* ((dirs (if sys?
                           (append *mstd-system-dirs* (list cur-dir))
                           (cons cur-dir *mstd-local-dirs*)))
                 (path (find-mvh-file hsym dirs)))
            (when (not path)
              (error "cannot find .mvh header" hsym dirs))
            (when sys?
              (set! included-sys (cons mname included-sys)))
            (when (not (member path included))
              (set! included (cons path included))
              (let ((hforms (read-all-sexps path))
                    (hdir   (marv-dirname path)))
                (for-each (lambda (hf) (process-header-item! hf hdir)) hforms))
              (when sys?
                (when (not (member path sys-mvh-paths))
                  (set! sys-mvh-paths (cons path sys-mvh-paths)))
                (when (not (member mname sys-mods))
                  (set! sys-mods (cons mname sys-mods)))))))))

    ;; Process all top-level forms before `(module ...)`
    (for-each
      (lambda (f)
        (when (not (and (pair? f) (eq? (car f) 'module)))
          (process-header-item! f base-dir)))
      forms)

    (when (not mod-form)
      (error "missing (module ...) form in .mvs file"))

    ;; First pass over `(module $m ...)`:
    ;;   - Collect any top-level `(record ...)` or `(const ...)` inside the module
    ;;   - Collect module-local `(global ...)` types and `(func ...)` signatures
    ;;     (including whether a module-local function is variadic `(param ...)`)
    (let* ((mname (cadr mod-form))
           (mstr  (strip-dollar mname))
           (mitems (cddr mod-form))
           (mod-globals '())  ;; alist: ($g . vtype)
           (mod-funcs '()))   ;; alist: ($fn . (fixed-ptypes rtype variadic? ret-rec fixed-arity))

      (define (lookup-record rsym)
        (let ((e (assq rsym record-env)))
          (if e (cdr e) (error "unknown record type" rsym))))

      (define (lookup-record-field rsym fsym)
        (let* ((rinfo (lookup-record rsym))
               (flds (list-ref rinfo 3)))
          (let loop ((fs flds))
            (cond
              ((null? fs) (error "unknown field in record" rsym fsym))
              ((eq? (caar fs) fsym) (car fs))
              (else (loop (cdr fs)))))))

      (define (scan-module-items!)
        (for-each
          (lambda (it)
            (when (pair? it)
              (case (car it)
                ((const c-const)
                 (process-interface-item! it mstr mstr base-dir))
                ((macro)
                 (process-interface-item! it mstr mstr base-dir))
                ((record struct)
                 (process-interface-item! it mstr mstr base-dir))
                ((global)
                 (let* ((gsym (cadr it))
                        (rest (cddr it))
                        (tspec (if (and (pair? (car rest)) (eq? (caar rest) 'export))
                                   (cadr rest)
                                   (car rest)))
                        (vtype (if (and (pair? tspec) (eq? (car tspec) 'mut))
                                   (normalize-marv-vtype (cadr tspec))
                                   (normalize-marv-vtype tspec))))
                   (set! mod-globals (cons (cons gsym vtype) mod-globals))))
                ((import)
                 (let ((desc (list-ref it 3)))
                   (when (pair? desc)
                     (case (car desc)
                       ((func)
                        (let ((fsym (cadr desc)))
                          (let plp ((cs (cddr desc)) (pts '()) (va? #f) (rt #f))
                            (if (null? cs)
                                (set! mod-funcs
                                  (cons (cons fsym (list (reverse pts) rt va? #f (length pts)))
                                        mod-funcs))
                                (let ((c (car cs)))
                                  (cond
                                    ((and (pair? c) (eq? (car c) 'param))
                                     (if (and (= (length (cdr c)) 1) (eq? (cadr c) '...))
                                         (plp (cdr cs) pts #t rt)
                                         (plp (cdr cs) (cons (normalize-marv-vtype (car (reverse (cdr c)))) pts) va? rt)))
                                    ((and (pair? c) (eq? (car c) 'result))
                                     (plp (cdr cs) pts va? (if (pair? (cdr c)) (normalize-marv-vtype (cadr c)) #f)))
                                    (else (plp (cdr cs) pts va? rt))))))))
                       ((global)
                        (let* ((gsym (cadr desc))
                               (tspec (caddr desc))
                               (vt (if (and (pair? tspec) (eq? (car tspec) 'mut))
                                       (normalize-marv-vtype (cadr tspec))
                                       (normalize-marv-vtype tspec))))
                          (set! mod-globals (cons (cons gsym vt) mod-globals))))))))
                ((func)
                 (let ((fsym (cadr it)))
                   (let plp ((cs (cddr it)) (pts '()) (va? #f) (rt #f) (rrec #f))
                     (if (null? cs)
                         (let ((farity (+ (if rrec 1 0) (length pts))))
                           (set! mod-funcs
                             (cons (cons fsym (list (reverse pts) rt va? rrec farity))
                                   mod-funcs)))
                         (let ((c (car cs)))
                           (cond
                             ((and (pair? c) (eq? (car c) 'param))
                              (let ((ps (cdr c)))
                                (cond
                                  ((and (= (length ps) 1) (eq? (car ps) '...))
                                   (plp (cdr cs) pts #t rt rrec))
                                  ((and (= (length ps) 2)
                                        (symbol? (car ps))
                                        (char=? (string-ref (symbol->string (car ps)) 0) #\$))
                                   (plp (cdr cs) (cons (normalize-marv-vtype (cadr ps)) pts) va? rt rrec))
                                  (else
                                   (plp (cdr cs)
                                        (append (reverse (map normalize-marv-vtype ps)) pts)
                                        va? rt rrec)))))
                             ((and (pair? c) (eq? (car c) 'result))
                              (if (and (pair? (cdr c)) (pair? (cadr c)) (memq (caadr c) '(record struct)))
                                  (plp (cdr cs) pts va? #f (cadadr c))
                                  (plp (cdr cs) pts va?
                                       (if (pair? (cdr c)) (normalize-marv-vtype (cadr c)) #f)
                                       rrec)))
                             (else
                              (plp (cdr cs) pts va? rt rrec)))))))))))
          mitems))

      ;; ---------------------------------------------------------------
      ;; MARV macros: expansion + stack-effect checking.
      ;;
      ;; A macro use is ($name arg ...) -- a $-symbol in head position.
      ;; No other MARV form has that shape, so macro calls cannot be
      ;; confused with instructions, (call ...)s, or data, and macro
      ;; names live in their own namespace (they cannot shadow -- or be
      ;; shadowed by -- instructions).  Macro definitions come from
      ;; (macro ...) items in .mvh interfaces and in the module itself
      ;; (see register-macro!).
      ;;
      ;; Expansion rewrites each (func ...) form before lower-func-def
      ;; runs.  Declared macro locals are renamed to fresh symbols and
      ;; spliced in as (local ...) items (both lower-func-def and
      ;; parse-func pick those up anywhere in the form); declared
      ;; labels and block/loop labels introduced by the template are
      ;; renamed likewise.  Everything else in a template is deliberate
      ;; unhygienic assembler capture (e.g. reading/assigning the
      ;; caller's $hp).  A macro expands to a single form, usually
      ;; (seq ...), and may leave any number of values on the stack --
      ;; its declared (effect ...) says how many, and is checked on
      ;; the fully expanded code of every use.
      ;; ---------------------------------------------------------------

      (define mx-fresh-counter 0)

      (define (mx-fresh sym)
        (set! mx-fresh-counter (+ mx-fresh-counter 1))
        (string->symbol
          (string-append (symbol->string sym) "$m"
                         (number->string mx-fresh-counter))))

      ;; symbol-wise substitution; env is an alist sym -> form
      (define (mx-subst form env)
        (cond ((symbol? form)
               (let ((e (assq form env))) (if e (cdr e) form)))
              ((pair? form)
               (cons (mx-subst (car form) env) (mx-subst (cdr form) env)))
              (else form)))

      (define (mx-distinct-list xs)
        (let loop ((xs xs) (seen '()))
          (cond ((null? xs) (reverse seen))
                ((memq (car xs) seen) (loop (cdr xs) seen))
                (else (loop (cdr xs) (cons (car xs) seen))))))

      ;; labels introduced by (block $L ...) / (loop $L ...) inside a
      ;; template, excluding names bound as macro parameters
      (define (mx-template-labels form params)
        (cond ((not (pair? form)) '())
              ((and (memq (car form) '(block loop))
                    (pair? (cdr form)) (symbol? (cadr form))
                    (not (memq (cadr form) params)))
               (cons (cadr form) (mx-template-labels (cdr form) params)))
              (else (append (mx-template-labels (car form) params)
                            (mx-template-labels (cdr form) params)))))

      ;; ---- stack-effect checker (abstract interpretation) -----------
      ;; Stacks are lists with the top in the car.  Types are i32,
      ;; i64, f32, f64, ptr, or 'any (unknown; matches everything).

      (define (mx-type-match? a b)
        (or (eq? a b) (eq? a 'any) (eq? b 'any)))

      (define (mx-pop1 stack want ctx)
        (cond ((null? stack)
               (error "macro effect check: stack underflow" ctx))
              ((mx-type-match? (car stack) want) (cdr stack))
              (else (error "macro effect check: type mismatch on stack"
                           ctx "expected" want "found" (car stack)))))

      ;; types are in push order (bottom first), as in *instr-table*
      (define (mx-pop-list stack types ctx)
        (let loop ((ts (reverse types)) (s stack))
          (if (null? ts) s (loop (cdr ts) (mx-pop1 s (car ts) ctx)))))

      (define (mx-push stack type)
        (if type (cons type stack) stack))

      (define (mx-stack-eq? s want)
        (and (= (length s) (length want))
             (let loop ((a (reverse s)) (b (reverse want)))
               (cond ((null? a) #t)
                     ((mx-type-match? (car a) (car b)) (loop (cdr a) (cdr b)))
                     (else #f)))))

      (define (mx-merge-stacks a b)
        (map (lambda (x y) (if (eq? x 'any) y x)) a b))

      (define (mx-tenv-type tenv sym)
        (let ((e (assq sym tenv))) (if e (cdr e) 'any)))

      (define (mx-global-type sym)
        (let ((e (assq sym mod-globals)))
          (if e (cdr e)
              (let ((g (assq sym iglobal-env)))
                (if g (list-ref (cdr g) 1) 'any)))))

      ;; labels are ((lbl entry-height rt kind) ...); kind is *block* or *loop*
      (define (mx-check-branch stack lbl labels ctx)
        (let ((e (assq lbl labels)))
          (when (not e)
            (error "macro effect check: branch to a label the template does not own (declare it in (labels ...) or keep branches inside the template)"
                   ctx lbl))
          (let ((height (cadr e)) (rt (caddr e)) (kind (cadddr e)))
            (cond ((eq? kind '*loop*)
                   (when (not (= (length stack) height))
                     (error "macro effect check: bad stack at loop branch" ctx lbl)))
                  (rt
                   (when (not (and (= (length stack) (+ height 1))
                                   (mx-type-match? (car stack) rt)))
                     (error "macro effect check: bad stack at branch" ctx lbl)))
                  (else
                   (when (not (= (length stack) height))
                     (error "macro effect check: bad stack at branch" ctx lbl)))))))

      (define (mx-end-check stack def ctx)
        (when (not (mx-stack-eq? stack (reverse (list-ref def 6))))
          (error "macro effect check: stack at template exit does not match the declared effect"
                 ctx (list-ref def 1)
                 "declared" (list-ref def 5) '-> (list-ref def 6))))

      (define (mx-step-call ins stack ctx)
        (let ((callee (cadr ins)))
          (let ((ce (assq callee ifunc-env))
                (me (assq callee mod-funcs)))
            (cond
              (ce (let ((d (cdr ce)))
                    (when (list-ref d 3)
                      (error "macro effect check: cannot check a call to a variadic function" ctx ins))
                    (when (list-ref d 4)
                      (error "macro effect check: cannot check a call to a record-returning function" ctx ins))
                    (mx-push (mx-pop-list stack (list-ref d 1) ins)
                             (list-ref d 2))))
              (me (let ((d (cdr me)))
                    (when (list-ref d 2)
                      (error "macro effect check: cannot check a call to a variadic function" ctx ins))
                    (when (list-ref d 3)
                      (error "macro effect check: cannot check a call to a record-returning function" ctx ins))
                    (mx-push (mx-pop-list stack (list-ref d 0) ins)
                             (list-ref d 1))))
              (else
               (error "macro effect check: call to unknown function (a signature is needed)"
                      ctx ins))))))

      ;; One flat instruction; returns the new stack or 'dead (every
      ;; continuing path leaves the function or an owned block).
      (define (mx-step ins stack labels tenv def ctx)
        (let ((op (car ins)))
          (cond
            ((eq? op 'local.get)
             (mx-push stack (mx-tenv-type tenv (cadr ins))))
            ((eq? op 'local.set)
             (mx-pop1 stack (mx-tenv-type tenv (cadr ins)) ins))
            ((eq? op 'local.tee)
             (mx-push (mx-pop1 stack (mx-tenv-type tenv (cadr ins)) ins)
                      (if (null? stack) 'any (car stack))))
            ((eq? op 'global.get)
             (mx-push stack (mx-global-type (cadr ins))))
            ((eq? op 'global.set)
             (mx-pop1 stack (mx-global-type (cadr ins)) ins))
            ((eq? op 'call)
             (mx-step-call ins stack ctx))
            ((eq? op 'return_call)
             (mx-end-check (mx-step-call ins stack ctx) def ctx)
             'dead)
            ((eq? op 'call_indirect)
             ;; flat form: (call_indirect ptypes rtype); funcref on top
             (mx-push (mx-pop-list stack (append (cadr ins) '(ptr)) ins)
                      (caddr ins)))
            ((eq? op 'return_call_indirect)
             (mx-end-check
               (mx-push (mx-pop-list stack (append (cadr ins) '(ptr)) ins)
                        (caddr ins))
               def ctx)
             'dead)
            ((eq? op 'br)
             (mx-check-branch stack (cadr ins) labels ins)
             'dead)
            ((eq? op 'br_if)
             (let ((s (mx-pop1 stack 'i32 ins)))
               (mx-check-branch s (cadr ins) labels ins)
               s))
            ((eq? op 'br_table)
             (let ((s (mx-pop1 stack 'i32 ins)))
               (for-each (lambda (l) (mx-check-branch s l labels ins))
                         (append (cadr ins) (list (caddr ins))))
               'dead))
            ((eq? op 'return)
             (mx-end-check stack def ctx)
             'dead)
            ((eq? op 'unreachable) 'dead)
            (else
             (let ((info (lookup-instr op)))
               (when (not info)
                 (error "macro effect check: unknown instruction in template" ctx op))
               (mx-push (mx-pop-list stack (instr-pop-types info) ins)
                        (instr-push-type info)))))))

      ;; Simulate a flat instruction list from STACK; returns the
      ;; resulting stack, or 'dead if control leaves on every path.
      (define (mx-sim insns stack labels tenv def ctx)
        (cond
          ((null? insns) stack)
          (else
           (let ((ins (car insns)) (rest (cdr insns)))
             (cond
               ((and (pair? ins) (memq (car ins) '(*block* *loop*)))
                (let* ((tag (car ins)) (lbl (cadr ins)) (rt (caddr ins))
                       (labels2 (cons (list lbl (length stack) rt tag) labels))
                       (res (mx-sim (cadddr ins) stack labels2 tenv def ctx)))
                  (when (not (eq? res 'dead))
                    (mx-check-block-end res stack rt ins ctx))
                  (mx-sim rest (mx-push stack rt) labels tenv def ctx)))
               ((and (pair? ins) (eq? (car ins) '*if*))
                (let* ((rt (cadr ins))
                       (s0 (mx-pop1 stack 'i32 ins))
                       (norm (lambda (r) (if (eq? r 'dead) (mx-push s0 rt) r)))
                       (st (norm (mx-sim (caddr ins) s0 labels tenv def ctx)))
                       (se (norm (mx-sim (cadddr ins) s0 labels tenv def ctx))))
                  (if (mx-stack-eq? st se)
                      (mx-sim rest (mx-merge-stacks st se) labels tenv def ctx)
                      (error "macro effect check: if branches disagree on stack effect"
                             ctx ins))))
               (else
                (let ((s2 (mx-step ins stack labels tenv def ctx)))
                  (if (eq? s2 'dead)
                      'dead
                      (mx-sim rest s2 labels tenv def ctx)))))))))

      (define (mx-check-block-end res entry rt ins ctx)
        (cond (rt
               (when (not (and (= (length res) (+ (length entry) 1))
                               (mx-type-match? (car res) rt)
                               (mx-stack-eq? (cdr res) entry)))
                 (error "macro effect check: block body does not produce its declared result"
                        ctx ins)))
              (else
               (when (not (mx-stack-eq? res entry))
                 (error "macro effect check: block body changes the stack"
                        ctx ins)))))

      ;; (const $X) pushes a value of unknown type; model it in the
      ;; checker only as an 'any-typed local read (the real code is
      ;; untouched -- lower-expr resolves consts after expansion).
      (define (mx-replace-consts form)
        (cond ((not (pair? form)) form)
              ((eq? (car form) 'const) '(local.get $mx-any$))
              (else (cons (mx-replace-consts (car form))
                          (mx-replace-consts (cdr form))))))

      ;; Check the declared (effect ...) of DEF against its fully
      ;; expanded instantiation FORM, given enclosing local types TENV.
      (define (mx-check-effect def form tenv ctx)
        (let* ((flat (flatten-one (mx-replace-consts
                                    (if (pair? form) form (list 'seq form)))))
               (start (reverse (list-ref def 5)))
               (res (mx-sim flat start '() tenv def ctx)))
          (cond ((eq? res 'dead) #t)
                ((mx-stack-eq? res (reverse (list-ref def 6))) #t)
                (else
                 (error "macro effect mismatch" ctx (list-ref def 1)
                        "declared" (list-ref def 5) '-> (list-ref def 6))))))

      ;; Expand all macro uses in one (func ...) form.
      (define (expand-func-macros fn-form)
        (if (null? macro-env)
            fn-form
            (let ((injected '())       ; (local $fresh type) items, reversed
                  (fuel 10000))
              (define (consume-fuel! ctx)
                (set! fuel (- fuel 1))
                (when (<= fuel 0)
                  (error "macro expansion too deep (recursive macro?)" ctx)))
              ;; local types of the enclosing function, for the checker
              (define (base-tenv clauses)
                (let loop ((cs clauses) (acc (list (cons '$mx-any$ 'any))))
                  (cond ((null? cs) acc)
                        ((and (pair? (car cs)) (memq (caar cs) '(param local)))
                         (let ((ps (cdar cs)))
                           (if (and (= (length ps) 2) (symbol? (car ps)))
                               (loop (cdr cs)
                                     (cons (cons (car ps)
                                                 (normalize-marv-vtype (cadr ps)))
                                           acc))
                               (loop (cdr cs) acc))))
                        (else (loop (cdr cs) acc)))))
              ;; Instantiate DEF with ARGS: rename declared locals,
              ;; labels, and template-introduced labels; substitute
              ;; parameters; splice fresh (local ...) items into the
              ;; enclosing function.  Returns (values form fresh-types).
              (define (instantiate def args ctx)
                (let ((mname (list-ref def 1))
                      (params (list-ref def 2))
                      (mlocals (list-ref def 3))
                      (mlabels (list-ref def 4))
                      (template (list-ref def 7)))
                  (when (not (= (length args) (length params)))
                    (error "macro arity mismatch" ctx "macro" mname
                           "expects" (length params)
                           "arguments, got" (length args)))
                  (let* ((tl (filter (lambda (l) (not (memq l mlabels)))
                                     (mx-template-labels template params)))
                         (ren (append
                                (map (lambda (lt) (cons (car lt) (mx-fresh (car lt))))
                                     mlocals)
                                (map (lambda (l) (cons l (mx-fresh l)))
                                     (mx-distinct-list (append mlabels tl)))))
                         (fresh-types (map (lambda (lt r) (cons (cdr r) (cdr lt)))
                                           mlocals ren))
                         (body (mx-subst (mx-subst template ren)
                                         (map cons params args))))
                    (for-each
                      (lambda (ft)
                        (set! injected
                              (cons (list 'local (car ft) (cdr ft)) injected)))
                      fresh-types)
                    (values body fresh-types))))
              (define (expand-form f tenv)
                (cond
                  ((not (pair? f)) f)
                  ((and (symbol? (car f)) (assq (car f) macro-env))
                   (let ((def (cdr (assq (car f) macro-env))))
                     (consume-fuel! (car f))
                     (let-values (((tmpl fresh-types)
                                   (instantiate def
                                                (map (lambda (a) (expand-form a tenv))
                                                     (cdr f))
                                                f)))
                       (let ((tenv2 (append fresh-types tenv)))
                         (let ((expanded (expand-form tmpl tenv2)))
                           (mx-check-effect def expanded tenv2 f)
                           expanded)))))
                  (else
                   (cons (expand-form (car f) tenv)
                         (expand-form (cdr f) tenv)))))
              (let* ((clauses (cddr fn-form))
                     (tenv0 (base-tenv clauses))
                     (expanded-items
                       (map (lambda (it)
                              (cond
                                ((and (pair? it)
                                      (memq (car it)
                                            '(export param result frame local)))
                                 it)
                                ((and (pair? it) (eq? (car it) 'body))
                                 (cons 'body
                                       (map (lambda (b) (expand-form b tenv0))
                                            (cdr it))))
                                (else (expand-form it tenv0))))
                            clauses)))
                (append (list 'func (cadr fn-form))
                        expanded-items
                        (reverse injected))))))

      ;; Transform a single `(func $fname ...)` definition
      (define (lower-func-def fn-form)
        (let* ((fname (cadr fn-form))
               (clauses (cddr fn-form))
               (local-types '())
               (orig-frame-sz 0)
               (max-va-end 0))

          ;; 1. Collect parameter and local types, plus original `(frame N)`
          (for-each
            (lambda (c)
              (when (pair? c)
                (case (car c)
                  ((param)
                   (let ((ps (cdr c)))
                     (cond
                       ((and (= (length ps) 1) (eq? (car ps) '...))
                        (set! local-types (cons (cons '$ap$ 'ptr) local-types)))
                       ((and (= (length ps) 2)
                             (symbol? (car ps))
                             (char=? (string-ref (symbol->string (car ps)) 0) #\$))
                        (set! local-types
                          (cons (cons (car ps) (normalize-marv-vtype (cadr ps))) local-types))))))
                  ((result)
                   (when (and (pair? (cdr c)) (pair? (cadr c)) (memq (caadr c) '(record struct)))
                     (set! local-types (cons (cons '$rp$ 'ptr) local-types))))
                  ((local)
                   (let ((ls (cdr c)))
                     (when (and (= (length ls) 2)
                                (symbol? (car ls))
                                (char=? (string-ref (symbol->string (car ls)) 0) #\$))
                       (set! local-types
                         (cons (cons (car ls) (normalize-marv-vtype (cadr ls))) local-types)))))
                  ((frame)
                   (when (and (pair? (cdr c)) (number? (cadr c)))
                     (set! orig-frame-sz (max orig-frame-sz (cadr c))))))))
            clauses)

          (let ((va-start-base (align-up orig-frame-sz 16)))

            ;; Infer the MARV scalar type (`i32`, `i64`, `f32`, `f64`, `ptr`) of an expression
            (define (infer-expr-type e)
              (cond
                ((not (pair? e)) 'i32)
                (else
                 (let ((op (car e)))
                   (case op
                     ((i32 i64 f32 f64 ptr) op)
                     ((local.get local.tee)
                      (let ((le (assq (cadr e) local-types)))
                        (if le (cdr le) 'i32)))
                     ((global.get)
                      (let* ((gs (cadr e))
                             (ge (assq gs mod-globals))
                             (ige (assq gs iglobal-env)))
                        (cond (ge (cdr ge))
                              (ige (list-ref (cdr ige) 1))
                              (else 'i32))))
                     ((call return_call)
                      (let* ((callee (cadr e))
                             (ife (assq callee ifunc-env))
                             (mfe (assq callee mod-funcs)))
                        (cond (ife (or (list-ref (cdr ife) 2) 'i32))
                              (mfe (or (list-ref (cdr mfe) 1) 'i32))
                              (else 'i32))))
                     ((field.get)
                      (let ((fentry (lookup-record-field (cadr e) (caddr e))))
                        (list-ref fentry 2)))
                     ((field.addr frame.addr ref.data ref.func
                       ptr.null ptr.add ptr.add64 ptr.sub ptr.of_i64 ptr.load
                       va.start)
                      'ptr)
                     ((const c.const)
                      (let ((ce (assq (cadr e) const-env)))
                        (if ce
                            (if (eq? (cadr ce) 'scalar)
                                (caddr ce)
                                (infer-expr-type (cddr ce)))
                            'i32)))
                     ((sizeof alignof offsetof va.count) 'i32)
                     ((sizeof64) 'i64)
                     ((va.arg)
                      (normalize-marv-vtype (caddr e)))
                     ((seq begin)
                      (if (pair? (cdr e))
                          (infer-expr-type (car (reverse (cdr e))))
                          'i32))
                     ((if block loop)
                      (if (and (pair? (cdr e))
                               (pair? (cadr e))
                               (eq? (caadr e) 'result))
                          (normalize-marv-vtype (cadadr e))
                          'i32))
                     ((select)
                      (if (pair? (cdr e))
                          (infer-expr-type (cadr e))
                          'i32))
                     (else
                      (let ((info (lookup-instr op)))
                        (if (and info (instr-push-type info))
                            (instr-push-type info)
                            (let ((s (symbol->string op)))
                              (cond
                                ((string-prefix? "i64." s)
                                 (if (or (string-prefix? "i64.eq" s)
                                         (string-prefix? "i64.ne" s)
                                         (string-prefix? "i64.lt" s)
                                         (string-prefix? "i64.gt" s)
                                         (string-prefix? "i64.le" s)
                                         (string-prefix? "i64.ge" s))
                                     'i32
                                     'i64))
                                ((string-prefix? "f64." s)
                                 (if (or (string-prefix? "f64.eq" s)
                                         (string-prefix? "f64.ne" s)
                                         (string-prefix? "f64.lt" s)
                                         (string-prefix? "f64.gt" s)
                                         (string-prefix? "f64.le" s)
                                         (string-prefix? "f64.ge" s))
                                     'i32
                                     'f64))
                                ((string-prefix? "f32." s)
                                 (if (or (string-prefix? "f32.eq" s)
                                         (string-prefix? "f32.ne" s)
                                         (string-prefix? "f32.lt" s)
                                         (string-prefix? "f32.gt" s)
                                         (string-prefix? "f32.le" s)
                                         (string-prefix? "f32.ge" s))
                                     'i32
                                     'f32))
                                (else 'i32)))))))))))

            ;; Lower an expression with current `va-cursor` offset
            (define (lower-expr x va-cursor)
              (cond
                ;; glued `offset=...` symbol (removed; see attr-value).
                ;; The two-element form `offset $SYM` has its const
                ;; resolved by the attr-aware list walk in the else
                ;; clause below.
                ((and (symbol? x)
                      (let ((s (symbol->string x)))
                        (and (> (string-length s) 7)
                             (string=? (substring s 0 7) "offset="))))
                 (error "glued offset= attributes are not supported; use the two-element form (offset <number>) or (offset $CONST)"
                        x))

                ((not (pair? x)) x)

                ;; Explicit type annotation wrapper `(i32 expr)` / `(i64 expr)` / `(f64 expr)` / `(ptr expr)`
                ((and (memq (car x) '(i32 i64 f32 f64 ptr))
                      (= (length x) 2))
                 (lower-expr (cadr x) va-cursor))

                ;; `(const $SYM)`
                ((and (memq (car x) '(const c.const))
                      (= (length x) 2)
                      (symbol? (cadr x))
                      (let ((s (symbol->string (cadr x))))
                        (and (> (string-length s) 0) (char=? (string-ref s 0) #\$))))
                 (let ((ce (assq (cadr x) const-env)))
                   (if ce
                       (if (eq? (cadr ce) 'scalar)
                           (let ((ct (caddr ce))
                                 (cv (cdddr ce)))
                             (if (eq? ct 'ptr)
                                 (if (= cv 0)
                                     '(ptr.null)
                                     `(ptr.of_i64 (i64.const ,cv)))
                                 (list (string->symbol (string-append (symbol->string ct) ".const")) cv)))
                           (lower-expr (cddr ce) va-cursor))
                       (error "unknown constant in (const ...)" (cadr x)))))

                ;; `(i32.const $SYM)` / `(i64.const $SYM)` / `(f32.const $SYM)` / `(f64.const $SYM)`
                ((and (memq (car x) '(i32.const i64.const f32.const f64.const))
                      (pair? (cdr x))
                      (symbol? (cadr x)))
                 (let ((ce (assq (cadr x) const-env)))
                   (if (and ce (eq? (cadr ce) 'scalar))
                       (cons (car x) (cons (cdddr ce) (map (lambda (sub) (lower-expr sub va-cursor)) (cddr x))))
                       x)))

                ;; Record layout queries: `(sizeof $R)`, `(sizeof64 $R)`, `(alignof $R)`, `(offsetof $R $f)`
                ((eq? (car x) 'sizeof)
                 (let ((rinfo (lookup-record (cadr x))))
                   (list 'i32.const (list-ref rinfo 1))))
                ((eq? (car x) 'sizeof64)
                 (let ((rinfo (lookup-record (cadr x))))
                   (list 'i64.const (list-ref rinfo 1))))
                ((eq? (car x) 'alignof)
                 (let ((rinfo (lookup-record (cadr x))))
                   (list 'i32.const (list-ref rinfo 2))))
                ((eq? (car x) 'offsetof)
                 (let ((fentry (lookup-record-field (cadr x) (caddr x))))
                   (list 'i32.const (list-ref fentry 3))))

                ;; Record field access: `(field.addr $R $f <ptr>)`, `(field.get $R $f <ptr>)`, `(field.set $R $f <ptr> <val>)`
                ((eq? (car x) 'field.addr)
                 (let* ((fentry (lookup-record-field (cadr x) (caddr x)))
                        (foff   (list-ref fentry 3))
                        (pexpr  (lower-expr (cadddr x) va-cursor)))
                   (if (= foff 0)
                       pexpr
                       `(ptr.add ,pexpr (i32.const ,foff)))))
                ((eq? (car x) 'field.get)
                 (let* ((fentry (lookup-record-field (cadr x) (caddr x)))
                        (foff   (list-ref fentry 3))
                        (lop    (list-ref fentry 6))
                        (pexpr  (lower-expr (cadddr x) va-cursor)))
                   (when (not lop)
                     (error "cannot field.get aggregate field (use field.addr)" x))
                   (if (= foff 0)
                       (list lop pexpr)
                       (list lop 'offset foff pexpr))))
                ((eq? (car x) 'field.set)
                 (let* ((fentry (lookup-record-field (cadr x) (caddr x)))
                        (foff   (list-ref fentry 3))
                        (sop    (list-ref fentry 7))
                        (pexpr  (lower-expr (cadddr x) va-cursor))
                        (vexpr  (lower-expr (list-ref x 4) va-cursor)))
                   (when (not sop)
                     (error "cannot field.set aggregate field (use field.addr)" x))
                   (if (= foff 0)
                       (list sop pexpr vexpr)
                       (list sop 'offset foff pexpr vexpr))))

                ;; Variadic function intrinsics: `(va.start)`, `(va.count)`, `(va.arg $ap <type>)`
                ((eq? (car x) 'va.start)
                 '(local.get $ap$))
                ((eq? (car x) 'va.count)
                 '(i32.load (ptr.sub (local.get $ap$) (i32.const 16))))
                ((eq? (car x) 'va.arg)
                 (let* ((ap-var (cadr x))
                        (vtype  (normalize-marv-vtype (caddr x)))
                        (slot-ptr `(ptr.sub (local.get ,ap-var) (i32.const 16)))
                        (load-expr
                          (case vtype
                            ((i32) `(i32.load ,slot-ptr))
                            ((i64) `(i64.load ,slot-ptr))
                            ((f32) `(f32.demote_f64 (f64.load ,slot-ptr)))
                            ((f64) `(f64.load ,slot-ptr))
                            ((ptr) `(ptr.load ,slot-ptr)))))
                   `(seq
                      (local.set ,ap-var (ptr.add (local.get ,ap-var) (i32.const 16)))
                      ,load-expr)))

                ;; Global get/set: resolve interface globals and mark used
                ((memq (car x) '(global.get global.set))
                 (let* ((gsym (cadr x))
                        (ige  (assq gsym iglobal-env))
                        (csym (if ige
                                  (let ((c (car (cdr ige))))
                                    (mark-used-imp! c)
                                    c)
                                  gsym)))
                   (cons (car x)
                         (cons csym
                               (map (lambda (sub) (lower-expr sub va-cursor)) (cddr x))))))

                ;; Data reference: `(ref.data $sym ...)`
                ((eq? (car x) 'ref.data)
                 (let* ((dsym (cadr x))
                        (ide  (assq dsym idata-env))
                        (csym (if ide
                                  (let ((c (car (cdr ide))))
                                    (mark-used-imp! c)
                                    c)
                                  dsym)))
                   (cons 'ref.data
                         (cons csym
                               (map (lambda (sub) (lower-expr sub va-cursor)) (cddr x))))))

                ;; Function reference: `(ref.func $sym)`
                ((eq? (car x) 'ref.func)
                 (let* ((fsym (cadr x))
                        (ife  (assq fsym ifunc-env))
                        (csym (if ife
                                  (let ((c (car (cdr ife))))
                                    (mark-used-imp! c)
                                    c)
                                  fsym)))
                   (list 'ref.func csym)))

                ;; Function calls: `(call $fn arg1 ...)` / `(return_call $fn arg1 ...)`
                ((memq (car x) '(call return_call))
                 (let* ((call-op (car x))
                        (fsym    (cadr x))
                        (raw-args (cddr x))
                        (ife     (assq fsym ifunc-env))
                        (mfe     (assq fsym mod-funcs))
                        (csym    (if ife
                                     (let ((c (car (cdr ife))))
                                       (mark-used-imp! c)
                                       c)
                                     fsym))
                        (variadic? (cond (ife (list-ref (cdr ife) 3))
                                         (mfe (list-ref (cdr mfe) 2))
                                         (else #f)))
                        (fixed-arity (cond (ife (list-ref (cdr ife) 6))
                                           (mfe (list-ref (cdr mfe) 4))
                                           (else (length raw-args)))))
                   (if (not variadic?)
                       (cons call-op
                             (cons csym
                                   (map (lambda (a) (lower-expr a va-cursor)) raw-args)))
                       ;; Variadic call: split into `fixed-arity` fixed args and trailing varargs
                       (if (< (length raw-args) fixed-arity)
                           (error "too few arguments to variadic function" x)
                           (let* ((fixed-args (let take-k ((as raw-args) (k fixed-arity) (acc '()))
                                                (if (= k 0)
                                                    (reverse acc)
                                                    (take-k (cdr as) (- k 1) (cons (car as) acc)))))
                                  (va-args    (drop-n fixed-arity raw-args)))
                             ;; Check for passthrough `(va.ap <ptr-expr>)`
                             (if (and (= (length va-args) 1)
                                      (pair? (car va-args))
                                      (memq (caar va-args) '(va.ap va.pass)))
                                 (let ((low-fixed (map (lambda (a) (lower-expr a va-cursor)) fixed-args))
                                       (low-ap    (lower-expr (cadar va-args) va-cursor)))
                                   `(,call-op ,csym ,@low-fixed ,low-ap))
                                 ;; Pack `M` variadic arguments into 16-byte `va_arg_t` slots in frame
                                 (let* ((m (length va-args))
                                        (va-base (align-up va-cursor 16))
                                        (va-bytes (* (+ m 1) 16))
                                        (next-cursor (+ va-base va-bytes))
                                        (_ (set! max-va-end (max max-va-end next-cursor)))
                                        (low-fixed (map (lambda (a) (lower-expr a next-cursor)) fixed-args))
                                        (store-stmts
                                          (let vloop ((vas va-args) (j 0) (acc '()))
                                            (if (null? vas)
                                                (reverse acc)
                                                (let* ((raw-va (car vas))
                                                       (vt (infer-expr-type raw-va))
                                                       (low-va (lower-expr raw-va next-cursor))
                                                       (slot-off (+ va-base (* (+ j 1) 16)))
                                                       (st-stmt
                                                         (case vt
                                                           ((i32) `(i32.store (frame.addr ,slot-off) ,low-va))
                                                           ((i64) `(i64.store (frame.addr ,slot-off) ,low-va))
                                                           ((f32) `(f64.store (frame.addr ,slot-off) (f64.promote_f32 ,low-va)))
                                                           ((f64) `(f64.store (frame.addr ,slot-off) ,low-va))
                                                           ((ptr) `(ptr.store (frame.addr ,slot-off) ,low-va))
                                                           (else  `(i64.store (frame.addr ,slot-off) ,low-va)))))
                                                  (vloop (cdr vas) (+ j 1) (cons st-stmt acc)))))))
                                   `(seq
                                      (i32.store (frame.addr ,va-base) (i32.const ,m))
                                      ,@store-stmts
                                      (,call-op ,csym ,@low-fixed (frame.addr ,(+ va-base 16)))))))))))

                (else
                 ;; Generic form: lower each element, resolving the
                 ;; two-element attributes `offset $CONST` /
                 ;; `align $CONST` (scalar consts) on the way.
                 (let attrloop ((subs x) (acc '()))
                   (cond
                     ((null? subs) (reverse acc))
                     ((and (memq (car subs) '(offset align))
                           (pair? (cdr subs))
                           (symbol? (cadr subs)))
                      (let ((ce (assq (cadr subs) const-env)))
                        (if (and ce (eq? (cadr ce) 'scalar))
                            (attrloop (cddr subs)
                                      (cons (cdddr ce) (cons (car subs) acc)))
                            (error "offset/align value must be a number or a scalar const"
                                   (car subs) (cadr subs)))))
                     (else
                      (attrloop (cdr subs)
                                (cons (lower-expr (car subs) va-cursor)
                                      acc))))))))

            ;; Lower all clauses of `(func $fname ...)`
            (let cloop ((cs clauses)
                        (hdr-acc '())
                        (body-acc '())
                        (saw-frame? #f)
                        (prepended-rp? #f))
              (if (null? cs)
                  (let* ((final-frame-sz (align-up (max orig-frame-sz max-va-end) 16))
                         (rev-hdr0 (reverse hdr-acc))
                         (rev-hdr
                           (if prepended-rp?
                               (if (and (pair? rev-hdr0)
                                        (pair? (car rev-hdr0))
                                        (eq? (caar rev-hdr0) 'export))
                                   (cons (car rev-hdr0) (cons '(param $rp$ ptr) (cdr rev-hdr0)))
                                   (cons '(param $rp$ ptr) rev-hdr0))
                               rev-hdr0))
                         (final-hdr
                           (if (> final-frame-sz 0)
                               (if saw-frame?
                                   (map (lambda (h)
                                          (if (and (pair? h) (eq? (car h) 'frame))
                                              (list 'frame final-frame-sz)
                                              h))
                                        rev-hdr)
                                   (append rev-hdr (list (list 'frame final-frame-sz))))
                               rev-hdr)))
                    `(func ,fname ,@final-hdr ,@(reverse body-acc)))
                  (let ((c (car cs)))
                    (if (and (pair? c) (memq (car c) '(export param result frame local)))
                        (case (car c)
                          ((export local)
                           (cloop (cdr cs) (cons c hdr-acc) body-acc saw-frame? prepended-rp?))
                          ((frame)
                           (cloop (cdr cs) (cons c hdr-acc) body-acc #t prepended-rp?))
                          ((param)
                           (let ((ps (cdr c)))
                             (if (and (= (length ps) 1) (eq? (car ps) '...))
                                 (cloop (cdr cs) (cons '(param $ap$ ptr) hdr-acc) body-acc saw-frame? prepended-rp?)
                                 (cloop (cdr cs) (cons c hdr-acc) body-acc saw-frame? prepended-rp?))))
                          ((result)
                           (if (and (pair? (cdr c)) (pair? (cadr c)) (memq (caadr c) '(record struct)))
                               ;; `(result (record $R))` -> prepend `(param $rp$ ptr)` before other params
                               (cloop (cdr cs) hdr-acc body-acc saw-frame? #t)
                               (cloop (cdr cs) (cons c hdr-acc) body-acc saw-frame? prepended-rp?))))
                        ;; Body instruction/expression: reset va-cursor to `va-start-base` at each top-level stmt
                        (cloop (cdr cs)
                               hdr-acc
                               (cons (lower-expr c va-start-base) body-acc)
                               saw-frame?
                               prepended-rp?))))))))

      ;; Lower all items inside `(module $m ...)`
      (scan-module-items!)
      (let* ((lowered-mbody
               (let mloop ((its mitems) (acc '()))
                 (if (null? its)
                     (reverse acc)
                     (let ((it (car its)))
                       (if (pair? it)
                           (case (car it)
                             ;; Strip compile-time `(const ...)` and `(record ...)` from core module
                             ((const c-const record struct macro)
                              (mloop (cdr its) acc))
                             ((func)
                              (mloop (cdr its) (cons (lower-func-def (expand-func-macros (normalize-func-locals it))) acc)))
                             (else
                              (mloop (cdr its) (cons it acc))))
                           (mloop (cdr its) (cons it acc)))))))
             ;; Also scan `lowered-mbody` for any flat/unparenthesized mentions of interface symbols
             (_ (for-each
                  (lambda (kv)
                    (let ((sym (car kv))
                          (csym (car (cdr kv))))
                      (when (sexp-mentions-sym? lowered-mbody sym)
                        (mark-used-imp! csym))))
                  ifunc-env))
             (_ (for-each
                  (lambda (kv)
                    (let ((sym (car kv))
                          (csym (car (cdr kv))))
                      (when (sexp-mentions-sym? lowered-mbody sym)
                        (mark-used-imp! csym))))
                  iglobal-env))
             (_ (for-each
                  (lambda (kv)
                    (let ((sym (car kv))
                          (csym (car (cdr kv))))
                      (when (sexp-mentions-sym? lowered-mbody sym)
                        (mark-used-imp! csym))))
                  idata-env))
             ;; Gather unique `(import ...)` forms for used interface declarations
             (spliced-ifunc-imps
               (let ilp ((cs (reverse used-imps)) (seen '()) (acc '()))
                 (if (null? cs)
                     (reverse acc)
                     (let ((c (car cs)))
                       (if (memq c seen)
                           (ilp (cdr cs) seen acc)
                           (let ((fe (assq c ifunc-env))
                                 (ge (assq c iglobal-env))
                                 (de (assq c idata-env)))
                             (cond
                               (fe (ilp (cdr cs) (cons c seen) (cons (list-ref (cdr fe) 5) acc)))
                               (ge (ilp (cdr cs) (cons c seen) (cons (list-ref (cdr ge) 2) acc)))
                               (de (ilp (cdr cs) (cons c seen) (cons (list-ref (cdr de) 1) acc)))
                               (else (ilp (cdr cs) seen acc)))))))))
             (spliced-raw-imps
               (filter (lambda (imp)
                         (let* ((desc (list-ref imp 3))
                                (fsym (if (and (pair? desc) (pair? (cdr desc))) (cadr desc) #f)))
                           (or (not fsym) (sexp-mentions-sym? lowered-mbody fsym))))
                       raw-imps)))
        (set! *last-expanded-sys-mvh-paths* (reverse sys-mvh-paths))
        (set! *last-expanded-sys-mods*      (reverse sys-mods))
        (cons 'module
              (cons mname
                    (append spliced-ifunc-imps spliced-raw-imps lowered-mbody)))))))

(define (marv-normalize-target-type t)
  (case t
    ((wcpl wo)      'wcpl)
    ((cc c99 c)     'cc)
    ((rv64g riscv s) 'rv64g)
    (else (error "unknown target type (expected wcpl, cc, or rv64g)" t))))

(define (marv-target-obj-ext ttype)
  (case (marv-normalize-target-type ttype)
    ((wcpl)  ".wo")
    ((rv64g) ".s")
    ((cc)    ".c")))

(define (marv-string-join strs sep)
  (cond
    ((null? strs) "")
    ((null? (cdr strs)) (car strs))
    (else (string-append (car strs) sep (marv-string-join (cdr strs) sep)))))

(define (compile-marv-sexp sexp target port)
  (let ((mod (parse-module (expand-mvs-forms (list sexp) ".")))
        (ttype (marv-normalize-target-type target)))
    (case ttype
      ((wcpl)  (emit-wo mod port))
      ((cc)    (emit-c99 mod port))
      ((rv64g) (emit-rv64g mod port)))))

(define (compile-marv-file infile target outport)
  (let* ((forms (read-all-sexps infile))
         (mod-sexp (expand-mvs-forms forms (marv-dirname infile)))
         (mod (parse-module mod-sexp))
         (ttype (marv-normalize-target-type target))
         (deps-str (marv-string-join *last-expanded-sys-mods* " ")))
    (when (> (string-length deps-str) 0)
      (case ttype
        ((wcpl)  (fprintf outport ";; marv-sys-deps: ~a~%" deps-str))
        ((cc)    (fprintf outport "/* marv-sys-deps: ~a */~%" deps-str))
        ((rv64g) (fprintf outport "# marv-sys-deps: ~a~%" deps-str))))
    (case ttype
      ((wcpl)  (emit-wo mod outport))
      ((cc)    (emit-c99 mod outport))
      ((rv64g) (emit-rv64g mod outport)))))

;; -----------------------------------------------------------------------------
;; Target Description (.mvt) Loader & Linker Runner
;; -----------------------------------------------------------------------------

(define (find-mvt-file targdes)
  (let* ((fname (if (marv-has-suffix? targdes ".mvt")
                    targdes
                    (string-append targdes ".mvt")))
         (dirs *mstd-target-dirs*))
    (if (file-exists? fname)
        fname
        (let loop ((ds dirs))
          (if (null? ds)
              (error "cannot find target description (.mvt) file" fname *mstd-target-dirs*)
              (let ((p (marv-join-path (car ds) fname)))
                (if (file-exists? p)
                    p
                    (loop (cdr ds)))))))))

(define (mvt-assq-val key alist default)
  (let ((e (assq key alist)))
    (if (and e (pair? (cdr e)))
        (cadr e)
        default)))

(define (load-mvt-file mvt-path)
  (let ((forms (read-all-sexps mvt-path)))
    (if (and (pair? forms)
             (pair? (car forms))
             (memq (caar forms) '(marv-target target c-config)))
        (let* ((alist (cdar forms))
               (raw-type (mvt-assq-val 'type alist 'cc))
               (ttype (marv-normalize-target-type raw-type))
               (obj-ext (mvt-assq-val 'obj-ext alist (marv-target-obj-ext ttype)))
               (mvt-dir (marv-dirname mvt-path)))
          (cons (cons 'type ttype)
                (cons (cons 'obj-ext obj-ext)
                      (cons (cons 'mvt-dir mvt-dir) alist))))
        (error "invalid .mvt target file" mvt-path))))

(define (marv-string-replace-all str pat repl)
  (let ((slen (string-length str))
        (plen (string-length pat)))
    (let loop ((i 0) (acc ""))
      (cond
        ((> (+ i plen) slen)
         (string-append acc (substring str i slen)))
        ((string=? (substring str i (+ i plen)) pat)
         (loop (+ i plen) (string-append acc repl)))
        (else
         (loop (+ i 1) (string-append acc (string (string-ref str i)))))))))

(define (marv-split-spaces str)
  (let ((len (string-length str)))
    (let loop ((i 0) (cur '()) (acc '()))
      (cond
        ((= i len)
         (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
        ((char-whitespace? (string-ref str i))
         (loop (+ i 1) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
        (else
         (loop (+ i 1) (cons (string-ref str i) cur) acc))))))

(define (marv-string-contains-idx str sub)
  (let ((slen (string-length str))
        (blen (string-length sub)))
    (let loop ((i 0))
      (cond
        ((> (+ i blen) slen) #f)
        ((string=? (substring str i (+ i blen)) sub) i)
        (else (loop (+ i 1)))))))

(define (marv-read-line port)
  (let loop ((acc '()))
    (let ((ch (read-char port)))
      (cond
        ((eof-object? ch)
         (if (null? acc) ch (list->string (reverse acc))))
        ((char=? ch #\newline)
         (list->string (reverse acc)))
        ((char=? ch #\return)
         (loop acc))
        (else
         (loop (cons ch acc)))))))

(define (mods->c-paths mod-names sys-dirs)
  (let loop ((ms mod-names) (acc '()))
    (if (null? ms)
        (reverse acc)
        (let* ((msym (string->symbol (string-append "$" (car ms))))
               (mvh-p (find-mvh-file msym sys-dirs)))
          (if mvh-p
              (loop (cdr ms) (cons (mvh-path->adapter-c-path mvh-p) acc))
              (loop (cdr ms) acc))))))

;; Extract system adapter `.c` dependencies from a pre-compiled object file
;; (via its `marv-sys-deps:` header comment, or sibling `.mvs` file if present).
(define (extract-sys-c-deps-from-object obj-file)
  (let ((sys-dirs *mstd-system-dirs*))
    (if (not (file-exists? obj-file))
        '()
        (let ((first-line
                (call-with-input-file obj-file
                  (lambda (p) (marv-read-line p)))))
          (if (string? first-line)
              (let ((idx (marv-string-contains-idx first-line "marv-sys-deps:")))
                (if idx
                    (let* ((rest (substring first-line (+ idx 14) (string-length first-line)))
                           (clean (marv-string-replace-all rest "*/" ""))
                           (mods (marv-split-spaces clean)))
                      (mods->c-paths mods sys-dirs))
                    '()))
              '())))))

(define (marv-dedup-strings strs)
  (let loop ((ss strs) (seen '()) (acc '()))
    (if (null? ss)
        (reverse acc)
        (if (member (car ss) seen)
            (loop (cdr ss) seen acc)
            (loop (cdr ss) (cons (car ss) seen) (cons (car ss) acc))))))

(define (marv-host-os)
  (if (memq 'windows (features)) "win32" "unix"))

(define *marv-process-seed* (modulo (abs (exact (truncate (current-jiffy)))) 1000000000))
(define (marv-process-id) *marv-process-seed*)

(define (marv-string-has-backslash? str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) #\\) #t)
        (else (loop (+ i 1)))))))

(define (marv-nonempty-env name)
  (let ((v (get-environment-variable name)))
    (if (and (string? v) (> (string-length v) 0))
        v
        #f)))

(define (marv-temp-dir)
  (let* ((raw (or (marv-nonempty-env "TMPDIR")
                  (marv-nonempty-env "TMP")
                  (marv-nonempty-env "TEMP")
                  (if (string=? (marv-host-os) "win32") "." "/tmp")))
         (len (string-length raw))
         (last (if (> len 0) (string-ref raw (- len 1)) #\/)))
    (if (or (char=? last #\/) (char=? last #\\))
        raw
        (string-append raw (if (marv-string-has-backslash? raw) "\\" "/")))))

(define *marv-temp-counter* 0)

(define (marv-make-temp-obj obj-ext)
  (let ((tdir (marv-temp-dir))
        (pid-str (number->string (marv-process-id))))
    (let loop ()
      (let* ((idx *marv-temp-counter*)
             (_ (set! *marv-temp-counter* (+ idx 1)))
             (cand (string-append tdir "marv_" pid-str "_" (number->string idx) obj-ext)))
        (if (file-exists? cand)
            (loop)
            cand)))))

(define (cleanup-temp-files! paths)
  (for-each
    (lambda (p)
      (when (file-exists? p)
        (delete-file p)))
    paths))

(define (run-target-linker! mvt-info outfile obj-files sys-c-files temp-objs verbose?)
  (let* ((all-inputs (marv-dedup-strings (append obj-files sys-c-files)))
         (src-str (marv-string-join all-inputs " "))
         (link-tmpl (mvt-assq-val 'link-cmd mvt-info #f))
         (cmd
           (if link-tmpl
               (marv-string-replace-all
                 (marv-string-replace-all link-tmpl "~src" src-str)
                 "~exe" outfile)
               (let ((cc-bin  (mvt-assq-val 'cc mvt-info "cc"))
                     (cflags  (mvt-assq-val 'cflags mvt-info '()))
                     (ldflags (mvt-assq-val 'ldflags mvt-info '()))
                     (ldlibs  (mvt-assq-val 'ldlibs mvt-info '())))
                 (marv-string-join
                   (append (list cc-bin)
                           cflags
                           ldflags
                           (list "-o" outfile)
                           all-inputs
                           ldlibs)
                   " ")))))
    (when verbose?
      (fprintf (current-error-port) "~a~%" cmd))
    (let ((rc (%system cmd)))
      (cleanup-temp-files! temp-objs)
      (when (not (= rc 0))
        (fprintf (current-error-port) "marv: linker command failed (exit ~a): ~a~%" rc cmd)
        (exit 1)))))

;; -----------------------------------------------------------------------------
;; Command-Line Interface (using `(skint getopt)`)
;; -----------------------------------------------------------------------------

(define *marv-cli-options*
  '((target  "-t" "--target"      "TARGDES" "Target designation (.mvt name/path, required)")
    (targdir "-T" "--target-dir"  "DIR"     "Add directory to .mvt lookup path (default: target/)")
    (compile "-c" "--compile"     #f        "Compile one .mvs file to target object (.wo, .s, .c)")
    (output  "-o" "--output"      "FILE"    "Output file (object with -c; executable without -c)")
    (libdir  "-L" "--system-dir"  "DIR"     "Add directory to system .mvh/.c lookup path (default: lib/)")
    (incdir  "-I" "--include-dir" "DIR"     "Add directory to local .mvh lookup path (default: ./)")
    (verbose "-v" "--verbose"     #f        "Print executed compiler/linker command")
    (split   #f   "--split-funcs" "N"       "Link mode/cc only: split generated C into pieces of at most N functions (0=off, default 300)")
    (help    "-h" "--help"        #f        "Display this help message")))

(define (marv-usage port)
  (fprintf port "MARV Compiler & Linker Driver~%")
  (fprintf port "usage: marv -t TARGDES [-c] [OPTION]... FILE...~%~%")
  (fprintf port "Options:~%")
  (print-command-line-options *marv-cli-options* port))

;; -----------------------------------------------------------------------------
;; Link-mode C splitting (driver)
;;
;; The C emitter produces one translation unit per module, and for a
;; large module that single unit is what makes the C compiler's memory
;; explode (measured: ~1.3 GiB of cc1 RSS for a 2,629-function module).
;; In link mode (no -c) with the cc target, the driver therefore splits
;; the *generated temporary* C file into pieces of at most
;; *marv-split-funcs* function definitions and hands all the pieces to
;; the linker command; each piece is compiled as its own translation
;; unit and the temporary files are deleted afterwards as before.
;; -c mode never comes here: it always produces exactly one file.
;;
;; The splitter relies only on the emitter's own output format:
;;   * every module function has a forward declaration before the data
;;     section, and its definition is a top-level "<type> <name>(...)
;;     {" line closed by a "}" in column 0;
;;   * in every piece, module function prototypes lose their `static`;
;;     data/global definitions stay in piece 0 and become `extern`
;;     declarations (without the initialiser) in the other pieces, so
;;     the pieces link together with one definition per symbol;
;;   * `main` (if present) and the data-relocation constructor stay in
;;     piece 0.
;; -----------------------------------------------------------------------------

(define (marv-parse-split-funcs s)
  (let ((n (string->number s)))
    (if (and (number? n) (>= n 0))
        n
        (error "invalid --split-funcs argument (expected a non-negative integer)" s))))

(define *marv-split-ctypes*
  '("marv_i32" "marv_u32" "marv_i64" "marv_u64"
    "marv_f32" "marv_f64" "marv_ptr" "void"))

(define (marv-split-prefix? s prefix)
  (let ((n (string-length prefix)))
    (and (>= (string-length s) n)
         (string=? (substring s 0 n) prefix))))

(define (marv-split-strip-static s)
  (if (marv-split-prefix? s "static ")
      (substring s 7 (string-length s))
      s))

;; first token of a declaration line, ignoring a leading "static"
(define (marv-split-first-token s)
  (let* ((s2 (marv-split-strip-static s))
         (sp (string-index s2 #\space)))
    (if sp (substring s2 0 sp) s2)))

;; "<type> <name>(..." -> name, else #f
(define (marv-split-decl-name s)
  (let ((p (string-index s #\()))
    (and p
         (> p 0)
         (let loop ((i (- p 1)))
           (cond ((<= i 0) #f)
                 ((char=? (string-ref s i) #\space)
                  (substring s (+ i 1) p))
                 (else (loop (- i 1))))))))

;; name if LINE is a forward declaration of a module function, else #f
(define (marv-split-proto-name line)
  (let ((len (string-length line)))
    (and (> len 1)
         (char=? (string-ref line (- len 1)) #\;)
         (string-index line #\()
         (member (marv-split-first-token line) *marv-split-ctypes*)
         (marv-split-decl-name line))))

;; name if LINE starts a top-level definition of a declared module
;; function (name in PROTOS), else #f
(define (marv-split-def-name line protos)
  (let ((len (string-length line)))
    (and (> len 3)
         (string=? (substring line (- len 3) len) ") {")
         (member (marv-split-first-token line) *marv-split-ctypes*)
         (let ((nm (marv-split-decl-name line)))
           (and nm (member nm protos) nm)))))

(define (marv-split-read-lines path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((acc '()))
        (let ((l (marv-read-line p)))
          (if (eof-object? l)
              (reverse acc)
              (loop (cons l acc))))))))

;; split LINES into (header blocks); a block is (name line ...) with
;; the lines in order; header is everything before the first block
(define (marv-split-analyze lines)
  (let loop ((ls lines) (protos '()) (header '()) (blocks '())
             (cname #f) (cur '()))
    (cond
      ((null? ls)
       (list (reverse header)
             (reverse (if cname
                          (cons (cons cname (reverse cur)) blocks)
                          blocks))))
      (cname
       (let ((cur2 (cons (car ls) cur)))
         (if (string=? (car ls) "}")
             (loop (cdr ls) protos header
                   (cons (cons cname (reverse cur2)) blocks) #f '())
             (loop (cdr ls) protos header blocks cname cur2))))
      (else
       (let ((dn (marv-split-def-name (car ls) protos)))
         (cond
           (dn (loop (cdr ls) protos header blocks dn (list (car ls))))
           ((null? blocks)
            (let ((pn (marv-split-proto-name (car ls))))
              (loop (cdr ls) (if pn (cons pn protos) protos)
                    (cons (car ls) header) blocks #f '())))
           (else
            (loop (cdr ls) protos header
                  (cons (append (car blocks) (list (car ls))) (cdr blocks))
                  #f '()))))))))

;; OUT is a reversed line list; drop the "__attribute__((constructor))"
;; trio from its head (they belong to the relocation constructor)
(define (marv-split-drop-attr-trio out)
  (if (and (pair? out) (pair? (cdr out)) (pair? (cddr out))
           (string=? (car out) "#endif")
           (string=? (cadr out) "__attribute__((constructor))")
           (string=? (caddr out) "#if defined(__GNUC__) || defined(__clang__)"))
      (cdddr out)
      out))

;; remove the data-relocation constructor (it must live in piece 0 only)
(define (marv-split-drop-ctor header)
  (let loop ((ls header) (out '()))
    (cond
      ((null? ls) (reverse out))
      ((string=? (car ls) "static void _marv_init_data_relocs(void) {")
       (let skip ((ls2 (cdr ls)))
         (cond ((null? ls2) (reverse (marv-split-drop-attr-trio out)))
               ((string=? (car ls2) "}")
                (loop (cdr ls2) (marv-split-drop-attr-trio out)))
               (else (skip (cdr ls2))))))
      (else (loop (cdr ls) (cons (car ls) out))))))

;; "MARV_ALIGNED(a) uint8_t name[N] = {...}" -> "MARV_ALIGNED(a) uint8_t name[N]"
(define (marv-split-data-decl body)
  (let ((idx (marv-string-contains-idx body " = {")))
    (if idx
        (substring body 0 idx)
        (let ((len (string-length body)))
          (if (and (> len 0) (char=? (string-ref body (- len 1)) #\;))
              (substring body 0 (- len 1))
              body)))))

;; "static <type> <name> = <init>;" (global definition)?
(define (marv-split-global-line? line)
  (let ((body (marv-split-strip-static line)))
    (and (member (marv-split-first-token body) *marv-split-ctypes*)
         (not (string-index body #\())
         (let ((idx (marv-string-contains-idx body " = ")))
           (and idx
                (not (and (substring body 0 idx)
                          (string-index (substring body 0 idx)
                                        (integer->char 91)))))))))

(define (marv-split-header-line line k)
  (cond
    ;; trampoline resume state: defined in piece 0, extern elsewhere.
    ;; (The array definition has no " = ", so marv-split-global-line?
    ;; below would not catch it; handle both lines explicitly.)
    ((string=? line "marv_i64 marv_tc_fuel = MARV_TC_FUEL;")
     (if (= k 0) line "extern marv_i64 marv_tc_fuel;"))
    ((string=? line "marv_i64 marv_tc_state[16];")
     (if (= k 0) line "extern marv_i64 marv_tc_state[16];"))
    ;; data segment definition: defined in piece 0, extern elsewhere
    ((or (marv-split-prefix? line "static MARV_ALIGNED(")
         (marv-split-prefix? line "MARV_ALIGNED("))
     (let ((body (marv-split-strip-static line)))
       (if (= k 0)
           body
           (string-append "extern " (marv-split-data-decl body) ";"))))
    ;; module function prototype: drop `static` in every piece
    ((and (marv-split-prefix? line "static ") (marv-split-proto-name line))
     (substring line 7 (string-length line)))
    ;; global definition: defined in piece 0, extern elsewhere
    ((marv-split-global-line? line)
     (let ((body (marv-split-strip-static line)))
       (if (= k 0)
           body
           (string-append "extern "
                          (substring body 0 (marv-string-contains-idx body " = "))
                          ";"))))
    (else line)))

(define (marv-split-header-for header k)
  (map (lambda (l) (marv-split-header-line l k))
       (if (= k 0) header (marv-split-drop-ctor header))))

(define (marv-split-chunk blocks n)
  (let loop ((bs blocks) (chunks '()) (cur '()) (cnt 0))
    (cond ((null? bs)
           (reverse (if (null? cur) chunks (cons (reverse cur) chunks))))
          ((>= cnt n)
           (loop bs (cons (reverse cur) chunks) '() 0))
          (else (loop (cdr bs) chunks (cons (car bs) cur) (+ cnt 1))))))

(define (marv-split-write-piece path header blocks)
  (when (file-exists? path) (delete-file path))
  (call-with-output-file path
    (lambda (p)
      (for-each (lambda (l) (display l p) (newline p)) header)
      (for-each
        (lambda (b)
          (let ((ls (cdr b)))
            (display (marv-split-strip-static (car ls)) p) (newline p)
            (for-each (lambda (l) (display l p) (newline p)) (cdr ls))))
        blocks))))

;; split the generated C file at PATH into pieces of at most MAX-FUNCS
;; function definitions; returns the list of piece paths (just (PATH)
;; when no split is needed).  Piece 0 reuses PATH itself.

;; Ensure the program-global trampoline state has exactly one
;; definition in a link (mirrors the split rule: definition in the
;; first piece, extern elsewhere).
(define *marv-link-module-index* 0)

;; does the raw C file at PATH already define the trampoline resume
;; state?  (A MARV-generated .c produced with -c always carries the
;; definitions, so when such a file is linked together with .mvs
;; inputs, no .mvs module may define them again.)
(define (marv-link-file-defines-tc-state? path)
  (and (marv-has-suffix? path ".c")
       (file-exists? path)
       (let loop ((ls (marv-split-read-lines path)))
         (cond ((null? ls) #f)
               ((string=? (car ls) "marv_i64 marv_tc_state[16];") #t)
               (else (loop (cdr ls)))))))

(define (marv-split-tc-state-lines lines module-index)
  (if (= module-index 0)
      lines
      (map (lambda (l)
             (cond
               ((string=? l "marv_i64 marv_tc_fuel = MARV_TC_FUEL;")
                "extern marv_i64 marv_tc_fuel;")
               ((string=? l "marv_i64 marv_tc_state[16];")
                "extern marv_i64 marv_tc_state[16];")
               (else l)))
           lines)))

(define (marv-split-c-temp path max-funcs verbose?)
  (let* ((all-lines (marv-split-tc-state-lines
                      (marv-split-read-lines path)
                      *marv-link-module-index*))
         (an (marv-split-analyze all-lines))
         (header (car an))
         (blocks (cadr an)))
    (if (<= (length blocks) max-funcs)
        (begin
          ;; no split needed, but a non-first module must still drop
          ;; its trampoline state definitions to externs
          (when (> *marv-link-module-index* 0)
            (let ((path2 path))
              (when (file-exists? path2) (delete-file path2))
              (call-with-output-file path2
                (lambda (p)
                  (for-each (lambda (l) (display l p) (newline p))
                            all-lines)))))
          (list path))
        (let* ((mainb (let loop ((bs blocks))
                        (cond ((null? bs) #f)
                              ((string=? (caar bs) "main") (car bs))
                              (else (loop (cdr bs))))))
               (rest (let loop ((bs blocks) (acc '()))
                       (cond ((null? bs) (reverse acc))
                             ((string=? (caar bs) "main") (loop (cdr bs) acc))
                             (else (loop (cdr bs) (cons (car bs) acc))))))
               (chunks0 (marv-split-chunk rest max-funcs))
               (chunks (cond ((not mainb) chunks0)
                             ((null? chunks0) (list (list mainb)))
                             (else (cons (cons mainb (car chunks0))
                                         (cdr chunks0))))))
          (when verbose?
            (fprintf (current-error-port)
                     "marv: split ~a (~a functions) into ~a pieces of at most ~a functions~%"
                     path (length blocks) (length chunks) max-funcs))
          (let loop ((ks chunks) (k 0) (acc '()))
            (if (null? ks)
                (reverse acc)
                (let ((dest (if (= k 0) path (marv-make-temp-obj ".c"))))
                  (marv-split-write-piece dest
                                          (marv-split-header-for header k)
                                          (car ks))
                  (loop (cdr ks) (+ k 1) (cons dest acc)))))))))

(define (marv-main args)
  (let loop ((args args)
             (targdes #f)
             (compile-only? #f)
             (outfile #f)
             (verbose? #f))
    (get-next-command-line-option args *marv-cli-options*
      (lambda (keysym optarg restargs)
        (case keysym
          ((target)
           (loop restargs optarg compile-only? outfile verbose?))
          ((targdir)
           (set! *mstd-target-dirs* (cons optarg *mstd-target-dirs*))
           (loop restargs targdes compile-only? outfile verbose?))
          ((compile)
           (loop restargs targdes #t outfile verbose?))
          ((output)
           (loop restargs targdes compile-only? optarg verbose?))
          ((libdir)
           (set! *mstd-system-dirs* (cons optarg *mstd-system-dirs*))
           (loop restargs targdes compile-only? outfile verbose?))
          ((incdir)
           (set! *mstd-local-dirs* (cons optarg *mstd-local-dirs*))
           (loop restargs targdes compile-only? outfile verbose?))
          ((verbose)
           (loop restargs targdes compile-only? outfile #t))
          ((split)
           (set! *marv-split-funcs* (marv-parse-split-funcs optarg))
           (loop restargs targdes compile-only? outfile verbose?))
          ((help)
           (marv-usage (current-output-port))
           (exit 0))
          ((#f)
           (when (not targdes)
             (fprintf (current-error-port) "marv: missing required -t TARGDES option~%~%")
             (marv-usage (current-error-port))
             (exit 1))
           (when (null? restargs)
             (fprintf (current-error-port) "marv: no input files specified~%~%")
             (marv-usage (current-error-port))
             (exit 1))
           (let* ((mvt-path (find-mvt-file targdes))
                  (mvt-info (load-mvt-file mvt-path))
                  (ttype    (cdr (assq 'type mvt-info)))
                  (obj-ext  (cdr (assq 'obj-ext mvt-info))))
             (if compile-only?
                 ;; Mode 1: `-c` given -- accept one `.mvs` file and generate its object file
                 (begin
                   (when (not (= (length restargs) 1))
                     (fprintf (current-error-port)
                              "marv: -c mode expects exactly one .mvs file (got ~a)~%"
                              (length restargs))
                     (exit 1))
                   (let ((infile (car restargs)))
                     (when (not (marv-has-suffix? infile ".mvs"))
                       (fprintf (current-error-port)
                                "marv: -c input file must have .mvs extension: ~a~%"
                                infile)
                       (exit 1))
                     (let ((dest (or outfile (marv-replace-ext infile obj-ext))))
                       (when verbose?
                         (fprintf (current-error-port)
                                  "marv -t ~a -c -o ~a ~a~%"
                                  targdes dest infile))
                       (when (file-exists? dest) (delete-file dest))
                       (call-with-output-file dest
                         (lambda (p) (compile-marv-file infile ttype p))))))
                 ;; Mode 2: `-c` not given -- compile any `.mvs` files to temporary
                 ;; objects, gather system `.c` dependencies, link with `-o FILE`,
                 ;; and delete temporary objects when done
                 (begin
                   (when (not outfile)
                     (fprintf (current-error-port)
                              "marv: -o FILE is required when -c is not specified~%")
                     (exit 1))
                   ;; If a raw (non-.mvs) C input already defines the
                   ;; trampoline resume state (e.g. a MARV-generated .c
                   ;; from an earlier -c compile), every .mvs module in
                   ;; this link must use externs: pretend one module
                   ;; has already been seen.
                   (when (and (eq? ttype 'cc)
                              (let loop ((fs restargs))
                                (cond ((null? fs) #f)
                                      ((and (not (marv-has-suffix? (car fs) ".mvs"))
                                            (marv-link-file-defines-tc-state? (car fs))) #t)
                                      (else (loop (cdr fs))))))
                     (set! *marv-link-module-index* 1))
                   (let floop ((fs restargs) (objs '()) (temp-objs '()) (sys-cs '()))
                     (if (null? fs)
                         (run-target-linker! mvt-info
                                             outfile
                                             (reverse objs)
                                             (reverse sys-cs)
                                             (reverse temp-objs)
                                             verbose?)
                         (let ((f (car fs)))
                           (if (marv-has-suffix? f ".mvs")
                               (let ((dest (marv-make-temp-obj obj-ext)))
                                 (when verbose?
                                   (fprintf (current-error-port)
                                            "marv -t ~a -c -o ~a ~a~%"
                                            targdes dest f))
                                 (when (file-exists? dest) (delete-file dest))
                                 (call-with-output-file dest
                                   (lambda (p) (compile-marv-file f ttype p)))
                                 ;; *marv-link-module-index* must still be THIS
                                 ;; module's index while marv-split-c-temp runs
                                 ;; (module 0 keeps the trampoline state
                                 ;; definitions, later modules get externs), so
                                 ;; it is incremented only AFTER the split below.
                                 (let ((pieces (cond ((and (eq? ttype 'cc)
                                                           (> *marv-split-funcs* 0))
                                                      (marv-split-c-temp dest *marv-split-funcs* verbose?))
                                                     ((and (eq? ttype 'cc)
                                                           (> *marv-link-module-index* 0))
                                                      ;; splitting disabled: still drop
                                                      ;; this module's trampoline state
                                                      ;; definitions to externs
                                                      (let ((ls (marv-split-tc-state-lines
                                                                 (marv-split-read-lines dest)
                                                                 *marv-link-module-index*)))
                                                        (when (file-exists? dest) (delete-file dest))
                                                        (call-with-output-file dest
                                                          (lambda (p)
                                                            (for-each (lambda (l) (display l p) (newline p)) ls)))
                                                        (list dest)))
                                                     (else (list dest))))
                                       (new-cs (map mvh-path->adapter-c-path
                                                    *last-expanded-sys-mvh-paths*)))
                                   (when (eq? ttype 'cc)
                                     (set! *marv-link-module-index*
                                           (+ *marv-link-module-index* 1)))
                                   (floop (cdr fs)
                                          (append (reverse pieces) objs)
                                          (append (reverse pieces) temp-objs)
                                          (append (reverse new-cs) sys-cs))))
                               (let ((new-cs (extract-sys-c-deps-from-object f)))
                                 (floop (cdr fs)
                                        (cons f objs)
                                        temp-objs
                                        (append (reverse new-cs) sys-cs))))))))))))))))

;; Run CLI automatically when invoked as a program/script
(let ((cmdline (command-line)))
  (when (and (pair? cmdline)
             (let* ((prog (car cmdline))
                    (plen (string-length prog)))
               (and (>= plen 8)
                    (string=? (substring prog (- plen 8) plen) "marv.scm"))))
    (marv-main (cdr cmdline))))


