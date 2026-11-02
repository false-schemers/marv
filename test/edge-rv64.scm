;; =============================================================================
;; test/edge-rv64.scm -- run test/edge.mvs's exported functions on the RV64G
;; simulator and print them in the same form the cc build does, so both can be
;; diffed against test/edge.expected.   [cco]
;;
;;   marv -T target -t rv64g -c -o /tmp/gh_marv_edge.s test/edge.mvs
;;   skint test/edge-rv64.scm
;; =============================================================================

(import (scheme base) (scheme char)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (skint)
        (skint hidden))

(include "rv64-sim.scm")

(define asm-file
  (let ((args (command-line)))
    (if (and (pair? args) (pair? (cdr args))) (cadr args) "/tmp/gh_marv_edge.s")))

(define sim (rv64-load-asm asm-file))

(define (show name args)
  (display name)
  (display "=")
  (display (rv64-call sim name args))
  (newline))

;; t23 takes the br_table index; every other entry point takes none
(for-each (lambda (n) (show n '()))
          '("t01" "t02" "t03" "t04" "t05" "t06" "t07" "t08" "t09" "t10" "t11" "t12"
            "t13" "t14" "t15" "t16" "t17" "t18" "t19" "t20" "t21" "t22"))
(show "t23" '(1))
(show "t24" '())
