#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         lsl-v2/private/runtime/allof
         lsl-v2/private/guard
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (define pos-even-ctc
    (new allof%
         [stx (syntax/unexpanded PositiveEven)]
         [conjuncts (list even-ctc pos-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send pos-even-ctc protect 2 '+)
   ((send pos-even-ctc protect 2 '+) 2 '-)  2

   #:? failed-guard?
   (send pos-even-ctc protect -2 '+)
   #:x ((send pos-even-ctc protect -2 '+) -2 '-)
   "expected: positive?"

   #:? failed-guard?
   (send pos-even-ctc protect 3 '+)
   #:x ((send pos-even-ctc protect 3 '+) 3 '-)
   "expected: even?"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x (AllOf integer? (Immediate (check positive?)))) (define x 10) x)  10
   #:x (run (: x (AllOf integer? (Immediate (check positive?)))) (define x #f) x)
   "expected: integer?"
   #:x (run (: x (AllOf integer? (Immediate (check positive?)))) (define x -10) x)
   "expected: (Immediate (check positive?))"
   ))
