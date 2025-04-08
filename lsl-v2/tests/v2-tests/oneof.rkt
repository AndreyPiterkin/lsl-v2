#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/contract
         racket/class
         racket/list
         lsl-v2/private/runtime/oneof
         lsl-v2/private/guard
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (define even-or-bool-ctc
    (new oneof%
         [stx (syntax/unexpanded (OneOf even? boolean?))]
         [disjuncts (list even-ctc bool-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send even-or-bool-ctc protect 2 '+)
   ((send even-or-bool-ctc protect 2 '+) 2 '-)  2

   #:? passed-guard?
   (send even-or-bool-ctc protect #t '+)
   ((send even-or-bool-ctc protect #t '+) #t '-)  #t

   #:? failed-guard?
   (send even-or-bool-ctc protect 3 '+)
   #:x ((send even-or-bool-ctc protect 3 '+) 3 '-)
   "expected: (OneOf even? boolean?)"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x (OneOf integer? boolean?)) (define x 10) x)  10
   (run (: x (OneOf integer? boolean?)) (define x #t) x)  #t
   #:x (run (: x (OneOf integer? boolean?)) (define x 1/2) x)
   "expected: (OneOf integer? boolean?)"
   ))
