#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         lsl-v2/private/runtime/immediate
         lsl-v2/private/guard
         lsl-v2/private/util
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (define bool-ctc
    (new immediate%
         [stx (syntax/unexpanded boolean?)]
         [checker boolean?]
         [generator (lambda (fuel) (< (random) 1/2))]
         [shrinker (lambda (fuel val) #f)]))

  (define pos-ctc
    (new immediate%
         [stx (syntax/unexpanded positive?)]
         [checker (lambda (x) (and (real? x) (positive? x)))]
         [generator (lambda (fuel) (+ 1 (if (zero? fuel) 0 (random fuel))))]
         [shrinker (lambda (fuel val) (floor (/ val 2)))]))

  (define even-ctc
      (new immediate%
          [stx (syntax/unexpanded even?)]
          [checker (lambda (x) (and (integer? x) (even? x)))]
          [generator (lambda (fuel) (if (zero? fuel) 0 (* 2 (random fuel))))]
          [shrinker (lambda (fuel val)
                      (define val* (floor (/ val 2)))
                      (if (odd? val*) (sub1 val*) val*))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send even-ctc protect 2 #f)
   ((send even-ctc protect 2 #f) 2 #f)  2

   #:? failed-guard?
   (send even-ctc protect 3 #f)
   #:x ((send even-ctc protect 3 #f) 3 #f)
   "expected: even?"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: x integer?) (define x 10) x)  10
   #:x (run (: x integer?) (define x 1/2) x)  "expected: integer?"

   (run (: x real?) (define x 3.14) x)  3.14
   #:x (run (: x real?) (define x #t) x)  "given: #t"

   (run (: x boolean?) (define x #t) x)  #t
   #:x (run (: x boolean?) (define x 0) x)  "(as server)"

   #:do (define even-sexp
          '(define-contract Even
             (Immediate (check (lambda (x) (if (integer? x) (even? x) #f))))))
   (run/sexp even-sexp '(: x Even) '(define x 2) 'x)  2
   #:x (run/sexp even-sexp '(: x Even) '(define x 1) 'x)  "expected: (Imm" ;; TODO: Fix error message.

   #:x
   (run (define (f x)
          (local [(: g (-> integer? integer?))
                  (define (g y) y)]
            (g x)))
        (f ""))
   "expected: integer?"))
