#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         lsl-v2/private/runtime/function
         lsl-v2/private/runtime/immediate
         lsl-v2/private/guard
         lsl-v2/private/proxy
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (define pte-ctc
    (new function%
         [stx (syntax/unexpanded (-> positive? even?))]
         [domain-order '(0)]
         [domains (list (lambda _ pos-ctc))]
         [codomain (lambda _ even-ctc)]
         [exns (list)])))

;; TODO: multiple domains
;; TODO: dependent domains
;; TODO: exceptions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:do (define (dbl x) (* 2 x))

   #:? passed-guard?
   (send pte-ctc protect dbl '+)
   #:do (define dbl* ((send pte-ctc protect dbl '+) dbl '-))

   #:? failed-guard?
   (send pte-ctc protect 2 '+)
   #:x ((send pte-ctc protect 2 '+) 2 '-)
   "(-> positive? even?)"

   #:do (define fst (lambda (x y) x))
   #:? failed-guard?
   (send pte-ctc protect fst '+)
   #:x ((send pte-ctc protect fst '+) fst '-)
   "given: 2-arity function"

   (dbl* 2)  4
   #:x (dbl* -1)
   "expected: positive?"

   ;; TODO: shrink
   ;; TODO: interact
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (: f (-> integer? integer?))
        (define (f x) x)
        (f 10))
   10

   (run (: f (Function (arguments [x integer?]
                                  [y (Immediate (check (lambda (z) (eq? x z))))])
                       (result integer?)))
        (define (f x y) (+ x y))
        (f 10 10))
   20

   (run (: f (Function (arguments [x integer?])
                       (result (Immediate (check (lambda (y) (eq? x y)))))))
        (define (f x) x)
        (f 10))
   10

   (run (: f (Function (arguments [x integer?])
                       (result (lambda (y) (eq? x y)))))
        (define (f x) x)
        (f 10))
   10

   #:x (run (: f (-> integer? boolean?))
            (define (f x) x)
            (f 10))
   "expected: boolean?"

   #:x (run (: f (Function (arguments [x integer?])
                           (result (Immediate (check (lambda (y) (eq? x y)))))))
            (define (f x) (+ x 1))
            (f 10))
   "expected: (Immediate (check (lambda (y) (eq? x y))))"

  #:x (run (: f (Function (arguments [x (Immediate (check (lambda (z) (eq? y z))))]
                                     [y (Immediate (check (lambda (z) (eq? x z))))])
                          (result integer?)))
           (define (f x y) (+ x y))
           (f 10 20))
  "cannot have cyclic dependency"

  #:x (run* (: f (-> integer? integer?))
            (define (f x y) x))
  "expected: 1-arity function"

  #:x
  (run (: f (-> integer? integer? integer?))
       (define (f x y) x)
       (f 1))
  "expected: 2 arguments"
  ))
