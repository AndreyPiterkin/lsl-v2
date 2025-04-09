#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/class
         lsl-v2/private/runtime/struct
         lsl-v2/private/guard
         lsl-v2/private/proxy
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  (provide (all-defined-out))

  (require (submod "immediate.rkt" examples))

  (struct eb root (e b)
    #:transparent
    #:mutable
    #:methods gen:equatable
    [(define (base-equal? self other)
       (and (equal? (eb-e self) (eb-e other))
            (equal? (eb-b self) (eb-b other))))])

  (define eb-struct-ctc
    (new struct%
         [stx (syntax/unexpanded (Thing Even Boolean))]
         [ctor eb]
         [pred eb?]
         [accessors (list eb-e eb-b)]
     ;     [mutators (list set-eb-e! set-eb-b!)]
         [contracts (list even-ctc bool-ctc)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit tests

(module+ test
  (require (submod ".." examples))

  (chk
   #:? passed-guard?
   (send eb-struct-ctc protect (eb 2 #f) '+)
   ((send eb-struct-ctc protect (eb 2 #f) '+) (eb 2 #f) '-)  (eb 2 #f)

   #:? failed-guard?
   (send eb-struct-ctc protect (eb 2 2) '+)
   #:x ((send eb-struct-ctc protect (eb 2 2) '+) (eb 2 2) '-)
   "expected: boolean?"

   #:? failed-guard?
   (send eb-struct-ctc protect (eb 3 #f) '+)
   #:x ((send eb-struct-ctc protect (eb 3 #f) '+) (eb 3 #f) '-)
   "expected: even?"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (define-struct foo (x))
        (format "~v" (make-foo 7)))
   "(make-foo 7)"

   (run (define-struct foo (x))
        (: f (-> (Struct foo (Integer)) Integer))
        (define (f st) (foo-x st))
        (f (make-foo 10)))
   10

   #:t
   (run (define-struct foo ())
        (: bar (-> (Struct foo []) Boolean))
        (define (bar v)
          (foo? v))
        (bar (make-foo)))

   #:x
   (run (define-struct foo (x))
        (: f (-> (Struct foo (Integer)) Integer))
        (define (f st) (foo-x st))
        (f (make-foo 1/2)))
   "expected: (Imm"
   ))
