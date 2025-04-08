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
   (run (define-contract Leaf false?)
        (define-contract (Tree X) (OneOf (Tuple X (Tree X) (Tree X)) Leaf))
        (define-contract NatTree (Tree natural?))

        (: tree1 NatTree)
        (define tree1 #f)

        (: tree2 NatTree)
        (define tree2 (list 1 #f #f))

        (: tree3 NatTree)
        (define tree3 (list 1 (list 2 #f #f) (list 3 #f #f)))
        tree3) (list 1 (list 2 #f #f) (list 3 #f #f))

   #:x (run (: x (OneOf integer? boolean?)) (define x 1/2) x)
   "expected: (OneOf integer? boolean?)"
   #:x (run (define-contract or-nat-even (OneOf even? natural?))
            (define-contract and-nat-even (AllOf even? natural?))

            (: x or-nat-even)
            (define x -2)

            (: y and-nat-even)
            (define y -1)
            y) ;; TODO: Why is this `y` needed to make the test pass?
   "expected: even?"

   #:x(run (define-contract Leaf false?)
        (define-contract (Tree X) (OneOf (Tuple X (Tree X) (Tree X)) Leaf))
        (define-contract NatTree (Tree natural?))

        (: tree1 NatTree)
        (define tree1 (list 1 (list 2 (list -1 #f #f) #f) (list 3 #f #f)))
        tree1) "expected: (OneOf (Tuple X (Tree X) (Tree X)) Leaf)"
   ))
