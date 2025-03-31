#lang lsl-v2

(define-contract or-nat-even (OneOf even? natural?))
(define-contract and-nat-even (AllOf even? natural?))

(: x or-nat-even)
(define x -2)

(: y and-nat-even)
(define y -1)