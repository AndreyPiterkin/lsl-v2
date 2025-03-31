#lang lsl-v2

(define-contract or-nat-even (OneOf even? natural?))

(: x or-nat-even)
(define x -1)