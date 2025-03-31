#lang lsl-v2

(define-contract (Leaf X) X)
(define-contract (Tree X) (OneOf (Tree X) (Leaf X)))

