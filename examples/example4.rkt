#lang lsl-v2

(define-contract (Leaf X) X)
(define-contract (Tree X) (OneOf (Tuple X (Tree X) (Tree X)) (Leaf X)))

(define-contract NatTree (Tree natural?))

(: tree1 NatTree)
(define tree1 (list 1 (list 2 3 4) 5))