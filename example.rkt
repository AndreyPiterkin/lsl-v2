#lang lsl-v2

(define-contract Even (Immediate (check even?)))

(: x Even)
(define x 5)