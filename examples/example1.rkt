#lang lsl-v2

(: p (Struct posn (integer? integer?)))
(define-struct posn (x y))

(define p (make-posn 1 2.5))
