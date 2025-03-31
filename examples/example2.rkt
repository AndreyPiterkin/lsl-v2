#lang lsl-v2

(: times (Function (arguments (n integer?)
                              (m (lambda (x)
                                   (>= x n))))
                   (result (lambda (r) (= r (* n m))))))
(define (times n m)
  (* n m))

(times 1 2)