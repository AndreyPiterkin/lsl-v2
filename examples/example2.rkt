#lang lsl-v2

(: times (Function (arguments (n integer?)
                              (m integer?))
                   (result (lambda (r) (= r (* n m))))))
(define (times n m)
  (+ n m))

(times 2 1)