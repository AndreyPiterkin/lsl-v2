#lang racket

(require syntax/parse)

(provide lsl-literals)

(define-literal-set lsl-literals
  #:datum-literals (and or cond else if quote begin local let let* letrec lambda define : define-contract)
  ())