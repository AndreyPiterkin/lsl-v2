#lang racket

(require syntax/parse)

(provide lsl-literals)

(define-literal-set lsl-literals
  #:datum-literals (and or cond else if quote #%let #%let* #%letrec #%lambda #%lsl-app #%define : define-contract)
  ())