#lang racket

(require syntax/parse)

(provide lsl-literals)

(define-literal-set lsl-literals
  #:datum-literals (cond else if quote #%let #%let* #%letrec #%lambda #%lsl-app #%lsl-id #%rkt-id #%define : define-contract)
  ())