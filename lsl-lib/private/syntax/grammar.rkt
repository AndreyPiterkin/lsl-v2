#lang racket

(require syntax/parse)

(provide lsl-literals
         contract-literals)

(define-literal-set lsl-literals
  #:datum-literals (cond else if quote #%let #%let* #%letrec #%lambda #%lsl-app #%lsl-id #%rkt-id #%define : define-contract)
  ())

(define-literal-set contract-literals
  #:datum-literals (#%ctc-id #%Immediate check generate shrink feature)
  ())