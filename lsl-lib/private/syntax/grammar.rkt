#lang racket

(require syntax/parse)

(provide lsl-literals
         contract-literals)

;; Set of literals that belong in lsl-form position
(define-literal-set lsl-literals
  #:datum-literals (cond else if quote #%let #%let* #%letrec #%lambda #%lsl-app #%lsl-id #%rkt-id #%define : define-contract)
  ())

;; Set of literals that are found in contract position
(define-literal-set contract-literals
  #:datum-literals (#%ctc-id #%ctc-app #%lsl-expr #%Immediate check generate shrink feature #%Function arguments result raises)
  ())