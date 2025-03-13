#lang racket

(require racket/class
         "../guard.rkt"
         "contract-common.rkt")

(provide function%)

(define function%
  (class contract%
    (super-new)

    (init-field stx arg-order args result (exns #f))))