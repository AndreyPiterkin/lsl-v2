#lang racket/base

(require racket/class
         racket/promise
         "contract-common.rkt")

(provide lazy%)

(define lazy%
  (class contract%
    (super-new)
    (init-field stx promise)

    (define/override (protect val pos)
      (send (force promise) protect val pos))))