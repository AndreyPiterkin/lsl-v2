#lang racket/base

(require racket/contract
         "../proxy.rkt"
         "../util.rkt"
         "../syntax/spec.rkt")

(provide lsl:equal? lsl:eq?)

(define (internal-eq? x y)
  (eq? (unproxy-eq x) (unproxy-eq y)))

;; TODO: remove custom unproxy-eq call here
(define (unproxy-eq st)
  (if (proxy? st)
      (unproxy-eq (proxy-target st))
      st))

(define-contracted-lsl-library
  (lsl:equal? (-> any? any? any?) equal?)
  (lsl:eq? (-> any? any? any?) internal-eq?))
