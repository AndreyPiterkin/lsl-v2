#lang racket/base

(require racket/class
         racket/match
         "../guard.rkt"
         "contract-common.rkt")

(provide oneof%)

(define oneof%
  (class contract%
    (super-new)
    (init-field stx disjuncts)

    ;; Any PositiveBlame -> [Listof Guard]
    (define (apply-contracts val pos)
      (map (lambda (c) (send c protect val pos)) disjuncts))

    ;; Any PositiveBlame -> Any
    (define/override (protect val pos)
      (define maybe-guard
        (for/or ([ctc disjuncts])
          (define guard (send ctc protect val pos))
          (if (passed-guard? guard)
              guard
              #f)))
      (if maybe-guard
          maybe-guard
          (failed-guard
          (lambda (val neg)
            (send this contract-error stx val pos)))))))