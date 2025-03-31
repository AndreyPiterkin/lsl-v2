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
      (define guards (apply-contracts val pos))
      (match (filter passed-guard? guards)
        [(list g)
         g]
        [_
         (failed-guard
          (lambda (val neg)
            (contract-error this stx val pos)))]))))