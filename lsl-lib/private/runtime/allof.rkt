#lang racket/base

(require racket/class
         racket/match
         "../guard.rkt"
         "contract-common.rkt")

(provide allof%)

(define allof%
  (class contract%
    (super-new)
    (init-field stx conjuncts)

    ;; Any PositiveBlame -> [Listof Guard]
    (define (apply-contracts val pos)
      (map (lambda (c) (send c protect val pos)) conjuncts))

    ;; Any PositiveBlame -> Any
    (define/override (protect val pos)
      (define guards (apply-contracts val pos))
      (define guard-ctor
        (if (andmap passed-guard? guards)
            passed-guard
            failed-guard))

      (guard-ctor
       (lambda (val neg)
         (for/fold ([val val])
                   ([guard (in-list guards)])
           (guard val neg)))))))