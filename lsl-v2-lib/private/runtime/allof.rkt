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

    ;; Any PositiveBlame -> Any
    (define/override (protect val pos)
      (define guards (guards-of val pos))
      (define guard-ctor
        (if (andmap passed-guard? guards)
            passed-guard
            failed-guard))

      (guard-ctor
       (lambda (val neg)
         (for/fold ([val val])
                   ([guard (in-list guards)])
           (guard val neg)))))

    (define (guards-of val pos)
      (let go ([conjuncts conjuncts])
        (match conjuncts
          [(list) null]
          [(cons conjunct conjuncts-rest)
           (define guard (send conjunct protect val pos))
           (if (passed-guard? guard)
               (cons guard (go conjuncts-rest))
               (list guard))])))))