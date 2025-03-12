#lang racket/base

(require racket/class
         "../guard.rkt"
         "contract-common.rkt")

(provide immediate%)

(define immediate%
  (class contract%
    (super-new)

    (init-field stx check (gen #f) (shrink #f) [features '()])

    (define/override (protect val pos)
      (if (check val)
          (passed-guard
            (λ (val neg)
              val))
          (failed-guard 
            (λ (val neg)
              (contract-error this stx val pos)))))))
