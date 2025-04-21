#lang racket/base

(require racket/class
         racket/struct
         "../guard.rkt"
         "contract-common.rkt")

(provide struct%)

(define struct%
  (class contract%
    (super-new)
    (init-field stx ctor pred accessors contracts)

    (define/override (protect val pos)
      (protect/immutable val pos))

    (define (protect/immutable val pos)
      (define guards
        (and (pred val)
             (for/list ([ctc (in-list contracts)]
                        [field (in-list (struct->list val))])
               (send ctc protect field pos))))
      (define struct-as-list (struct->list val))
      (if (and guards (andmap passed-guard? guards))
          (passed-guard
           (lambda (val neg)
             (define fields
               (for/list ([ctc (in-list contracts)]
                          [field (in-list struct-as-list)])
                 ((send ctc protect field pos) field neg)))
             (apply ctor fields)))
          (failed-guard
           (lambda (val neg)
             (unless guards
               (send this contract-error stx val pos))
             (for ([guard (in-list guards)]
                   [field (in-list struct-as-list)])
               (guard field neg))))))))