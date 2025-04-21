#lang racket

(require racket/class
         "../guard.rkt"
         "../util.rkt"
         "contract-common.rkt")

(provide immediate%)

(define immediate%
  (class contract%
    (super-new)

    (init-field stx checker (generator #f) (shrinker #f) [features '()])

    (define/override (protect val pos)
      (if (checker val)
          (passed-guard
           (λ (val neg)
             val))
          (failed-guard 
           (λ (val neg)
             (send this contract-error stx val pos)))))

    (define/override (generate fuel)
      (cond
        [generator
         (define val (generator fuel))
         (unless (checker val)
           (raise
            (exn:fail:invalid
             (format "contract-generate: generated value ~a does not satisfy contract" val)
             (current-continuation-marks)
             (list stx)
             val)))
         val]
        [else (give-up stx)]))

    (define/override (shrink fuel val)
      (if shrinker (shrinker fuel val) val))

    (define/override (interact val name mode)
      #f)

    (define/override (describe val)
      (for/list ([feat (in-list features)])
        (match-define (list name func) feat)
        (cons (string->symbol name) (func val))))))
