#lang racket/base

(require racket/class
         racket/list
         "../guard.rkt"
         "contract-common.rkt")

(provide list%)

(define list%
  (class contract%
    (super-new)
    (init-field stx fixed? contracts)

    ;; Natural -> [Listof Contract]
    ;; Creates a list of contracts of the given length, or returns
    ;; this contracts' list if it is a fixed-length.
    (define (contracts-list len)
      (if fixed?
          contracts
          (make-list (first contracts) len)))

    (define/override (protect val pos)
      (define is-list? (list? val))
      ;; TODO: this is super ugly
      (define val-length (and is-list?
                              (length val)))
      (define contracts^ (and is-list?
                              (contracts-list val-length)))
      (define guard-args (and is-list?
                              (= val-length (length contracts^))
                              (list contracts^ val)))
      (define guards (and guard-args
                          (apply map
                                 (lambda (ctc elem)
                                   (send ctc protect elem pos))
                                 guard-args)))
      (define guard-ctor
        (if (and guards (andmap passed-guard? guards))
            passed-guard
            failed-guard))
      (guard-ctor
       (lambda (val neg)
         (unless guards
           (contract-error this stx val pos))
         (map (lambda (guard elem) (guard elem neg)) guards val))))))