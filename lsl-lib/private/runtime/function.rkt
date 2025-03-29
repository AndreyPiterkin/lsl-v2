#lang racket

(require racket/class
         "../guard.rkt"
         "../proxy.rkt"
         "contract-common.rkt")

(provide function%)

(define function%
  (class contract%
    (super-new)

    (init-field stx domain-order domains codomain (exns #f))
    (define arity (length domains))


    (define/override (protect val pos)
      (define val-proc? (procedure? val))
      (define val-arity? (and val-proc? (procedure-arity-includes? val arity)))
      (if val-arity?
          (passed-guard
           (lambda (val neg)
             (define (contract-apply . args)
               (define n-args (length args))
               (unless (= n-args arity)
                 (contract-error this stx val pos
                                 #:expected (args-error arity)
                                 #:given (args-error n-args)))


               (define args*
                 (for/fold ([guarded-args '()])
                           ([i domain-order]
                            [domain domains])
                   (define arg (list-ref args i))
                   (define contract (apply domain guarded-args))
                   (define guard (send contract protect arg neg))

                   ;; todo: without append
                   (append guarded-args (list (guard arg pos)))))

               (define result (apply val args*))
               (define guard (send (apply codomain args*) protect result pos))
               (guard result neg))
             (proc val contract-apply this (thunk val))))
          (failed-guard
           (lambda (val neg)
             (if val-proc?
                 (contract-error this stx val pos
                                 #:expected (format ARITY-FMT arity)
                                 #:given (format ARITY-FMT (procedure-arity val)))
                 (contract-error this stx val pos))))))))

(define ARITY-FMT "~a-arity function")

(define (args-error n)
  (if (= n 1)
      (format "~a argument" n)
      (format "~a arguments" n)))