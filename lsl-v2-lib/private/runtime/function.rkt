#lang racket

(require racket/class
         "../guard.rkt"
         "../proxy.rkt"
         racket/function
         "../util.rkt"
         "contract-common.rkt"
         )

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

               (define args^
                 (for/fold ([guarded-args '()])
                           ([i domain-order]
                            [domain domains])
                   (define arg (list-ref args i))
                   (define contract (apply domain guarded-args))
                   (define guard (send contract protect arg neg))
                   ;; have to append because it has to be constructed in the un-reversed order
                   (append guarded-args (list (guard arg pos)))))

               ;; reconstruct the order of the arguments
               (define args^^
                 (for/fold ([args (build-list n-args (lambda (_) 0))])
                           ([i domain-order]
                            [arg args^])
                   (list-set args i arg)))

               (define result (apply val args^^))
               (define guard (send (apply codomain args^^) protect result pos))
               (guard result neg))
             (proc val contract-apply this (thunk val))))

          (failed-guard
           (lambda (val neg)
             (if val-proc?
                 (contract-error this stx val pos
                                 #:expected (format ARITY-FMT arity)
                                 #:given (format ARITY-FMT (procedure-arity val)))
                 (contract-error this stx val pos))))))

    (define/override (generate fuel)
      (define generated
        (λ/memoize args (send (apply codomain args) generate fuel)))
      (procedure-reduce-arity generated arity))

    (define/override (interact val name contract->value)
      ;; Instantiates a contract with its dependents then generates a value.
      (define ((dom-apply acc) dom)
        (define ctc (apply dom acc))
        (contract->value ctc))
      (define args (foo domains domain-order dom-apply empty))
      (cond
        [(ormap none? args) #f]
        [else
         (define init-exn (fail-exn val args))
         (cond
           [init-exn
            (define-values (best-args best-exn)
              (find-best-args val args init-exn))
            (list (if (empty? best-args)
                      (format "(~a)" name)
                      (format "(~a ~a)" name (string-join (map ~v best-args))))
                  best-exn)]
           [else #f])]))

    (define (find-best-args val args last-exn)
      (define args* (shrink* args))
      (cond
        [(or (ormap none? args*) (equal? args args*))
         (values args last-exn)]
        [(fail-exn val args*) => (λ (exn) (find-best-args val args* exn))]
        [else (values args last-exn)]))

    ;; Produces shrunken arguments.
    (define (shrink* args)
      (define ((shrink-apply acc k) dom)
        (define ctc (apply dom acc))
        (send ctc shrink (list-ref args k) 50)) ;; TODO: Do fuel properly.
      (bar domains domain-order shrink-apply empty 0))

    (define (fail-exn val args)
      (define x
        (with-handlers ([exn? values])
          (apply val args)))
      (cond
        [(exn:fail:lsl:user? x)
         (define val (exn:fail:lsl:user-val x))
         (and (not (ormap (λ (pred?) (pred? val)) exns)) x)]
        [else
         (and (exn? x) x)]))))

(define ARITY-FMT "~a-arity function")

(define (args-error n)
  (if (= n 1)
      (format "~a argument" n)
      (format "~a arguments" n)))

(define (list-update-many xs ks f)
  (for/fold ([acc xs])
            ([k (in-list ks)])
    (list-update acc k (f acc k))))

;; Generates random inputs for the domains.
(define (foo domains ks f acc)
  (cond [(empty? domains) acc]
        [(cons? domains) (foo (rest domains) ks f (cons ((f acc) (first domains)) acc))]))

;; Produces shrinked versions of the inputs for the domains.
(define (bar domains ks f acc idx)
  (cond [(empty? domains) acc]
        [(cons? domains) (bar (rest domains) ks f (cons ((f acc idx) (first domains)) acc) (add1 idx))]))
