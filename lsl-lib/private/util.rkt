#lang racket/base

(require (for-syntax syntax/id-table
                     racket/string
                     racket/base)
         racket/contract)

(provide (for-syntax contract-table-set!
                     contract-table-ref
                     strip)
         (struct-out exn:fail:gave-up)
         (struct-out exn:fail:invalid)
         (struct-out exn:fail:cyclic)
         (struct-out base-seal)
         any?
         any-list?
         error-if-parametric
         give-up)

;; data
(struct base-seal ())

;; exns
(struct exn:fail:gave-up exn:fail:syntax ())
(struct exn:fail:invalid exn:fail:syntax (witness))
(struct exn:fail:cyclic exn:fail (srclocs)
  #:property prop:exn:srclocs
  (λ (self) (exn:fail:cyclic-srclocs self)))

(define any?
  (flat-named-contract
   'non-parametric?
   (not/c base-seal?)))

(define any-list?
  (flat-named-contract
   'list-without-parametric?
   (λ (xs)
     (and (list? xs) (andmap any? xs)))))

(define (error-if-parametric x)
  (when (base-seal? x)
    (error 'if "cannot use parametric value ~a" x))
  x)


(define (give-up stx)
  (raise
   (exn:fail:gave-up
    "contract-generate: failed to generate value satisfying contract"
    (current-continuation-marks)
    (list (syntax-property stx 'unexpanded)))))


;; syntax

(begin-for-syntax
  ;; free-contract-table: [FreeIdTable ValSyntax ContractStx]
  ;; Maps identifiers to their annotation contracts
  (define free-contract-table (make-free-id-table))

  (define (contract-table-set! id val)
    (free-id-table-set! free-contract-table id val)
    (void))

  (define (contract-table-ref id)
    (free-id-table-ref free-contract-table id #f))

  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

