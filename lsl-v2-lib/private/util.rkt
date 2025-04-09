#lang racket/base

(require (for-syntax racket/string
                     racket/base
                     syntax/parse)
         racket/contract)

(provide (for-syntax strip)
         (struct-out exn:fail:gave-up)
         (struct-out exn:fail:invalid)
         (struct-out base-seal)
         (struct-out none)
         any?
         any-list?
         error-if-parametric
         give-up
         λ/memoize)

;; data
(struct base-seal ())
(struct none ())

;; exns
(struct exn:fail:gave-up exn:fail:syntax ())
(struct exn:fail:invalid exn:fail:syntax (witness))

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
  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

(define-syntax λ/memoize
  (syntax-parser
    [(_ args:id body:expr)
     #'(let ([table (make-hash)])
         (λ args (hash-ref! table args (λ () body))))]))
