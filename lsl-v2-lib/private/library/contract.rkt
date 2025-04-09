#lang racket

(require racket/require)

(require lsl-v2/private/syntax/sugar
         lsl-v2/private/syntax/spec
         lsl-v2/private/library/number)

(require (filtered-in
          (lambda (name) (regexp-replace #rx"lsl:" name ""))
          (combine-in
           "core.rkt"
           "boolean.rkt"
           "char.rkt"
           "equal.rkt"
           "function.rkt"
           "list.rkt"
           "number.rkt"
           "string.rkt"
           "symbol.rkt")))

;; TODO: provide built-in contracts (Record, Natural, etc)
;; i.e. things that aren't syntax rewrites, but legitimate core defs

(provide (all-defined-out))

;(define-lsl-library lsl:Integer Integer)

(#%lsl
 #;(define-contract Any
     (Immediate
      (check (λ _ #t))
      (generate
       (λ (fuel)
         ((random-ref
           (list (λ () (contract-generate Boolean fuel))
                 (λ () (contract-generate Integer fuel))
                 (λ () (contract-generate Real fuel))
                 (λ () (contract-generate Natural fuel))
                 (λ () (contract-generate String fuel))
                 (λ () (contract-generate Symbol fuel)))))))))

 (define-contract Boolean
   (Immediate (check boolean?)
              (generate (λ (fuel) (< (random) 1/2)))))

 (define-contract True (Immediate (check (λ (x) (equal? x #t)))
                                  (generate (λ (fuel) #t))))
 (define-contract False (Immediate (check (λ (x) (equal? x #f)))
                                   (generate (λ (fuel) #f))))

 (define-contract (Maybe T) (OneOf False T))

 (define-contract Integer
   (Immediate (check integer?)
              (generate (λ (fuel) (if (zero? fuel) 0 (random (* -1 fuel) fuel))))
              (shrink (λ (fuel val)
                        (if (zero? val) 0 (floor (/ val 2)))))))

 (define-contract Natural
   (Immediate (check natural?)
              (generate (λ (fuel) (random 0 (add1 fuel))))))

 (define-contract Real
   (Immediate (check real?)
              (generate (λ (fuel) (- (* 2 fuel (random)) fuel)))))

 (define-contract (NonemptyList X)
   (AllOf (List X) cons?)))