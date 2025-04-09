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
 (define-contract Integer
   (Immediate (check lsl:integer?)
              (generate (λ (fuel) (if (zero? fuel) 0 (random (* -1 fuel) fuel))))
              (shrink (λ (fuel val)
                        (if (zero? val) 0 (floor (/ val 2))))))))