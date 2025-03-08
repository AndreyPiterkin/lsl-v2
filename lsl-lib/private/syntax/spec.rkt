#lang racket

(require syntax-spec-v3
         "../util.rkt"
         "../runtime/contract-common.rkt"
         syntax/location
         (for-syntax syntax/parse
                     "compile.rkt"
                     syntax/stx))

(provide (all-defined-out)
         (for-space ctc (all-defined-out))
         (for-syntax (all-defined-out)))

(syntax-spec
 (binding-class ctc-nt #:description "contract binding")
 (extension-class ctc-macro)

 (nonterminal ctc
              #:description "contract"
              #:binding-space ctc
              #:allow-extension ctc-macro
              (#%ctc-expanded ((~datum Immediate) ((~datum check) pred:racket-expr)
                                                  ((~datum generate) gen:racket-expr)
                                                  ((~datum shrink) shrk:racket-expr)
                                                  ((~datum feature) feat-name:string feat:racket-expr) ...))
              id:ctc-nt
              e:racket-expr)

 (host-interface/definition
  (define-contract c:ctc-nt contract-body:ctc)
  #:binding (export c)
  #:lhs [#'c]
  #:rhs [(compile-contract #'contract-body)])

 (host-interface/definition
  (define-protected v:racket-var b:racket-expr)
  #:binding (export v)
  #:lhs [#'v]
  #:rhs [(define maybe-ctc (contract-table-ref #'v))
         (if maybe-ctc
             #`((send #,maybe-ctc protect b (positive-blame 'v (quote-module-name))) b (negative-blame 'v (quote-module-name)))
             #'b)])

 (host-interface/expression
  (: v:racket-var c:ctc)
  
  (define compiled-ctc (compile-contract #'c))
  (define maybe-ctc (contract-table-ref #'v))
  (when maybe-ctc
    (raise-syntax-error (syntax-e #'v) (format "contract previously declared for ~a" (syntax-e #'v)) maybe-ctc))

  (contract-table-set! #'v compiled-ctc)
  #'(void)))

(define-dsl-syntax Immediate ctc-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ ~! (~alt (~once ((~datum check) pred:expr))
                   (~once ((~datum generate) gen:expr))
                   (~once ((~datum shrink) shrk:expr))
                   ((~datum feature) feat-name:string feat:expr)) ...)
       #:with new-stx #'(#%ctc-expanded Immediate (check pred) (generate gen) (shrink shrk) (feature feat-name feat) ...)
       (syntax-property (datum->syntax #'new-stx (syntax-e #'new-stx) stx #'new-stx) 'unexpanded stx)])))