#lang racket

(require syntax-spec-v3
         "../util.rkt"
         "../runtime/contract-common.rkt"
         syntax/location
         (for-syntax syntax/parse
                     "compile.rkt"
                     "../util.rkt"
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
              ((~datum Immediate) ((~datum check) pred:racket-expr)
                                  ((~datum generate) gen:racket-expr)
                                  ((~datum shrink) shrk:racket-expr)
                                  ((~datum feature) feat-name:string feat:racket-expr) ...)
              ;; TODO: other contract forms
              id:ctc-nt)

 ;; TODO: is there a point compiling the contract here? the version in the contract-table
 ;; is the version compiled in :/internal
 (host-interface/definition
  (define-contract/internal c:ctc-nt contract-body:ctc unexpanded:expr)
  #:binding (export c)
  #:lhs [#'c]
  #:rhs [(compile-contract #'contract-body #'unexpanded)])

 (host-interface/definition
  (define-protected v:racket-var b:racket-expr)
  #:binding (export v)
  #:lhs [#'v]
  #:rhs [(define maybe-ctc (contract-table-ref #'v))
         (if maybe-ctc
             #`((send #,maybe-ctc protect b (positive-blame 'v (quote-module-name))) b (negative-blame 'v (quote-module-name)))
             #'b)])

 (host-interface/expression
  (:/internal v:racket-var c:ctc unexpanded:expr)
  (define maybe-ctc (contract-table-ref #'v))
  (when maybe-ctc
    (raise-syntax-error (syntax-e #'v) (format "contract previously declared for ~a" (syntax-e #'v)) maybe-ctc))

  (define compiled-ctc (compile-contract #'c #'unexpanded))
  (contract-table-set! #'v compiled-ctc)
  #'(void)))



;; TODO: define-contract and : look very similar, should refactor
(define-syntax (define-contract stx)
  (syntax-parse stx
    [(_ head:id b)
     (contract-add! #'head)
     (define/syntax-parse body-stx
       (syntax-parse #'b
         [i:id
          (if (contract? #'i)
              #'i
              #'(Immediate (check i) (generate #f) (shrink #f)))]
         [((~datum Immediate) ~! (~alt (~once ((~datum check) pred:expr))
                                       (~once ((~datum generate) gen:expr))
                                       (~once ((~datum shrink) shrk:expr))
                                       ((~datum feature) feat-name:string feat:expr)) ...)
          #'(Immediate (check pred)
                       (generate gen)
                       (shrink shrk)
                       (feature feat-name feat) ...)]
         [e:expr
          #'(Immediate (check e) (generate #f) (shrink #f))])) 
     #'(define-contract/internal head body-stx b)]))


(define-syntax (: stx)
  (syntax-parse stx
    [(_ head:id b)
     (define/syntax-parse body-stx
       (syntax-parse #'b
         [i:id
          (if (contract? #'i)
              #'i
              #'(Immediate (check i) (generate #f) (shrink #f)))]
         [((~datum Immediate) ~! (~alt (~once ((~datum check) pred:expr))
                                       (~once ((~datum generate) gen:expr))
                                       (~once ((~datum shrink) shrk:expr))
                                       ((~datum feature) feat-name:string feat:expr)) ...)
          #'(Immediate (check pred)
                       (generate gen)
                       (shrink shrk)
                       (feature feat-name feat) ...)]
         [e:expr
          #'(Immediate (check e) (generate #f) (shrink #f))]))
     #'(:/internal head body-stx b)]))

(define-syntax (define stx)
  (syntax-parse stx
    [(_ h b)
     #'(define-protected h b)]))
