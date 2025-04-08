#lang racket/base

(require syntax-spec-v3
         "compile.rkt"
         racket/contract
         (for-syntax racket/base
                     syntax/parse
                     (only-in syntax-spec-v3/private/ee-lib/main lookup in-space)))

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         (for-space lsl (all-defined-out)))

(begin-for-syntax
  ;; Stx Stx -> Stx
  ;; Tags the new syntax with the old syntax under the property 'unexpanded
  (define (tag-syntax-with-unexpanded new-stx old-stx)
    (syntax-property new-stx 'unexpanded old-stx))

  (define-syntax-class extension-class
    (pattern (~or* (~datum lsl-form-macro)
                   (~datum contract-macro))))

  (define (transform stx transformer)
    (define stx^ (transformer stx))
    (tag-syntax-with-unexpanded stx^ stx)))

(define-syntax define-lsl-syntax
  (syntax-parser
    [(_ name:id extension:extension-class transformer:expr)
     #'(define-dsl-syntax name extension
         (lambda (stx)
           (transform stx transformer)))]))

(define-syntax define-lsl-form-syntax
  (syntax-parser
    [(_ name:id transformer:expr)
     #'(define-lsl-syntax name lsl-form-macro transformer)]))

(define-syntax define-contract-syntax
  (syntax-parser
    [(_ name:id transformer:expr)
     #'(define-lsl-syntax name contract-macro transformer)]))



(syntax-spec
 (binding-class lsl-id #:description "lsl binding")
 (binding-class ctc-id #:description "contract binding")
 (extension-class lsl-form-macro)
 (extension-class contract-macro)

 (nonterminal/exporting
  lsl-form
  #:description "lsl top-level form"
  #:allow-extension lsl-form-macro
  ;; shadowing identifiers like "provide"
  #:binding-space lsl
  (provide v:lsl-id ...)
  
  e:lsl-def-or-expr
  #:binding (re-export e))

 (nonterminal/exporting
  lsl-def-or-expr
  #:allow-extension lsl-form-macro

  (#%define-contract def:ctc-id c:ctc)
  #:binding (export def)

  (: v:lsl-id c:ctc)

  (#%define v:lsl-id e:lsl-expr)
  #:binding (export v)
  e:lsl-expr)

 (nonterminal
  lsl-expr
  #:description "lsl expression"
  #:allow-extension lsl-form-macro
  ;; shadowing quote, etc
  #:binding-space lsl

  (quote t:literal)
  i:lsl-id
 

  ;; TODO: optional else clause (macro)
  (cond [c:lsl-expr e:lsl-expr] ...
        [(~datum else) else:lsl-expr])

  ;; TODO: other core/special forms: and, or, set!, require, define-struct,
  ;;                                 define-mutable-struct, raise

  (if c:lsl-expr
      t:lsl-expr
      e:lsl-expr)

  (#%lambda (v:lsl-id ...) b:lsl-expr)
  #:binding (scope (bind v) ... b)

  (#%let ([v:lsl-id e:lsl-expr] ...)
         b:lsl-expr)
  #:binding (scope (bind v) ... b)

  ;; todo: is this the correct semantics?
  (#%letrec ([v:lsl-id e:lsl-expr] ...)
            body:lsl-expr)
  #:binding (scope (bind v) ... body)

  (#%lsl-app f:lsl-expr arg:lsl-expr ...)

  (~> (f:expr e:expr ...)
      (tag-syntax-with-unexpanded #'(#%lsl-app f e ...) this-syntax))

  (~> (~or lit:number lit:string lit:boolean)
      (tag-syntax-with-unexpanded #'(quote lit) this-syntax)))

 (nonterminal literal
              #:description "literal value"
              n:number
              s:string
              b:boolean
              i:id)

 (nonterminal
  ctc
  #:description "contract"
  #:allow-extension contract-macro
  (#%ctc-id i:ctc-id)

  (#%contract-lambda (arg:ctc-id ...) c:ctc)
  #:binding (scope (bind arg) ... c)

  (#%ctc-app i:ctc-id args:ctc ...)

  (#%Immediate ((~datum check) pred:lsl-expr)
               ((~datum generate) gen:lsl-expr)
               ((~datum shrink) shrk:lsl-expr)
               ((~datum feature) feat-name:lsl-expr feat:lsl-expr) ...)

  (#%Function ((~datum arguments) [v:lsl-id e:ctc] ...)
              ((~datum result) c:ctc))
  #:binding (scope (bind v) ... e ... c)

  (#%OneOf c:ctc ...)
  (#%AllOf c:ctc ...)
  (#%Tuple c:ctc ...)
  (#%List c:ctc)

  (~> x:id
      #:when (lookup #'x (binding-class-predicate ctc-id))
      (tag-syntax-with-unexpanded #'(#%ctc-id x) this-syntax))

  (~> (i:id e:expr ...)
      #:when (lookup #'i (binding-class-predicate ctc-id))
      (tag-syntax-with-unexpanded #'(#%ctc-app i e ...) this-syntax))

  (~> e:expr
      (tag-syntax-with-unexpanded #'(#%Immediate (check e) (generate #f) (shrink #f)) this-syntax))) 

 (host-interface/definitions
  (#%lsl e:lsl-form ...)
  #:binding ((re-export e) ...)
  ;; TODO: expand to explicit module begin; for some reason I get a massive error
  #'(begin (compile-lsl/lsl-form e) ...))

 (host-interface/definitions
  (define-lsl-library (v:lsl-id r:racket-expr)...+)
  #:binding ((export v) ...)
  #'(begin (define v r) ...))

 (host-interface/definitions
  (define-contracted-lsl-library (v:lsl-id c:racket-expr e:racket-expr) ...+)
  #:binding ((export v) ...)
  #'(begin (define/contract v c e) ...)))