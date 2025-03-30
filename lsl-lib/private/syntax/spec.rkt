#lang racket/base

(require syntax-spec-v3
         "compile.rkt"
         (for-syntax syntax/parse
                     "compile.rkt"
                     (only-in syntax-spec-v3/private/ee-lib/main lookup in-space)
                     (except-in racket/base
                                string)))

(provide #%lsl
         (for-syntax (all-defined-out))
         (for-space lsl (all-defined-out)))

(begin-for-syntax
  ;; Stx Stx -> Stx
  ;; Tags the new syntax with the old syntax under the property 'unexpanded
  (define (tag-syntax-with-unexpanded new-stx old-stx)
    (syntax-property new-stx 'unexpanded old-stx))

  (define ctc-ref-compiler
    (make-variable-like-reference-compiler
     (lambda (ident)
       (syntax-parse ident
         [x:id (syntax/loc #'x (contract-pos x))])))))

(syntax-spec
 (binding-class lsl-id #:description "lsl binding")
 (binding-class ctc-id #:description "contract binding" #:reference-compiler ctc-ref-compiler)
 (extension-class lsl-macro #:binding-space lsl)

 (nonterminal/exporting
  lsl-form
  #:description "lsl form"
  #:binding-space lsl
  #:allow-extension lsl-macro

  ;; Contracts are only allowed as top-level expressions
  (define-contract def:ctc-id c:ctc)
  #:binding (export def)

  (define-contract (def:ctc-id arg:id ...) c:ctc)
  #:binding (export def)

  (: v:lsl-id c:ctc)

  e:lsl-def-or-expr
  #:binding (re-export e))

 (nonterminal/exporting
  lsl-def-or-expr
  #:description "lsl definition or expression"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (#%define v:lsl-id e:lsl-expr)
  #:binding (export v)

  e:lsl-expr)

 (nonterminal
  lsl-expr
  #:description "lsl expression"
  #:binding-space lsl
  #:allow-extension lsl-macro

  (quote t:literal)
  (#%lsl-id i:lsl-id)
  (#%rkt-id e:racket-expr)

  ;; TODO: optional else clause
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

  (#%let* (b:binding ...)
          body:lsl-expr)
  #:binding (nest b ... body)

  (#%letrec (b:rec-binding ...)
            body:lsl-expr)
  #:binding (nest b ... body)

  (#%lsl-app f:lsl-expr arg:lsl-expr ...)

  (~> (f:expr e:expr ...)
      (tag-syntax-with-unexpanded #'(#%lsl-app f e ...) this-syntax))

  (~> (~or lit:number lit:string lit:boolean)
      (tag-syntax-with-unexpanded #'(quote lit) this-syntax))

  ;; TODO: how to get sensible errors if contracts are written in LSL position

  ;; conversion of identifiers to special forms to differentiate between LSL and Racket vars
  ;; see https://github.com/michaelballantyne/hosted-minikanren/blob/main/private/spec.rkt#L47
  (~> x:id
      #:when (lookup #'x (binding-class-predicate lsl-id))
      (tag-syntax-with-unexpanded #'(#%lsl-id x) this-syntax))
  (~> x:id
      (tag-syntax-with-unexpanded #'(#%rkt-id x) this-syntax)))

 (nonterminal literal
              #:description "literal value"
              n:number
              s:string
              b:boolean
              i:id)

 (nonterminal/nesting
  binding (hole)
  #:binding-space lsl
  [v:lsl-id e:lsl-expr]
  #:binding (scope (bind v) hole))

 (nonterminal/nesting
  rec-binding (hole)
  #:binding-space lsl
  [v:lsl-id e:lsl-expr]
  #:binding (scope (bind v) e hole))

 (nonterminal
  ctc
  #:description "contract"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (#%ctc-id i:ctc-id)
  ;; TODO: contracts parameterized over other contracts
  (#%ctc-app i:ctc-id args:lsl-expr ...)

  (#%Immediate ((~datum check) pred:lsl-expr)
               ((~datum generate) gen:lsl-expr)
               ((~datum shrink) shrk:lsl-expr)
               ((~datum feature) feat-name:lsl-expr feat:lsl-expr) ...)

  (#%Function ((~datum arguments) [v:lsl-id e:ctc] ...)
              ((~datum result) c:ctc))
  #:binding (scope (bind v) ... e ... c)

  (~> x:id
      #:when (lookup #'x (binding-class-predicate ctc-id))
      (tag-syntax-with-unexpanded #'(#%ctc-id x) this-syntax))

  (~> (i:id e:expr ...)
      #:when (lookup #'i (binding-class-predicate ctc-id))
      (tag-syntax-with-unexpanded #'(#%ctc-app i e ...) this-syntax))

  ;; TODO: if I expand to `Immediate`, it enters an infinite loop?
  (~> e:expr
      (tag-syntax-with-unexpanded #'(#%Immediate (check e) (generate #f) (shrink #f)) this-syntax)))

 (host-interface/definitions
  (#%lsl e:lsl-form ...)
  #:binding ((re-export e) ...)
  #'(begin (compile-lsl e) ...)))