#lang racket/base

(require syntax-spec-v3
         "compile.rkt"
         (for-syntax syntax/parse
                     "compile.rkt"
                     racket/list
                     (only-in syntax-spec-v3/private/ee-lib/main lookup in-space)
                     (except-in racket/base
                                string)))

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         (for-space lsl (all-defined-out)))

(begin-for-syntax
  (define ctc-ref-compiler
    (make-variable-like-reference-compiler
     (lambda (ident)
       (syntax-parse ident
         [x:id (syntax/loc #'x (contract-pos x))])))))

(syntax-spec
 (binding-class lsl-nt #:description "lsl binding")
 (binding-class ctc-nt #:description "contract binding" #:reference-compiler ctc-ref-compiler)
 (extension-class lsl-macro #:binding-space lsl)

 (nonterminal/exporting
  lsl-form
  #:description "lsl form"
  #:binding-space lsl
  #:allow-extension lsl-macro
  ;; TODO: should contract forms only be top level?
  (define-contract c:ctc-nt body:ctc)
  #:binding (export c)
  (: v:lsl-nt c:ctc)

  e:lsl-def-or-expr
  #:binding (re-export e))

 (nonterminal/exporting
  lsl-def-or-expr
  #:description "lsl definition or expression"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (#%define v:lsl-nt e:lsl-expr)
  #:binding (export v)

  e:lsl-expr)

 (nonterminal
  lsl-expr
  #:description "lsl expression"
  #:binding-space lsl
  #:allow-extension lsl-macro

  (quote t:literal)
  (#%lsl-id i:lsl-nt)
  (#%rkt-id e:racket-expr)

  (cond [c:lsl-expr e:lsl-expr] ...
        [(~datum else) else:lsl-expr])
  (if c:lsl-expr
      t:lsl-expr
      e:lsl-expr)

  (#%lambda (v:lsl-nt ...) b:lsl-expr)
  #:binding (scope (bind v) ... b)

  (#%let ([v:lsl-nt e:lsl-expr] ...)
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
      #'(#%lsl-app f e ...))

  (~> (~or lit:number lit:string lit:boolean)
      #'(quote lit))

  ;; conversion of identifiers to special forms to differentiate between LSL and Racket vars
  ;; see https://github.com/michaelballantyne/hosted-minikanren/blob/main/private/spec.rkt#L47
  (~> x:id
      #:when (lookup #'x (binding-class-predicate lsl-nt))
      #'(#%lsl-id x))
  (~> x:id
      #'(#%rkt-id x)))

 (nonterminal literal
              #:description "literal value"
              n:number
              s:string
              b:boolean
              i:id)

 (nonterminal/nesting
  binding (hole)
  #:binding-space lsl
  [v:lsl-nt e:lsl-expr]
  #:binding (scope (bind v) hole))

 (nonterminal/nesting
  rec-binding (hole)
  #:binding-space lsl
  [v:lsl-nt e:lsl-expr]
  #:binding (scope (bind v) e hole))

 (nonterminal
  ctc
  #:description "contract"
  #:binding-space lsl
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; IMMEDIATE

  (~> ((~datum Immediate) (~alt (~once ((~datum check) pred:expr))
                                (~optional ((~datum generate) gen:expr) #:defaults ((gen #'#f)))
                                (~optional ((~datum shrink) shrk:expr) #:defaults ((shrk #'#f)))
                                ((~datum feature) feat-name:expr feat:expr)) ...)
      #'(#%Immediate (check pred)
                     (generate gen)
                     (shrink shrk)
                     (feature feat-name feat) ...))

  (#%Immediate ((~datum check) pred:lsl-expr)
               ((~datum generate) gen:lsl-expr)
               ((~datum shrink) shrk:lsl-expr)
               ((~datum feature) feat-name:lsl-expr feat:lsl-expr) ...)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (#%ctc-id i:ctc-nt)
  (~> x:id
      #:when (lookup #'x (binding-class-predicate ctc-nt))
      #'(#%ctc-id x))
  e:lsl-expr)

 (host-interface/definitions
  (#%lsl e:lsl-form ...)
  #:binding ((re-export e) ...)
  #'(begin (compile-lsl e) ...)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special forms

(define-syntax define-lsl-syntax
  (syntax-parser
    [(_ name:id transformer:expr)
     #'(define-dsl-syntax name lsl-macro transformer)]))

(define-lsl-syntax define
  (syntax-parser
    [(_ (x:id args:id ...)
        e:expr)
     #'(#%define x (#%lambda (args ...) e))]
    [(_ x:id e:expr)
     #'(#%define x e)]))

(define-lsl-syntax lambda
  (syntax-parser
    [(_ (args:id ...)
        e:expr)
     #'(#%lambda (args ...) e)]))

(define-lsl-syntax local
  (syntax-parser
    #:datum-literals (define)
    [(_ ((define v:id b:expr) ...)
        e:expr)
     (define duplicate (check-duplicates (attribute v) bound-identifier=?))
     (when duplicate
       (raise-syntax-error #f "duplicate binding in localL " duplicate))
     #'(#%let* ([v b] ...) e)]
    [(_ ((define (v:id args:id ...) b:expr) ...)
        e:expr)
     (define duplicate (check-duplicates (attribute v) bound-identifier=?))
     (when duplicate
       (raise-syntax-error #f "duplicate binding" duplicate))
     #'(#%let* ([v (#%lambda (args ...) b)] ...) e)]))

(define-lsl-syntax let
  (syntax-parser
    [(_ ((v:id b:expr) ...)
        e:expr)
     #'(#%let ((v b) ...) e)]))

(define-lsl-syntax let*
  (syntax-parser
    [(_ ((v:id b:expr) ...)
        e:expr)
     #'(#%let* ((v b) ...) e)]))

(define-lsl-syntax letrec
  (syntax-parser
    [(_ ((v:id b:expr) ...)
        e:expr)
     #'(#%letrec ((v b) ...) e)]))
