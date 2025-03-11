#lang racket/base

(require syntax-spec-v3
         "compile.rkt"
         (for-syntax syntax/parse
                     "compile.rkt"
                     racket/list
                     (except-in racket/base
                                string)))

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         (for-space lsl (all-defined-out)))

(syntax-spec
 (binding-class lsl-nt #:description "lsl binding" #:reference-compiler immutable-reference-compiler)
 (extension-class lsl-macro #:binding-space lsl)

 (nonterminal/exporting
  lsl-form
  #:description "lsl form"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (define-contract c:lsl-nt body:ctc)
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
  
  i:lsl-nt
  n:number
  s:string
  b:boolean

  (and e:lsl-expr ...)
  (or e:lsl-expr ...)

  (~datum ..)
  (~datum ...)
  (~datum ....)
  (cond [c:lsl-expr e:lsl-expr] ...
        [(~datum else) else:lsl-expr])
  (if c:lsl-expr
      t:lsl-expr
      e:lsl-expr)
  (quote e:lsl-expr)

  (rkt e:racket-expr)
  
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
      #'(#%lsl-app f e ...)))

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
  ((~datum Immediate) ((~datum check) pred:lsl-expr)
                      #;((~datum generate) gen:lsl-expr)
                      #;((~datum shrink) shrk:lsl-expr)
                      #;((~datum feature) feat-name:lsl-expr feat:lsl-expr) #;...)
  e:lsl-expr)

 (host-interface/definitions
  (#%lsl e:lsl-form ...)
  #:binding ((re-export e) ...)
  #'(begin (compile-lsl e) ...)))

(define-syntax define-lsl-syntax
  (syntax-parser
    [(_ name:id trans:expr)
     #'(define-dsl-syntax name lsl-macro trans)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special forms

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
