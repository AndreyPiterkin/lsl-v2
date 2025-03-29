#lang racket/base

(require syntax-spec-v3
         "compile.rkt"
         (for-syntax syntax/parse
                     "compile.rkt"
                     "grammar.rkt"
                     racket/list
                     (only-in syntax-spec-v3/private/ee-lib/main lookup in-space)
                     (except-in racket/base
                                string)))

(provide (all-defined-out)
         ;; TODO: don't provide all out, only the forms students should be able to write
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
  ;; TODO: should contract forms only be top level?
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
      #'(#%lsl-app f e ...))

  (~> (~or lit:number lit:string lit:boolean)
      #'(quote lit))

  ;; TODO: how to get sensible errors if contracts are written in LSL position

  ;; conversion of identifiers to special forms to differentiate between LSL and Racket vars
  ;; see https://github.com/michaelballantyne/hosted-minikanren/blob/main/private/spec.rkt#L47
  (~> x:id
      #:when (lookup #'x (binding-class-predicate lsl-id))
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
      (tag-syntax-with-unexpanded #'(#%ctc-id x) #'x))

  (~> (i:id e:expr ...)
      #:when (lookup #'i (binding-class-predicate ctc-id))
      #'(#%ctc-app i e ...))

  (~> e:expr
      #'(Immediate (check e))))

 (host-interface/definitions
  (#%lsl e:lsl-form ...)
  #:binding ((re-export e) ...)
  ;; TODO: sensible errors for contracts in lsl pos, static check?
  #'(begin (compile-lsl e) ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special forms
;; TODO: move these into a sugar file

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
       (raise-syntax-error #f "duplicate binding in local" duplicate))
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


;; CONTRACT SUGAR

(define-lsl-syntax Immediate
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ ~! (~alt (~optional ((~datum check) pred:expr) #:defaults ((pred #'(lambda (_) #t))))
                 (~optional ((~datum generate) gen:expr) #:defaults ((gen #'#f)))
                 (~optional ((~datum shrink) shrk:expr) #:defaults ((shrk #'#f)))
                 ((~datum feature) feat-name:expr feat:expr)) ...)
     (tag-syntax-with-unexpanded
      #'(#%Immediate (check pred)
                     (generate gen)
                     (shrink shrk)
                     (feature feat-name feat) ...)
      this-syntax)]))

(define-lsl-syntax Function
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ ~! (~alt (~once ((~datum arguments) [x:id a:expr] ...))
                 (~once ((~datum result) r:expr))) ...)
     (tag-syntax-with-unexpanded
      #'(#%Function (arguments [x a] ...)
                    (result r))
      this-syntax)]))
