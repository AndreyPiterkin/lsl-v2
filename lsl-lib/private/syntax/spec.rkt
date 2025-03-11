#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse
                     "compile.rkt"
                     syntax/stx)
         racket/block)

(provide (all-defined-out)
         (for-space ctc (all-defined-out))
         (for-space lsl (all-defined-out))
         (for-syntax (all-defined-out)))

(syntax-spec
 (binding-class ctc-nt #:description "contract binding")
 (binding-class lsl-nt #:description "lsl binding")
 (extension-class lsl-macro)

 (nonterminal/nesting
  lsl-form (hole)
  #:description "lsl form"
  #:binding-space lsl
  c:ctc-def
  #:binding (nest c hole)
                  
  e:lsl-expr+def
  #:binding (nest e hole))

 (nonterminal/nesting
  lsl-expr+def (hole)
  #:description "lsl definition or expression"
  #:binding-space lsl
  #:allow-extension lsl-macro
  e:lsl-expr
  l:lsl-def
  #:binding (nest l hole))

 (nonterminal/nesting
  lsl-def (hole)
  #:description "lsl definition"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (define/i v:lsl-nt e:lsl-expr)
  #:binding (scope (bind v) hole))

 (nonterminal
  lsl-expr
  #:description "lsl expression"
  #:binding-space lsl
  n:number
  s:string
  b:boolean
  id:lsl-nt

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
  
  (begin e:lsl-expr+def ... last:lsl-expr)
  #:binding (nest e ... last)

  ;; TODO: what to do about require?
  (require s:string)

  ;; TODO: probably also want implicit begin positions in function bodies, maybe as a macro?
  (lambda (v:lsl-nt ...) b:lsl-expr)
  #:binding (scope (bind v) ... b)

  (let ([v:lsl-nt e:lsl-expr] ...)
    b:lsl-expr)
  #:binding (scope (bind v) ... b)

  (let* (b:binding ...)
    body:lsl-expr)
  #:binding (nest b ... body)
  
  (letrec (b:rec-binding ...)
    body:lsl-expr)
  #:binding (nest b ... body)

  (local (b:local-binding ...)
    body:lsl-expr)
  #:binding (nest b ... body))

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

 (nonterminal/nesting
  local-binding (hole)
  #:binding-space lsl
  [v:lsl-nt e:lsl-expr]
  #:binding (scope (bind v) e hole)

  d:lsl-def
  #:binding (nest d hole))

 (nonterminal
  ctc
  #:description "contract"
  #:binding-space ctc
  ((~datum Immediate) ((~datum check) pred:lsl-expr)
                      ((~datum generate) gen:lsl-expr)
                      ((~datum shrink) shrk:lsl-expr)
                      ((~datum feature) feat-name:lsl-expr feat:lsl-expr) ...)
  c:ctc-nt
  e:lsl-expr)

 (nonterminal/nesting
  ctc-def (hole)
  #:description "contract definition"
  #:binding-space ctc
  
  (define-contract c:ctc-nt contract:ctc)
  #:binding (scope (bind c) contract hole)

  (: v:lsl-nt c:ctc))


 ;; only having this seems very suspect...
 (host-interface/expression
  (#%lsl e:lsl-form ... )
  #:binding (nest e ... ())
   #`(block #,@(map compile-lsl (attribute e)))))

(define-dsl-syntax define lsl-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ (x:id args:id ...)
          e:expr ...)
       #'(define/i x
           (lambda (args ...)
             (begin
               e ...)))]
      [(_ x:id e:expr)
       #'(define/i x e)])))