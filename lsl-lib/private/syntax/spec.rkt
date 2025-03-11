#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse
                     "compile.rkt"
                     syntax/stx))

(provide (all-defined-out)
         (for-space ctc (all-defined-out))
         (for-space lsl (all-defined-out))
         (for-syntax (all-defined-out)))

(syntax-spec
 (binding-class ctc-nt #:description "contract binding")
 (binding-class lsl-nt #:description "lsl binding")
 
 (extension-class lsl-macro)

 (nonterminal/exporting
  lsl-def-or-expr
  #:description "lsl definition or expression"
  #:binding-space lsl
  #:allow-extension lsl-macro
  (define/i v:lsl-nt e:lsl-expr)
  #:binding (export v)

  (define-contract c:ctc-nt body:ctc)
  #:binding (export c)
  
  (: v:lsl-nt c:ctc)
  e:lsl-expr)

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
  
  (begin e:lsl-def-or-expr ... last:lsl-expr)
  #:binding (scope (import e) ... last)


  ;; TODO: probably also want implicit begin positions in function bodies, maybe as a macro?
  (lambda (v:lsl-nt ...) e:lsl-def-or-expr ... b:lsl-expr)
  #:binding (scope (bind v) ... (import e) ... b)

  (let ([v:lsl-nt e:lsl-expr] ...)
    b:lsl-expr)
  #:binding (scope (bind v) ... b)

  (let* (b:binding ...)
    body:lsl-expr)
  #:binding (nest b ... body)
  
  (letrec (b:rec-binding ...)
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

 ;; TODO: I removed the local case since I wasn't sure how to handle it--maybe that's a macro

 (nonterminal
  ctc
  #:description "contract"
  #:binding-space ctc
  ((~datum Immediate) ((~datum check) pred:lsl-expr)
                      #;((~datum generate) gen:lsl-expr)
                      #;((~datum shrink) shrk:lsl-expr)
                      #;((~datum feature) feat-name:lsl-expr feat:lsl-expr) #;...)
  c:ctc-nt
  e:lsl-expr)

 (host-interface/definitions
  (#%lsl e:lsl-def-or-expr ...)
  #:binding ((re-export e) ...)
  #`(begin #,@(map compile-lsl (attribute e)))))

;; not great
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