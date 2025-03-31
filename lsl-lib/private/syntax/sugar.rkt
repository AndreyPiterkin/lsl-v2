#lang racket/base

(require syntax-spec-v3
         "spec.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     "spec.rkt"
                     "grammar.rkt"))

(provide (for-space lsl (all-defined-out)))

(define-syntax define-lsl-syntax
  (syntax-parser
    [(_ name:id transformer:expr)
     #'(define-dsl-syntax name lsl-macro
         (lambda (stx)
           (define stx^ (transformer stx))
           (tag-syntax-with-unexpanded stx^ stx)))]))

;; CORE FORM SUGAR

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
     #'(#%Immediate (check pred)
                    (generate gen)
                    (shrink shrk)
                    (feature feat-name feat) ...)]))

(define-lsl-syntax Function
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ ~! (~alt (~once ((~datum arguments) [x:id a:expr] ...))
                 (~once ((~datum result) r:expr))) ...)
     #'(#%Function (arguments [x a] ...)
                   (result r))]))

;; TODO: I don't like that I have to do this... (this being tagging with unexpanded by using these
;; sugar transformers
(define-lsl-syntax OneOf
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ ~! c:expr ...)
     #'(#%OneOf c ...)]))

(define-lsl-syntax AllOf
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ ~! c:expr ...)
     #'(#%AllOf c ...)]))