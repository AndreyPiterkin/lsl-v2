#lang racket/base

(require "spec.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/function
                     syntax/parse
                     "grammar.rkt"))

(provide (all-defined-out))

;; CORE FORM SUGAR
(begin-for-syntax
  (define-syntax-class local-defines
    #:description "local definition"
    #:datum-literals (define define-contract :)
    (pattern (~or (~and (~or (define v:id b:expr)
                             (define (v:id args:id ...) body:expr)
                             (define-contract v:id c:expr)
                             (define-contract (v:id args:id ...) c:expr))
                        (~bind (bound-id #'v)))
                  (: v:id c:expr)
                  (~and inv:expr
                        (~bind (invalid #'inv))))))

  (define (check-invalid-local-definitions! invalids)
    (for/list ([invalid invalids])
      (when invalid
        (raise-syntax-error 'local "expected local definition, found expression" invalid))))

  (define (check-duplicate-bindings! bound-ids)
    (define bound-ids^ (filter identity bound-ids))
    (define maybe-duplicate (check-duplicates bound-ids^ bound-identifier=?))
    (when maybe-duplicate
      (raise-syntax-error 'local "definition occurs locally more than once" maybe-duplicate))))

(define-lsl-form-syntax define
  (syntax-parser
    [(_ (x:id args:id ...)
        e:expr)
     #'(#%define x (#%lambda (args ...) e))]
    [(_ x:id e:expr)
     #'(#%define x e)]))

(define-lsl-form-syntax define-contract
  (syntax-parser
    [(_ (x:id args:id ...)
        e:expr)
     #'(#%define-contract x (#%contract-lambda (args ...) e))]
    [(_ x:id e:expr)
     #'(#%define-contract x e)]))

(define-lsl-form-syntax lambda
  (syntax-parser
    [(_ (args:id ...)
        e:expr)
     #'(#%lambda (args ...) e)]))

(define-lsl-form-syntax local
  (syntax-parser
    [(_ (d:local-defines ...)
        e:expr)
     (check-invalid-local-definitions! (attribute d.invalid))
     (check-duplicate-bindings! (attribute d.bound-id))
     #'(#%local (d ...) e)]))

(define-lsl-form-syntax let
  (syntax-parser
    [(_ ((v:id b:expr) ...)
        e:expr)
     #'(#%let ((v b) ...) e)]))

(define-lsl-form-syntax let*
  (syntax-parser
    [(_ ((v:id b:expr))
        e:expr)
     #'(#%let ((v b))
              e)]
    [(_ ((v:id b:expr) rest ...)
        e:expr)
     #'(#%let ((v b))
              (let* (rest ...) e))]))

(define-lsl-form-syntax letrec
  (syntax-parser
    [(_ ((v:id b:expr) ...)
        e:expr)
     #'(#%letrec ((v b) ...) e)]))


;; CONTRACT SUGAR

;; nicer error messages with optional
(define-contract-syntax Immediate
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ (check pred:expr))
     #'(#%Immediate (check pred)
                    (generate #f)
                    (shrink #f))]
    [(_ (~alt (~optional (check pred:expr) #:defaults ((pred #'(lambda (_) #t))))
              (~optional (generate gen:expr) #:defaults ((gen #'#f)))
              (~optional (shrink shrk:expr) #:defaults ((shrk #'#f)))
              (feature feat-name:expr feat:expr)) ...)
     #'(#%Immediate (check pred)
                    (generate gen)
                    (shrink shrk)
                    (feature feat-name feat) ...)]))

(define-contract-syntax Function
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ (arguments [x:id a:expr] ...)
        (result r:expr))
     #'(#%Function (arguments [x a] ...)
                   (result r))]))

(define-contract-syntax ->
  (syntax-parser
    [(_ args ... res)
     (define/syntax-parse (id ...) (map (lambda (_) (gensym)) (attribute args)))
     #'(#%Function (arguments [id args] ...)
                   (result res))]))

;; TODO: I don't like that I have to do this... (this being tagging with unexpanded by using these
;; sugar transformers)
(define-contract-syntax OneOf
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ c:expr ...)
     #'(#%OneOf c ...)]))

(define-contract-syntax AllOf
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ c:expr ...)
     #'(#%AllOf c ...)]))

(define-contract-syntax List
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ c:expr)
     #'(#%List c)]))

(define-contract-syntax Tuple
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ c:expr ...)
     #'(#%Tuple c ...)]))

