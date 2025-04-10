#lang racket/base


(require (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/struct
                     racket/syntax
                     syntax/parse/class/struct-id
                     "grammar.rkt")
         "../runtime/function.rkt"
         "../runtime/oneof.rkt"
         "../runtime/allof.rkt"
         "../runtime/list.rkt"
         "../runtime/lazy.rkt"
         "../runtime/struct.rkt"
         "compile-util.rkt"
         "../runtime/runtime-util.rkt"
         syntax-spec-v3
         racket/class
         racket/struct
         racket/local
         racket/promise
         syntax/location)

(provide compile-lsl/lsl-form)

(define-syntax (compile-lsl/lsl-form stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (provide args ...))
     #'(provide args ...)]
    [(_ e)
     #'(compile-lsl/lsl-def-or-expr e)]))

(define-syntax (compile-lsl/lsl-def-or-expr stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (#%define v b))
     #'(compile-define v b)]
    [(_ (#%define-struct name (field ...) ctor pred accessor ...))
     #`(compile-define-struct name (field ...) ctor pred accessor ...)]
    [(_ (: v ctc))
     (do-attach-contract #'v #'ctc)
     #'(begin)]
    [(_ (#%define-contract cid ctc))
     #'(compile-define-contract cid ctc)]
    [(_ e)
     #'(compile-lsl/lsl-expr e)]))

(define-syntax (compile-define stx)
  (syntax-parse stx
    [(_ v b)
     (define body #'(compile-lsl/lsl-expr b))
     (define maybe-ctc (contract-table-ref #'v))
     (define/syntax-parse body^
       (if maybe-ctc
           #`(attach-contract 'v (compile-contract #,maybe-ctc) #,body)
           body))
     #'(define v
         body^)]))

(define-syntax (attach-contract stx)
  (syntax-parse stx
    [(_ sym ctc body)
     #'(rt:attach-contract sym body (quote-module-name) ctc)]))

(begin-for-syntax
  (define (do-attach-contract id ctc)
    (when (contract-table-ref id)
      (raise-syntax-error (syntax->datum id) "value has previously attached contract" id))

    (void (contract-table-set! id ctc))))

(define-syntax (compile-define-struct stx)
  (syntax-parse stx
    [(_ name (field ...) ctor pred accessor ...)
     #:with prefix (gensym)
     #:with name^ (format-id #'name "~a:~a" #'prefix #'name)
     #:with (_ _ pred^ accessor^ ...)
     (build-struct-names #'name^ (syntax->list #'(field ...)) #f #t)
     #`(begin
         (struct name^ (field ...)
           #:transparent
           #:constructor-name ctor
           #:name name
           #:property prop:custom-print-quotable 'never
           #:methods gen:custom-write
           [(define write-proc
              (make-constructor-style-printer
               (lambda (obj) 'ctor)
               (lambda (obj) (list (accessor^ obj) ...))))])
         ;; TODO: hide arity errors
         (define pred (procedure-rename pred^ 'pred))
         (define accessor (procedure-rename accessor^ 'accessor))
         ...)]))

(define-syntax (compile-define-contract stx)
  (syntax-parse stx
    [(_ cid ctc)
     #'(define cid (compile-contract ctc))]))

(define-syntax (compile-lsl/lsl-expr stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (quote t))
     #''t]
    [(_ i:id)
     #'i]
    [(_ (cond [c e] ...
              [else el]))
     #'(cond [(compile-lsl/lsl-expr c) (compile-lsl/lsl-expr e)] ...
             [else (compile-lsl/lsl-expr el)])]
    [(_ (cond [c e] ...))
     #'(cond [(compile-lsl/lsl-expr c) (compile-lsl/lsl-expr e)] ...
             [else (void)])]
    [(_ (and e ...))
     #'(and (compile-lsl/lsl-expr e) ...)]
    [(_ (or e ...))
     #'(or (compile-lsl/lsl-expr e) ...)]
    [(_ (if c t e)) #'(if (compile-lsl/lsl-expr c)
                          (compile-lsl/lsl-expr t)
                          (compile-lsl/lsl-expr e))]
    [(_ (contract-generate c))
     #'(rt:contract-generate (compile-contract c))]
    [(_ (contract-generate c fuel))
     #'(rt:contract-generate (compile-contract c) (compile-lsl/lsl-expr fuel))]
    [(_ (#%lambda (args ...) e))
     #'(lambda (args ...) (compile-lsl/lsl-expr e))]
    [(_ (#%local (d ...) b))
     #'(local ((compile-lsl/lsl-def-or-expr d) ...)
         b)]
    [(_ (#%let ((b e) ...) body))
     #'(let ((b (compile-lsl/lsl-expr e)) ...)
         (compile-lsl/lsl-expr body))]
    [(_ (#%letrec ((b e) ...) body))
     #'(letrec ((b (compile-lsl/lsl-expr e)) ...)
         (compile-lsl/lsl-expr body))]
    [(_ (#%lsl-app f args ...))
     #'(#%app (compile-lsl/lsl-expr f)
              (compile-lsl/lsl-expr args) ...)]))

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (define/syntax-parse unexpanded
    (syntax-parse stx
      [(_ ctc)
       (get-unexpanded #'ctc)]))
  (syntax-parse stx
    #:literal-sets (contract-literals)
    [(_ (#%Immediate (check pred:expr)))
     #'(compile-immediate unexpanded pred)]
    [(_ (#%Immediate (check pred:expr)
                     (generate g:expr)
                     (shrink shr:expr)
                     (feature feat-name:expr feat:expr) ...))
     #'(compile-immediate unexpanded pred g shr (feat-name feat) ...)]
    [(_ (#%Function (arguments (x:id c:expr) ...)
                    (result r:expr)))
     #'(compile-function unexpanded (x c) ... r)]
    [(_ (#%OneOf e:expr ...))
     #'(compile-oneof unexpanded e ...)]
    [(_ (#%AllOf e:expr ...))
     #'(compile-allof unexpanded e ...)]
    [(_ (#%List e:expr))
     #'(compile-list unexpanded e)]
    [(_ (#%Tuple e:expr ...))
     #'(compile-tuple unexpanded e ...)]
    [(_ (#%Struct i:struct-id (e:expr ...)))
     #'(compile-struct unexpanded i e ...)]
    [(_ (#%ctc-id i:id))
     #`(if (procedure? i)
           ;; TODO: refactor this away
           (compile-contract #,(syntax-property #'(#%ctc-app i) 'unexpanded #'(#%ctc-id i:id)))
           i)]
    [(_ (#%contract-lambda (arg:id ...) c:expr))
     #'(lambda (arg ...) (compile-contract c))]
    [(_ (#%ctc-app i:id e:expr ...))
     #'(compile-app unexpanded i e ...)]))

(define-syntax compile-immediate
  (syntax-parser
    #:literal-sets (contract-literals)
    [(_ unexpanded pred)
     (define/syntax-parse pred-unexpanded (get-unexpanded #'pred))
     #'(rt:make-immediate #'unexpanded #'pred-unexpanded (compile-lsl/lsl-expr pred))]
    [(_ unexpanded
        pred
        g
        shr
        (name feat) ...)
     (define/syntax-parse pred-unexpanded (get-unexpanded #'pred))
     (define/syntax-parse check #'(compile-lsl/lsl-expr pred))
     (define/syntax-parse gen #'(compile-lsl/lsl-expr g))
     (define/syntax-parse shrink #'(compile-lsl/lsl-expr shr))
     (define/syntax-parse lo-features
       #'(list (list (compile-lsl/lsl-expr name)
                     (compile-lsl/lsl-expr feat))
               ...))

     #'(rt:make-immediate #'unexpanded #'pred-unexpanded check gen shrink lo-features)]))

(define-syntax compile-function
  (syntax-parser
    [(_ unexpanded (x c) ... r)
     (define arg-clauses (compute-arg-clause-mapping (attribute x) (attribute c)))
     (define/syntax-parse ((x^ c^ i) ...) (order-clauses this-syntax arg-clauses))
     (define ids (attribute x^))
     ;; maybe instead of lambdas, do let*?
     (define/syntax-parse ((x^^ ...) ...) (build-list (length ids) (lambda (i) (take ids i))))

     (define/syntax-parse (c^^ ...) #'((compile-contract c^) ...))
     (define/syntax-parse r^ #'(compile-contract r))
     #'(new function%
            [stx #'unexpanded]
            [domain-order (list (#%datum . i) ...)]
            [domains (list (lambda* (x^^ ...) c^^) ...)]
            [codomain (lambda* (x^ ...) r^)])]))

(define-syntax compile-oneof
  (syntax-parser
    [(_ unexpanded c ...)
     (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
     #'(new oneof%
            [stx #'unexpanded]
            [disjuncts (list compiled-ctc ...)])]))

;; Compiles the given allof contract
(define-syntax compile-allof
  (syntax-parser
    [(_ unexpanded c ...)
     (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
     #'(new allof%
            [stx #'unexpanded]
            [conjuncts (list compiled-ctc ...)])]))

;; Compiles the given list contract
(define-syntax compile-list
  (syntax-parser
    [(_ unexpanded c)
     (define/syntax-parse compiled-ctc #'(compile-contract c))
     #'(new list%
            [stx #'unexpanded]
            [fixed? #f]
            [contracts (list compiled-ctc)])]))

;; Compiles the given tuple contract
(define-syntax compile-tuple
  (syntax-parser
    [(_ unexpanded c ...)
     (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
     #'(new list%
            [stx #'unexpanded]
            [fixed? #t]
            [contracts (list compiled-ctc ...)])]))

(define-syntax compile-struct
  (syntax-parser
    [(_ unexpanded i:struct-id c ...)
     (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
     #'(new struct%
            [stx #'unexpanded]
            [ctor i.constructor-id]
            [pred i.predicate-id]
            [accessors (list i.accessor-id ...)]
            [contracts (list compiled-ctc ...)])]))

;; Compiles the given contract instantiation to a lazy contract, to avoid
;; infinite recursion when instantiating the contract if it is recursive
;; delays evaluation until the contract is checked, at which point
;; recurs in parallel with the structure of the value, so can't recur infinitely
(define-syntax compile-app
  (syntax-parser
    [(_ unexpanded i c ...)
     (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
     #'(new lazy%
            [stx #'unexpanded]
            [promise (delay (#%app i compiled-ctc ...))])]))
