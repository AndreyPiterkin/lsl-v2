#lang racket/base


(require (for-syntax (except-in racket/base string)
                     racket/class
                     racket/list
                     syntax/parse
                     "grammar.rkt")
         "../runtime/immediate.rkt"
         "../runtime/function.rkt"
         "../util.rkt"
         "compile-util.rkt"
         syntax-spec-v3
         racket/class
         racket/stxparam)

(provide compile-lsl
         contract-pos
         (for-syntax string))

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (syntax-parse stx
    #:literal-sets (contract-literals)
    ;; TODO: does this need to be a macro, or can it be a compile-time helper?
    ;; TODO: compile-immediate, compile-contract, etc helpers
    [(_ (~and (#%Immediate (check pred:expr)
                           (generate g:expr)
                           (shrink shr:expr)
                           (feature feat-name:expr feat:expr) ...) stx^))
     #`(let ([check (make-contract-to-lsl-boundary (compile-lsl pred))]
             [gen (make-contract-to-lsl-boundary (compile-lsl g))]
             [shrink (make-contract-to-lsl-boundary (compile-lsl shr))]
             [features (list (list
                              (make-contract-to-lsl-boundary (compile-lsl feat-name))
                              (make-contract-to-lsl-boundary (compile-lsl feat)))
                             ...)]
             [stx #'#,(syntax-property #'stx^ 'unexpanded)])
         (unless (procedure? check)
           (raise-syntax-error #f "invalid immediate contract (must be a predicate)" #'pred))
         (new immediate%
              [stx stx]
              [checker check]
              [generator gen]
              [shrinker shrink]
              [features features]))]
    [(_ (~and (#%Function (arguments (x:id c:expr) ...)
                          (result r:expr))
              stx^))
     (define arg-fv-table (clauses->fv-assoc (attribute x) (attribute c)))
     (define/syntax-parse ((x^ c^ i) ...) (sort-domains! #'stx^ arg-fv-table))
     
     (define ids (attribute x^))
     (define/syntax-parse ((x^^ ...) ...) (build-list (length ids) (lambda (i) (take ids i))))
     
     (define/syntax-parse (c^^ ...) #'((compile-contract c^) ...))
     (define/syntax-parse r^ #'(compile-contract r))
     #`(new function%
            [stx #'#,(syntax-property #'stx^ 'unexpanded)]
            [domain-order (list (#%datum . i) ...)]
            [domains (list (lambda* (x^^ ...) c^^) ...)]
            [codomain (lambda* (x^ ...) r^)])]
    [(_ (#%ctc-id i:id))
     #'(if (procedure? i)
           (raise-syntax-error #f "must instantiate parameterized contract" #'i)
           i)]
    [(_ (#%ctc-app i:id e:expr ...))
     ;; TODO: what if we have a parameterized contract with a mix of contract and lsl args...
     #'(i (make-contract-to-lsl-boundary (compile-lsl e)) ...)]))

;; Compile the given LSL form
(define-syntax (compile-lsl stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (quote t)) #''t]
    [(_ i:id) #'i]
    [(_ (cond [c e] ... [else el])) #'(cond [(compile-lsl c) (compile-lsl e)] ... [else (compile-lsl el)])]
    [(_ (if c t e)) #'(if (compile-lsl c) (compile-lsl t) (compile-lsl e))]
    [(_ (#%rkt-id ((~datum #%host-expression) e:id))) #'e]
    [(_ (#%lsl-id e:id)) #'e]
    [(_ (~and (#%lambda (args ...) e) stx^))
     #'(lambda (args ...) (compile-lsl e))]
    [(_ (#%lsl-app f args ...)) #'(#%app (compile-lsl f) (compile-lsl args) ...)]
    [(_ (#%let ((b e) ...) body)) #'(let ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%let* ((b e) ...) body)) #'(let* ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%letrec ((b e) ...) body)) #'(letrec ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%define v b))
     ;; TODO: both these forms should be helper functions
     (define maybe-ctc (contract-table-ref #'v))
     (define/syntax-parse body #'(compile-lsl b))
     (if maybe-ctc
         #`(define v
             #,(attach-contract #''v maybe-ctc #'body))
         #'(define v (compile-lsl b)))]
    [(_ (: v ctc))
     (define maybe-ctc (contract-table-ref #'v))
     (when maybe-ctc
       (raise-syntax-error (syntax->datum #'v) "value has previously attached contract" #'v))
     (define compiled-ctc
       #'(make-lsl-to-contract-boundary (compile-contract ctc)))
     (contract-table-set! #'v compiled-ctc)
     #'(void)]
    
    ;; TODO: is this the desired behavior for contract defs?
    ;; Also, this has to be two forms because if define-contract was re-written
    ;; like define with lambda too early, the lambda would be treated as part of the
    ;; lsl-expr pred in contract position
    [(_ (define-contract name contract))
     #'(define name
         (make-lsl-to-contract-boundary (compile-contract contract)))]
    [(_ (define-contract (name args ...) contract))
     #'(define name
         (lambda (args ...)
           (make-lsl-to-contract-boundary (compile-contract contract))))]))
