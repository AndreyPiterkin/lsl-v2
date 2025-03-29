#lang racket/base


(require (for-syntax racket/base
                     racket/class
                     racket/syntax-srcloc
                     racket/undefined
                     racket/function
                     racket/sequence
                     racket/list
                     syntax/parse
                     mischief/sort
                     mischief/dict
                     syntax/id-table
                     "grammar.rkt"
                     "../util.rkt")
         "../runtime/immediate.rkt"
         "../runtime/function.rkt"
         "../runtime/contract-common.rkt"
         "../util.rkt"
         syntax/location
         racket/stxparam
         syntax-spec-v3
         racket/class)

(provide compile-lsl
         contract-pos
         (for-syntax string))

(define-syntax-parameter contract-pos
  (lambda (stx)
    (syntax-parse stx
      [(_ x:expr)
       (raise-syntax-error #f "illegal contract use in lsl expression" #'x)])))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val))))

  (define (make-invalid-contract-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ x:expr)
         (raise-syntax-error #f "illegal contract use in lsl expression" #'x)])))

  (define (make-valid-contract-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ ctc:expr)
         #'ctc])))

  ;; ContractSyntax  [IdTable Identifier [List [Listof Identifier] ContractSyntax Natural]] -> [Listof [List Identifier ContractSyntax Natural]]
  ;; Sorts the hash of identifier to their clause's free vars by constructing a DAG
  ;; ordering.
  ;; RAISES: Syntax error if there is a cyclic dependency, highlighting the given contract syntax
  (define (sort-domains! stx clauses)
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))

    ;; Convert free identifiers to symbols first to normalize them
    (define id-symbols (map syntax-e (free-id-table-keys clauses)))
  
    ;; Create a mapping from symbols to original identifiers
    (define sym->id 
      (for/hash ([id (free-id-table-keys clauses)])
        (values (syntax-e id) id)))
  
    ;; Create a neighbor function that works with symbols
    (define (sym-neighbors sym)
      (define id (hash-ref sym->id sym))
      (define deps (car (free-id-table-ref clauses id)))
      (map syntax-e deps))
  
    (define sorted-syms (topological-sort id-symbols sym-neighbors #:cycle cycle))
  
    ;; Convert back to identifiers
    (define sorted-ids (map (λ (sym) (hash-ref sym->id sym)) sorted-syms))
  
    (map (lambda (id) (list id
                            (second (free-id-table-ref clauses id))
                            (third (free-id-table-ref clauses id))))
         sorted-ids))

  ;; [Listof Identifier] [Listof ContractSyntax] -> [IdTable Identifier [List [Listof Identifier] ContractSyntax Natural]]
  ;; Constructs an association list from identifier to that argument's free var list,
  ;; contract clause, and position
  (define (clauses->fv-assoc ids args)
    (define id-hash
      (for/hash ([id ids]
                 [arg args]
                 [i (in-naturals)])
        (values id
                (list (free-identifiers arg #:allow-host? #t) arg i))))
    (make-immutable-free-id-table id-hash))

  ;; SymbolSyntax ContractSyntax LslExprSyntax -> Syntax
  ;; attaches the given contract to the identifier, renaming the value if it is a procedure
  (define (attach-contract id ctc val)
    #`(let* ([name #,id]
             [body (rename-if-proc name #,val)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc #,ctc])
        (rt-attach-contract! pos neg ctc body))))


(define-syntax (make-contract-to-lsl-boundary lsl-stx)
  (syntax-parse lsl-stx
    [(_ l)
     #'(syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
         l)]))

(define-syntax (make-lsl-to-contract-boundary ctc-stx)
  (syntax-parse ctc-stx
    [(_ c)
     #'(syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
         c)]))

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


;; PositiveBlame NegativeBlame Contract Any -> Any
;; Attaches the contract to the given value with the corresponding blame targets
;; If the given value doesn't satisfy the given contract, throws a contract error
(define (rt-attach-contract! pos neg ctc val)
  ((send ctc protect val pos)
   val
   neg))

;; Symbol Any -> Any
;; If the given _val_ is a procedure, renames it to the given name, otherwise leaves it untouched.
(define (rename-if-proc name val)
  (if (procedure? val)
      (procedure-rename val name)
      val))

;; TODO: move this helper macro
(define-syntax lambda*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(lambda (x* ...) e)]))

