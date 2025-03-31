#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/parse
                     syntax/id-table
                     mischief/sort
                     "../util.rkt")
         racket/stxparam
         syntax-spec-v3)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract position helpers

;; Demarcates contract position, to be used in the contract binding ref compiler
(define-syntax-parameter contract-pos
  (syntax-parser
    [(_ x:expr)
     (raise-syntax-error #f "illegal contract use in lsl expression" #'x)]))

(begin-for-syntax
  ;; Produces an invalid contract tansformer that raises a syntax error for invalid contract use
  (define (make-invalid-contract-transformer)
    (syntax-parser
      [(_ x:expr)
       (raise-syntax-error #f "illegal contract use in lsl expression" #'x)]))

  ;; Produces a valid contract tansformer that expands to the value inside
  (define (make-valid-contract-transformer)
    (syntax-parser
      [(_ ctc:expr)
       #'ctc])))

;; Creates a boundary where the nested expression cannot contain contracts
(define-syntax make-contract-to-lsl-boundary
  (syntax-parser
    [(_ l)
     #'(syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
         l)]))

;; Creates a boundary where the nested expression can contain contracts
(define-syntax make-lsl-to-contract-boundary
  (syntax-parser
    [(_ c)
     #'(syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
         c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dependent function contract compilation

(begin-for-syntax
  ;; ContractSyntax  [IdTable Identifier [List [Listof Identifier] ContractSyntax Natural]] -> [Listof [List Identifier ContractSyntax Natural]]
  ;; Sorts the hash of identifier to their clause's free vars by constructing a DAG
  ;; ordering.
  ;; RAISES: Syntax error if there is a cyclic dependency, highlighting the given contract syntax
  (define (sort-domains! stx clauses)
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))

    (define id-symbols (map syntax-e (free-id-table-keys clauses)))
  
    (define sym->id 
      (for/hash ([id (free-id-table-keys clauses)])
        (values (syntax-e id) id)))
  
    (define (sym-neighbors sym)
      (define id (hash-ref sym->id sym))
      (define deps (car (free-id-table-ref clauses id)))
      (map syntax-e deps))
  
    (define sorted-syms (topological-sort id-symbols sym-neighbors #:cycle cycle))
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
    (make-immutable-free-id-table id-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; lambda with any number of wildcard arguments
(define-syntax lambda*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(lambda (x* ...) e)]))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val)))))
