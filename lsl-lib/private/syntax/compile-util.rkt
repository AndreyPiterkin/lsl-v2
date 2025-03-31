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
  ;; An ArgClause is a (arg-clause Identifier [Listof Identifier] ContractSyntax Natural])
  ;; Stores the relevant information about an arm of a dependent function contract
  ;; the argument id, the free variables, the contract, and the position in the arg list.
  (struct arg-clause (id deps ctc-stx pos))

  ;; [Listof Identifier] [Listof ContractSyntax] -> [Hash Symbol ArgClause]
  ;; Constructs a hashtable from symbol to ArgClause
  (define (compute-arg-clause-mapping ids args)
    (for/hash ([id ids]
               [arg args]
               [i (in-naturals)])
      (values (syntax->datum id)
              (arg-clause id (free-identifiers arg #:allow-host? #t) arg i))))

  ;; [Hash Symbol ArgClause] -> Symbol -> [Listof Symbol]
  ;; Get the "neighbors"--the depedencies of the computed arg clause--as symbols for top sort
  (define ((get-deps mapping) key)
    (define clause (hash-ref mapping key))
    (define deps (arg-clause-deps clause))
    (map syntax->datum deps))


  ;; [Hash Symbol ArgClause] -> Symbol -> [List Identifier ContractSyntax Natural]
  ;; Extracts all but the dependencies of the computed arg-clause as a list for unpacking
  ;; in syntax-parse
  (define ((key->ordered-mapping mapping) key)
    (define clause (hash-ref mapping key))
    (list (arg-clause-id clause)
          (arg-clause-ctc-stx clause)
          (arg-clause-pos clause)))

  ;; ContractSyntax [Hash Symbol ArgClause] -> [Listof [List Identifier ContractSyntax Natural]]
  ;; Given the argument clauses, produce a topologically sorted list of function contract arms.
  ;; RAISES: Syntax error if there is a cyclic dependency, highlighting the given contract syntax
  (define (order-clauses stx args)
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))
    (define sorted-args
      (topological-sort (hash-keys args)
                        (get-deps args)
                        #:cycle cycle))
    (map (key->ordered-mapping args)
         sorted-args)))

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
    (pattern val #:when (string? (syntax-e #'val))))

  ;; Given a cons of syntax, extract the last one
  (define (get-last-unexpanded lostx)
    (if (cons? lostx)
        (get-last-unexpanded (cdr lostx))
        lostx)))
