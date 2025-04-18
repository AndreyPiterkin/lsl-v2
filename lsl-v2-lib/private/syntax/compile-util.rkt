#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     racket/function
                     syntax/parse
                     mischief/sort
                     "grammar.rkt")
         syntax-spec-v3)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract table
(begin-for-syntax
  (define contract-table (local-symbol-table))

  (define (contract-table-set! id val)
    (symbol-table-set! contract-table id val))

  (define (contract-table-ref id)
    (symbol-table-ref contract-table id #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dependent function contract compilation

(begin-for-syntax
  (struct exn:fail:cyclic exn:fail (srclocs)
    #:property prop:exn:srclocs
    (lambda (self) (exn:fail:cyclic-srclocs self)))

  ;; An ArgClause is a (arg-clause Identifier [Listof Identifier] ContractSyntax Natural])
  ;; Stores the relevant information about an arm of a dependent function contract
  ;; the argument id, the free variables, the contract, and the position in the arg list.
  (struct arg-clause (id deps ctc-stx pos) #:transparent)

  ;; [Listof Identifier] [Listof ContractSyntax] -> [Hash Symbol ArgClause]
  ;; Constructs a hashtable from symbol to ArgClause
  (define (compute-arg-clause-mapping ids args)
    (define args-set (apply immutable-symbol-set ids))
    (for/hash ([id ids]
               [arg args]
               [i (in-naturals)])
      (define free-vars-set (apply immutable-symbol-set (free-identifiers arg)))
      (define free-args (symbol-set-intersect free-vars-set args-set))
      (values (syntax->datum id)
              (arg-clause id (sequence->list (in-symbol-set free-args)) arg i))))

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

  ;; Given a cons of syntax, extract the last one
  (define (get-last-unexpanded lostx)
    (if (cons? lostx)
        (get-last-unexpanded (cdr lostx))
        lostx))

  ;; Given syntax, extract the most unexpanded version of it from its properties
  (define (get-unexpanded stx)
    (define maybe-unexpanded (get-last-unexpanded (syntax-property stx 'unexpanded)))
    (if maybe-unexpanded
        maybe-unexpanded
        stx)))
