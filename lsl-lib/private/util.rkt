#lang racket

(require (for-syntax syntax/id-table))

(provide (for-syntax contract-table-set!
                     contract-table-ref
                     contract-add!
                     contract?))

(begin-for-syntax
  ;; TODO: this contracts table is an ugly hack, used as a set for us to be able to differentiate
  ;; contract bindings (ctc-nt) from syntax-spec from ISL identifiers used in implicit contract position
  (define contracts (make-free-id-table))
  (define (contract-add! id)
    (free-id-table-set! contracts id #t)
    (void))

  (define (contract? id)
    (free-id-table-ref contracts id #f))
  

  ;; free-contract-table: [FreeIdTable ValSyntax ContractStx]
  ;; Maps identifiers to their annotation contracts
  (define free-contract-table (make-free-id-table))

  (define (contract-table-set! id val)
    (free-id-table-set! free-contract-table id val)
    (void))

  (define (contract-table-ref id)
    (free-id-table-ref free-contract-table id #f)))