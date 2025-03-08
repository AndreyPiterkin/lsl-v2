#lang racket

(require (for-syntax syntax/id-table))

(provide (for-syntax contract-table-set!
                     contract-table-ref))

(begin-for-syntax
  
  (define free-contract-table (make-free-id-table))

  (define (contract-table-set! id val)
    (free-id-table-set! free-contract-table id val)
    (void))

  (define (contract-table-ref id)
    (free-id-table-ref free-contract-table id #f)))