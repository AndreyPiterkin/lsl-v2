#lang racket/base

(require (for-syntax syntax/id-table
                     racket/string
                     racket/base))

(provide (for-syntax contract-table-set!
                     contract-table-ref
                     strip))

(begin-for-syntax
  ;; free-contract-table: [FreeIdTable ValSyntax ContractStx]
  ;; Maps identifiers to their annotation contracts
  (define free-contract-table (make-free-id-table))

  (define (contract-table-set! id val)
    (free-id-table-set! free-contract-table id val)
    (void))

  (define (contract-table-ref id)
    (free-id-table-ref free-contract-table id #f))

  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

