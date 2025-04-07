#lang racket/base

(require racket/class)

(provide contract->predicate
         (struct-out passed-guard)
         (struct-out failed-guard))

(struct guard ())
(struct passed-guard (proc)
  #:property prop:procedure 0)
(struct failed-guard (proc)
  #:property prop:procedure 0)

(define ((contract->predicate ctc) val)
  (passed-guard? (send ctc protect val #f)))