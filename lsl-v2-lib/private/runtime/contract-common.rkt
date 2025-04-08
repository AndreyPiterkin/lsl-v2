#lang racket/base

(require racket/class
         racket/string
         racket/format
         racket/match
         racket/syntax-srcloc
         errortrace/errortrace-key)


(provide contract%
         (struct-out blame)
         (struct-out positive-blame)
         (struct-out negative-blame)
         blame->polarity
         (struct-out exn:fail:lsl:contract)
         unimplemented-error!
         contract-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

;; disable contracts?
(define current-contract-disable (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exn definitions 

(struct exn:fail:lsl:contract exn:fail (srclocs)
  #:property prop:exn:srclocs
  (lambda (self) (exn:fail:lsl:contract-srclocs self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exn functions

;; Symbol -> Void
;; Raises a user error that the given name is not implemented
(define (unimplemented-error! name)
  (raise-user-error name "is not implemented"))

;; Contract Syntax Any Blame -> Void
;; Raises a contract error for the given offending value and its attached contract and blame
(define (contract-error _ctc stx val blm
                        #:expected [expected (syntax->datum stx)]
                        #:given [given (~v val)])
  (define error-msg
    (match blm
      [(blame name path)
       (define polarity (blame->polarity blm))
       (format BLM-CTC-FMT name expected given path polarity)]
      [_ (format UNK-CTC-FMT expected given)]))
  (custom-error stx error-msg))

;; ContractSyntax String -> Void
;; Raises an error with the given syntax used for source location info and the given message.
(define (custom-error stx msg)
  (define cms (current-continuation-marks))
  (define stx-srclocs
    (cond
      [(and stx (syntax-srcloc stx)) => list]
      [else null]))
  (define cm-srclocs
    (match (continuation-mark-set->list cms errortrace-key)
      [(cons (cons _ srcloc-list) _) (list (apply srcloc srcloc-list))]
      [_ null]))
  (raise (exn:fail:lsl:contract msg cms (append stx-srclocs cm-srclocs))))

(define BLM-CTC-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a"
     "given: ~a"
     "blaming: ~a (as ~a)")
   "\n  "))

(define UNK-CTC-FMT
  (string-join
   '("contract violation"
     "expected: ~a"
     "given: ~a")
   "\n  "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract core

(define contract%
  (class object%
    (super-new)
    (define/public (protect val pos-blame)
      (unimplemented-error! 'protect))


    (define/public (generate fuel)
      (unimplemented-error! 'generate))

    (define/public (shrink val)
      (unimplemented-error! 'shrink))

    (define/public (interact val name mode)
      (unimplemented-error! 'interact))

    (define/public (describe val)
      (unimplemented-error! 'describe))))
         
(struct blame (name path))
(struct positive-blame blame ())
(struct negative-blame blame ())

(define (blame->polarity blm)
  (if (positive-blame? blm) "server" "client"))
