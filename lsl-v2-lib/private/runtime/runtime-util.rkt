#lang racket

(require "contract-common.rkt"
         "immediate.rkt")

(provide (prefix-out rt: (all-defined-out)))

;; Symbol Any Symbol Contract -> Any
;; Attaches the contract to the given value with the source module path, returning the
;; value itself if it passes contract check.
(define (attach-contract name val module-path ctc)
  (let ([val^ (rename-if-proc name val)]
        [pos (positive-blame name module-path)]
        [neg (negative-blame name module-path)])
    ((send ctc protect val^ pos)
     val^
     neg)))

;; Symbol Any -> Any
;; If the given _val_ is a procedure, renames it to the given name, otherwise leaves it untouched.
(define (rename-if-proc name val)
  (if (procedure? val)
      (procedure-rename val name)
      val))

;; Procudure ProcedureSyntax -> Void
;; Ensures the given procedure is a predicate
(define (validate-flat-contract! proc proc-stx)
  (unless (procedure? proc)
    (raise-syntax-error #f
                        "invalid immediate contract (must be a predicate)"
                        proc-stx)))

(define (make-immediate ctc-unexpanded check-unexpanded check (gen #f) (shrink #f) (features '()))
  (validate-flat-contract! check check-unexpanded)
  (new immediate%
       [stx ctc-unexpanded]
       [env (environment)]
       [checker check]
       [generator gen]
       [shrinker shrink]
       [features features]))

(define DEFAULT-FUEL 50)

(define (contract-generate ctc [fuel DEFAULT-FUEL]) 
  (send ctc generate fuel))