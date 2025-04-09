#lang racket

(require "contract-common.rkt"
         "immediate.rkt"
         "../proxy.rkt"
         "../util.rkt"
         rackunit)

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

;; Contract Identifier -> Contract
;; Returns the given contract if it is not a procedure, otherwise errors
(define (validate-contract-id ctc-id ctc-stx)
  (if (procedure? ctc-id)
      (raise-syntax-error #f "must instantiate parameterized contract" ctc-stx)
      ctc-id))

(define (make-immediate ctc-unexpanded check-unexpanded check gen shrink features)
  (validate-flat-contract! check check-unexpanded)
  (new immediate%
       [stx ctc-unexpanded]
       [checker check]
       [generator gen]
       [shrinker shrink]
       [features features]))

(define DEFAULT-FUEL 50)

(define (contract-generate ctc [fuel DEFAULT-FUEL]) 
  (send ctc generate fuel))

;; TODO: parameterize by scaling?
(define (scale-fuel x)
  (if (zero? x) x (inexact->exact (ceiling (log x)))))

(define-check (check-contract val name n)
  (define ctc (proxy->contract val))
  (if ctc
      (for ([fuel (in-range 2 (+ n 2))])
        (do-check-contract
         ctc val name
         (λ (ctc) (send ctc generate (scale-fuel fuel)))))
      (fail-check (format "unknown contract for ~a" name))))

(define (do-check-contract ctc val name contract->value)
  (match (send ctc interact val name contract->value)
    [(list eg exn)
     (fail-check (format VERIFY-FMT eg (indent (exn-message exn))))]
    [(none)
     (fail-check "failed to generate values associated with contract")]
    [#f
     (void)]))

(define (indent str)
  (string-append "  " (string-replace str "\n" "\n    ")))

(define VERIFY-FMT
  (string-join
   '("discovered a counterexample"
     "counterexample: ~a"
     "error:"
     "~a")
   "\n  "))