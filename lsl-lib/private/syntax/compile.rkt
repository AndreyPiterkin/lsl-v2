#lang racket/base


(require (for-syntax racket/class
                     syntax/parse
                     "grammar.rkt"
                     racket/base)
         "../runtime/immediate.rkt"
         "../runtime/contract-common.rkt"
         "../util.rkt"
         syntax/location
         racket/stxparam
         racket/class)

(provide compile-lsl
         contract-pos
         (for-syntax string))

(define-syntax-parameter contract-pos
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id)
       (raise-syntax-error (syntax->datum #'x) "illegal contract use in lsl expression" #'x)])))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val))))

  (define (make-contract-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ ctc:id)
         #'ctc])))

  ;; SymbolSyntax ContractSyntax LslExprSyntax -> Syntax
  ;; attaches the given contract to the identifier, renaming the value if it is a procedure
  (define (attach-contract id ctc val)
    #`(let* ([name #,id]
             [body (rename-if-proc name #,val)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc #,ctc])
        (rt-attach-contract pos neg ctc body))))

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (define quoted-stx #`#'#,stx)
  (syntax-parse stx
    #:literal-sets (contract-literals)
    [(_ (#%Immediate (check pred:expr)
                     (generate g:expr)
                     (shrink shr:expr)
                     (feature feat-name:expr feat:expr) ...))
     #`(new immediate%
            [stx #,quoted-stx] 
            [check (compile-lsl pred)]
            [gen (compile-lsl g)]
            [shrink (compile-lsl shr)]
            [features (list (cons (compile-lsl feat-name) (compile-lsl feat)) ...)])]
    [(_ (#%ctc-id i:id)) #'i]
    [(_ e:expr)
     #`(new immediate%
            [stx #,quoted-stx]
            [check (compile-lsl e)])]))

;; Compile the given LSL form
(define-syntax (compile-lsl stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (quote t)) #''t]
    [(_ i:id) #'i]
    [(_ (cond [c e] ... [else el])) #'(cond [(compile-lsl c) (compile-lsl e)] ... [else (compile-lsl el)])]
    [(_ (if c t e)) #'(if (compile-lsl c) (compile-lsl t) (compile-lsl e))]
    [(_ (#%rkt-id ((~datum #%host-expression) e:id))) #'e]
    [(_ (#%lsl-id e:id)) #'e]
    [(_ (#%lambda (args ...) e)) #'(lambda (args ...) (compile-lsl e))]
    [(_ (#%lsl-app f args ...)) #'((compile-lsl f) (compile-lsl args) ...)]
    [(_ (#%let ((b e) ...) body)) #'(let ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%let* ((b e) ...) body)) #'(let* ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%letrec ((b e) ...) body)) #'(letrec ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%define v b))
     (define maybe-ctc (contract-table-ref #'v))
     (define/syntax-parse body #'(compile-lsl b))
     (if maybe-ctc
         #`(define v
             #,(attach-contract #''v maybe-ctc #'body))
         #'(define v (compile-lsl b)))]
    [(_ (: v ctc))
     (define maybe-ctc (contract-table-ref #'v))
     (when maybe-ctc
       (raise-syntax-error (syntax->datum #'v) "value has previously attached contract" #'v))
     (define compiled-ctc
       #'(syntax-parameterize ([contract-pos (make-contract-transformer)])
           (compile-contract ctc)))
     (contract-table-set! #'v compiled-ctc)
     #'(void)]
    [(_ (define-contract name contract))
     ;; TODO: is this the desired behavior?
     #'(define name
         (syntax-parameterize ([contract-pos (make-contract-transformer)])
           (compile-contract contract)))]))

;; PositiveBlame NegativeBlame Contract Any -> Any
;; Attaches the contract to the given value with the corresponding blame targets
;; If the given value doesn't satisfy the given contract, throws a contract error
(define (rt-attach-contract! pos neg ctc val)
  ((send ctc protect val pos)
   val
   neg))

;; Symbol Any -> Any
;; If the given _val_ is a procedure, renames it to the given name, otherwise leaves it untouched.
(define (rename-if-proc name val)
  (if (procedure? val)
      (procedure-rename val name)
      val))

