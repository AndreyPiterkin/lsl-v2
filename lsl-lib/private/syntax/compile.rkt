#lang racket/base


(require (for-syntax racket/class
                     syntax/parse
                     "grammar.rkt"
                     racket/base)
         "../runtime/immediate.rkt"
         "../runtime/function.rkt"
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
      [(_ x:expr)
       (raise-syntax-error #f "illegal contract use in lsl expression" #'x)])))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val))))

  (define (make-invalid-contract-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ x:expr)
         (raise-syntax-error #f "illegal contract use in lsl expression" #'x)])))

  (define (make-valid-contract-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ ctc:expr)
         #'ctc])))

  ;; SymbolSyntax ContractSyntax LslExprSyntax -> Syntax
  ;; attaches the given contract to the identifier, renaming the value if it is a procedure
  (define (attach-contract id ctc val)
    #`(let* ([name #,id]
             [body (rename-if-proc name #,val)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc #,ctc])
        (rt-attach-contract! pos neg ctc body))))


(define-syntax (make-contract-to-lsl-boundary lsl-stx)
  (syntax-parse lsl-stx
    [(_ l)
     #'(syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
         l)]))

(define-syntax (make-lsl-to-contract-boundary ctc-stx)
  (syntax-parse ctc-stx
    [(_ c)
     #'(syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
         c)]))

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (define quoted-stx #`#'#,stx)
  (syntax-parse stx
    #:literal-sets (contract-literals)
    [(_ (~and (#%Immediate (check pred:expr)
                           (generate g:expr)
                           (shrink shr:expr)
                           (feature feat-name:expr feat:expr) ...)
              stx^))
     ;; TODO: compile check/gen/shrink similarly to on line 92? i.e. check if theyre procs?
     #`(new immediate%
            [stx #'#,(syntax-property #'stx^ 'unexpanded)]
            [check (make-contract-to-lsl-boundary (compile-lsl pred))]
            [gen (make-contract-to-lsl-boundary (compile-lsl g))]
            [shrink (make-contract-to-lsl-boundary (compile-lsl shr))]
            [features (list (cons
                             (make-contract-to-lsl-boundary (compile-lsl feat-name))
                             (make-contract-to-lsl-boundary (compile-lsl feat))) ...)])]
    [(_ (~and (#%Recursive name:id
                           c:expr
                           (args:id ...))
              stx^))
     #`
     (lambda (args ...)
       (let* ([ctc (compile-contract c)]
              [name ctc])
         ctc))]
    [(_ (~and (#%Function (arguments (x:id c:expr) ...)
                          (result r:expr))
              stx^))
     #`(new function%
            [stx #'#,(syntax-property #'stx^ 'unexpanded)]
            [arg-order (list)]
            [args (list (cons (make-contract-to-lsl-boundary (compile-lsl 'x))
                              (compile-contract c)) ...)]
            [result (make-contract-to-lsl-boundary (compile-lsl 'r))])]
    [(_ (#%ctc-id i:id))
     #'(if (procedure? i) (raise-syntax-error #f "must instantiate parameterized contract" #'i) i)]
    [(_ (#%ctc-app i:id e:expr ...))
     ;; TODO: compile e; should it be compile-contract, or should it be compile-lsl?
     #'(i e ...)]
    [(_ e:expr)
     #`(let ([pred (make-contract-to-lsl-boundary (compile-lsl e))])
         (unless (procedure? pred)
           (raise-syntax-error #f "invalid immediate contract (must be a predicate)" #'e))
         (new immediate%
              [stx #'#,(syntax-property #'stx^ 'unexpanded)]
              [check pred]))]))

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
    [(_ (~and (#%lambda (args ...) e) stx^))
     #'(lambda (args ...) (compile-lsl e))]
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
       #'(syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
           (compile-contract ctc)))
     (contract-table-set! #'v compiled-ctc)
     #'(void)]
    
    ;; TODO: is this the desired behavior for contract defs?
    ;; Also, this has to be two forms because if define-contract was re-written
    ;; like define with lambda too early, the lambda would be treated as part of the
    ;; #%lsl-expr pred in contract position
    [(_ (define-contract name contract))
     #'(define name
         (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
           (compile-contract contract)))]
    [(_ (define-contract (name args ...) contract))
     #'(define name
         (lambda (args ...)
           (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
             (compile-contract contract))))]
    [(_ e)
     #'(contract-pos (compile-contract e))]))

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

