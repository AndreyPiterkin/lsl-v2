#lang racket/base


(require (for-syntax (except-in racket/base string)
                     racket/class
                     racket/list
                     syntax/parse
                     "grammar.rkt")
         "../runtime/contract-common.rkt"
         "../runtime/immediate.rkt"
         "../runtime/function.rkt"
         "../util.rkt"
         "compile-util.rkt"
         syntax-spec-v3
         racket/class
         syntax/location)

(provide compile-lsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILE-LSL

;; Compile the given LSL form
(define-syntax (compile-lsl stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (quote t))
     #''t]
    [(_ i:id)
     #'i]
    [(_ (cond [c e] ... [else el]))
     #'(cond [(compile-lsl c) (compile-lsl e)] ...
             [else (compile-lsl el)])]
    [(_ (if c t e)) #'(if (compile-lsl c)
                          (compile-lsl t)
                          (compile-lsl e))]
    [(_ (#%rkt-id ((~datum #%host-expression) e:id)))
     #'e]
    [(_ (#%lsl-id e:id))
     #'e]
    [(_ (#%lambda (args ...) e))
     #'(lambda (args ...) (compile-lsl e))]
    [(_ (#%lsl-app f args ...))
     #'(#%app (compile-lsl f)
              (compile-lsl args) ...)]
    [(_ (#%let ((b e) ...) body))
     #'(let ((b (compile-lsl e)) ...)
         (compile-lsl body))]
    [(_ (#%let* ((b e) ...) body))
     #'(let* ((b (compile-lsl e)) ...)
         (compile-lsl body))]
    [(_ (#%letrec ((b e) ...) body))
     #'(letrec ((b (compile-lsl e)) ...)
         (compile-lsl body))]
    [(_ (#%define v b))
     (compile-define #'v #'b)]
    [(_ (: v ctc))
     (compile-attach-contract #'v #'ctc)]
    [(_ (~and (define-contract _ _)
              define-contract-stx))
     (compile-define-contract #'define-contract-stx)]))

(begin-for-syntax
  ;; LslIdentifier LslExprStx -> RacketDefineStx
  ;; Compiles the `define` form
  (define (compile-define id body)
    (define body^ #`(compile-lsl #,body))
    (define maybe-ctc (contract-table-ref id))
    (define id-as-symbol #`'#,id)
    
    (define body^^
      (if maybe-ctc
          (attach-contract id-as-symbol
                           maybe-ctc
                           body^)
          body^))

    #`(define #,id #,body^^))

  ;; SymbolSyntax ContractSyntax LslExprSyntax -> Syntax
  ;; attaches the given contract to the identifier, renaming the value if it is a procedure
  (define (attach-contract id ctc val)
    #`(let* ([name #,id]
             [body (rt-rename-if-proc name #,val)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc #,ctc])
        (rt-attach-contract pos neg ctc body)))

  ;; LslIdentifier ContractStx -> VoidSyntax
  ;; Compiles the contract attach
  (define (compile-attach-contract lsl-id ctc)
    (when (contract-table-ref lsl-id)
      (raise-syntax-error (syntax->datum lsl-id) "value has previously attached contract" lsl-id))
    
    (define compiled-ctc #`(make-lsl-to-contract-boundary (compile-contract #,ctc)))
    (contract-table-set! lsl-id compiled-ctc)
    #'(void))

  ;; DefineContractStx -> RacketDefineStx
  ;; Compiles the given contract definition
  (define (compile-define-contract def-ctc-stx)
    (syntax-parse def-ctc-stx
      [(_ name contract)
       #'(define name (make-lsl-to-contract-boundary (compile-contract contract)))]
      [(_ (name args ...) contract)
       #'(define name
           (lambda (args ...)
             (make-lsl-to-contract-boundary (compile-contract contract))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILE-CONTRACT

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (syntax-parse stx
    #:literal-sets (contract-literals)
    [(_ (~and (#%Immediate (check pred:expr)
                           (generate g:expr)
                           (shrink shr:expr)
                           (feature feat-name:expr feat:expr) ...) stx^))
     #`(let ([check (make-contract-to-lsl-boundary (compile-lsl pred))]
             [gen (make-contract-to-lsl-boundary (compile-lsl g))]
             [shrink (make-contract-to-lsl-boundary (compile-lsl shr))]
             [features (list (list
                              (make-contract-to-lsl-boundary (compile-lsl feat-name))
                              (make-contract-to-lsl-boundary (compile-lsl feat)))
                             ...)]
             [stx #'#,(syntax-property #'stx^ 'unexpanded)])
         (unless (procedure? check)
           (raise-syntax-error #f "invalid immediate contract (must be a predicate)" #'pred))
         (new immediate%
              [stx stx]
              [checker check]
              [generator gen]
              [shrinker shrink]
              [features features]))]
    [(_ (~and (#%Function (arguments (x:id c:expr) ...)
                          (result r:expr))
              stx^))
     (define arg-fv-table (clauses->fv-assoc (attribute x) (attribute c)))
     (define/syntax-parse ((x^ c^ i) ...) (sort-domains! #'stx^ arg-fv-table))
     
     (define ids (attribute x^))
     (define/syntax-parse ((x^^ ...) ...) (build-list (length ids) (lambda (i) (take ids i))))
     
     (define/syntax-parse (c^^ ...) #'((compile-contract c^) ...))
     (define/syntax-parse r^ #'(compile-contract r))
     #`(new function%
            [stx #'#,(syntax-property #'stx^ 'unexpanded)]
            [domain-order (list (#%datum . i) ...)]
            [domains (list (lambda* (x^^ ...) c^^) ...)]
            [codomain (lambda* (x^ ...) r^)])]
    [(_ (#%ctc-id i:id))
     #'(if (procedure? i)
           (raise-syntax-error #f "must instantiate parameterized contract" #'i)
           i)]
    [(_ (#%ctc-app i:id e:expr ...))
     #'(i (make-contract-to-lsl-boundary (compile-lsl e)) ...)]))