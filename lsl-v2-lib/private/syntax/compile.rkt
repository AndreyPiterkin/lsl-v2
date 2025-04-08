#lang racket/base


(require (for-syntax (except-in racket/base string)
                     racket/class
                     racket/list
                     syntax/parse
                     "grammar.rkt")
         "../runtime/contract-common.rkt"
         "../runtime/immediate.rkt"
         "../runtime/function.rkt"
         "../runtime/oneof.rkt"
         "../runtime/allof.rkt"
         "../runtime/list.rkt"
         "../runtime/lazy.rkt"
         "../util.rkt"
         "compile-util.rkt"
         syntax-spec-v3
         racket/class
         racket/promise
         syntax/location)

(provide compile-lsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILE-LSL

;; todo: compiler should match nesting of forms

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
    ;; this should go away
    [(_ (#%rkt-id ((~datum #%host-expression) e:id)))
     #'e]
    [(_ (#%lsl-id e:id))
     #'e]
    [(_ (#%lambda (args ...) e))
     #'(lambda (args ...) (compile-lsl e))]
    [(_ (provide args ...))
     #'(provide args ...)]
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
    ;; this double pattern match is unnecessary, only one form now exists
    [(_ (~and (#%define-contract _ _)
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
    ;; put most of this in a helper function to call instead
    #`(let* ([name #,id]
             [body (rt-rename-if-proc name #,val)]
             ;; pull above
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc #,ctc])
        (rt-attach-contract pos neg ctc body)))

  ;; LslIdentifier ContractStx -> VoidSyntax
  ;; Compiles the contract attach
  (define (compile-attach-contract lsl-id ctc)
    (when (contract-table-ref lsl-id)
      (raise-syntax-error (syntax->datum lsl-id) "value has previously attached contract" lsl-id))

    ;; dont compile here, compile in define
    (define compiled-ctc #`(make-lsl-to-contract-boundary (compile-contract #,ctc)))
    (contract-table-set! lsl-id compiled-ctc)
    #'(begin))

  ;; DefineContractStx -> RacketDefineStx
  ;; Compiles the given contract definition
  (define (compile-define-contract def-ctc-stx)
    (syntax-parse def-ctc-stx
      [(_ name contract)
       #'(define name (make-lsl-to-contract-boundary (compile-contract contract)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILE-CONTRACT

;; Compile the given contract form
(define-syntax (compile-contract stx)
  (syntax-parse stx
    #:literal-sets (contract-literals)
    [(_ (~and (#%Immediate (check pred:expr)
                           (generate g:expr)
                           (shrink shr:expr)
                           (feature feat-name:expr feat:expr) ...)
              immediate-stx))
     (compile-immediate #'immediate-stx)]
    [(_ (~and (#%Function (arguments (x:id c:expr) ...)
                          (result r:expr))
              function-stx))
     (compile-function #'function-stx)]
    [(_ (~and (#%OneOf e:expr ...) oneof-stx))
     (compile-oneof #'oneof-stx)]
    [(_ (~and (#%AllOf e:expr ...) allof-stx))
     (compile-allof #'allof-stx)]
    [(_ (~and (#%List e:expr) list-stx))
     (compile-list #'list-stx)]
    [(_ (~and (#%Tuple e:expr ...) tuple-stx))
     (compile-tuple #'tuple-stx)]
    [(_ (#%ctc-id i:id))
     #'(rt-validate-contract-id i #'i)]
    [(_ (#%contract-lambda (arg:id ...) c:expr))
     #'(lambda (arg ...) (compile-contract c))]
    [(_ (~and (#%ctc-app i:id e:expr ...) app-stx))
     (compile-app #'app-stx)]))

(begin-for-syntax
  ;; ImmediateCtcStx -> RuntimeCtcStx
  ;; Compiles the immediate contract into its runtime representation
  (define compile-immediate
    (syntax-parser
      #:literal-sets (contract-literals)
      [(_ (check pred)
          (generate g)
          (shrink shr)
          (feature name feat) ...)
       ;; TODO: somehow introduce unexpanded into scope so all compile-xyzcontract functions dont
       ;; have to do this themselves?
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
     

       (define/syntax-parse check #'(make-contract-to-lsl-boundary (compile-lsl pred)))
       (define/syntax-parse gen #'(make-contract-to-lsl-boundary (compile-lsl g)))
       (define/syntax-parse shrink #'(make-contract-to-lsl-boundary (compile-lsl shr)))
       (define/syntax-parse lo-features
         #'(list (list (make-contract-to-lsl-boundary (compile-lsl name))
                       (make-contract-to-lsl-boundary (compile-lsl feat)))
                 ...))

       ;; inline some of this, pull out into runtime function
       #'(let ([check^ check])
           (rt-validate-flat-contract! check^ #'pred) ;; todo: get unexpanded for pred
           (new immediate%
                [stx #'unexpanded]
                [checker check^]
                [generator gen]
                [shrinker shrink]
                [features lo-features]))]))

  ;; FunctionCtcStx -> RuntimeCtcStx
  ;; Compiles the given function contact syntax
  (define compile-function
    (syntax-parser
      [(_ (arguments (x c) ...)
          (result r))
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define arg-clauses (compute-arg-clause-mapping (attribute x) (attribute c)))
       (define/syntax-parse ((x^ c^ i) ...) (order-clauses this-syntax arg-clauses))

       (define ids (attribute x^))
       ;; maybe instead of lambdas, do let*?
       (define/syntax-parse ((x^^ ...) ...) (build-list (length ids) (lambda (i) (take ids i))))

       (define/syntax-parse (c^^ ...) #'((compile-contract c^) ...))
       (define/syntax-parse r^ #'(compile-contract r))
       #`(new function%
              [stx #'unexpanded]
              [domain-order (list (#%datum . i) ...)]
              [domains (list (lambda* (x^^ ...) c^^) ...)]
              [codomain (lambda* (x^ ...) r^)])]))

  ;; OneOfCtcStx -> RuntimeCtcStx
  ;; Compiles the given oneof contract
  (define compile-oneof
    (syntax-parser
      [(_ c ...)
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
       #'(new oneof%
              [stx #'unexpanded]
              [disjuncts (list compiled-ctc ...)])]))

  ;; AllOfCtcStx -> RuntimeCtcStx
  ;; Compiles the given allof contract
  (define compile-allof
    (syntax-parser
      [(_ c ...)
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
       #'(new allof%
              [stx #'unexpanded]
              [conjuncts (list compiled-ctc ...)])]))

  ;; ListCtcStx -> RuntimeCtcStx
  ;; Compiles the given list contract
  (define compile-list
    (syntax-parser
      [(_ c)
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define/syntax-parse compiled-ctc #'(compile-contract c))
       #'(new list%
              [stx #'unexpanded]
              [fixed? #f]
              [contracts (list compiled-ctc)])]))

  ;; TupleCtcStx -> RuntimeCtcStx
  ;; Compiles the given tuple contract
  (define compile-tuple
    (syntax-parser
      [(_ c ...)
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
       #'(new list%
              [stx #'unexpanded]
              [fixed? #t]
              [contracts (list compiled-ctc ...)])]))

  ;; AppCtcStx -> RuntimeCtcSyntax
  ;; Compiles the given contract instantiation to a lazy contract, to avoid
  ;; infinite recursion when instantiating the contract if it is recursive
  (define compile-app
    (syntax-parser
      [(_ i c ...)
       (define/syntax-parse unexpanded
         (get-last-unexpanded (syntax-property this-syntax 'unexpanded)))
       (define/syntax-parse (compiled-ctc ...) #'((compile-contract c) ...))
       ;; explore why this works
       #'(new lazy%
              [stx #'unexpanded]
              [promise (delay (#%app i compiled-ctc ...))])])))