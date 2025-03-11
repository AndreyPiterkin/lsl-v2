#lang racket/base


(require (for-template
          racket/base
          racket/class
          "../runtime/immediate.rkt"
          "../runtime/contract-common.rkt"
          "../util.rkt"
          syntax/location)
         
         syntax/parse
         "grammar.rkt")

(provide compile-lsl)

(define (compile-contract stx)
  (define quoted-stx #`#'#,stx)
  (syntax-parse stx
    #:datum-literals (Immediate check generate shrink)
    [(Immediate (check pred:expr)
                #;(generate g:expr)
                #;(shrink shr:expr)
                #;(feature feat-name:string feat:expr) #;...)
     #`(new immediate%
            [stx #,quoted-stx] 
            [check pred]
            #;[gen g]
            #;[shrink shr]
            #;[features (list (cons feat-name feat) ...)])]
    [i:id
     ;; TODO: somewhere along the way tag unexpanded stx
     (if (contract? #'i)
         stx
         (compile-contract #'(Immediate (check i))))]))

(define (compile-lsl stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(~or e:number e:string e:boolean e:id) #'e]
    [(and e:expr ...) #'(and e ...)]
    [(or e:expr ...) #'(and e ...)]
    [(cond [c e] ... [else el]) #'(cond [c e] ... [else el])]
    [(if c t e) #'(if c t e)]
    [(quote x) #''x]
    [(begin e ...) #'(begin e ...)]
    [(lambda (args ...) e) #'(lambda (args ...) e)]
    [(let (e ...) body) #'(let (e ...) body)]
    [(let* (e ...) body) #'(let* (e ...) body)]
    [(letrec (e ...) body) #'(letrec (e ...) body)]
    [(local (e ...) body) #'(local (e ...) body)]
    [(define/i v b)
     (define maybe-ctc (contract-table-ref #'v))
     (if maybe-ctc
         #`(define v ((send #,maybe-ctc protect b (positive-blame 'v (quote-module-name))) b (negative-blame 'v (quote-module-name))))
         #'(define v b))]
    [(: v ctc)
     (define maybe-ctc (contract-table-ref #'v))
     (when maybe-ctc
       (raise-syntax-error (syntax-e #'v) (format "contract previously declared for ~a" (syntax-e #'v)) maybe-ctc))

     (define compiled-ctc (compile-contract #'ctc))
     (contract-table-set! #'v compiled-ctc)
     #'(void)]
    [(define-contract name contract)
     (contract-add! #'name)
     #`(define name #,(compile-contract #'contract))]))

