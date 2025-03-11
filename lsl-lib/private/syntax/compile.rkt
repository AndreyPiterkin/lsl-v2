#lang racket/base


(require (for-syntax racket/class
                     syntax/parse
                     "grammar.rkt"
                     racket/base)

         "../runtime/immediate.rkt"
         "../runtime/contract-common.rkt"
         "../util.rkt"
         syntax/location
         racket/class)

(provide compile-lsl
         (for-syntax string))

(begin-for-syntax
  (define-syntax-class string
    (pattern val #:when (string? (syntax-e #'val))))

  (define (compile-contract stx)
    (define quoted-stx #`#'#,stx)
    (syntax-parse stx
      #:datum-literals (rkt Immediate check generate shrink)
      [(Immediate (check pred:expr)
                  (generate g:expr)
                  (shrink shr:expr)
                  (feature feat-name:string feat:expr) ...)
       #`(new immediate%
              [stx #,quoted-stx] 
              [check pred]
              [gen g]
              [shrink shr]
              [features (list (cons feat-name feat) ...)])]
      [i:id #'i]
      [(rkt e)
       #`(new immediate%
              [stx #,quoted-stx] 
              [check e])])))


;; TODO: validate legal contract positions
(define-syntax (compile-lsl stx)
  (println stx)
  (syntax-parse stx
    #:literal-sets (lsl-literals)
    [(_ (~or e:number e:string e:boolean)) #'e]
    [(_ i:id) #'i]
    [(_ (and e:expr ...)) #'(and (compile-lsl e) ...)]
    [(_ (or e:expr ...)) #'(or (compile-lsl e) ...)]
    [(_ (cond [c e] ... [else el])) #'(cond [(compile-lsl c) (compile-lsl e)] ... [else (compile-lsl el)])]
    [(_ (if c t e)) #'(if (compile-lsl c) (compile-lsl t) (compile-lsl e))]
    [(_ (quote x)) #''(compile-lsl x)]
    [(_ (rkt ((~datum #%host-expression) e))) #'e]
    [(_ (#%lambda (args ...) e)) #'(lambda (args ...) (compile-lsl e))]
    [(_ (#%lsl-app f args ...)) #'((compile-lsl f) (compile-lsl args) ...)]
    [(_ (#%let ((b e) ...) body)) #'(let ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%let* ((b e) ...) body)) #'(let* ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%letrec ((b e) ...) body)) #'(letrec ((b (compile-lsl e)) ...) (compile-lsl body))]
    [(_ (#%define v b))
     (define maybe-ctc (contract-table-ref #'v))
     (if maybe-ctc
         #`(define v ((send #,maybe-ctc protect (compile-lsl b) (positive-blame 'v (quote-module-name))) (compile-lsl b) (negative-blame 'v (quote-module-name))))
         #'(define v (compile-lsl b)))]
    [(_ (: v ctc))
     ;; TODO: attach contract
     #'(void)]
    [(_ (define-contract name contract))
     #`(define name #,(compile-contract #'contract))]))

