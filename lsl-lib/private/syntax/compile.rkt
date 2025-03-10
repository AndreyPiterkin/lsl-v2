#lang racket/base

(provide compile-contract)

(require (for-template
          racket/base
          racket/class
          "../runtime/immediate.rkt"
          "../util.rkt")
         syntax/parse)

;; TODO: not sure why syntax-property doesn't work on host-interface expressions, so compile-contract
;; currently takes a second arg for the original unexpanded version
(define (compile-contract stx srcloc)
  (define stx^ (syntax-property (datum->syntax stx (syntax-e stx) srcloc stx) 'unexpanded srcloc))
  (define quoted-stx #`#'#,stx^)
  (syntax-parse stx
    #:datum-literals (Immediate check generate shrink)
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
    [i:id stx^]))

