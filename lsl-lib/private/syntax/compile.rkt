#lang racket/base

(provide compile-contract)

(require (for-template
          racket/base
          racket/class
          "../runtime/immediate.rkt"
          "../util.rkt")
         syntax/parse)

(define (compile-contract stx)
  (define quoted-stx #`#'#,stx)
  (syntax-parse stx
    #:datum-literals (#%host-expression #%ctc-expanded Immediate check generate shrink)
    [(#%ctc-expanded Immediate (check pred:expr)
                     (generate g:expr)
                     (shrink shr:expr)
                     (feature feat-name:string feat:expr) ...)
     #`(new immediate%
            [stx #,quoted-stx] 
            [check pred]
            [gen g]
            [shrink shr]
            [features (list (cons feat-name feat) ...)])]
    [(#%host-expression e:expr)
     #`(new immediate%
            [stx #,quoted-stx] 
            [check e]
            [gen #f]
            [shrink #f])]
    [i:id
     #'i]))

