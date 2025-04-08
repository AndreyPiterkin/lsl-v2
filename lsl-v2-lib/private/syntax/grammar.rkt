#lang racket

(require syntax/parse)

(provide lsl-literals
         contract-literals)


#|

GRAMMAR

<lsl-form> := (define-contract <ctc-id> <ctc>)
            | (define-contract (<ctc-id> <id> ...) <ctc>)
            | (: <lsl-id> <ctc>)
            | <lsl-def-or-expr>

<lsl-def-or-expr> := (define <lsl-id> <lsl-expr>)
                   | (define (<lsl-id> <lsl-id> ...) <lsl-expr>)
                   | <lsl-expr>


<ctc> := (Immediate (check <lsl-expr>)
                    (generate <lsl-expr>)
                    (shrink <lsl-expr>)
                    (feature <lsl-expr> <lsl-expr>) ...)
       | (Function (arguments (<id> <ctc>) ...)
                   (result <ctc>))
       | (<ctc-id> <ctc> ...) ;; parameterized contracts, i.e. (Tree X)
       | <ctc-id>
       | <lsl-expr>

<lsl-expr> := <datum>
            | (quote <datum>)
            | <lsl-id>
            | <rkt-id>
            | (cond [<lsl-expr> <lsl-expr>] ...
                    [else <lsl-expr>)
            | (cond [<lsl-expr> <lsl-expr>] ...)
            | (if <lsl-expr> <lsl-expr> <lsl-expr>)
            | (lambda (<lsl-id> ...) <lsl-expr>)
            | (let ([<lsl-id> <lsl-expr>] ...)
                 <lsl-expr>)
            | (let* ([<lsl-id> <lsl-expr>] ...)
                  <lsl-expr>)
            | (letrec ([<lsl-id> <lsl-expr>] ...)
                    <lsl-expr>)
            | (local [<local-definition> ...]
                 <lsl-expr>)
            | (<lsl-expr> <lsl-expr> ...)



<ctc-id> := <id> ;; separate binding classes for ctc and lsl, so they can't be used interchangably
<lsl-id> := <id>


<local-definition> := (define <lsl-id> <lsl-expr>)
                    | (define (<lsl-id> <lsl-id> ...) <lsl-expr>)


|#


;; Set of literals that belong in lsl-form position
(define-literal-set lsl-literals
  #:datum-literals (cond else if quote #%let #%let* #%letrec provide #%lambda #%lsl-app #%lsl-id #%rkt-id #%define : #%define-contract #%contract-lambda)
  ())

;; Set of literals that are found in contract position
(define-literal-set contract-literals
  #:datum-literals (#%ctc-id
                    #%ctc-app
                    #%Immediate check generate shrink feature
                    #%Function arguments result raises
                    #%OneOf
                    #%AllOf
                    #%List
                    #%Tuple)
  ())