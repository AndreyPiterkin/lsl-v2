#lang racket

(require syntax/parse)

(provide lsl-literals
         contract-literals)


#|

GRAMMAR

<lsl-form> := (provide <lsl-id> ...)
            | <lsl-def>
            | <lsl-expr>

<lsl-def> := (define <lsl-id> <lsl-expr>)
           | (define (<lsl-id> <lsl-id> ...) <lsl-expr>)
           | (define-contract <ctc-id> <ctc>)
           | (define-contract (<ctc-id> <id> ...) <ctc>)
           | (: <lsl-id> <ctc>)


<ctc> := (Immediate (check <lsl-expr>)
                    (generate <lsl-expr>)
                    (shrink <lsl-expr>)
                    (feature <lsl-expr> <lsl-expr>) ...)
       | (Function (arguments (<id> <ctc>) ...)
                   (result <ctc>))
       | (OneOf <ctc> ...)
       | (AllOf <ctc> ...)
       | (List <ctc>)
       | (Tuple <ctc> ...)
       | (<ctc-id> <ctc> ...) ;; parameterized contracts, i.e. (Tree X)
       | <ctc-id>
       | <lsl-expr>

<lsl-expr> := <datum>
            | (quote <datum>)
            | <lsl-id>
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


|#


;; Set of literals that belong in lsl-form position
(define-literal-set lsl-literals
  #:datum-literals (cond else if quote #%let #%letrec #%local provide #%lambda #%lsl-app #%define : #%define-contract #%contract-lambda)
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
