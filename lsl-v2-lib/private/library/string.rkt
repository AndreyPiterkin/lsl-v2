#lang racket/base

(require racket/contract
         racket/string
         racket/list
         "../syntax/spec.rkt")

(provide (except-out (all-defined-out)
                     explode implode))

(define (explode s)
  (drop-right (drop (string-split s "") 1) 1))

(define (implode l)
  (apply string-append l))

(define-lsl-library
(lsl:explode explode)
(lsl:implode implode)
(lsl:format format)
(lsl:list->string list->string)
(lsl:make-string make-string)
(lsl:string string)
(lsl:string->list string->list)
(lsl:string->number string->number)
(lsl:number->string number->string)
(lsl:string->symbol string->symbol)
(lsl:string-append string-append)
(lsl:string-contains? string-contains?)
(lsl:string-copy string-copy)
(lsl:string-downcase string-downcase)
(lsl:string-length string-length)
(lsl:string-ref string-ref)
(lsl:string-upcase string-upcase)
(lsl:string? string?)
(lsl:substring substring)
(lsl:string<? string<?))

