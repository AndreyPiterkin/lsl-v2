#lang racket/base


(require "../util.rkt"
         racket/bool
         racket/contract
         racket/provide
         "../syntax/spec.rkt")

(provide (all-defined-out))

(define-lsl-library
  (lsl:true #t)
  (lsl:false #f))

(define-contracted-lsl-library
  (lsl:boolean=? (-> boolean? boolean? boolean?) equal?)
  (lsl:boolean? (-> any? boolean?) boolean?)
  (lsl:false? (-> any? boolean?) false?)
  (lsl:not (-> boolean? boolean?) not))
