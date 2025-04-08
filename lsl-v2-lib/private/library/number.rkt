#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/provide
         racket/math
         "../util.rkt"
         "../syntax/spec.rkt")

(provide (except-out (all-defined-out) natural?))

(define-lsl-library
  (lsl:random random)
  (lsl:sqr sqr)
  (lsl:sqrt sqrt)
  (lsl:* *)
  (lsl:+ +)
  (lsl:- -)
  (lsl:/ /)
  (lsl:< <)
  (lsl:<= <=)
  (lsl:= =)
  (lsl:> >)
  (lsl:>= >=)
  (lsl:abs abs)
  (lsl:add1 add1)
  (lsl:ceiling ceiling)
  (lsl:even? even?)
  (lsl:exact->inexact exact->inexact)
  (lsl:expt expt)
  (lsl:floor floor)
  (lsl:inexact->exact inexact->exact)
  (lsl:max max)
  (lsl:min min)
  (lsl:modulo modulo)
  (lsl:negative? negative?)
  (lsl:odd? odd?)
  (lsl:pi pi)
  (lsl:positive? positive?)
  (lsl:quotient quotient)
  (lsl:remainder remainder)
  (lsl:sgn sgn)
  (lsl:sub1 sub1)
  (lsl:zero? zero?))

(define (natural? x)
  (and (integer? x) (>= x 0)))

(define-contracted-lsl-library
  (lsl:natural? (-> any? boolean?) natural?)
  (lsl:integer? (-> any? boolean?) integer?)
  (lsl:number? (-> any? boolean?) number?)
  (lsl:real? (-> any? boolean?) real?))
