#lang racket/base

(require racket/base
         racket/contract
         racket/provide
         "../util.rkt"
         "../syntax/spec.rkt")

(provide (all-defined-out))

(define-lsl-library
  (lsl:char->integer char->integer)
  (lsl:char-alphabetic? char-alphabetic?)
  (lsl:char-ci<=? char-ci<=?)
  (lsl:char-ci<? char-ci<?)
  (lsl:char-ci=? char-ci=?)
  (lsl:char-ci>=? char-ci>=?)
  (lsl:char-ci>? char-ci>?)
  (lsl:char-downcase char-downcase)
  (lsl:char-lower-case? char-lower-case?)
  (lsl:char-numeric? char-numeric?)
  (lsl:char-upcase char-upcase)
  (lsl:char-upper-case? char-upper-case?)
  (lsl:char-whitespace? char-whitespace?)
  (lsl:char<=? char<=?)
  (lsl:char<? char<?)
  (lsl:char=? char=?)
  (lsl:char>=? char>=?)
  (lsl:char>? char>?)
  (lsl:char? char?))
