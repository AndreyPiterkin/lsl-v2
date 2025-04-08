#lang racket/base

(require racket/contract
         racket/symbol
         racket/bool
         "../syntax/spec.rkt")

(provide (all-defined-out))
(define-lsl-library
  (lsl:symbol=? symbol=?)
  (lsl:symbol? symbol?)
  (lsl:symbol->string symbol->string))
