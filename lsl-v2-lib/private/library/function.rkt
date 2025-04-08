#lang racket/base


(require racket/contract
         racket/list
         racket/function
         "../util.rkt"
         "../syntax/spec.rkt")

(provide (all-defined-out))

(define-contracted-lsl-library
  (lsl:andmap (-> (unconstrained-domain-> boolean?) list? list? ... any) andmap)
  (lsl:ormap (-> (unconstrained-domain-> boolean?) list? list? ... any) ormap)
  (lsl:procedure? (-> any? boolean?) procedure?))

(define-lsl-library
  (lsl:apply apply)
  (lsl:argmax argmax)
  (lsl:argmin argmin)
  (lsl:compose compose)
  (lsl:filter filter)
  (lsl:foldl foldl)
  (lsl:foldr foldr)
  (lsl:identity identity)
  (lsl:map map)
  (lsl:memf memf)
  (lsl:sort sort))
