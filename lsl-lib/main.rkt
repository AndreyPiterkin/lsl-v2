#lang racket

(require "private/syntax/spec.rkt")

(define-contract Even even?)

(: x Even)
(define x 4)

