#lang racket

(require "private/syntax/spec.rkt")

(define-contract Even integer?)

(: x Even)
(define-protected x 5)