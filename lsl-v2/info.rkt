#lang info

(define name "lsl-v2")
(define collection "lsl-v2")

;; dependencies

(define deps
  '("errortrace-lib"
    "base"
    "lsl-v2-lib"))

(define implies
  '("lsl-v2-lib"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "sandbox-lib"
    "scribble-lib"))