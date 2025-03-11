#lang racket/base

(require "private/syntax/spec.rkt"
         "private/library/core.rkt")

(provide (all-from-out "private/syntax/spec.rkt"))
(provide (all-from-out "private/library/core.rkt"))

(module reader syntax/module-reader
  #:language 'lsl-v2)