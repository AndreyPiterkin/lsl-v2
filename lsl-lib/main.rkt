#lang racket/base

(require "private/library/core.rkt")

(provide (all-from-out "private/library/core.rkt"))

(module reader syntax/module-reader
  #:language 'lsl-v2)