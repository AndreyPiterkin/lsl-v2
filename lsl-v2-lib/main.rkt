#lang racket/base

(require "private/library/core.rkt"
         "private/library/boolean.rkt"
         "private/library/char.rkt"
         "private/library/contract.rkt"
         "private/library/equal.rkt"
         "private/library/function.rkt"
         "private/library/list.rkt"
         "private/library/number.rkt"
         "private/library/string.rkt"
         "private/library/symbol.rkt")

(provide (all-from-out "private/library/core.rkt"
                       "private/library/boolean.rkt"
                       "private/library/char.rkt"
                       "private/library/contract.rkt"
                       "private/library/equal.rkt"
                       "private/library/function.rkt"
                       "private/library/list.rkt"
                       "private/library/number.rkt"
                       "private/library/string.rkt"
                       "private/library/symbol.rkt"))

(module reader syntax/module-reader
  #:language 'lsl-v2)
