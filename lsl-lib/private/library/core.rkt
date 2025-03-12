#lang racket

(require
  (for-space lsl "../syntax/spec.rkt")
  "../syntax/spec.rkt"
  "../util.rkt"
  racket/provide
  (for-syntax syntax/parse))

(provide #%top-interaction
         #%app
         #%top
         #%datum
         quote
         (rename-out [#%lsl #%module-begin]))

(provide (all-from-out "../syntax/spec.rkt")
         (for-space lsl (all-from-out "../syntax/spec.rkt")))

(provide natural?
         integer?
         random
         sqr
         sqrt
         *
         +
         -
         /
         <
         <=
         =
         >
         >=
         abs
         add1
         ceiling
         even?
         exact->inexact
         expt
         floor
         inexact->exact
         max
         min
         modulo
         negative?
         odd?
         pi
         positive?
         quotient
         remainder
         sgn
         sub1
         zero?)
