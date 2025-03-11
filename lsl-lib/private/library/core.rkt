#lang racket

(require "../syntax/spec.rkt"
         "../util.rkt"
         racket/provide
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header))

(provide (all-from-out "../syntax/spec.rkt")
         #%top-interaction
         #%app
         #%top
         quote
         (rename-out [#%lsl #%module-begin])
         (filtered-out
          (strip "$")
          (combine-out $#%datum)))

(provide
 natural?
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

(define-syntax $#%datum
  (syntax-parser
    [(_ . (~or e:number e:boolean e:string e:character))
     #'(#%datum . e)]))