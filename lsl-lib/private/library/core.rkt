#lang racket

(require
  (for-space lsl "../syntax/spec.rkt")
  "../syntax/spec.rkt")

(provide #%top-interaction
         #%app
         #%top
         #%datum
         quote
         (rename-out [#%lsl #%module-begin]))

;; TODO: only provide-out sugar and available student forms (nothing beginning with #%)
(provide (all-from-out "../syntax/spec.rkt")
         (for-space lsl (all-from-out "../syntax/spec.rkt")))