#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         racket/generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out root)
         (struct-out proxy)
         (struct-out proc)
         gen:equatable
         unproxy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data
;; defines some "proxy" that has equality and hashing over it, with a custom
;; write proc, and it can be called since it has a prop:proc to wrap its args
;; in the wrapping function provided
;; has a wrap, unwrap, and contract fields as well as the value to write

(define-generics equatable
  (base-equal? equatable other))

(struct root ()
  #:transparent
  #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc self other recur)
     (if (or (proxy? self) (proxy? other))
         (recur (unproxy self) (unproxy other))
         (base-equal? self other)))
   (define (hash-proc self recur)
     (recur (unproxy self)))
   (define (hash2-proc self recur)
     (recur (unproxy self)))])

(struct proxy root (target info contract unwrap)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (recur (unproxy self) port))])

(struct proc proxy ()
  #:property prop:procedure
  (λ (self . args)
    (match-define (proxy target wrapper _ _) self)
    (apply wrapper args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (unproxy st)
  (if (proxy? st)
      (unproxy ((proxy-unwrap st)))
      st))
