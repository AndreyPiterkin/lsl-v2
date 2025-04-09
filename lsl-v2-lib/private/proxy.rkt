#lang racket/base

(require racket/match
         racket/generic)

(provide (struct-out root)
         (struct-out proxy)
         (struct-out proc)
         gen:equatable
         unproxy
         proxy->contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

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
         [else (lambda (p port) (print p port mode))]))
     (recur (unproxy self) port))])

(struct proc proxy ()
  #:property prop:procedure
  (lambda (self . args)
    (match-define (proxy target wrapper _ _) self)
    (apply wrapper args)))

;; Any -> Any
;; Unwraps all nested proxies around the given value if it is a proxy, otherwise
;; return the value given.
(define (unproxy st)
  (if (proxy? st)
      (unproxy ((proxy-unwrap st)))
      st))

(define (proxy->contract p)
  (and (proxy? p) (proxy-contract p)))