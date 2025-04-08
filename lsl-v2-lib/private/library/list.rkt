#lang racket/base

(require racket/contract
         racket/list
         "../util.rkt"
         "../syntax/spec.rkt"
         "equal.rkt")


(provide (except-out (all-defined-out)
                     member?
                     remove-all
                     memq?
                     memq))

(define-lsl-library
  (lsl:empty empty)
  (lsl:build-list build-list)
  (lsl:range range)
  (lsl:append append)
  (lsl:assoc assoc)
  (lsl:assq assq)
  (lsl:car car)
  (lsl:cdr cdr)
  (lsl:cons cons)
  (lsl:eighth eighth)
  (lsl:fifth fifth)
  (lsl:first first)
  (lsl:fourth fourth)
  (lsl:length length)
  (lsl:list list)
  (lsl:list-ref list-ref)
  (lsl:null null)
  (lsl:rest rest)
  (lsl:reverse reverse)
  (lsl:second second)
  (lsl:seventh seventh)
  (lsl:sixth sixth)
  (lsl:third third))

(define (member? x l)
  (not (equal? #f (member x l))))

(define (remove-all x l)
  (filter (lambda (y) (not (equal? y x))) l))

(define (memq? x l)
  (ormap (lambda (el) (eq? x el)) l))

(define memq memq?)

(define-contracted-lsl-library
 (lsl:member? (-> any? any-list? any) member?)
 (lsl:memq? (-> any? any-list? boolean?) memq?)
 (lsl:memq (-> any? any-list? boolean?) memq)
 (lsl:remove (-> any? any-list? any) remove)
 (lsl:list? (-> any? boolean?) list?)
 (lsl:cons? (-> any? boolean?) cons?)
 (lsl:empty? (-> any? boolean?) empty?)
 (lsl:null? (-> any? boolean?) null?)
 (lsl:remove-all (-> any? any-list? any) remove-all)
 (lsl:remove-duplicates (-> any-list? any-list?) remove-duplicates))
