#lang lsl-v2


(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Leaf X) (Struct leaf [X]))
(define-contract (Node X Y) (Struct node [X Y]))

; A [Tree X] is one of:
;  - (make-leaf X)
;  - (make-node [Tree X] [Tree X])


(define-contract (Tree X)
  (OneOf
   (Leaf X)
   (Node (Tree X) (Tree X))))




;; examples

(define tree-leaf (make-leaf 1))
(define tree-node (make-node (make-leaf 2) (make-node tree-leaf tree-leaf)))

(define-contract IntTree (Tree Integer))


(: height (-> IntTree Natural))
(define (height t)
  (cond [(leaf? t) 0]
        [(node? t) (add1 (max (height (node-left t)) (height (node-right t))))]))


(height tree-node)









(: in-range (Function (arguments (max (lambda (x)
                                        (>= x min)))
                                 (min integer?)
                                 (v integer?))
                      (result boolean?)))

(define (in-range max min v)
  (<= min v max))

(in-range 5 6 3)

#;
(begin
  (define in-range
    (rt:attach-contract 'in-range
                        (lambda (min max v) (<= min v max))
                        (quote-module-name)
                        (new function%
                             [stx #'(Function (arguments (min integer?)
                                                         (max (lambda (x)
                                                                (>= x min)))
                                                         (v integer?))
                                              (result boolean?))]
                             ;; top sort happens and sets up the domain-order, the
                             ;; contracts, and the result
                             [domain-order (list 2 0 1)]
                             [domains (list (lambda ()
                                              (rt:make-immediate #'integer?
                                                                 #'integer?
                                                                 integer?))
                                            (lambda (v) (rt:make-immediate
                                                         #'integer?
                                                         #'integer?
                                                         integer?))
                                            (lambda (v min)
                                              (rt:make-immediate #'(lambda (x) (>= x min))
                                                                 #'(lambda (x) (>= x min))
                                                                 (lambda (x) (>= x min)))))]
                             [codomain (lambda (v min max) (rt:make-immediate #'boolean?
                                                                              #'boolean?
                                                                              boolean?))])))

  (in-range 6 5 3))