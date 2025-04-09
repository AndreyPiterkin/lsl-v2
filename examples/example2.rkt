#lang lsl-v2

(: in-range (Function (arguments (min integer?)
                                 (max (lambda (x)
                                        (>= x min)))
                                 (v integer?))
                      (result boolean?)))

(define (in-range min max v)
  (<= min v max))

(in-range 6 5 3)

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
                                                                 integer?
                                                                 #f
                                                                 #f
                                                                 '()))
                                            (lambda (v) (rt:make-immediate
                                                         #'integer?
                                                         #'integer?
                                                         integer?
                                                         #f
                                                         #f
                                                         '()))
                                            (lambda (v min)
                                              (rt:make-immediate #'(lambda (x) (>= x min))
                                                                 #'(lambda (x) (>= x min))
                                                                 (lambda (x) (>= x min))
                                                                 #f
                                                                 #f
                                                                 '())))]
                             [codomain (lambda (v min max) (rt:make-immediate #'boolean?
                                                                              #'boolean?
                                                                              boolean?
                                                                              #f
                                                                              #f
                                                                              '()))])))

  (#%app in-range 6 5 3))