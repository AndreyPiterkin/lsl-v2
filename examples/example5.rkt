#lang lsl-v2

(define-struct add (left right))
(define-struct mul (left right))
(define-struct div (num den))
(define-struct var ())
(define-contract ArithExpr (OneOf Real
                                  (Struct add [ArithExpr ArithExpr])
                                  (Struct mul [ArithExpr ArithExpr])
                                  (Struct div [ArithExpr ArithExpr])
                                  (Struct var [])))

(define-struct divide-by-zero ())
(: eval (Function (arguments [_ Real] [_ ArithExpr]) (result Real) #;(raises divide-by-zero)))
(define (eval v a)
  (cond [(number? a) a]
        [(add? a) (+ (eval v (add-left a)) (eval v (add-right a)))]
        [(mul? a) (* (eval v (mul-left a)) (eval v (mul-right a)))]
        [(div? a) (let ([d (eval v (div-den a))])
                    (if (zero? d)
                        (raise (make-divide-by-zero))
                        (/ (eval v (div-num a)) d)))]
        [(var? a) v]))

(eval 3 (make-add (make-mul 2 (make-var)) 2))
