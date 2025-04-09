#lang racket

#;(
   (define-struct cons (first rest))
   (define-struct empty ())

   (define (ListofNatural) (OneOf (Struct cons (Natural ListofNatural))
                                  (Struct empty ())))

   (define ListofNatural
     (lambda ()
       (new oneof%
            [disjuncts (list (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))])
                             (new struct% [struct empty] [contracts (list)]))])))

   (new oneof%
        [disjuncts (list (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))])
                         (new struct% [struct empty] [contracts (list)]))])

   (send ListofNatural protect '(cons 1 (cons 2 empty)))
   (send (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))]) protect '(cons 1 (cons 2 empty)))
   (cons 1 (cons 2 empty))
   (send Natural protect 1)
   (send (new lazy% [promise (delay (ListofNatural))]) protect (cons 2 empty))
   (send (ListofNatural) protect (cons 2 empty))
   (send (new oneof%
              [disjuncts (list (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))])
                               (new struct% [struct empty] [contracts (list)]))])
         protect
         (cons 2 empty))
   (send (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))]) protect (cons 2 empty))
   (send Natural protect 2)
   (send (new lazy% [promise (delay (ListofNatural))]) protect empty)
   (send (ListofNatural) protect empty)
   (send (new oneof%
              [disjuncts (list (new struct% [struct cons] [contracts (list Natural (new lazy% [promise (delay (ListofNatural))]))])
                               (new struct% [struct empty] [contracts (list)]))]) protect empty)
   (send (new struct% [struct empty] [contracts (list)]) protect empty)
   (void)
   )