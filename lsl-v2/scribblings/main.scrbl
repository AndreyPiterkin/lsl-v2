#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[racket/require]
@require[racket/list
         racket/sandbox
	 scribble/html-properties
	 scribble/core
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; utils

@(define (select ids)
   (let* ([ids (sort ids symbol<?)]
          [ids (map (lambda (id) (racket #,#`#,id)) ids)])
     (apply elem (add-between ids ", "))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Logical Student Language V2}
@author{Andrey Piterkin}
@author{Luke Jianu}
@author{Cameron Moy}

@defmodule[lsl-v2 #:lang]

The Logical Student Language (LSL)
is a teaching language that extends the
Intermediate Student Language (ISL)
from @emph{How to Design Programs}
with features that support formally reasoning about programs.
In particular,
LSL tightly integrates contracts
and property-based randomized testing.

@section{Inherited from ISL}

@(define stx (select '(... lambda λ local letrec let* let define quote cond else if and or require)))
@(define test (select '(check-expect check-random check-satisfied check-within check-error check-member-of check-range)))
@(define eqn (select '(eq? equal?)))
@(define bool (select '(boolean=? boolean? not true false)))
@(define nums (select '(* + / = - < <= > >= abs add1 ceiling even? exact->inexact floor inexact->exact integer? max
                        min modulo negative? number? natural? odd? pi positive? quotient real? remainder sgn sub1 sqr zero?
			sqrt expt random)))
@(define str (select '(string=? format string-length string-append list->string make-string string string->list
                       string->number string->symbol string-contains? string-copy string-downcase string-ref
                       string-upcase string? substring implode explode number->string symbol->string string<?)))
@(define lst (select '(append assoc assq build-list car cdr cons cons? eighth empty? fifth first fourth length
                       list list-ref list? memq memq? null null? remove remove-all rest reverse second seventh sixth third empty
		       member?)))
@(define ho (select '(identity andmap apply argmax argmin compose filter foldl foldr map memf ormap
                      procedure? sort)))

@tabular[#:style (style #f (list (attributes '((class . "boxed") (style . "border-spacing: 10px 5px")))))
         #:column-properties '(top)
(list (list @elem{Syntax} @stx)
      (list @elem{Testing} @test)
      (list @elem{Equality} @eqn)
      (list @elem{Booleans} @bool)
      (list @elem{Numbers} @nums)
      (list @elem{Strings} @str)
      (list @elem{Lists} @lst)
      (list @elem{Functions} @ho))]

@section{Extended from ISL}
@defform[(define-struct structure-name (field-name ...))]{

  Defines a new structure called @racket[structure-name]. The structure's fields are
  named by the @racket[field-name]s. After the @racket[define-struct], the following new
  functions are available:

  @itemize[

    @item{@racketidfont{make-}@racket[structure-name] : takes a number of
          arguments equal to the number of fields in the structure,
          and creates a new instance of that structure.}

    @item{@racket[structure-name]@racketidfont{-}@racket[field-name] : takes an
          instance of the structure and returns the value in the field named by
          @racket[field-name].}
    @item{@racketidfont{set-}@racket[structure-name]@racketidfont{-}@racket[field-name]@racketidfont{!} : takes an
          instance of the structure and a value, and sets the field named by
          @racket[field-name] in the given instance of the structure to the given value.}

    @item{@racket[structure-name]@racketidfont{?} : takes any value, and returns
          @racket[#t] if the value is an instance of the structure.}
  ]

  The name of the new functions introduced by @racket[define-struct]
  must not be the same as that of other functions or variables,
  otherwise @racket[define-struct] reports an error.
  }

@section{Contracts}

@defform[(define-contract id contract)]{
  Creates @racket[id] as an alias for @racket[contract].
}

@defform[(: id contract)]{
  Annotates @racket[id] with a contract.
  When @racket[id] is created with @racket[define],
  the contract is attached to the defined value.
}

@subsection{Primitives}

@defform[(Immediate clause ...)]{
  An @emph{immediate contract} is one that can be checked immediately,
  without imposing any future checks.
  As a shorthand,
  an ordinary LSL expression @racket[e]
  in a contract position
  is automatically converted to the immediate contract
  @racket[(Immediate (check e))].
 
  @defsubform[(check predicate-expr)]{
    The @racket[predicate-expr] is expected to produce a predicate
    that determines if a value satisfies the contract.
  }

  @defsubform[(generate gen-expr)]{
    The @racket[gen-expr] should evaluate to a function
    that takes a single natural number,
    the @emph{fuel},
    and produces a value that satisfies the contract.
    Fuel provides a rough measure of how
    hard the generator is willing to work
    to produce a value.
  }

  @defsubform[(feature name-string feature-expr)]{
    The @racket[feature-expr] function takes one argument,
    a value satisfying the contract. It then computes some
    property of that value for use in Tyche.
    An @racket[Immediate] can have more than one @racket[feature].
  }

  @defsubform[(shrink shrink-expr)]{
    The @racket[shrink-expr] function takes two arguments,
    fuel and a value to shrink. The value to shrink is
    guaranteed to satisfy the contract.
  }
}

@defform[(Function clause ...)]{
  A function contract protects a function by constraining its
  inputs and outputs. Arguments are labeled so that dependent
  properties can be checked.
  
  @defsubform[(arguments [id contract] ...)]{
    Describes the contracts on arguments.
    Any @racket[id] can be used in any @racket[contract]
    and will be bound to the concrete argument when the
    function is applied,
    so long as there is no cyclic dependency.
  }

  @defsubform[(result contract)]{
    Describes the result contract.
    All @racket[id] values are available in @racket[contract].
  }

}

@defform[(List contract)]{
  Describes a list of arbitrary length
  whose elements satisfy @racket[contract].

}

@defform[(Tuple contract ...)]{
  Describes lists where the k-th element
  satisfies the k-th @racket[contract].
  The list must have exactly the same number of elements
  as there are contracts.
}

@defform[(OneOf contract ...)]{
  A value satisfying a @racket[OneOf] contract
  must satisfy @emph{exactly one} of the
  given @racket[contract]s.
}

@defform[(AllOf contract ...)]{
  A value satisfying an @racket[AllOf] contract
  must satisfy @emph{all} of the
  given @racket[contract]s.
}

@defform[(Struct struct-id (field-contract ...))]{
  Struct contracts describe each individual field on the struct.
}

@subsection{Derived}

@deftogether[(@defidform[Any]
              @defidform[True]
              @defidform[False]
              @defidform[Boolean]
              @defidform[Natural]
              @defidform[Integer]
              @defidform[Real]
              @defidform[String]
              @defidform[Symbol])]{
  Atomic contracts that recognize their respective types.
}


@defform[(-> arg-contract ... result-contract)]{
  A shorthand for a @racket[Function] contract
  with no dependencies.
}


@section{Property-Based Randomized Testing}

@defform*[[(check-contract id)
           (check-contract id attempts)
           (check-contract id attempts size-expr)]]{
  Attempts to break the contract placed on @racket[id]
  by randomly generating inputs.
  The number of times it attempts to do so is
  controlled by @racket[attempts].
  The @racket[size-expr] controls the fuel given to
  generators. If @racket[size-expr] is a number,
  that is used as the fuel. If it is a procedure,
  then it accepts the iteration number and
  returns the fuel.
}

@defform[(contract-generate contract maybe-fuel)
         #:grammar
         [(maybe-attempts (code:line)
                          nat-expr)]]{
  Generates a value that satisfies the given contract
  using the supplied fuel.
}
