#lang scribble/manual

@title{Logical Student Language}
@author{Andrey Piterkin}
@author{Luke Jianu}
 
@section{TODOs}
@margin-note{If you give a mouse a cookie, he's going to ask for a
glass of milk.
 }
What is the purpose of the DSL you propose? What are the concepts in the domain, how do they map to
linguistic features, and what computation arises from them?

Examples of programs in your DSL.

Grammars (for macros) and signatures (for functions) for the syntax of your DSL, together with
brief purpose statements. If your DSL is large, you can present a subset in-class and provide the full specification in your design document.

Implementation milestones, breaking down the work in a sensible order. Depending on your DSL, it might
make most sense to work bottom-up, starting from runtime support and layering syntax on top. Or, it might make sense to start from the syntax of your DSL and work down towards the runtime.

@section{Purpose}

The Logical Student Language is a simple extension to ISL+ that allows
students to practice writing formal specifications.

In HtDP, students write informal specifications for their programs. Students
describe new forms of data by writing data definitions, and describe
the behavior of functions with signatures and purpose statements, as comments.

In contrast, a formal specification is written as code, and therefore is formal in the sense
that it's unambiguous. LSL supports writing data definitions, signatures, and purpose statements
in code. We call these bits of code @italic{contracts}. In addition, students can determine whether
their code satisfies these formal specifications (contracts) using a testing technique called @italic{property-based testing}.

@section{Concepts}
There are two core concepts in LSL: contracts and property-based testing (PBT).

A contract is a claim about a value or its behavior.

PBT is XYZ.

@section{Examples}

1. Data definition in code.

2. Signature in code.

3. Purpose statement in code.

4. XYZ.

@section{Grammars and Signatures}

LSL defines the following core forms on top of those provided by ISL+. 

@codeblock{
;; Binds a contract to the given id
(define-contract <id> <contract>)

<contract> := (Immediate <immediate-clause> ...)
            | (Function <args> <result> <raises>)
            | (List <contract>)
            | (Tuple <contract> ...+)
            | (OneOf <contract> ...+)
            | (AllOf <contract> ...+)
            | (Struct <id> [<contract> ...])
            | (All (<id> ...) <contract>)
            | (Exists (<id> ...) <contract>)
            | <expr> ;; must evaluate to a predicate

<immediate-clause> := (check <expr>)            ;; the expr in check position must
                                                ;; evaluate to a predicate
                    | (generate <expr>)         ;; must evaluate to a function
                                                ;; (-> Natural X) where X satisfies
                                                ;; the contract
                    | (feature <string> <expr>) ;; expr must evaluate
                                                ;; to a function accepting
                                                ;; X where X satisfies the contract
                    | (shrink <expr>)           ;; expr must evaluate to
                                                ;; a function (-> Natural X X)

<args> := (arguments [id contract] ...) ;; cyclic dependencies are not allowed
<result> := <contract> ;; all id values bound in <args> are available in <result>
<raises> := <exn-id>
   
;; Annotates an id with a contract
(: <id> <contract>)

;; Attempts to break the contract placed on <id>
(check-contract <id> <natural> <size-expr>)
;; size-expr should evaluate to a fuel number, or a
;; function from the iteration number to fuel

;; Generates a value that satisfies the given contract using the supplied fuel.
(contract-generate <contract> <maybe-fuel>)
<maybe-fuel> := |
                | <natural>
}

These core forms provide the capability to define and compose contracts, to annotate values with
contracts, and to check contracts with property-based testing.

In addition to core forms, LSL provides atomic contracts and various derived forms, which
reduce the burden on students to implement these contracts themselves.

@codeblock{
<contract> := ...
            | Any
            | True
            | False
            | Boolean
            | Natural
            | Integer
            | Real
            | String
            | Symbol
            | (Constant <expr>)  ;; equal? <expr>
            | (Maybe <contract>) ;; #f or <contract>
            | (-> <contract> ... <contract>)
}


@section{Implementation Timeline}

To implement the re-write of LSL, we plan on working bottom-up:

1. Define specification for the language

2. Decide on the contract interface

3. Implement the contract runtime for core forms

4. Implement derived forms

5. Provide ordinary functionality from ISL+

6. Implement syntax transformations and static checking.
