#lang scribble/base
 
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

TODO: Copy from slide and review it a bit.

@section{Implementation Timeline}

TODO: Copy from slide and review it.
