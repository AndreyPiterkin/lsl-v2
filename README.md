# LSL-V2: Logical Student Language V2 

The Logical Student Language is a simple extension to ISL+ that allows students
to practice writing formal specifications.

In HtDP, students write informal specifications for their programs. Students
describe new forms of data by writing data definitions, and describe the
behavior of functions with signatures and purpose statements, as comments.

In contrast, a formal specification is written as code, and therefore is formal
in the sense that it's unambiguous. LSL supports writing data definitions,
signatures, and purpose statements in code. We call these bits of code
_contracts_. In addition, students can determine whether their code
satisfies these formal specifications (contracts) using a testing technique
called _property-based testing_.

This project is a re-implementation of LSL. The goal of the rewrite is
to simplify the implementation, as well as improving the static checks.

Look inside [lsl-v2/examples/](https://github.com/AndreyPiterkin/lsl-v2/tree/main/examples) for
example programs.

## Installing and Running

Check out the repo and run `raco pkg install --auto lsl-v2-lib/ lsl-v2/` to install the language.

Access the docs with `raco docs lsl-v2`
