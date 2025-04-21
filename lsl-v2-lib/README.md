# LSL-V2

## Project Structure Overview

- `lsl-v2-lib`: The total internals of `lsl-v2`, including all macros and library definitions and the reader
    - `main.rkt`: Defines the module reader for `lsl-v2`, and provides all identifiers defined in `private`
    - `private/`: All internal definitions
        - `guard.rkt`: Defines guard structs, which capture whether a contract check passed or
        failed, or partially passed
        - `proxy.rkt`: Defines the proxy, which wraps functions and checks contracts on every
        application
        - `util.rkt`: Contains miscellaneous utilities used by all files
        - `library`: Defines non-syntax library functions, like `+`, `map`, etc. imported from
        Racket
        - `runtime`: Contains the representation of contracts at runtime
        - `syntax`: Syntax-spec definitions, grammar, and macro sugar

`library` is relatively uninteresting, as it simply re-provides identifiers pulled in from Racket.
As for the other two folders, the following is an explanation in slightly higher detail:

`runtime` defines the runtime representation of contracts as Racket classes, and provides the
runtime implementation for contract checking for all of the kinds of contracts supported in lsl-v2:
function, list, oneof, allof, immediate, etc, and also contains runtime utilities that the
compilation phase expands to.

`syntax` is broken up into several files:
- `grammar.rkt`, which defines in comment form the complete language grammar, and also defines all
of the datum literals used in compilation
- `spec.rkt` defines the syntax spec, all of the nonterminal forms (based on the grammar), and the
top-level `#%lsl` form. This form must surround all LSL-V2 expressions, and is exported as
`#%module-begin` from `library/core.rkt`.
- `sugar.rkt` defines all of the de-sugaring, as DSL macros, which expand into forms defined in
`spec.rkt`
- `compile-util.rkt` provides helpers for static checks and errors
- `compile.rkt` implements the full LSL-V2 compiler, whose entry point is `compile-lsl/lsl-form`.

## Compilation Overview

There are two passes in LSL compilation. The first is handled via desugaring, with the macros
defined in `private/syntax/sugar.rkt`. These introduce identifiers, re-write contracts into known
forms, and tag the syntax with the unexpanded syntax via `syntax-property`, so that error messages
report in terms of the syntax a programmer would write.

The second phase is compilation, which enters at the top-level in `compile-lsl/lsl-form`. The
structure of the compiler mirrors 1:1 the structure of the nonterminal definitions in
`private/syntax/spec.rkt`. 
