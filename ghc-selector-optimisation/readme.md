# GHC's field selector optimisation

While looking at implementing GHC ticket [#17991][] I learned about the what
and why of the field selector optimisation.

[#17991]: https://gitlab.haskell.org/ghc/ghc/-/issues/17991


## What are selectors?

When compiling GHC's intermediary language STG selectors are identified by
expressions/closures that look like this:

```
-- STG code
case variable of
  Constructor ... x ... -> x
```

So selectors are case expressions with one free variable that match one constructor
selecting just one field of that constructor.

Haskell expressions of that same form will most likely be compiled to similar
looking STG but other source Haskell forms like

```
-- Haskell code, fst as defined in the Prelude
fst :: (a,b) -> a
fst (x,_) = x
```

will also be compiled to STG code that matches the is selector criteria. These
selector expressions are identified and compiled specially in the first equation of
[`GHC.StgToCmm.Bind.mkRhsClosure`][mkRhsClosure].

[mkRhsClosure]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/compiler/GHC/StgToCmm/Bind.hs#L261


## Compilation of closures

GHC compiles closures to various special pieces of data and code in the final executable:

- Header 
    - Closure type
    - Closure info table
- Payload, pointers and values
- Code to evaluate the closure

Selectors get their own special closure type called `THUNK_SELECTOR` (defined
in [includes/rts/storage/ClosureTypes.h][ClosureTypes] and their own special
info table and code built in handcrafted Cmm. 

The Cmm code lives in [rts/StgStdThunks.cmm][StgStdThunks] written there by a
weird amalgamation of C-preprocessor macros and the GHC specific Cmm language.
The code starts with `INFO_TABLE_SELECTOR`, a special directive to the Cmm
parser that makes the compiler emit a thunk with the special closure type and
closure layout and following it is code that will evaluate the selectee if
necessary and then select the wanted field from the constructor that is the
selectee.

[ClosureTypes]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/includes/rts/storage/ClosureTypes.h#L44
[StgStdThunks]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/rts/StgStdThunks.cmm#L66-108
