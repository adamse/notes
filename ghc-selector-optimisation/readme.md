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

Will also be compiled to STG code that matches the is selector criteria. These
selector expressions are identified and compiled specially in the first equation of
[`GHC.StgToCmm.Bind.mkRhsClosure`][mkRhsClosure].

[mkRhsClosure]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/compiler/GHC/StgToCmm/Bind.hs#L261


## Compilation of field selector closures

GHC compiles closures to various special pieces of data and code in the final
executable:

- Header
    - Closure type
    - Closure info table, describing the layout of the payload
- Payload, pointers and values
- Code to evaluate the closure

(This is described better and in more detail on the GHC wiki about [heap
objects][heap-objects].)

Selectors get their own special closure type called `THUNK_SELECTOR` (defined
in [includes/rts/storage/ClosureTypes.h][ClosureTypes] and their own special
info table and code built in handcrafted Cmm.

The Cmm code lives in [rts/StgStdThunks.cmm][StgStdThunks] written there by a
weird amalgamation of C-preprocessor macros and the GHC specific Cmm language.

The code starts with `INFO_TABLE_SELECTOR`, a special directive to the Cmm
parser that makes the compiler emit a thunk with the special closure type and
closure layout and following it is the Cmm code that will evaluate the selectee
if necessary and then pick out the required field from it. Finally it jumps to
`stg_ap_0_fast(field)`, presumably to evaluate the field contents as needed.

The Cmm code comes in two flavours `upd` and `noupd` (probably meaning update),
I do not yet know what the significance of this is.

From the code in `StgStdThunks.cmm` we can see that the special selector thunks
are only available for fields one through 15 (the GHC issue is about extending
this to larger numbers). For field numbers larger than 15 GHC will currently
compile each selector to a regular `StgThunk` closure and emit code that only
applies to that closure.

[heap-objects]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects
[ClosureTypes]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/includes/rts/storage/ClosureTypes.h#L44
[StgStdThunks]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/rts/StgStdThunks.cmm#L66-149


## Field selectors during GC

All this special machinery has a purpose! During garbage collection the RTS
will recognise these special thunks and if possible evaluate them. This happens
in [`rts/sm/Evac.c:eval_thunk_selector`][eval_thunk_selector].

For example if the GC finds a field selector thunk like
`stg_sel_2_upd(evaluated_data)` it will leave just the value of field 2
in place of the selector thunk once it is done. But it is even more powerful:

If there is a chain of selector thunks like
`stg_sel_2_upd(stg_sel_3_upd(stg_sel_4_upd(some_data)))` it will leave behind
just the second field of the third field of the 4th field of `some_data` (if
there were not thunks blocking selecting).

This means that during program evaluation we might not have to run the code
associated with selecting a field, seems like a big win if it works out! Pretty
neat.

[eval_thunk_selector]: https://gitlab.haskell.org/ghc/ghc/-/blob/c4de6a7a5c6433ae8c4df8a9fa09fbd9f3bbd0bf/rts/sm/Evac.c#L1121


## Limitations and solutions

As the GHC ticket and the code we've seen the special casing only kicks in if
we are interested in selecting fields 1-15. The suggestion is the introduce an
additional special info table that has an additional payload naming the field
to select. This seems like a neat generalisation since we get just 1 (or 2, for
the `upd` and `noupd` variants) extra piece of code and eliminate all "normal"
thunks for all field selectors as well as potentially enabling the GC to do its
magic.


