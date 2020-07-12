# Datalog and Haskell

Small steps towards various analyses of Haskell code based on information in
.hie files using datalog.

- `Extract.hs` reads its own `.hie` file and produces 3 Souffle fact files:

    1. `defined.facts` -- top level values defined in the module
    2. `exported.facts` -- values exported from the module
    3. `uses.facts` -- mapping from defined values to uses of internal an external values

- `haskell.dl` constructs the callgraph from the three facts files.

- `unused.dl` uses the callgraph to find unused values. A small analysis in the
    spirit of [weeder][].

[weeder]: https://hackage.haskell.org/package/weeder
