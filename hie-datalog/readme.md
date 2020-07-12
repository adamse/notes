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

To see the result run `nix-shell --command "make all"`. At the end you will get
a `unused.csv` listing the unused top level values in the `Extract.hs` file. 

At this moment it looks like this:

```
Extract	show	115	13
Extract	show	199	13
Extract	showList	115	13
Extract	showList	199	13
Extract	showsPrec	115	13
Extract	showsPrec	199	13
Extract	asdf	125	1
Extract	definitionFromName	219	1
```

Listing the module, value name and line and column where the unused value is
defined.
