{ pkgs ? import <nixpkgs> {}
, ghc ? "ghc883"
}:
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.packages.${ghc}.ghcWithHoogle (_: []))
    pkgs.gcc
  ];
}