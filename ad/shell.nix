{ pkgs ? import <nixpkgs> {} }:

let
  hask = pkgs.haskell.packages.ghc883.ghcWithPackages (p: [ p.erf p.generic-lens p.Chart p.Chart-cairo ]);
in
pkgs.mkShell {
  buildInputs = [ pkgs.ghcid hask ];
}
