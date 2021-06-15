{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.random
    p.vector
    p.lens
    p.mtl
  ]);
in

pkgs.mkShell {
  buildInputs = [
    ghc
    pkgs.haskellPackages.haskell-language-server
  ];
}
