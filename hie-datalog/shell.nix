{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.packages.ghc8101.ghcWithPackages (p: [
      p.ghc
    ]))
    pkgs.souffle
    pkgs.ghcid
  ];
}
