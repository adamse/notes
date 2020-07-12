{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.packages.ghc8101.ghcWithPackages (p: [
      p.ghc
      p.extra
      p.lens
      p.generic-lens
    ]))
    pkgs.souffle
    pkgs.ghcid
  ];
}
