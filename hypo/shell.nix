{ pkgs ? import <nixpkgs> {} }:

let
  hs = pkgs.haskellPackages.ghcWithPackages (p: [ p.random ]);
in

pkgs.mkShell {
  buildInputs = [
    hs
    pkgs.ghcid
  ];
}
