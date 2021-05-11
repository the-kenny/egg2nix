{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
pkgs.eggDerivation {
  src = ./.;

  name = "egg2nix-0.4";
  buildInputs = with eggs; [
    args matchable srfi-1
  ];
}
