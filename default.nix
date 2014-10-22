{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
pkgs.eggDerivation {
  src = ./.;

  name = "egg2nix-0.2";
  buildInputs = with eggs; [
    matchable http-client
  ];
}
