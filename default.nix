{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
pkgs.eggDerivation {
  src = ./.;

  name = "egg2nix-0.1";
  buildInputs = with eggs; [
    versions matchable http-client
  ];

  installPhase = ''
    mkdir -p $out/bin/
    mv egg2nix.scm $out/bin/egg2nix
    chmod +x $out/bin/egg2nix

    runHook postInstall #important - wraps the stuff in $out/bin/
  '';
}
