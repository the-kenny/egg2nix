/* Build instructions for the continuous integration system Hydra. */

{ egg2nix ? { outPath = ../egg2nix; revCount = 0; gitTag = "dirty"; }
}:

let
  pkgs = import <nixpkgs> { };
  version = egg2nix.gitTag;
  versionSuffix = "";
in
rec {

  # tarball = pkgs.releaseTools.sourceTarball {
  #   name = "egg2nix-tarball";
  #   src = egg2nix;
  #   inherit version versionSuffix;
  # };

  build = pkgs.lib.genAttrs [ "x86_64-linux" ] (system:
    let
      pkgs = import <nixpkgs> { inherit system; }; 
    in
    pkgs.callPackage (import "${egg2nix}/default.nix") {});
}
