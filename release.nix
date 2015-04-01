/* Build instructions for the continuous integration system Hydra. */

{ egg2nix ? { outPath = ../egg2nix; revCount = 0; gitTag = "dirty"; }
, supportedSystems ? [ "x86_64-linux" "i686-linux" "x86_64-darwin" "x86_64-freebsd" "i686-freebsd" ]
}:

let
  pkgs = import <nixpkgs> { };
  version = egg2nix.gitTag;
  versionSuffix = "";
in
rec {
  build = pkgs.lib.genAttrs supportedSystems (system:
    let
      pkgs = import <nixpkgs> { inherit system; }; 
    in
    pkgs.callPackage (import "${egg2nix}/default.nix") {});
}
