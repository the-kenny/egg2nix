/* Build instructions for the continuous integration system Hydra. */

{ nixpkgs ? <nixpkgs>
, supportedSystems ? [ "x86_64-linux" "i686-linux" "x86_64-darwin" "x86_64-freebsd" "i686-freebsd" ]
}:

let
  pkgs = import nixpkgs { };
  version = egg2nix.gitTag;
  versionSuffix = "";
in
rec {
  build = pkgs.lib.genAttrs supportedSystems (system:
    let
      pkgs = import <nixpkgs> { inherit system; }; 
    in
    pkgs.callPackage (import ./default.nix) {});
}
