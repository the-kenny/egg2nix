/*
   test for example like this
   $ nix-build pkgs/top-level/release-python.nix
*/

{ supportedSystems ? [ "x86_64-linux" "i686-linux" "x86_64-darwin" "x86_64-freebsd" "i686-freebsd" ]}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });
mapTestOn {
  chicken = { type = "job"; systems = [ supportedSystems ]; schedulingPriority = 4; };  
  chickenEggs = packagePlatforms pkgs.chickenEggs;
}
