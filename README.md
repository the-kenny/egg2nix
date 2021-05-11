# egg2nix

egg2nix is a tool to automatically generate nix-expressions for
CHICKEN eggs. Nix is a purely functional package manager and pretty
cool.

## Usage

`egg2nix [-v] file > output.nix`

If you pass `-` instead of an input file, egg2nix will read from
stdin.
