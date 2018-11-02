#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
rm -rf nix-derivation-1.0.0
cabal get nix-derivation-1.0.0
cd nix-derivation-1.0.0

# Enter a temporary development environment from Nixpkgs.
nix-shell "<nixpkgs>" -A haskellPackages.nix-derivation.env --run "cabal configure"
cabal build

# Generate a development environment with cabal2nix.
cabal2nix --shell . >shell.nix
cabal clean
nix-shell --run "cabal configure --enable-test"
cabal test

# Interactive development.
: cabal repl lib:nix-derivation
