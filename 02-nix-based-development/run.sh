#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
rm -rf nix-derivation-1.0.1
cabal get nix-derivation-1.0.1
cd nix-derivation-1.0.1

# Enter a temporary development environment from Nixpkgs.
# The --allow-newer is due to https://github.com/Gabriel439/Haskell-Nix-Derivation-Library/issues/1.
nix-shell "<nixpkgs>" -A haskellPackages.nix-derivation.env --run "cabal configure --allow-newer"
cabal build

# Generate a development environment with cabal2nix.
cabal2nix --shell . >shell.nix
cabal clean
nix-shell --run "cabal configure --enable-test --allow-newer"
cabal test

# Interactive development.
: cabal repl lib:nix-derivation
