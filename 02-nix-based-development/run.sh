#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
rm -rf nix-derivation-1.0.1
cabal get nix-derivation-1.0.1
cd nix-derivation-1.0.1

# Enter a temporary development environment from Nixpkgs.
# The --allow-newer is due to https://github.com/Gabriel439/Haskell-Nix-Derivation-Library/issues/1.
nix-shell "<nixpkgs>" -A haskellPackages.nix-derivation.env --run "cabal v1-configure --allow-newer"
cabal v1-build

# Generate a development environment with cabal2nix.
cabal2nix --shell . >shell.nix
cabal v1-clean
nix-shell --run "cabal v1-configure --enable-test --allow-newer"
cabal v1-test

# Interactive development.
: cabal v1-repl lib:nix-derivation
