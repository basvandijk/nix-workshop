#! /bin/sh

set -e
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
cabal update
cabal get mtl-compat-0.2.1.3
cd mtl-compat-0.2.1.3

# Build it with cabal-install.
cabal new-build

# Build it with stack.
stack config set system-ghc --global true
stack init
stack build

# Build it with a more recent version of GHC.
nix-shell -p haskell.compiler.ghc821 --run "cabal new-build"
