#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
cabal new-update
rm -rf mtl-compat-0.2.1.3
cabal get mtl-compat-0.2.1.3
cd mtl-compat-0.2.1.3

# Build it with cabal-install.
cabal new-build

# Build it with stack.
stack config set system-ghc --global true
stack config set install-ghc --global false
# "stack init" currently doesn't work. It detects lts-12.16, but the
# nixpkgs-unstable channel hasn't updated to ghc-8.4.4 yet. To avoid
# this issue for the time being, we hard-code an older version.
echo > stack.yaml "resolver: lts-12.14"
echo >>stack.yaml "packages:"
echo >>stack.yaml "  - ."

# Build it with a more recent version of GHC.
nix-shell -p haskell.compiler.ghc861 --run "cabal new-build"
