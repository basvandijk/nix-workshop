#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
cabal get hopenssl-2.2.1
cd hopenssl-2.2.1

# Set up development environment.
cabal2nix --shell . >shell.nix

# Can configure because of missing OpenSSL library.
nix-shell --run "cabal configure" || true

# Check which outputs exist for OpenSSL.
nix-instantiate --eval "<nixpkgs>" -A openssl.outputs
inc=$(nix-build --no-out-link "<nixpkgs>" -A openssl.dev)
lib=$(nix-build --no-out-link "<nixpkgs>" -A openssl.out)
nix-shell --run "cabal configure --extra-include-dirs=$inc/include --extra-lib-dirs=$lib/lib --enable-test"
