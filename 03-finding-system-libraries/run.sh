#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
cabal get hopenssl-2.2.1
cd hopenssl-2.2.1

# Set up development environment.
cabal2nix --shell . >shell.nix

# Cannot configure because of missing OpenSSL library.
nix-shell --run "cabal configure" || true

# Check which outputs exist for OpenSSL.
nix-instantiate --eval "<nixpkgs>" -A openssl.outputs
inc=$(nix-build --no-out-link "<nixpkgs>" -A openssl.dev)
lib=$(nix-build --no-out-link "<nixpkgs>" -A openssl.out)
nix-shell --run "cabal configure --extra-include-dirs=$inc/include --extra-lib-dirs=$lib/lib --enable-test"

# Configure a global extra search path.
echo >>~/.cabal/config "extra-include-dirs: $HOME/.nix-profile/include"
echo >>~/.cabal/config "extra-lib-dirs:     $HOME/.nix-profile/lib"
install -D ../nixpkgs-config.nix ~/.config/nixpkgs/config.nix
nix-env --install --attr system-libraries-env

# Write a cabal.project.local file for cabal new-build.
cat >cabal.project.local <<EOF
-- cabal.project.local

package hopenssl
  extra-include-dirs: $inc/include
  extra-lib-dirs:     $lib/lib
EOF
cabal new-build

# Now build it with stack.
stack init
echo >>stack.yaml ""
echo >>stack.yaml "extra-include-dirs: [ $inc/include ]"
echo >>stack.yaml "extra-lib-dirs:     [ $lib/lib ]"
stack build

rm -rf stack.yaml .stack-work
echo >>~/.stack/config.yaml ""
echo >>~/.stack/config.yaml "extra-include-dirs: [ $HOME/.nix-profile/include ]"
echo >>~/.stack/config.yaml "extra-lib-dirs:     [ $HOME/.nix-profile/lib ]"
stack init
stack build
