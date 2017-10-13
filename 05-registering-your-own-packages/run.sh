#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
test -d cabal || git clone --depth=1 git://github.com/haskell/cabal.git

# Register the builds in the package set.
cat <<EOF >~/.config/nixpkgs/config.nix
{ pkgs }:

{
  packageOverrides = super: let self = super.pkgs; in {

    haskellPackages = super.haskellPackages.override {

      overrides = self: super: {

        Cabal-git = self.callPackage $PWD/cabal/Cabal {};

        cabal-install-git = (self.callPackage $PWD/cabal/cabal-install {}).overrideScope (self: super: {

          Cabal = self.Cabal-git;

          # Test suite stanza forces Cabal < 1.25.
          hackage-security = pkgs.haskell.lib.dontCheck super.hackage-security;

        });

      };

    };

    cabal-install-git = self.haskellPackages.cabal-install-git;

  };

}
EOF

# Install it.
nix-env -f "<nixpkgs>" -iA cabal-install-git
