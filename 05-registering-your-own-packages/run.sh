#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
test -d cabal || {
    nix-env -f "<nixpkgs>" -iA git
    git clone --depth=1 git://github.com/haskell/cabal.git
}

cd cabal/Cabal && cabal2nix . >default.nix && cd -
cd cabal/cabal-install && cabal2nix . >default.nix && cd -

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
        });

      };

    };

    cabal-install-git = self.haskellPackages.cabal-install-git;

  };

}
EOF

# Install it.
nix-env -f "<nixpkgs>" -iA cabal-install-git
