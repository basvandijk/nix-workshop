#! /bin/sh

set -eu
cd "$(dirname "$0")"

# Get the source code of the desired Haskell package.
cabal get mustache-2.2.3
cd mustache-2.2.3

# Construct a GHC environment with all necessary packages.
nix-shell --run "cabal configure --enable-test"  -p '
  haskellPackages.ghcWithPackages (pset: with pset; [
    aeson base-unicode-symbols cmdargs either hspec lens
    tar temporary text th-lift unordered-containers wreq
    yaml zlib
  ])'
cabal build

# Now include hoogle databases and haddock information.
nix-shell --run "cabal configure --enable-test"  -p '
  haskellPackages.ghcWithHoogle (pset: with pset; [
    aeson base-unicode-symbols cmdargs either hspec lens
    tar temporary text th-lift unordered-containers wreq
    yaml zlib
  ])'
cabal build

# Make this compiler our default GHC.
mkdir -p ~/.config/nixpkgs
cat <<EOF >~/.config/nixpkgs/config.nix
{
  packageOverrides = super: let self = super.pkgs; in {

    haskell-env = self.haskellPackages.ghcWithHoogle (pset: with pset; [
      aeson base-unicode-symbols cmdargs either hspec lens
      tar temporary text th-lift unordered-containers wreq
      yaml zlib
    ]);
  };
}
EOF
nix-env -iA haskell-env
