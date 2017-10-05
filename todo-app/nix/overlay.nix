self: super:

let
  haskellOverrides = import ./haskell-overrides.nix self;
in

{
  haskellPackages = super.haskellPackages.override haskellOverrides;
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcjsHEAD = super.haskell.packages.ghcjsHEAD.override haskellOverrides;
    };
  };
}
