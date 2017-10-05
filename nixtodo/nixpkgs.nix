# This expression returns the nix/store path to our version of nixpkgs.
# It ensures that all engineers use the same revision of nixpkgs.
#
# This technique was inspired by the article:
#
#   Reproducible Development Environments by Rok Garbas
#   https://garbas.si/2015/reproducible-development-environments.html

let pkgs = import <nixpkgs> {};

    nixpkgs = pkgs.fetchFromGitHub {
       owner   = "NixOS";
       repo    = "nixpkgs";
       rev     = "3c0ea4fa4b931501212e1cf2708ea67cc3dbbcdf";
       sha256  = "11iwvnbwns4gdfakjr8qxzzcbzb718chlihxrm2fypg91k1kh7ws";
     };

    patches = [
      # # Adds the postage package and NixOS module
      # (pkgs.fetchpatch {
      #   url = "https://github.com/NixOS/nixpkgs/commit/943c78b10d8ed4418dbb6fb9a89e6f416af511d5.patch";
      #   sha256 = "13vhrkihbw7nrwplxlhfvwm493h53y7yzs8j5nsxnhv70hhpiwc4";
      # })
    ];

in pkgs.runCommand ("nixpkgs-" + nixpkgs.rev) {inherit nixpkgs patches; } ''
  cp -r $nixpkgs $out
  chmod -R +w $out
  for p in $patches ; do
    echo "Applying patch $p"
    patch -d $out -p1 < "$p"
  done
''
