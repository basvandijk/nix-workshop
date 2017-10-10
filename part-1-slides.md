% Nix Workshop
% https://github.com/basvandijk/nix-workshop
% Haskell eXchange 2017

-------------------------------------------------------------------------------

# Get Access to Nix

- Install Nix:

    - Run `./docker-run.sh` in checkout of https://github.com/basvandijk/nix-workshop.

    - `curl https://nixos.org/nix/install | sh`

    - https://nixos.org/nix/download.html

- Install NixOS:

    - https://nixos.org/nixos/download.html

-------------------------------------------------------------------------------

# Get Access to Nixpkgs

    $ git clone git://github.com/NixOS/nixpkgs.git

    $ git clone git://github.com/NixOS/nixpkgs-channels.git

    $ nix-channel --add \
        https://nixos.org/channels/nixpkgs-unstable \
        nixpkgs
    $ nix-channel --update

    $ nix-channel --add \
        https://nixos.org/channels/nixos-17.09 \
        nixpkgs
    $ nix-channel --update

-------------------------------------------------------------------------------

# Popular Nix Commands

    $ nix-env --install --dry-run ghc cabal-install
    $ nix-env -qaP --description Hackage

    $ nix-channel --list

    $ nix-shell --pure -p ghc
    $ nix-shell "<nixpkgs>" -A ghc

    $ nix-build --no-out-link "<nixpkgs>" -A ghc

    $ nix-instantiate "<nixpkgs>" -A stdenv

    $ nix-collect-garbage --delete-older-than 30d
