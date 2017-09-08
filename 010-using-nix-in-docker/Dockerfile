FROM nixos/nix

# Switch the installation to the release-17.09 branch.
RUN nix-channel --add https://nixos.org/channels/nixos-17.09 nixpkgs
RUN nix-channel --update
RUN nix-env -u --always

# Make sure that "<nixpkgs>" refers to the right place.
RUN echo >>/etc/profile export NIX_PATH=nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs/nixpkgs

# Make sure that nix-env and friends find all Nixpkgs attributes in the
# top-level namespace, i.e. without any "nixpkgs" or "nixos" prefixes.
RUN rm -r ~/.nix-defexpr
RUN ln -s /nix/var/nix/profiles/per-user/root/channels/nixpkgs/nixpkgs ~/.nix-defexpr

# Install a basic Haskell development environment.
RUN nix-env -iA cabal-install stack ghc

WORKDIR /root
RUN echo >hello-world.hs 'main = putStrLn "Hello, World"'
RUN ghc --make hello-world

# Install a more sophisticated shell environment.
RUN nix-env -iA bashInteractive bash-completion
RUN echo >>/etc/bashrc . /etc/profile
RUN echo >>/etc/bashrc BASH_COMPLETION_COMPAT_DIR=~/.nix-profile/etc/bash_completion.d
RUN echo >>/etc/bashrc 'test -z "${PS1:-}" || . ~/.nix-profile/share/bash-completion/bash_completion'
