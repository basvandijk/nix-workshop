FROM nixos/nix

# Switch the installation to the release-17.09 branch.
RUN nix-channel --add https://nixos.org/channels/nixos-17.09 nixpkgs \
 && nix-channel --update \
 && nix-env -u --always \
 && echo >>/etc/profile export NIX_PATH=nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs/nixpkgs

# Make sure that nix-env and friends find all Nixpkgs attributes in the
# top-level namespace, i.e. without any "nixpkgs" or "nixos" prefixes.
RUN rm -r ~/.nix-defexpr \
 && ln -s /nix/var/nix/profiles/per-user/root/channels/nixpkgs/nixpkgs ~/.nix-defexpr

# Install a basic system development environment.
RUN nix-env -iA       \
      bash-completion \
      bashInteractive \
      coreutils       \
      curl            \
      dnsutils        \
      gcc             \
      git             \
      nano            \
      openssh         \
      pkgconfig       \
      wget            \
 && echo >>/etc/shells /nix/var/nix/profiles/default/bin/bash \
 && echo >>/etc/profile 'test -z "${BASH_VERSION:-}" || . /etc/bash.bashrc' \
 && echo >>/etc/bash.bashrc BASH_COMPLETION_COMPAT_DIR=~/.nix-profile/etc/bash_completion.d \
 && echo >>/etc/bash.bashrc 'test -z "${PS1:-}" || . ~/.nix-profile/share/bash-completion/bash_completion'

# Set up a basic Haskell development environment.
RUN nix-env -iA cabal-install stack ghc
RUN cabal update

WORKDIR /root
