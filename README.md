Advanced Haskell Development with Nix
=====================================

A workshop at [Haskell eXchange 2017] hosted by:

* [Peter Simons]
* [Bas van Dijk]

This repository contains the workshop materials.

Abstract
========

This workshop introduces Haskell developers to the purely functional package
manager [Nix] through a series of interactive practical examples. The first
hour of the workshop focuses on practical use of Nix as a tool for *Haskell
developers*: we'll examine how to create declarative reproducible development
environments for advanced Haskell hacking, how to extend and customize the Nix
package set with our own libraries, and how to integrate external system
libraries into our build processes.

The second hour looks at Nix as a tool for *Haskell application deployment*:
we'll use Nix to manage the development, continuous integration, and deployment
of a non-trivial GHCJS web application with a Haskell back-end and a PostgreSQL
database. (See the [nixtodo repo](https://github.com/basvandijk/nixtodo)).

We assume no prior knowledge of Nix, but we do assume that participants have a
basic understanding of basic Haskell development with `Cabal`, `cabal-install`,
or `stack`. All examples used in this workshop live in the Git repository
<https://github.com/basvandijk/nix-workshop>. We encourage participants to
clone that repository and to make sure they have access to a working Nix
installation *before* the event starts. We provide a [Docker
image](https://hub.docker.com/r/psimons/hex2017/) for these purposes that users
can enter by running the
[`docker-run.sh`](https://github.com/basvandijk/nix-workshop/blob/master/docker-run.sh)
script found in the `nix-workshop` repository. Participants who can't or don't
want to use Docker should please install Nix by other means as explained on the
page <https://nixos.org/nix/download.html>.

[Peter Simons]: https://github.com/peti
[Bas van Dijk]: https://github.com/basvandijk
[Haskell eXchange 2017]: https://skillsmatter.com/conferences/8522-haskell-exchange-2017
[Nix]: https://nixos.org/nix/
