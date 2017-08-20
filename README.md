Advanced Haskell Development with Nix
=====================================

A workshop at [Haskell eXchange 2017] hosted by:

* [Peter Simons]
* [Bas van Dijk]

This repository contains the workshop materials.

Abstract
========

Building Haskell-based systems with a small team in a startup setting can be
challenging. Engineers need to work on various Haskell packages that might
depend on system level libraries, binaries and services. Systems go through an
continuous cycle of development, building, testing and deployment. How can you
make sure that each phase of that cycle is reproducible so engineers can work
together efficiently?

In this 2-part workshop, you will explore a technology called Nix that can make
each of these phases fully reproducible.

In the first part, you will learn the distribution-agnostic package manager Nix,
the discerning Haskell hacker's choice for creating advanced development
environments on various flavors of Linux and Darwin. Unfortunately, Nix has a
bit of an attitude and doesn't respond much to commands most other package
managers listen to, like "install" or "update". Instead, Nix speaks a mysterious
language of its own and wants you to communicate with it in "Nix expressions",
"derivations", and "environments". The complexity of these concepts has driven
many programmers to a point of desperation where they were actually prepared to
read the damn things user manual, and that is a situation this workshop intends
to remedy.

Let's not concern ourselves with user manuals. Instead, let's just do it.

Given a laptop that has the Nix package manager installed as described here, you
will take a practical look at the Haskell infrastructure it provides. That means
we will ...

* install a selection of Haskell compilers and

* compile some Haskell packages with them. Then, you'll

* let Nix do those builds, or rather we'll use Nix to

* download pre-built binaries for those packages from the central cache rather
  than compiling them locally.

* Equipped with that knowledge, you'll set up development environments that

* integrate Nix with cabal-install and stack,

* integrate system level libraries (libz, OpenSSL, etc.), and

* can be shared with other Nix users so that they can create the same
  environment.

* Last but not least, you'll add our own private packages to Nix so that they
  can be used as dependencies and

* extend and customize the Nix package set to make it fit our needs if it
  doesn't.

In the second part of the workshop, you, along with the workshop hosts, will
pretend you are all in a startup together and are building a product (a GHCJS
web application with a Haskell backend and a PostgreSQL database). You'll see
how to best structure our mono-repository to manage multiple Haskell packages of
which our product is composed. You'll learn how to setup the Nix infrastructure
such that you and your colleagues can work together. We'll introduce NixOS after
which we can write a NixOS module for running our Haskell backend using
systemd. We will show how to setup and use hydra, a Continuous Integration
server, for building and testing our packages. Finally you'll learn how to
deploy our product to our production machines using a tool called nixops.

Generally speaking, the idea is to look at concrete problems that will arise in
a Haskell developers daily life and to find a solution with Nix that can be
tried out and played with on the spot. The intention is to have a very
interactive workshop!

Participants should have a reasonable understanding of Haskell and some
knowledge of how to use cabal-install and/or stack. Experience with Nix is not
required, however.


[Peter Simons]: https://github.com/peti
[Bas van Dijk]: https://github.com/basvandijk
[Haskell eXchange 2017]: https://skillsmatter.com/conferences/8522-haskell-exchange-2017
