Introduction
============



Part 1
======

Problem description, background, introduction to Nix.

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


Part 2
======

* Describe the product that we would like to build: TODO-list web-app with a
  backend for persistent storage. The product consists of the following
  components:

  * Frontend: A GHCJS single-page-app using [miso] for the
    Model-View-Controller.  Note that we can just extend the [todo-mvc example]
    with a backend.

  * Backend: a [servant-server] serving the frontend and some endpoints for
    listing and adding TODO items.

  * todo-list-api: a Haskell package exporting a [servant] API that is used by
    both the frontend (for call the backend) and the backend (for serving the
    endpoints).

  * PostgreSQL database


[miso]: https://haskell-miso.org/
[todo-mvc example]: https://todo-mvc.haskell-miso.org/
[servant-server]: http://hackage.haskell.org/package/servant-server
[servant]: http://hackage.haskell.org/package/servant
