Touist is composed of:
- `touist` is the command-line tool for solving `.touist` problems
- `touist.jar`, (in `support/gui/`) is a graphical interface for `touist`.

Building `touist`
=================

## Prerequisites
Before you go, make sure you have OCaml installed. To install it:

    apt-get install ocaml                # Ubuntu
    brew install ocaml                   # MacOS

Afterwards, install the OPAM (OCaml package manager) dependencies:

    opam init
    opam install menhir fileutils minisat

## Build on Linux & MacOS

    ./configure
    make

## Build it on windows

You will need to get Cygwin + OCaml to build a native `touist` binary. Follow
the instructions for installing [Opam and Ocaml on
windows](http://fdopen.github.io/opam-repository-mingw/installation/)
(visited on 2016-10-26). Thanks to fdopen on github, we have opam on
windows! After installing cygwin, please refer to `ci/appveyor-script.sh`
for the different build steps.

## What is `oasis`?

As you probably noticed, there is a file named `_oasis`. This file contains
the instructions to create the build system (`./configure`, `setup.ml` and
`Makefile`) for the project. It works similarly to the `configure.ac` when
you use `autoconf`. If you need to change the build behaviour (e.g. add a
pre-build script, move some file after building...) you must install
`oasis`:

    opam install oasis

## Notes on version numbers

To be able to have a version number synchronized with the `git` version,
the makefile `Makefile` will try to run the command:

	git describe --tags

and use the `cppo` preprocessor on `src/lib/version.cppo.ml` to produce the
file `_build/src/lib/version.ml` containing the git version.

----------

Building the Java GUI (`touist.jar`)
===================================

Steps:

1. first, build `touist` (see above)
2. go to `support/gui` and run `./gradlew build`
   (see `support/gui/INSTALL.md` for more details)
3. to run it, you can do `./gradlew run`
