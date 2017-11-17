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
    opam install menhir minisat jbuilder re

## Build on Linux & MacOS

    jbuilder build
    jbuilder install
    jbuilder uninstall

## Build it on windows

You will need to get Cygwin + OCaml to build a native `touist` binary. Follow
the instructions for installing [Opam and Ocaml on
windows](http://fdopen.github.io/opam-repository-mingw/installation/)
(visited on 2016-10-26). Thanks to fdopen on github, we have opam on
windows! After installing cygwin, please refer to `ci/appveyor-script.sh`
for the different build steps.

## What are the `jbuild` files?

As you probably noticed, there are many files named `jbuild`. These files
contain the instructions so that `jbuilder` can builder everything.
I know that it is using s-expressions and that people prefer json or yaml (to
be fair, me too) and that there is no syntax highlighting or anything.
But `jbuilder` is very nice and more importantly, fast and maintained.

----------

Building the Java GUI (`touist.jar`)
===================================

Steps:

1. first, build `touist` (see above)
2. go to `support/gui` and run `./gradlew build`
   (see `support/gui/INSTALL.md` for more details)
3. to run it, you can do `./gradlew run`
