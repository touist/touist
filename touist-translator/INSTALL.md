## Building the `touistc` binary

### Prerequisites
Before you go, make sure you have the following installed:
- `ocaml` **>= 4.02.3**
- `menhir` **>= 20150118** (an equivalent of ocamlyacc)
- `fileutils` (for reading/writing POSIX files)
- `minisat` (for solving SAT problems)

**Note**: the binary `touistc` produced must be moved into `./touist-gui/external`. After moving it, you go into `./touist-gui` and run `ant`.

### Build it on linux
```shell
apt-get install opam
opam install menhir fileutils
opam pin add -y minisat "https://github.com/maelvalais/ocaml-minisat.git#v0.0.2"
make
```
### Build it on Mac OS X
```shell
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install opam
opam install menhir fileutils minisat
opam pin add -y minisat "https://github.com/maelvalais/ocaml-minisat.git#v0.0.2"
make
```
### Build it on windows
Just follow the instructions for installing [Opam and Ocaml on windows](http://fdopen.github.io/opam-repository-mingw/installation/) (link working on 2016-10-26). Thanks to fdopen on github, we have opam on windows!

## What is `oasis`?

As you probably noticed, there is a file named `_oasis`. This file contains the instructions to create the build system (`./configure`, `setup.ml` and `Makefile`) for the project. It works similarly to the `configure.ac` when you use `autoconf`. If you need to change the build behaviour (e.g. add a pre-build script, move some file after building...) you must install `oasis`:

    opam install oasis

## Notes on version numbers

To be able to have a version number synchronized with the `git` version,
the makefile `TopMakefile` (run it with `make -f TopMakefile` will try to run
the command:

	git describe --tags

and put the result into `src/version.ml`. If `git` is not found, the version
number will be `n/a`.
