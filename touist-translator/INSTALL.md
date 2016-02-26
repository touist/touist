## Building the `touistc` binary

### Prerequisites
Before you go, make sure you have the following installed:
- `ocaml` **4.01.0** (or latest)
- `menhir` (equivalent of ocamlyacc)
- `fileutils` (for reading/writing POSIX files)

**Note**: the binary `touistc` produced must be moved into `./touist-gui/external`. After moving it, you go into `./touist-gui` and run `ant`.

### Build it on linux
```shell
apt-get install opam
opam install menhir fileutils
make
```
### Build it on Mac OS X
```shell
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install opam
opam install menhir fileutils
make
```
### Build it on windows
First, you need to install `wodi`. 
Then, in `wodi`, you must install `menhir` and `fileutils`.
Then, just launch `make` in `touist-translator`.
### Build it without `make`
Alternatively, you can build by using `ocamlbuild -use-menhir -package fileutils
touistc.native`

## What is `oasis`?

As you probably noticed, there is a file named `_oasis`. This file contains the instructions to create the build system (`./configure`, `setup.ml` and `Makefile`) for the project. It works similarly to the `configure.ac` when you use `autoconf`. If you need to change the build behaviour (e.g. add a pre-build script, move some file after building...) you must install `oasis`:

    opam install oasis

## Notes on version numbers

To be able to have a version number synchronized with the `git` version,
the script `./generate-version-number.sh` will try to run the command 

	git describe --tags

and put the result into `version.ml`. If `git` is not found, the version
number will be `n/a`.
