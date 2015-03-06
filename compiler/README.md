# Compiler

This is the compiler that produces files to the
[DIMACS](http://www.satcompetition.org/2009/format-benchmarks2009.html) format.

## Build

There're two dependencies:
- menhir
- fileutils

You can install them with `opam`:
    $ opam install menhir fileutils

Then, to build the compiler just run `make`.
You can also run `make install` to install it to the `/usr/local/bin/` directory
and `make uninstall` to remove it.
To clean up, use `make clean` and `make distclean`.
