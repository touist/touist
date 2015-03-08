# Compiler

This is the compiler that compiles files from the `touisl` language to the well-known [DIMACS](http://www.satcompetition.org/2009/format-benchmarks2009.html) format.

`touisl` is the high-level logic langage for expressing succintly complex formulas in propositionnal logic (and more).

## Grammar

Input language grammar

TODO

## Example

### The input file
File `foo.touistl`:
```
;; this is a comment
;; comment MUST be preceded by TWO semicolons
begin sets
  $A = [a, b, c]
  $B = card($A)
end sets

begin formula
  bigand $i in $A do
    A($i) and B($i)
  end

  bigand $i in $A when ($B > 2) do
    not B($i) and C($i)
  end
end formula
```
### Running the example 
```
touistc.native foo.touistl
```
### Understanding the output files
There will be two output file:
- a DIMACS file
- a mapping of the variables' name to their DIMACS representation (a number)

`foo.cnf`:
```
c CNF format file
p cnf 9 12
9 0
6 0
8 0
4 0
7 0
2 0
-6 0
5 0
-4 0
3 0
-2 0
1 0
```

`.foo_table`:
```
A(b) 8
B(a) 6
C(a) 5
B(b) 4
C(c) 1
C(b) 3
B(c) 2
A(a) 9
A(c) 7
```

## Build

Before you go, make sure you have the following installed:
- `ocaml` **4.02.0** (or latest)
- `menhir` (equivalent of ocamlyacc)
- `fileutils` (for reading/writing POSIX files)

You can install them with `opam` (`opam` is an `apt-get`-like package manager for OCaml):
```
$ opam install menhir fileutils
```

Then, to build the compiler just run `make`.
You can also run `make install` to install it to the `/usr/local/bin/` directory
and `make uninstall` to remove it.
To clean up, use `make clean` and `make distclean`.

Alternatively, you can build by using `ocamlbuild -use-menhir -package fileutils
touistc.native`.
