# Compiler

This is the compiler that compiles files from the `touistl` language to the well-known [DIMACS](http://www.satcompetition.org/2009/format-benchmarks2009.html) format.

`touistl` is the high-level logic langage for expressing succintly complex formulas in propositionnal logic (and more).

## Grammar

Input language grammar

TODO

## Syntax

### Reserved keywords

These are the reserved keywords of the _touistl_ language:  
**begin, end, sets, formula, if, then, else, in, not, and, or, xor, bigand, bigor, 
exact, atmost, atleast, mod, Top, Bot.**

### Operators and functions

| Boolean   | Arithmetic | Clause  | Set     |
|:----------|:-----------|:--------|:--------|
| not       | +, -       | not     | \[\]    |
| &&        | \*, /      | and     | \[..\]  |
| \|\|      | mod        | or      | union() |
| xor       | sqrt()     | xor     | inter() |
| =>        | int()      | =>      | diff()  |
| <=>       | float()    | <=>     | card()  |
| <, >      |            | bigand  | .()     |
| <=, >=    |            | bigor   |         |
| ==, !=    |            | exact   |         |
| in        |            | atmost  |         |
| empty()   |            | atleast |         |
| susbset() |            |         |         |


## Usage
From the `touistc.native --help`:
```
TouistL compiles files from the TouIST Language to SAT-DIMACS/SMT-LIB2
Usage: touistc.native [-o translatedFile] [-table tableFile] file
Note: if either tableFile or translatedFile is missing,
artibrary names will be given.
  -o The translated file
  -table The literals table table (for SAT_DIMACS)
  -help  Display this list of options
  --help  Display this list of options
```

## Example

### Input file
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
    A|$i| and B|$i|
  end

  bigand $i in $A when ($B > 2) do
    not B|$i| and C|$i|
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

## What you need to build it
Before you go, make sure you have the following installed:
- `ocaml` **4.01.0** (or latest)
- `menhir` (equivalent of ocamlyacc)
- `fileutils` (for reading/writing POSIX files)

### Build it on linux
```shell
$ apt-get install opam
$ opam install menhir fileutils
$ make
```
### Build it on Mac OS X
```shell
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
$ brew install opam
$ opam install menhir fileutils
$ make
```
### Build it on windows
First, you need to install `wodi`. 
Then, in `wodi`, you must install `menhir` and `fileutils`.
Then, just launch `make` in `touist-translator`.
### Build it without `make`
Alternatively, you can build by using `ocamlbuild -use-menhir -package fileutils
touistc.native`
