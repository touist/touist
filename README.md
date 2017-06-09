TouIST, the language for propositional logic
===========================================


|      Mac, Linux      |         Windows         | [Reference manual][ref] |
| :------------------: | :---------------------: | :------------------: |
| [![trv-img]][travis] | [![apvy-img]][appveyor] | [![cir-img]][circle] |

[circle]: https://circleci.com/gh/touist/touist/tree/master
[cir-img]: https://circleci.com/gh/touist/touist/tree/master.svg?style=svg
[travis]: https://travis-ci.org/touist/touist
[trv-img]: https://travis-ci.org/touist/touist.svg?branch=master
[appveyor]: https://ci.appveyor.com/project/maelvalais/touist-kila4/branch/master
[apvy-img]: https://ci.appveyor.com/api/projects/status/rayupfflmut8xbe0/branch/master?svg=true

## Try it

* You can install the command-line-based `touist` though opam:

      opam install touist

  If you want to use the optional solvers, they must be installed first so
  that `touist` knows you want to include them:
  1. SMT solver: `opam install yices2`
  2. QBF solver: `opam install qbf`
  and then `opam reinstall/install touist`.

  You can try `touist` with

      echo 'a and b => c' | touist - --solve

 * `touist` also has a java-based graphical interface can be downloaded in the
   [releases] page. It embeds the command-line tool. The graphical interface
   works on **macOS** (x86_64 only), **Linux** x86\_64 and **Windows** (x86 or
   x64\_86).

You can also look at the [Touist language reference][ref]
([pdf version][ref-pdf]).

[releases]: https://github.com/touist/touist/releases
[ref]: http://touist.github.io/reference-manual.html
[ref-pdf]: http://touist.github.io/reference-manual.pdf

## Description

TouIST is a user-friendly tool for solving propositionnal logic problems using
a high-level logic language (known as the _bigand_ format or syntax or
language). This language allows complex expressions like _big and_, _sets_...

We can for example solve the problem "Wolf, Sheep, Cabbage", or a sudoku, or
any problem that can be expressed in propositionnal logic.

The TouIST has been initialized by Frederic Maris and Olivier Gasquet,
associate professors at the _Institut de Recherche en Informatique de Toulouse_
(IRIT). It is a "second" or "new" version of a previous program, [SAToulouse].

The development is done by a team of five students in first year of master's
degree at the _Université Toulouse III — Paul Sabatier_. This project is a part
of their work at school. See [CONTRIBUTORS].

Here is the main screen with the formulas:  
![formulas]

And the screen with the sets:  
![sets]

[SAToulouse]: http://www.irit.fr/satoulouse
[CONTRIBUTORS]: https://github.com/touist/touist/blob/master/CONTRIBUTORS.md
[formulas]: https://cloud.githubusercontent.com/assets/2195781/13850422/185bcf66-ec5a-11e5-9fee-59b5c2ae38b7.png
[sets]: https://cloud.githubusercontent.com/assets/2195781/13850431/20162d82-ec5a-11e5-884a-e8b6aaafe416.png

Touist is platform-specific because of the ocaml `touist` translator that
translates the high-level `.touistl` (touist language files) into `SAT_DIMACS`
or `SMT2` is compiled into an architecture-specific binary (for performances).

We have some issues with compiling the ocaml translator for Windows. Some of
the first releases have been compiled for Windows, but the tool we used has
been discontinued ([see corresponding issue][issue5]).

[issue5]: https://github.com/touist/touist/issues/5

## What is Touist made of?
Touist uses Java (>= jre7) and embeds an architecture-specific binary, [touist]
(we coded it in ocaml), which translates touistl language to dimacs. The dimacs
files are then given to another binary, the SAT (or SMT) solver, and then
displayed to the user (cf. [DIMACS] and [SMT2]).

_touist_ can also be used in command-line.

[touist]: https://github.com/touist/touist/tree/master
[DIMACS]: http://www.satcompetition.org/2009/format-benchmarks2009.html
[SMT2]: http://smtlib.github.io/jSMTLIB/SMTLIBTutorial.pdf

## Rebuilding-it
See the [./INSTALL.md][install] file.

[install]: https://github.com/touist/touist/blob/master/INSTALL.md

------------
Here is a small figure showing the architecture of the whole program:  
![Architecture of touist][arch]

[arch]: https://cloud.githubusercontent.com/assets/2195781/7631517/94c276e0-fa43-11e4-9a5c-351b84c2d1e1.png

## Bugs and feature requests
You can report bugs by creating a new Github issue. Feature requests can also
be submitted using the issue system.

You can contribute to the project by forking/pull-requesting.


