TouIST, the language for propositional logic
===========================================


|      Mac, Linux      |         Windows         | [Reference manual][ref] |    Come and discuss!    |
|:--------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
| [![trv-img]][travis] | [![apvy-img]][appveyor] |  [![cir-img]][circle]   | [![gitter-img]][gitter] |

[circle]: https://circleci.com/gh/touist/touist/tree/master
[cir-img]: https://circleci.com/gh/touist/touist/tree/master.svg?style=svg
[travis]: https://travis-ci.org/touist/touist
[trv-img]: https://travis-ci.org/touist/touist.svg?branch=master
[appveyor]: https://ci.appveyor.com/project/maelvalais/touist-kila4/branch/master
[apvy-img]: https://ci.appveyor.com/api/projects/status/rayupfflmut8xbe0/branch/master?svg=true
[gitter]: https://gitter.im/touist-project/touist
[gitter-img]: https://badges.gitter.im/touist-project/touist.svg "Join the chat at https://gitter.im/touist-project/touist"

[![Screenshot of the graphical interface with a QBF
problem](docs/images/screenshot.png)](https://github.com/maelvalais/allumettes)

## Try it

1. The main command-line program, `touist`, can be installed using opam:

       opam install touist

   If you want to use the optional solvers, they must be installed first so
   that `touist` knows you want to include them:

   * SMT solver: `opam install yices2`
   * QBF solver: `opam install qbf`

   and then `opam reinstall/install touist`.

   Now, if we want to know if `a ⋀ b ⇒ c` is statisfiable:

       echo 'a and b => c' | touist - --solve

2. `touist` also has a java-based graphical interface can be downloaded in the
   [releases] page. It embeds the command-line tool. The graphical interface
   works on **macOS** (x86_64 only), **Linux** x86\_64 and **Windows** (x86 or
   x64\_86).

   ⚠️ WARNING ⚠️ On macOS Sierra, `TouIST.app` will show a 'broken' message.
   You must run `sudo spctl --master-disable` which will enable the
   _Open app from anywhere_ thing. If we want to avoid this, we have to pay
   Apple $99 every year for signing the app. We should do that at some point!

3. You can also look at the [Touist language reference][ref]
   ([pdf version][ref-pdf]).

4. Syntax coloring is also available in **[VSCode]** through the extension
   [touist][touist-vscode]. I really encourage people to give a try to VSCode.
   Even though it comes from Microsoft, it is ([kind of][vscode-license])
   open-source.

   ![touist vscode extension](https://github.com/touist/touist-vscode/raw/master/images/screenshot.png)

[releases]: https://github.com/touist/touist/releases
[ref]: http://www.irit.fr/touist/doc/reference-manual.html
[ref-pdf]: http://www.irit.fr/touist/doc/reference-manual.pdf
[VSCode]: https://code.visualstudio.com
[vscode-license]: https://github.com/Microsoft/vscode/issues/60#issuecomment-161792005
[touist-vscode]: https://marketplace.visualstudio.com/items?itemName=maelvalais.touist

## Description

TouIST is a user-friendly tool for solving propositionnal logic problems using
a high-level logic language (known as the _bigand_ format or syntax or
language). This language allows complex expressions like _big and_, _sets_...

We can for example solve the problem "Wolf, Sheep, Cabbage", or a sudoku, or
any problem that can be expressed in propositionnal logic.

The TouIST has been initialized by Frederic Maris and Olivier Gasquet,
associate professors at the _Institut de Recherche en Informatique de Toulouse_
(IRIT). It is a "second" or "new" version of a previous program,
[SAToulouse](#references).

The development is done by a team of five students[1] in first year of master's
degree at the _Université Toulouse III — Paul Sabatier_. This project is a part
of their work at school.

[1]: https://github.com/touist/touist/blob/master/CONTRIBUTORS.md

## What is Touist made of?

1. the main program, `touist`, is written in OCaml and is compiled
   into a native and standalone binary. It does the parsing, the transformations
   (e.g., latex) and embeds one solver per theory (SAT, SMT and QBF) in order
   to solve the problem.

2. the java-based graphical interface uses Java (>= jre7) and Swing; it embeds
   a copy of the `touist` binary.

Here is a small figure showing the architecture of the whole thing:  
![Architecture of touist][arch]

[arch]: docs/images/architecture.png
[DIMACS]: http://www.satcompetition.org/2009/format-benchmarks2009.html
[SMT2]: http://smtlib.github.io/jSMTLIB/SMTLIBTutorial.pdf

## Rebuilding-it
See the [./INSTALL.md][install] file.

[install]: https://github.com/touist/touist/blob/master/INSTALL.md

## Tested architectures


|                        | GNU/Linux, BSD | Windows                | macOS |
| ---------------------- | -------------- | ---------------------- | ----- |
| `touist` (opam)        | yes            | yes (mingw32+mingw64)  | yes   |
| `qbf` (opam)           | yes            | yes (minw32 only [^1]) | yes   |
| `yices2` (opam) [^2]   | yes            | no                     | yes   |
| `yices2` (source) [^2] | yes            | yes (mingw32+mingw64)  | yes   |

[^1]: the `qbf` package only works on mingw32 because of a slight bug in the
     `./configure` of quantor-3.2. See maelvalais's PR on the ocaml-qbf repo.

[^2]: yices2 needs the gmp library on the system. On linux and macos, opam
     will install it for you using the command `opam depext conf-gmp`.

## Bugs and feature requests
You can report bugs by creating a new Github issue. Feature requests can also
be submitted using the issue system.

You can contribute to the project by forking/pull-requesting.


## References

1. Khaled Skander Ben Slimane, Alexis Comte, Olivier Gasquet, Abdelwahab Heba,
  Olivier Lezaud, Frédéric Maris, and Maël Valais [La Logique Facile Avec
  TouIST (formalisez et Résolvez Facilement Des Problèmes Du Monde Réel
  )][pfia_touist2015]. In Actes Des 9es Journées d’Intelligence Artificielle
  Fondamentale (IAF 2015). 2015.

2. Khaled Skander Ben Slimane, Alexis Comte, Olivier Gasquet, Abdelwahab Heba,
  Olivier Lezaud, Frederic Maris, and Mael Valais. [Twist Your Logic with
  TouIST][ttl_touist2015]. CoRR abs/1507.03663. 2015.

3. Gasquet O., Schwarzentruber F., Strecker M. [Satoulouse: The Computational
  Power of Propositional Logic Shown to Beginners][ttl_satoulouse2011]. In:
  Blackburn P., van Ditmarsch H., Manzano M., Soler-Toscano F. (eds) Tools
  for Teaching Logic. Lecture Notes in Computer Science, vol 6680. Springer,
  Berlin, Heidelberg. 2011.

[ttl_satoulouse2011]: https://www.irit.fr/~Martin.Strecker/Publications/ticttl_satoulouse2011.pdf
[ttl_touist2015]: https://arxiv.org/abs/1507.03663
[pfia_touist2015]: http://​pfia2015.​inria.​fr/​actes/​download.​php?​conf=​IAF&​file=​Ben_​Slimane_​IAF_​2015.​pdf