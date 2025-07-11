# TouIST, the IDE & Language for Logic (backed by SAT, SMT, QBF and Modal Logic solvers)

[![ci](https://github.com/touist/touist/workflows/ci/badge.svg)](https://github.com/touist/touist/actions?query=workflow%3Aci)

**July 4th, 2018:** on **macOS**, TouIST.jar only works with Java <= 8 (cf. [issue](https://github.com/touist/touist/issues/308)).
I am working on [a fix](https://github.com/touist/touist/pull/301). For now, the workaround is to use Java 8 😕.

|                         |
| :---------------------: |
| [🔰 Reference manual][ref] |

**New:** you can start hacking with `touist` using **Gitpod**; to start the
online editor with the touist project open, [click
here](https://gitpod.io/#/https://github.com/touist/touist/)! Once there,
`dune build` to rebuild and `touist` to test touist.

[![Screenshot of the graphical interface with a QBF
problem](docs/images/screenshot.png)](https://github.com/maelvalais/allumettes)

## Install

1. TouIST has a java-based graphical interface (which embeds the
   command-line tool). It can be downloaded from the [releases] page and is available
   for Linux, Windows and macOS. Two options are available: the plain **jar**
   for any platform or the non-signed **native** version for macOS and
   Windows (see below warning).

   ⚠ WARNING ⚠ On macOS Sierra, the native `TouIST.app` will show a
   _broken_ message. You must run `sudo spctl --master-disable` which will
   enable the _Open apps from anywhere_.

   ⚠ WARNING ⚠ On Windows 10, the native `TouIST.exe` can't be opened unless
   the _Windows Defender SmartScreen_ feature is disabled. You can still use
   the **jar** version.

2. If you only want the command-line program `touist`, it can be installed
   using either `brew` ([linux](http://linuxbrew.sh/)/[mac](http://brew.sh))
   or [`opam`][opam].

   Using `brew` (**recommended**, provides pre-built binaries):

   ```sh
   brew install touist/touist/touist          # stable version
   brew install touist/touist/touist --HEAD   # git-master version
   ```

   Using [`opam`][opam] (`yices2` and `qbf` are optionnal, you can skip them if
   you don't need the embedded SMT/QBF solvers):

   ```sh
   opam install yices2 qbf touist
   opam pin add touist --dev-repo          # git-master version
   ```

   Now, if we want to know if `a ⋀ b ⇒ c` is satisfiable:

   ```sh
   touist - --solve <<< "a and b => c"
   ```

   where `-` tells touist to expect a formula on stdin and `<<<` gives the right-hand string to stdin.

   The manual (`man touist` or `touist --help`) comes very handy, take a
   look at it!

   To build `touist` from source, see `src/HOWTODEBUG.md`.

3. You can also look at the [TouIST reference manual][ref]
   ([pdf version][ref-pdf]).

4. Syntax coloring is also available for **[VSCode][vscode]** (search
   for the `touist` extension) and for **[Vim]** (Vim support is experimental).

   ![touist vscode extension](https://github.com/touist/touist-vscode/raw/master/images/screenshot.png)

[opam]: https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system
[releases]: https://github.com/touist/touist/releases
[ref]: https://touist.github.io/doc/reference-manual.html
[ref-pdf]: https://touist.github.io/doc/reference-manual.pdf
[vscode]: https://marketplace.visualstudio.com/items?itemName=maelvalais.touist
[Vim]: https://github.com/touist/touist-vim

## Description

TouIST is a user-friendly tool for solving propositional logic problems using
a high-level logic language (known as the _bigand_ format or syntax or
language). This language allows complex expressions like _big and_, _sets_...

We can for example solve the problem "Wolf, Sheep, Cabbage", or a sudoku, or
any problem that can be expressed in propositional logic.

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
   (e.g., latex) and embeds one solver per theory (SAT, SMT, QBF and MODAL LOGIC) in order
   to solve the problem.

2. the java-based graphical interface uses Java (>= jre7) and Swing; it embeds
   a copy of the `touist` binary.

Here is a small figure showing the architecture of the whole thing:  
![Architecture of touist][arch]

[arch]: docs/images/architecture-with-modal-logic.png
[DIMACS]: http://www.satcompetition.org/2009/format-benchmarks2009.html
[SMT2]: http://smtlib.github.io/jSMTLIB/SMTLIBTutorial.pdf

## Rebuilding-it

See the [./INSTALL.md](/INSTALL.md) file for building from source.

## Tested architectures

|                        | GNU/Linux, BSD | Windows                | macOS |
| ---------------------- | -------------- | ---------------------- | ----- |
| `touist` (opam)        | yes            | yes (mingw32+mingw64)  | yes   |
| `qbf` (opam)           | yes            | yes (minw32 only [^1]) | yes   |
| `yices2` (opam) [^2]   | yes            | no                     | yes   |
| `yices2` (source) [^2] | yes            | yes (mingw32+mingw64)  | yes   |
| `--modal-logic`        | yes ([^3])     | no                     | no    |

[^1]: the `qbf` package only works on mingw32 because of a slight bug in the
     `./configure` of quantor-3.2. See maelvalais's PR on the ocaml-qbf repo.

[^2]: yices2 needs the gmp library on the system. On linux and macos, opam
     will install it for you using the command `opam depext conf-gmp`.

[^3]: --modal-logic requires launching the touist right from the root directory
     of the touist project.

## TouIST API

You can also use the `touist` library; it is installed using `opam install
touist` and requires the version 3.4.0 or above. The API reference is
[here][api]. For example, in `utop`:

```ocaml
#require "touist";;
open Touist;;
"a and b and c"
    |> Parse.parse_sat
    |> Eval.eval
    |> Cnf.ast_to_cnf
    |> SatSolve.minisat_clauses_of_cnf
    |> SatSolve.solve_clauses
        ~print:(fun tbl model _ -> SatSolve.Model.pprint tbl model |> print_endline);;
```

will return

```plain
1 c
1 b
1 a
- : SatSolve.ModelSet.t = <abstr>
```

The API is kind of _spread_ among many modules (which could be gathered in one
single module), sorry for that! We really hope to have some time to
put everything in a nice module well organized.

[api]: http://www.irit.fr/touist/api

## Bugs and feature requests

You can report bugs by creating a new Github issue. Feature requests can
also be submitted using the issue system. You can contribute to the project
by forking/pull-requesting.

## References

1. Olivier Gasquet, Andreas Herzig, Dominique Longin, Frédéric Maris, and
   Maël Valais. [TouIST Again… (Formalisez et Résolvez Facilement Des
   Problèmes Avec Des Solveurs SAT, SMT et QBF).][pfia_touist2017] In Actes Des 10es
   Journées d’Intelligence Artificielle Fondamentale (IAF 2017). 2017.

2. Khaled Skander Ben Slimane, Alexis Comte, Olivier Gasquet, Abdelwahab Heba,
  Olivier Lezaud, Frédéric Maris, and Maël Valais [La Logique Facile Avec
  TouIST (formalisez et Résolvez Facilement Des Problèmes Du Monde Réel
  )][pfia_touist2015]. In Actes Des 9es Journées d’Intelligence Artificielle
  Fondamentale (IAF 2015). 2015.

3. Khaled Skander Ben Slimane, Alexis Comte, Olivier Gasquet, Abdelwahab Heba,
  Olivier Lezaud, Frederic Maris, and Mael Valais. [Twist Your Logic with
  TouIST][ttl_touist2015]. CoRR abs/1507.03663. 2015.

4. Gasquet O., Schwarzentruber F., Strecker M. [Satoulouse: The Computational
  Power of Propositional Logic Shown to Beginners][ttl_satoulouse2011]. In:
  Blackburn P., van Ditmarsch H., Manzano M., Soler-Toscano F. (eds) Tools
  for Teaching Logic. Lecture Notes in Computer Science, vol 6680. Springer,
  Berlin, Heidelberg. 2011.

[ttl_satoulouse2011]: https://www.irit.fr/~Martin.Strecker/Publications/ticttl_satoulouse2011.pdf
[ttl_touist2015]: https://arxiv.org/abs/1507.03663
[pfia_touist2015]: http://pfia2015.inria.fr/actes/download.php?conf=IAF&file=Ben_Slimane_IAF_2015.pdf
[pfia_touist2017]: https://www.irit.fr/publis/LILAC/Conf_sans_actes/2017_Gasquet_et_al_IAF.pdf
