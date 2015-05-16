TouIST Project
==============
![Formulas](https://cloud.githubusercontent.com/assets/2195781/7631376/8b0a1e66-fa41-11e4-9d14-5fd39da7c494.png)

TouIST is a user-friendly tool for solving propositionnal logic problems using a high-level logic language (known as the _bigand_ format or syntax or language). This language allows complex expressions like _big and_, _sets_... 

We can for example solve the problem "Wolf, Sheep, Cabbage", or a sudoku, or any problem that can be expressed in propositionnal logic.

The TouIST has been initialized by Frederic Maris and Olivier Gasquet, associate professors at the _Institut de Recherche en Informatique de Toulouse_ (IRIT). It is a "second" or "new" version of a previous program, [SAToulouse](http://www.irit.fr/satoulouse/).

The development is done by a team of five students in first year of master's degree at the _Université Toulouse III — Paul Sabatier_. This project is a part of their work at school.

## Download it
[Get the last release](https://github.com/olzd/touist/releases). For now, Touist is compatible with:

- Mac OS X Intel 64 bits
- Linux 64 bits

Touist is platform-specific because of the ocaml `touist` translator that translates the high-level `.touistl` (touist language files) into `SAT_DIMACS` or `SMT2` is compiled into an architecture-specific binary (for performances).

We have some issues with compiling the ocaml translator for Windows. Some of the first releases have been compiled for Windows, but the tool we used has been discontinued ([see corresponding issue](https://github.com/olzd/touist/issues/5)).

## How does Touist work?
![archi](https://cloud.githubusercontent.com/assets/2195781/7631517/94c276e0-fa43-11e4-9a5c-351b84c2d1e1.png)

- **GUI interface**: the Java/Swing window that opens where you click on `touist.jar`. When you click on `Test`, the GUI will ask the Translator to translate the touistl file. Then, the GUI asks the Solver to compute the models.
- **Translator**: the program `external/touistc` that translates a `.touistl` file into a `.sat/.smt` file. This program, developed during this project, can be also used in command line. `./external/touistc --help` will show you what you can do.
- **Solver SAT/SMT**: it takes the `.sat`/`.smt` file and computes the models. We just embed existing binaries/libraries.

## What exists besides Touist
SAToulouse had several drawbacks and had to be re-written. If you want to have a look at what SAToulouse looks like, see http://www.irit.fr/satoulouse/.