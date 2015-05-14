Touist help
===========

## Quick start
#### How to write your formulas?
Each line (or block) from the `Formulas` tab represents a new formula. When you create a new line, the program will understand that it means a conjunction (_and_) of the different lines you wrote.

#### `Formulas` and `Sets`?
All your logic formulas can be inserted into the `Formulas` tab. But if you want to separate your data (the `sets`), you can do it using the `Sets` tab. There, you can create sets that can be used in the `Formulas` tab.

#### Using `SMT` (integers instead of booleans)
You can choose between SAT and SMT:

- SAT accepts only simple prepositions that you would use in a formula. For example `a and b or c`
- SMT can handle arithmetic expressions instead of simple prepositions: `a and b or (x>4)`

## What is Touist?
_From [the article published for the ttl2015 conference](https://drive.google.com/open?id=0B5mz8k-t6PT0RTBIT3RJeEI0amM&authuser=0):_
SAT provers are powerful tools for solving real-sized logic problems, but using them requires solid programming knowledge and may be seen w.r.t. logic like assembly language w.r.t. programming. Something like a high level language was missing to ease various users to take benefit of these tools. TouIST aims at filling this gap. It is devoted to propositional logic and its main features are 

1. to offer a high-level logic langage for expressing succintly complex formulas (e.g. formulas describing Sudoku rules, planification problems...)
2. to find models to these formulas by using the adequate powerful prover, which the user has no need to know about. It consists in a friendly interface that offers several syntactic facilities and which is connected with some sufficiently powerful provers allowing to automatically solve big instances of difficult problems (such as time-tables or Sudokus). It can interact with various provers: pure SAT solver but also SMT provers (SAT modulo theories - like linear theory of reals, etc) and thus may also be used by beginners for experiencing with pure propositional problems up to graduate students or even researchers for solving planification problems involving big sets of fluents and numerical constraints on them.

## Tips
- You can navigate between the tokens of the snippets (the chunks of code on the left) using `ctrl+left` and `ctrl+right`. This feature is kind of "hidden"...
- You can save your work using `ctrl+s`.
- You can use `ctrl+z/y` to undo/redo your edits
