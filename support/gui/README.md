TouIST main program
===================

This folder contains the source code for the main program of the TouIST 
project. It is named GUI because of it's main feature: be the user 
interface for the SAT/SMT solver and `touist` translator. The whole GUI is written in Java/SWING.


## Java Architecture
![conception](https://www.lucidchart.com/publicSegments/view/54f46f57-1ff4-46e0-b146-65000a009e9c/image.png)

## How to change the external solver used
We had planned, at first, to allow the user to pick the solver he wanted. Because most solvers have "standard" input/output behaviours, it might be quite easy to do so.

But for now, changing the solver requires to:

- create a new `SolverWithYourExternalSolver` class that extends the `Solver` abstract class
- implement the methods that you must override (`launch`, `nextmodel`...)

It shouldn't be really hard to re-implement a new `SolverWithYourExternalSolver`, the only big problem is the multiple problems that the `exec()` environment brings in terms of communication between an external program and your own code.

We fixed that in our own implementation of `Solver`, called `SolverTestSAT4J` that uses `minisat.jar` using multiple `wait(100ms)` and so on. It drew a lot of problems like how to know when to write into `stdin`...


