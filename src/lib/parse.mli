(** Parse a TouIST string into an Abstract Syntaxic Tree (AST).

    After this step, the AST (its type is {!Types.Ast.t}) can go through
    different functions:

    - (1) {!Eval.eval} for type-checking and
    evaluation of the expressions (bigor, bigand, variables...):
    - (2) {!Cnf.ast_to_cnf} and then {!SatSolve.minisat_clauses_of_cnf}
          to transform the AST into a clause ready to use by Minisat
    - (2') {!Smt.to_smt2} to transform the AST into LIB-SMT2
    - (2'') {!Qbf.prenex} to transform the CNF AST into QDIMACS
    - (3) {!SatSolve.minisat_clauses_of_cnf} and {!SatSolve.solve_clauses}
          to solve the SAT problem
*)

(** {2 Parsing TouIST} *)

(** [parse_sat text] parses [text] and outputs the corresponding [ast].

    [~debug:true] enable the display of the automata state number on parser
    errors.

    [~filename:"foo.touist"] enables the display of a file name in errors.
*)
val parse_sat :
  ?debug_syntax:bool -> ?filename:string -> string -> Types.Ast.t

val parse_smt :
  ?debug_syntax:bool -> ?filename:string -> string -> Types.Ast.t

val parse_qbf :
  ?debug_syntax:bool -> ?filename:string -> string -> Types.Ast.t

(** {2 Utility functions} *)

val string_of_chan : in_channel -> string
val string_of_file : string -> string
