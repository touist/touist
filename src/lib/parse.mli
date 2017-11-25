(** Parse a TouIST string into an Abstract Syntaxic Tree (AST).

    After this step, the AST (its type is {!Touist.Types.Ast.t}) can go through
    different functions:

    - (1) {!Touist.Eval.eval} for type-checking and
    evaluation of the expressions (bigor, bigand, variables...):
    - (2) {!Touist.Cnf.ast_to_cnf} and then {!Touist.SatSolve.minisat_clauses_of_cnf}
          to transform the AST into a clause ready to use by Minisat
    - (2') {!Touist.Smt.to_smt2} to transform the AST into LIB-SMT2
    - (2'') {!Touist.Qbf.prenex} to transform the CNF AST into QDIMACS
    - (3) {!Touist.SatSolve.minisat_clauses_of_cnf} and {!Touist.SatSolve.solve_clauses}
          to solve the SAT problem
*)

(** {2 Parsing TouIST} *)

(** [parse_sat text] parses [text] and outputs the corresponding [ast].

    [~debug:true] enable the display of the automata state number on parser
    errors.

    [~filename:"foo.touist"] enables the display of a file name in errors.
*)
val parse_sat :
  ?debug_syntax:bool -> ?filename:string -> string -> Touist.Types.Ast.t

val parse_smt :
  ?debug_syntax:bool -> ?filename:string -> string -> Touist.Types.Ast.t

val parse_qbf :
  ?debug_syntax:bool -> ?filename:string -> string -> Touist.Types.Ast.t

(** {2 Utility functions} *)

val string_of_chan : in_channel -> string
val string_of_file : string -> string
