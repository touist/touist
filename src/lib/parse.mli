(** Parse a TouIST string into an Abstract Syntaxic Tree (AST).

    After this step, the AST (its type is {!TouistTypes.Ast.t}) can go through
    different functions:

    - (1) {!TouistEval.eval} for type-checking and
    evaluation of the expressions (bigor, bigand, variables...):
    - (2) {!TouistCnf.ast_to_cnf} and then {!TouistSatSolve.minisat_clauses_of_cnf}
          to transform the AST into a clause ready to use by Minisat
    - (2') {!TouistSmt.to_smt2} to transform the AST into LIB-SMT2
    - (2'') {!TouistQbf.prenex} to transform the CNF AST into QDIMACS
    - (3) {!TouistSatSolve.minisat_clauses_of_cnf} and {!TouistSatSolve.solve_clauses}
          to solve the SAT problem
*)

(** {2 Parsing TouIST} *)

(** [parse_sat text] parses [text] and outputs the corresponding [ast].

    [~debug:true] enable the display of the automata state number on parser
    errors.

    [~filename:"foo.touist"] enables the display of a file name in errors.
*)
val parse_sat :
  ?debug_syntax:bool -> ?filename:string -> string -> TouistTypes.Ast.t

val parse_smt :
  ?debug_syntax:bool -> ?filename:string -> string -> TouistTypes.Ast.t

val parse_qbf :
  ?debug_syntax:bool -> ?filename:string -> string -> TouistTypes.Ast.t

(** {2 Utility functions} *)

val string_of_chan : in_channel -> string
val string_of_file : string -> string
