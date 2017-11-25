(** Process an evaluated AST given by {!TouistEval.eval} and produces
    a string in SMT-LIB2 format.

    [to_smt2] is the main function. *)

val to_smt2 : string -> TouistTypes.AstSet.elt -> Buffer.t
