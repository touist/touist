(** Process an evaluated AST given by {!Touist.Eval.eval} and produces
    a string in SMT-LIB2 format.

    [to_smt2] is the main function. *)

val to_smt2 : string -> Touist.Types.AstSet.elt -> Buffer.t
