(** {b Requires [qbf]} Process a CNF AST to clauses in order to solve
    them with Quantor. *)

(* [ocamlqbf_of_ast] transforms a touist quantified formula to a Qbf.QFormula.
   The formula does no need to be in cnf. After calling this function, you
   should use [QFormula.cnf].
   This function had been written before {!Touist.Qbf.cnf} existed, this is why
   I wrote a second function [qcnf_of_cnf] which allows me to use my own
   CNF function (i.e., {!Touist.Qbf.cnf}). *)
val ocamlqbf_of_ast :
  Touist.Types.Ast.t -> Qbf.QFormula.t * (Qbf.Lit.t, string) Hashtbl.t

val qcnf_of_cnf :
  Touist.Types.Ast.t -> Qbf.QCNF.t * (Qbf.Lit.t, string) Hashtbl.t

val solve :
  ?hidden:bool -> Qbf.QCNF.t * (Qbf.Lit.t, string) Hashtbl.t -> string option

val enabled : bool
(** Is this library enabled? (requires qbf to be installed) *)
