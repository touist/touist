(** Process a CNF AST given by {!TouistCnf.ast_to_cnf}
    to create Minisat-compatible clauses with {!minisat_clauses_of_cnf} and
    solve them with {!solve_clauses}.
*)

(** {2 CNF to clauses} *)

(** [minisat_clauses_of_cnf ast] takes a CNF [ast] and outputs
    - a list of lists of Minisat litterals,
    - a mapping table (Minisat litterals -> name of the proposition)
*)
val minisat_clauses_of_cnf :
  TouistTypes.Ast.t ->
  Minisat.Lit.t list list * (Minisat.Lit.t, string) Hashtbl.t

(** {2 Solving clauses (using Minisat)} *)

module Model :
sig
  type t = (Minisat.Lit.t * Minisat.value) list
  val dump : (Minisat.Lit.t * Minisat.value) list -> string
  val pprint :
    ?sep:string ->
    ('a, string) Hashtbl.t -> ('a * Minisat.value) list -> string
end
module ModelSet :
sig
  include Set.S
  val dump : t -> string
  val pprint : (Minisat.Lit.t, string) Hashtbl.t -> t -> string
end

(** [solve_clauses] finds the models for the given clauses.

    [print model N ] is a function that will print a model as soon as it is
      found. [N] is the number of the model, it begins at 1.
      It can be useful to print the models as they appear because finding all
      models (if [limit] is large) can be extremely long.
      Example: [~print:(TouistSat.Model.pprint table model)]

    [verbose] allows to turn on the verbose mode of minisat; apparently, this
      minisat feature doesn't seem to be working and doesn't display any time
      information.

    [continue model nth] is a function called after every model that has been
      found. [model] contains the found model and [N] says that this model was
      the nth model found. This function tells [solve_clauses] to go on searching
      models or not.
*)
val solve_clauses :
?verbose:bool ->
?print:(Model.t -> int -> unit) ->
?continue:(Model.t -> int -> bool) ->
Minisat.Lit.t list list * (Minisat.Lit.t, string) Hashtbl.t ->
ModelSet.t ref

(* [string_of_clause] dumps the clause in its literal-number form:
   e.g., 1 -5 3 9 -2 -7 *)
val string_of_clause : Minisat.Lit.t list -> string

(* [string_of_clauses] does {!string_of_clause} with newlines between clauses. *)
val string_of_clauses : Minisat.Lit.t list list -> string
