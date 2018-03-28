(** Transform an evaluated AST into prenex form, CNF and QDIMACS.

    The order of calls is {!prenex} -> {!cnf} -> {!print_qdimacs}
*)

(** {2 Prenex and CNF}

    From an evaluated AST, you want to
    - (1) get the Prenex Normal (PNF) Form using {!prenex}
    - (2) on the PNF, get the Prenex-CNF using {!cnf}

*)

(** [prenex ast] takes an evaluated AST and applies the transformation rules
    in order to transform an evaluated AST into Prenex Normal Form (PNF).

    {b IMPORTANT} Because we do not know any to transform 'xor' and '<=>',
    these two connectors will be re-written using the other connectors.

    @see <https://fr.wikipedia.org/wiki/Forme_prénexe> Transformation
         rules on Wikipedia (FR)
*)
val prenex : ?debug:bool -> Types.AstSet.elt -> Types.Ast.t

(** [cnf ast] calls {!Cnf.ast_to_cnf} on the inner formula
    (with no quantifiers) and existentially quantifies any Tseitlin
    variable in an innermost way.

    [ast] must be in Prenex Normal Form. *)
val cnf : ?debug_cnf:bool -> Types.Ast.t -> Types.AstSet.elt

(** {2 CNF to clauses} *)

(** [A] means 'forall', [E] means 'exists' *)
type 'a quantlist = A of 'a list | E of 'a list

(** [qbfclauses_of_cnf] translates an AST (which is in CNF) to the tuple
    [(quants, int_clauses, int_table)]:
    - 1) [quants] is a list of quantlist which reprensents the grouped
         quantifiers in the Prenex Normal Form.
    - 2) [int_clauses] a list of lists of integers which represents the
         CNF formula embedded in the Prenex Normal Form.
    - 3) [int_table] is the mapping table from litteral integers to names.
*)
val qbfclauses_of_cnf :
  Types.Ast.t ->
  int quantlist list * int list list * (int, string) Hashtbl.t


(** [print_qdimacs (quants, int_clauses, int_table) out] takes the
    result of {!qbfclauses_of_cnf} and prints the following:
    - 1) If [~out_table] is given, print the mapping table from litterals
         integers to names. If [out] and [out_table] are the same, then the
         mapping table will be printed in DIMACS comments
         (e.g., 'c p(a,b) 5').
    - 2) the DIMACS standard header line ('p cnf 3 2')
    - 3) the quantifiers lines grouped (one quantifier per line, beginning with
        'e' or 'a' and ending by 0)
    - 4) the clauses (one conjunction per line, one line is a disjunction,
         minus means 'not').

    @see <http://www.qbflib.org/qdimacs.html> QDIMACS standard *)
val print_qdimacs :
  ?line_begin:string ->
  ?debug_dimacs:bool ->
  int quantlist list * int list list * (int, string) Hashtbl.t ->
  ?out_table:out_channel -> out_channel -> unit

(** {2 Utility functions} *)

(** [is_unquant] checks that the given formula does not contain any quantors. *)
val is_unquant : Types.AstSet.elt -> bool
val is_prenex : Types.AstSet.elt -> bool

(** [regroup_quantors] gathers all succeeding Forall and Exists to a list
    of list such that each sublist only contains one type of quantor.
    Example:   {[
      Forall ("a",Forall ("b",Exists ("c", Forall ("d",_)))
    ]}  becomes  {[
      [A of ["a";"b"]; E of ["c"]; A of ["d"]]
    ]}
*)
val regroup_quantors :
  Types.Ast.t ->
  string quantlist list -> string quantlist list * Types.Ast.t

(** [add_suffix] (used internally) adds a '_1' to a proposition name; if it
    already has '_1', replace it with '_2' and so on. **)
val add_suffix : string -> string