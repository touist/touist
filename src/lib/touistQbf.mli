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
val prenex : ?debug:bool -> TouistTypes.AstSet.elt -> TouistTypes.Ast.t

(** [cnf ast] calls {!TouistCnf.ast_to_cnf} on the inner formula
    (with no quantifiers) and existentially quantifies any Tseitlin
    variable in an innermost way.

    [ast] must be in Prenex Normal Form. *)
val cnf : ?debug:bool -> TouistTypes.Ast.t -> TouistTypes.AstSet.elt

(** [print_qdimacs out out_table ast] takes a Prenex-CNF formula
    {!TouistTypes.Ast.t} and prints the following:
    - 1) the mapping table (litterals int to name)
    - 2) dimacs header line ('p cnf 3 2')
    - 3) the quantifiers lines grouped (one quantifier per line, beginning with
    'e' or 'a' and ending by 0)
    - 4) the clauses (one conjunction per line, one line is a disjunction,
    minus means 'not'). *)
val print_qdimacs : ?debug_dimacs:bool -> out_channel -> out_channel option -> TouistTypes.Ast.t -> (int, string) Hashtbl.t

(** {2 CNF to clauses} *)

(** [A] means 'forall', [E] means 'exists' *)
type 'a quantlist = A of 'a list | E of 'a list

(** [qbfclauses_of_cnf] translates an AST (which is in CNF) to three
    structures:
    - 1) a list of quantlist which reprensents the grouped quantifiers in the
         Prenex Normal Form.
    - 2) a list of lists of integers which represents the CNF formula embedded
         in the Prenex Normal Form.
    - 3) a correspondance table 'int -> litteral names'
*)
val qbfclauses_of_cnf :
  TouistTypes.Ast.t ->
  int quantlist list * int list list * (int, string) Hashtbl.t

(** {2 Utility functions} *)

(** [is_unquant] checks that the given formula does not contain any quantors. *)
val is_unquant : TouistTypes.AstSet.elt -> bool
val is_prenex : TouistTypes.AstSet.elt -> bool

(** [regroup_quantors] gathers all succeeding Forall and Exists to a list
    of list such that each sublist only contains one type of quantor.
    Example:   {[
      Forall ("a",Forall ("b",Exists ("c", Forall ("d",_)))
    ]}  becomes  {[
      [A of ["a";"b"]; E of ["c"]; A of ["d"]]
    ]}
*)
val regroup_quantors :
  TouistTypes.Ast.t ->
  string quantlist list -> string quantlist list * TouistTypes.Ast.t