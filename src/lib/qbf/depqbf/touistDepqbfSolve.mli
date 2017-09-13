(** {b Requires [qbf]*} Process a CNF AST to clauses in order to solve
    them with Depqbf.

    * there is no easy way to get [qbf.depqbf]. The steps are
      - (1) install the depqbf library (libqdpll.a); for example, [brew install depqbf] on macos
      - (2) download ocaml-qbf (https://github.com/c-cube/ocaml-qbf/)
      - (3) [./configure --enable-depqbf && make && make install] *)

val depqbf_clauses_of_cnf :
  TouistTypes.Ast.t ->
  Qbf.Lit.t TouistQbf.quantlist list * Qbf.Lit.t list list *
  (Qbf.Lit.t, string) Hashtbl.t

val depqbf_of_cnf :
  TouistTypes.Ast.t -> Depqbf.t * (Qbf.Lit.t, string) Hashtbl.t
val solve_depqbf : ?hidden:bool -> TouistTypes.Ast.t -> string option
