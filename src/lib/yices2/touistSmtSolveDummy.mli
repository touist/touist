(** {b Requires [yices2]} Process an evaluated AST in order to solve
    it with Yices2. *)

val ast_to_yices :
  TouistTypes.AstSet.elt -> _ * (string, _) Hashtbl.t

(** Turn a model into a string. *)
val string_of_model :
  ?value_sep:string ->
  (string, _) Hashtbl.t -> _ -> string


(** [solve logic form] solves the Yices2 formula [form].
    [logic] can be "QF_LIA", "QF_LRA"...

    @see <http://yices.csl.sri.com/doc/smt-logics.html> Available logics
*)
val solve : string -> _ -> _ option

(** Tell if this logic string (e.g., QF_LIA) is supported by Yices2. *)
val logic_supported : string -> bool

(** Is this library enabled? (requires [yices2] to be installed) *)
val enabled : bool