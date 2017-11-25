(** Evaluate an AST produced by {!Parse.parse_sat} (or any other parse
    function) so it becomes a semantically correct formula.

    [eval] is the main function.
*)

(** [eval] checks the types, evaluates TouIST-specific constructs (variables,
    bigand, bigor, let...) and returns an {b evaluated} formula.

    [ast] is the AST given by {!Parse.parse_sat} (or any other
    parsing function).

    [onlychecktypes] will limit the evaluation to its minimum in order to get
    type errors as fast as possible.

    [smt] enables the SMT mode. By default, the SAT/QBF mode is used.

    @raise Err.Fatal msg where {!Err.msg} contains the error. *)
val eval :
  ?smt:bool ->
  ?onlychecktypes:bool -> Types.Ast.t -> Types.Ast.t



(* [ast_without_loc] removes the location attached by the parser to the ast
   node. This location 'Loc (ast,loc)' allows to give the location in error
   messages.

   [ast_without_loc] must be called before any {[
     match ast with | Inter (x,y) -> ...
   ]} *)
val ast_without_loc : Types.Ast.t -> Types.Ast.t

(** [has_top_or_bot ast] checks if there is any Bot or Top in [ast]. *)
val has_top_or_bot : Types.Ast.t -> bool
