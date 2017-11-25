(** Transform any AST (at any stage of transformation) to a string. *)

(** [string_of_ast ast] takes an abstract syntaxic tree [ast] and turn it into
    a string using its content.
    [~debug:true] allows to include all user-hidden AST stuff.
    [~show_var:f] passes [f] each time. (????)
    [~utf8:true] will output utf-8 symbols instead of plain text, e.g.,
    ∃, ∈, ⋀ instead of exists, in, and.
    [~parenthesis:true] will display more parenthesis than usual in order to
    show the priority on operators.

    NOTE: the [parenthesis] param has not been implemented yet (!!).
*)
val string_of_ast :
  ?utf8:bool ->
  ?show_var:(Types.AstSet.elt -> string) ->
  ?debug:bool -> ?parenthesis:bool -> Types.AstSet.elt -> string

(** [string_of_ast_list sep ast_list] does the same as {!string_of_ast} except
    that it prints a list of ast separated by the string [sep]. *)
val string_of_ast_list :
  ?utf8:bool ->
  ?show_var:(Types.AstSet.elt -> string) ->
  ?debug:bool ->
  ?parenthesis:bool -> string -> Types.AstSet.elt list -> string

(** [string_of_ast_type] gives the type of an AST. *)
val string_of_ast_type : ?debug:bool -> Types.Ast.t -> string

