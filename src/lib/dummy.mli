(** [fresh_dummy] generates a 'dummy' proposition named ["&i"] with [i] being a
    self-incrementing index.
    This function allows to speed up and simplify the translation of some
    forms of Or.
    NOTE: OCaml's functions can't have 0 param: we use the unit [()]. *)
val fresh_dummy : unit -> Types.Ast.t

(** [uuid] generates an uuid that using a self-incrementing index.*)
val uuid : unit -> int

(** [is_dummy name] tells (using the [name] of a litteral) is a 'dummy' literal
    that was introduced during cnf conversion; these literals are identified
    by their prefix '&'. *)
val is_dummy : string -> bool