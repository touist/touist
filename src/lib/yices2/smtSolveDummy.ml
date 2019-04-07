(** {b Requires [yices2]} Process an evaluated AST in order to solve
    it with Yices2. *)

let ast_to_yices _ = failwith "Yices2 not available, recompile with Yices2"
let string_of_model ?(value_sep="\n") _ _ = failwith ("Yices2 not available, recompile with Yices2" ^ value_sep)
let solve _ _ = failwith "Yices2 not available, recompile with Yices2"
let logic_supported _ = failwith "Yices2 not available, recompile with Yices2"
let enabled = false