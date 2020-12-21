let ocamlqbf_of_ast _ = failwith "Qbf not available, recompile with Qbf"

let qcnf_of_cnf _ = failwith "Qbf not available, recompile with Qbf"

let solve ?(hidden = false) _ =
  failwith ("Qbf not available, recompile with Qbf" ^ string_of_bool hidden)

let enabled = false
