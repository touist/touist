let dummy_term_count = ref 0
let fresh_dummy () =
  incr dummy_term_count; Types.Ast.Prop ("&" ^ (string_of_int !dummy_term_count))

let is_dummy (name:string) : bool = (name).[0] = '&'