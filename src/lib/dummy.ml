let dummy_term_count = ref 0
let uuid () =
  incr dummy_term_count; !dummy_term_count

let fresh_dummy () =
  Types.Ast.Prop ("&" ^ (string_of_int (uuid ())))

let is_dummy (name:string) : bool = (name).[0] = '&'