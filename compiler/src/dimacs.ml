open Ast

let to_dimacs prop =
  let table = Hashtbl.create 10
  and num_sym = ref 1 in
  let rec go acc = function
    | Top                     -> failwith "clause is always true"
    | Bottom                  -> failwith "clause is always false"
    | Term   (x, None)        -> acc ^ string_of_int (gensym x)
    | Term   (x, _)           -> failwith ("unevaluated term: " ^ x)
    | Not    (Term (x, None)) -> acc ^ string_of_int (- (gensym x))
    | And    (x, y)           -> (go acc x) ^ "0\n" ^ (go acc y)
    | Or     (x, y)           -> (go acc x) ^ " " ^ (go acc y)
    | _ -> failwith "non CNF clause"
  and gensym x =
    try Hashtbl.find table x
    with Not_found ->
      let n = !num_sym in
      Hashtbl.add table x n; incr num_sym; n
  in (go "" prop) ^ " 0", table


let test () =
  let p = Implies (And (Term ("a", None),
                        Term ("b", None)),
                   Implies (Or (And (Not (Term ("a", None)), Term ("c", None)),
                                     Term ("c", None)),
                                Not (Term ("a", None)))) in
  let str, table = Cnf.to_cnf p |> to_dimacs in
  print_string str

let () = test ()
