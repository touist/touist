open Ast

let to_dimacs prop =
  let table    = Hashtbl.create 10
  and num_sym  = ref 1
  and nbclause = ref 0 in
  let rec go acc = function
    | Top                     -> acc (*failwith "clause is always true"*)
    | Bottom                  -> acc (*failwith "clause is always false"*)
    | Term   (x, None)        -> acc ^ string_of_int (gensym x)
    | Term   (x, _)           -> failwith ("unevaluated term: " ^ x)
    | Not    (Term (x, None)) -> acc ^ string_of_int (- (gensym x))
    | And    (x, y)           -> incr nbclause; (go acc x) ^ " 0\n" ^ (go acc y)
    | Or     (x, y)           -> (go acc x) ^ " " ^ (go acc y)
    | _ -> failwith "non CNF clause"
  and gensym x =
    try Hashtbl.find table x
    with Not_found ->
      let n = !num_sym in
      Hashtbl.add table x n; incr num_sym; n
  in
  let str = (go "" prop) ^ " 0\n" in
  let header =
    "c CNF format file\np cnf " ^ string_of_int (Hashtbl.length table)
                                ^ " "
                                ^ string_of_int (!nbclause+1)
                                ^ "\n"
  in
  header ^ str, table

let string_of_table table =
  Hashtbl.fold (fun k v acc -> acc ^ k ^ " " ^ (string_of_int v) ^ "\n") table ""
