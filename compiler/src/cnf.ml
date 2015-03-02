open Ast

let to_cnf p =
  let rec remove_impl = function
    | Top -> Top
    | Bottom -> Bottom
    | Term _ as x -> x
    | Not     x      -> Not (remove_impl x)
    | And     (x, y) -> And (remove_impl x, remove_impl y)
    | Or      (x, y) -> Or (remove_impl x, remove_impl y)
    | Xor     (x, y) -> Xor (remove_impl x, remove_impl y)
    | Implies (x, y) -> Or (Not (remove_impl x), remove_impl y)
    | Equiv   (x, y) -> Not (Xor (remove_impl x, remove_impl y))
    | Bigand _ | Bigor _ -> failwith "all bigor and bigand should have been eliminated"
  in
  let rec remove_xor = function
    | Top | Bottom | Term _ as x -> x
    | Not  x      -> Not (remove_xor x)
    | And  (x, y) -> And (remove_xor x, remove_xor y)
    | Or   (x, y) -> Or (remove_xor x, remove_xor y)
    | Xor  (x, y) -> And (Or (remove_xor x, remove_xor y), Not (And (remove_xor x, remove_xor y)))
    | Implies _ | Equiv _ -> failwith "there shouldn't be any implies/equiv left"
    | Bigand _ | Bigor _  -> failwith "all bigor and bigand should have been eliminated"
  in
  let rec push_neg_in = function
    | Top | Bottom | Term _ as x -> x
    | Not  Top            -> Bottom
    | Not  Bottom         -> Top
    | Not  (Not x)        -> push_neg_in x
    | Not  (And (x, y))   -> Or (push_neg_in (Not x), push_neg_in (Not y))
    | Not  (Or (x, y))    -> And (push_neg_in (Not x), push_neg_in (Not y))
    | Not  x              -> Not (push_neg_in x)
    | And  (x, y)         -> And (push_neg_in x, push_neg_in y)
    | Or   (x, y)         -> Or (push_neg_in x, push_neg_in y)
    | Xor     _           -> failwith "there shouldn't be any xors left"
    | Implies _ | Equiv _ -> failwith "there shouldn't be any implies/equiv left"
    | Bigand _ | Bigor _  -> failwith "all bigor and bigand should have been eliminated"
  in
  let rec push_disj_in = function
    | Top | Bottom | Term _ as x    -> x
    | Not  x                 -> Not (push_disj_in x)
    (*| Or   (x, And (y, z)) -> And (push_disj_in (Or (x, y)), push_disj_in (Or (x, y)))
    | Or   (And (x, y), z) -> And (push_disj_in (Or (x, z)), push_disj_in (Or (y, z)))
    | Or   (x, y)          -> Or (push_disj_in x, push_disj_in y) *)
    | Or (x, y)              -> dist (push_disj_in x) (push_disj_in y)
    | And  (x, y)            -> And (push_disj_in x, push_disj_in y)
    | Xor     _           -> failwith "there shouldn't be any xors left"
    | Implies _ | Equiv _ -> failwith "there shouldn't be any implies left"
    | Bigand _ | Bigor _  -> failwith "all bigor and bigand should have been eliminated"
  and dist x y =
    match x, y with
    | And (x, y), z -> And (push_disj_in (Or (x, z)), push_disj_in (Or (x, z)))
    | x, And (y, z) -> And (push_neg_in (Or (x, y)), push_disj_in (Or (x, z)))
    | x, y -> Or (push_disj_in x, push_disj_in y)
  in
  let rec simplify = function
    | And (x, Top) -> x
    | And (Top, x) -> x
    | And (x, Bottom) -> Bottom
    | And (Bottom, x) -> Bottom
    | And (Not (Term (x, None)), Term (y, None)) as p -> if x = y then Bottom else p
    | And (Term (x, None), Not (Term (y, None))) as p -> if x = y then Bottom else p
    | And (x, y) -> if equal x y then simplify x else And (simplify x, simplify y)
    | Or (x, Top) -> Top
    | Or (Top, x) -> Top
    | Or (x, Bottom) -> x
    | Or (Bottom, x) -> x
    | Or (Not (Term (x, None)), Term (y, None)) as p -> if x = y then Top else p
    | Or (Term (x, None), Not (Term (y, None))) as p -> if x = y then Top else p
    | Or (Or (Term (x, None), y), Not (Term (z, None))) as p -> if x = z then simplify y else p
    | Or (Or (x, Term (y, None)), Not (Term (z, None))) as p -> if y = z then simplify x else p
    | Or (Or (Not (Term (x, None)), y), Term (z, None)) as p -> if x = z then simplify y else p
    | Or (Or (x, Not (Term (y, None))), Term (z, None)) as p -> if y = z then simplify x else p
    | Or (Or (Term (x, None), Not (Term (y, None))), z) as p -> if x = y then simplify z else p
    | Or (Or (Not (Term (x, None)), Term (y, None)), z) as p -> if x = y then simplify z else p
    | Or (x, y) -> if equal x y then simplify x else Or (simplify x, simplify y)
    | x -> x
  and equal x y =
    match compare x y with
    | 0 -> true
    | _ -> false
  in
  remove_impl p |> remove_xor |> push_neg_in |> push_disj_in |> simplify

let to_dimacs prop =
  let table = Hashtbl.create 10
  and num_sym = ref 1 in
  let rec to_list = function
    | Term (x ,None) -> [gensym x]
    | Term (x, _) -> failwith "unevaluated term"
    | Not (Term (x, None)) -> [- (gensym x)]
    | And (x, y) -> (to_list x)@[0]@(to_list y)
    | Or (x, y) -> (to_list x)@(to_list y)
    | _ -> failwith "to_dimacs error"
  and gensym x =
    try Hashtbl.find table x
    with Not_found -> 
      let n = !num_sym in
      Hashtbl.add table x n; incr num_sym; n 
  and to_string =
    List.fold_left (fun acc x ->
      if x = 0 then acc ^ " 0\n" else acc ^ string_of_int x ^ " ") "" 
  in (to_list prop |> to_string) ^ "0", table

let test () =
  let p = Implies (And (Term ("a", None),
                        Term ("b", None)),
                   Implies (Or (And (Not (Term ("a", None)), Term ("c", None)),
                                     Term ("c", None)),
                                Not (Term ("a", None)))) in
  let str, table = to_cnf p |> to_dimacs in
  print_string str

let () = test ()
