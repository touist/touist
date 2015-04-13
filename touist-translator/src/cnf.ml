open Syntax

let to_cnf p =
  let rec remove_impl = function
    | Top | Bottom | Term _ as x -> x
    | CNot     x      -> CNot (remove_impl x)
    | CAnd     (x, y) -> CAnd (remove_impl x, remove_impl y)
    | COr      (x, y) -> COr (remove_impl x, remove_impl y)
    | CXor     (x, y) -> CXor (remove_impl x, remove_impl y)
    | CImplies (x, y) -> COr (CNot (remove_impl x), remove_impl y)
    | CEquiv   (x, y) -> CNot (CXor (remove_impl x, remove_impl y))
    | Bigand _ | Bigor _ | CIf _ | CVar _ ->
        failwith "all bigor, bigand, if and var should have been eliminated"
  in
  let rec remove_xor = function
    | Top | Bottom | Term _ as x -> x
    | CNot  x              -> CNot (remove_xor x)
    | CAnd  (x, y)         -> CAnd (remove_xor x, remove_xor y)
    | COr   (x, y)         -> COr (remove_xor x, remove_xor y)
    | CXor  (x, y)         -> CAnd (COr (remove_xor x, remove_xor y), CNot (CAnd (remove_xor x, remove_xor y)))
    | CImplies _ | CEquiv _ -> failwith "there shouldn't be any implies/equiv left"
    | Bigand _ | Bigor _ | CIf _ | CVar _  ->
        failwith "all bigor, bigand, if and var should have been eliminated"
  in
  let rec push_neg_in = function
    | Top | Bottom | Term _ as x -> x
    | CNot  Top            -> Bottom
    | CNot  Bottom         -> Top
    | CNot  (CNot x)       -> push_neg_in x
    | CNot  (CAnd (x, y))  -> COr (push_neg_in (CNot x), push_neg_in (CNot y))
    | CNot  (COr (x, y))   -> CAnd (push_neg_in (CNot x), push_neg_in (CNot y))
    | CNot  x              -> CNot (push_neg_in x)
    | CAnd  (x, y)         -> CAnd (push_neg_in x, push_neg_in y)
    | COr   (x, y)         -> COr (push_neg_in x, push_neg_in y)
    | CXor     _           -> failwith "there shouldn't be any xors left"
    | CImplies _ | CEquiv _ -> failwith "there shouldn't be any implies/equiv left"
    | Bigand _ | Bigor _ | CIf _ | CVar _  ->
        failwith "all bigor, bigand, if and var should have been eliminated"
  in
  let rec push_disj_in = function
    | Top | Bottom | Term _ as x -> x
    | CNot  x               -> CNot (push_disj_in x)
    | COr (x, y)            -> dist (push_disj_in x) (push_disj_in y)
    | CAnd (x, y)           -> CAnd (push_disj_in x, push_disj_in y)
    | CXor     _            -> failwith "there shouldn't be any xors left"
    | CImplies _ | CEquiv _ -> failwith "there shouldn't be any implies left"
    | Bigand _ | Bigor _ | CIf _ | CVar _ ->
        failwith "all bigor, bigand, if and var should have been eliminated"
  and dist x y =
    match x, y with
    | CAnd (x, y), z -> CAnd (push_disj_in (COr (x, z)), push_disj_in (COr (y, z)))
    | x, CAnd (y, z) -> CAnd (push_disj_in (COr (x, y)), push_disj_in (COr (x, z)))
    | x, y          -> COr (push_disj_in x, push_disj_in y)
  in
  remove_impl p |> remove_xor |> push_neg_in |> push_disj_in
