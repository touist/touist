module type S = sig
  include Set.S
  val of_list: elt list -> t
  val combinations: int -> t -> elt list list
  val exact: int -> t -> (elt list * elt list) list
  val atleast: int -> t -> elt list list
  val atmost: int -> t -> elt list list
end

module Make (Ord : Set.OrderedType) = struct
  include Set.Make(Ord)

  let of_list =
    List.fold_left (fun acc x -> add x acc) empty

  (* Inefficient implementation - Found on Rosetta Code ^_^ *)
  let combinations k set =
    let rec comb k lst =
      match k,lst with
      | 0,_     -> [[]]
      | _,[]    -> []
      | k,x::xs -> List.map (fun y -> x::y) (comb (pred k) xs) @ comb k xs
    in comb k (elements set)

  (* [exact N list] returns a list of couples (combination,others) such that
     - 'combination' is the list of the terms such as there are exactly N elmts
     - 'others' is the rest of the elemts that have not been picked for that
       combination. 
     When translating that to a formula, for the couple ([a,b,c],[m]) that would
     become: a and b and c and not m. 
     Returns an empty list when exact(k,[]), k!=0. *)
  let exact k set =
    let rec go k l =
      match k,l with
      | 0,_     -> [([],l)] (* exact 0 -> all terms in the list must be 'not' *)
      | _,[]    -> []       (* exact on empty set -> no couple at all *)
      | k,x::xs ->
          List.map (fun (comb,rest) ->
            (x::comb, elements (diff set (of_list (x::comb)))))
          (go (pred k) xs) @ go k xs
    in go k (elements set)

  
  let atleast = combinations

  let atmost k set =
    let n = cardinal set in
    combinations (n-k) set
end
