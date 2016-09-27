module type S = sig
  include Set.S
  val of_list: elt list -> t
  val combinations: int -> t -> elt list list
  val exact: int -> t -> (elt list * elt list) list
  val atleast: int -> t -> elt list list
  val atmost: int -> t -> elt list list
  val map: (elt -> elt) -> t -> t
 (* val powerset: t -> *)
end

module Make (Ord : Set.OrderedType) = struct
  include Set.Make(Ord) 
  
  let of_list =
    List.fold_left (fun acc x -> add x acc) empty

  (* Inefficient implementation - Found on Rosetta Code ^_^ *)
  (* L'ensemble des sous-ensembles de cardinal k d'un ensemble.
  * Retourne la liste correspondante
  *)
  let combinations k set =
    let rec comb k lst =
      match k,lst with
      | 0,_     -> [[]]
      | _,[]    -> []
      | k,x::xs -> List.map (fun y -> x::y) (comb (pred k) xs) @ comb k xs
    in comb k (elements set)

  let exact k set =
    let rec go k l =
      match k,l with
      | 0,_     -> [([],[])]
      | _,[]    -> []
      | k,x::xs ->
          List.map (fun (comb,rest) ->
            (x::comb, elements (diff set (of_list (x::comb)))))
          (go (pred k) xs) @ go k xs
    in go k (elements set)

  let atleast = combinations

  (* TO FIX: this function should return the set of wrong values *)
  let atmost k set =
          let n = cardinal set in
          combinations (n-k) set 
end

module PowerSet(S: Set.S) = struct
  include Set_ext.Make(S)
  (* map : returns the set of results of the function f applied 
   * to each element of the set s *)
  let map f s =
          let work x r = add (f x) r in
          fold work s empty

  (* powerset : returns the set of subsets of s *)
  let powerset s =
          let base = singleton empty in
          let work x r = union r (map (add x) r) in 
          fold work s base
end
