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

  (** [exact] returns a list of  *)
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

  let atmost k set =
    let n = cardinal set in
    combinations (n-k) set
end
