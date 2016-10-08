module type S = sig
  include Set.S

  (** Avoid the dependency to 4.02.0 *)
  val of_list : elt list -> t

  (** Return the different ways to choose k elements among a set of n
   * elements
   *)
  val combinations : int -> t -> elt list list

  (** Return a list of tuples. The first member is a combination of k
   * elements in the set and the second member is the list of every other
   * set elements not in the combination
   *)
  val exact: int -> t -> (elt list * elt list) list

  (** Actually an alias for the combinations function:
    * combinations k set
    *)
  val atleast: int -> t -> elt list list

  (** Equivalent to:
    * combinations (n-k) set, where n = card(set)
    *)
  val atmost: int -> t -> elt list list
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
