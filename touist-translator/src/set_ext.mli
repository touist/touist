(* Specification made by : ocamlc -i set_ext.ml > set_ext.mli *)
module type S =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt

     (** Avoid the dependency to 4.02.0 *)
    val of_list : elt list -> t

    (** Return the different ways to choose k elements among a set of
     *  n elements 
     *)
    val combinations : int -> t -> elt list list

    (** Return a list of tuples. The first member is a combination of k
     * elements in the set and the second member is the list of every other
     * set elements not in the combination
     *)
    val exact : int -> t -> (elt list * elt list) list

    (** Actually an alias for the combinations function:
     * combinations k set
     *)
    val atleast : int -> t -> elt list list

    (** Equivalent to:
     * combinations (n-k) set, where n = card(set)
     *)
    val atmost : int -> t -> elt list list
  end
module Make :
  functor (Ord : Set.OrderedType) ->
    sig
      type elt = Ord.t
      type t = Set.Make(Ord).t 
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val find : elt -> t -> elt
      val of_list : elt list -> t
      val combinations : int -> t -> elt list list
      val exact : int -> t -> (elt list * elt list) list
      val atleast : int -> t -> elt list list
      val atmost : int -> t -> elt list list
    end
module PowerSet :
  functor (S : Set.S) ->
    sig
      type elt = S.t
      type t = Set.Make(S).t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val max_elt : t -> elt
      val choose : t -> elt
      val split : elt -> t -> t * bool * t
      val find : elt -> t -> elt
      val of_list : elt list -> t
      val map : (elt -> elt) -> t -> t
      val powerset : S.t -> t
    end
