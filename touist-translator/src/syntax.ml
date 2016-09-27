(*
 * syntax.ml: defition of the types constituting the abstract syntaxic tree (ast)
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available 
 * under the terms of the GNU Lesser General Public License (LGPL) 
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *)

module IntSet = Set_ext.Make(struct
  type t = int
  let compare = Pervasives.compare
end)


module FloatSet = Set_ext.Make(struct
  type t = float
  let compare = Pervasives.compare
end)

module StringSet = Set_ext.Make(String)

(* TO USE? recursive module used to create recursive
 * superset but it didn't succeed*)

(*module rec Value : sig
  type t =
      | ISet of IntSet.t
      | FSet of FloatSet.t
      | SSet of StringSet.t
      | SupSet of SuperSet.t 
  val compare : t -> t -> int
  end
  = struct
    type t =
     | ISet of IntSet.t
     | FSet of FloatSet.t
     | SSet of StringSet.t
     | SupSet of SuperSet.t
     
     let compare s1 s2 =
     	match (s1, s2) with
	| ISet x, ISet y -> IntSet.compare x y
	| FSet x, FSet y -> FloatSet.compare x y
	| SSet x, SSet y -> StringSet.compare x y
	| SupSet x, SupSet y -> SuperSet.compare x y
	| _ -> failwith "incompatible types"
and SuperSet : Set.S with type elt = Value.t = Set_ext.PowerSet(Value)
*)

(* Supersets that aren't recursive *)
module SuperSet = Set_ext.PowerSet(IntSet)
module SuperFloatSet = Set_ext.PowerSet(FloatSet)
module SuperStringSet = Set_ext.PowerSet(StringSet)

module GenSet = struct
  type t =
    | Empty
    | ISet of IntSet.t
    | FSet of FloatSet.t
    | SSet of StringSet.t
    | SupSet of SuperSet.t
    | SupFSet of SuperFloatSet.t
    | SupSSet of SuperStringSet.t

end

type prog =
  | Prog of affect list option * clause list
and affect =
  | Affect of var * exp
and var = string * exp list option
and exp =
  | Int              of int
  | Float            of float
  | Bool             of bool
  | Var              of var
  | Set              of GenSet.t
  | Set_decl         of exp list
  | Clause           of clause
  | Neg              of exp
  | Add              of exp * exp
  | Sub              of exp * exp
  | Mul              of exp * exp
  | Div              of exp * exp
  | Mod              of exp * exp
  | Sqrt             of exp
  | To_int           of exp
  | To_float         of exp
  | Not              of exp
  | And              of exp * exp
  | Or               of exp * exp
  | Xor              of exp * exp
  | Implies          of exp * exp
  | Equiv            of exp * exp
  | Equal            of exp * exp
  | Not_equal        of exp * exp
  | Lesser_than      of exp * exp
  | Lesser_or_equal  of exp * exp
  | Greater_than     of exp * exp
  | Greater_or_equal of exp * exp
  | Union            of exp * exp
  | Powerset	     of exp
  | Inter            of exp * exp
  | Diff             of exp * exp
  | Range            of exp * exp
  | Empty            of exp
  | Card             of exp
  | Subset           of exp * exp
  | In               of exp * exp
  | If               of exp * exp * exp
and clause =
  | CInt              of int
  | CFloat            of float
  | CNeg              of clause
  | CAdd              of clause * clause 
  | CSub              of clause * clause 
  | CMul              of clause * clause 
  | CDiv              of clause * clause 
  | CEqual            of clause * clause 
  | CNot_equal        of clause * clause 
  | CLesser_than      of clause * clause 
  | CLesser_or_equal  of clause * clause 
  | CGreater_than     of clause * clause 
  | CGreater_or_equal of clause * clause 
  | Top
  | Bottom
  | Term     of var
  | CVar     of var
  | CNot     of clause
  | CAnd     of clause * clause
  | COr      of clause * clause
  | CXor     of clause * clause
  | CImplies of clause * clause
  | CEquiv   of clause * clause
  | Exact    of exp * exp
  | Atleast  of exp * exp
  | Atmost   of exp * exp
  | Bigand   of string list * exp list * exp option * clause
  | Bigor    of string list * exp list * exp option * clause
  | CIf      of exp * clause * clause
