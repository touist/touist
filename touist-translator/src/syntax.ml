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

module GenSet = struct
  type t =
    | Empty
    | ISet of IntSet.t
    | FSet of FloatSet.t
    | SSet of StringSet.t
end

type prog =
  | Prog of exp list * exp list option
and var = string * exp list option
and exp =
  | Int              of int
  | Float            of float
  | Bool             of bool
  | Var              of var (* Var and Term are the ONLY to be able to *)
  | Term             of var (* have the var type to avoid spagetti in exp *)
  | Set              of GenSet.t
  | Set_decl         of exp list
  | Neg              of exp
  | Add              of exp * exp
  | Sub              of exp * exp
  | Mul              of exp * exp
  | Div              of exp * exp
  | Mod              of exp * exp
  | Sqrt             of exp
  | To_int           of exp
  | To_float         of exp
  | Top
  | Bottom
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
  | Inter            of exp * exp
  | Diff             of exp * exp
  | Range            of exp * exp
  | Empty            of exp
  | Card             of exp
  | Subset           of exp * exp
  | In               of exp * exp
  | If               of exp * exp * exp
  | Exact    of exp * exp
  | Atleast  of exp * exp
  | Atmost   of exp * exp
  | Bigand   of exp list * exp list * exp option * exp
  | Bigor    of exp list * exp list * exp option * exp
  | Let      of exp * exp * exp
  | Affect   of exp * exp
