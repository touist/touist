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
  | Prog of ast list * ast list option
and var = string * ast list option
and ast =
  | Int              of int
  | Float            of float
  | Bool             of bool
  | Var              of var (* Var and Term are the ONLY to be able to *)
  | Term             of var (* have the var type to avoid spagetti in ast *)
  | Set              of GenSet.t
  | Set_decl         of ast list
  | Neg              of ast
  | Add              of ast * ast
  | Sub              of ast * ast
  | Mul              of ast * ast
  | Div              of ast * ast
  | Mod              of ast * ast
  | Sqrt             of ast
  | To_int           of ast
  | To_float         of ast
  | Top
  | Bottom
  | Not              of ast
  | And              of ast * ast
  | Or               of ast * ast
  | Xor              of ast * ast
  | Implies          of ast * ast
  | Equiv            of ast * ast
  | Equal            of ast * ast
  | Not_equal        of ast * ast
  | Lesser_than      of ast * ast
  | Lesser_or_equal  of ast * ast
  | Greater_than     of ast * ast
  | Greater_or_equal of ast * ast
  | Union            of ast * ast
  | Inter            of ast * ast
  | Diff             of ast * ast
  | Range            of ast * ast
  | Empty            of ast
  | Card             of ast
  | Subset           of ast * ast
  | In               of ast * ast
  | If               of ast * ast * ast
  | Exact    of ast * ast
  | Atleast  of ast * ast
  | Atmost   of ast * ast
  | Bigand   of ast list * ast list * ast option * ast
  | Bigor    of ast list * ast list * ast option * ast
  | Let      of ast * ast * ast
  | Affect   of ast * ast
