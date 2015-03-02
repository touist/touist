module IntSet = Set.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

module FloatSet = Set.Make(struct
  type t = float
  let compare = Pervasives.compare
end)

module StringSet = Set.Make(String)

module GenSet = struct
  type t =
    | IS of IntSet.t
    | FS of FloatSet.t
    | SS of StringSet.t
end

type prog =
  | Begin of affect list option * clause_exp list
and affect =
  | Affect of string * exp
and exp =
  | Scalar    of arith_exp
  | SetExp    of set_exp
  | BoolExp   of bool_exp
  | ClauseExp of clause_exp
  | ListExp   of list_exp
  | Dot       of set_exp * arith_exp
and arith_exp =
  | Int      of int
  | Float    of float
  | Add      of arith_exp * arith_exp
  | Sub      of arith_exp * arith_exp
  | Mul      of arith_exp * arith_exp
  | Div      of arith_exp * arith_exp
  | Mod      of arith_exp * arith_exp
  | Sqrt     of arith_exp
  | To_int   of arith_exp
  | To_float of arith_exp
  | Card     of set_exp
and bool_exp =
  | Bool             of bool
  | Not              of bool_exp
  | And              of bool_exp  * bool_exp
  | Or               of bool_exp  * bool_exp
  | Xor              of bool_exp  * bool_exp
  | Implies          of bool_exp  * bool_exp
  | Equiv            of bool_exp  * bool_exp
  | Equal            of arith_exp * arith_exp
  | Not_equal        of arith_exp * arith_exp
  | Lesser_than      of arith_exp * arith_exp
  | Lesser_or_equal  of arith_exp * arith_exp
  | Greater_than     of arith_exp * arith_exp
  | Greater_or_equal of arith_exp * arith_exp
  | Empty            of set_exp
and clause_exp =
  | Var     of string * arith_exp option
  | Not     of clause_exp
  | And     of clause_exp  * clause_exp
  | Or      of clause_exp  * clause_exp
  | Xor     of clause_exp  * clause_exp
  | Implies of clause_exp  * clause_exp
  | Equiv   of clause_exp  * clause_exp
  | Bigand  of string list * set_exp list * exp
  | Bigor   of string list * set_exp list * exp
and set_exp =
  | Set   of GenSet.t
  | Union of set_exp * set_exp
  | Inter of set_exp * set_exp
  | Diff  of set_exp * set_exp
and list_exp =
  | List  of int list 
  | Range of arith_exp * arith_exp
