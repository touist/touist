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
  | Begin of affect list option * exp list
and affect =
  | Affect of string * exp
and exp =
  | Var       of string
  | IntExp    of int_exp
  | FloatExp  of float_exp
  | SetExp    of set_exp
  | BoolExp   of bool_exp
  | ClauseExp of clause_exp
  | Dot       of set_exp * exp
  | If        of bool_exp * exp * exp
and int_exp =
  | Var of string
  | Int of int
  | Add of int_exp * int_exp
  | Sub of int_exp * int_exp
  | Mul of int_exp * int_exp
  | Div of int_exp * int_exp
  | Mod of int_exp * int_exp
  | To_int of float_exp
and float_exp =
  | Var of string
  | Float of float
  | Add of float_exp * float_exp
  | Sub of float_exp * float_exp
  | Mul of float_exp * float_exp
  | Div of float_exp * float_exp
  | Sqrt of float_exp
  | To_float of int_exp
and bool_exp =
  | Var              of string
  | Bool              of bool
  | Not               of bool_exp
  | And               of bool_exp  * bool_exp
  | Or                of bool_exp  * bool_exp
  | Xor               of bool_exp  * bool_exp
  | Implies           of bool_exp  * bool_exp
  | Equiv             of bool_exp  * bool_exp
  | Equal             of int_exp * int_exp
  | Not_equal         of int_exp * int_exp
  | Lesser_than       of int_exp * int_exp
  | Lesser_or_equal   of int_exp * int_exp
  | Greater_than      of int_exp * int_exp
  | Greater_or_equal  of int_exp * int_exp
  | FEqual            of float_exp * float_exp
  | FNot_equal        of float_exp * float_exp
  | FLesser_than      of float_exp * float_exp
  | FLesser_or_equal  of float_exp * float_exp
  | FGreater_than     of float_exp * float_exp
  | FGreater_or_equal of float_exp * float_exp
  | Empty             of set_exp
and clause_exp =
  | Top
  | Bottom
  | Term    of string * term_opt option
  | Not     of clause_exp
  | And     of clause_exp  * clause_exp
  | Or      of clause_exp  * clause_exp
  | Xor     of clause_exp  * clause_exp
  | Implies of clause_exp  * clause_exp
  | Equiv   of clause_exp  * clause_exp
  | Bigand  of string list * set_exp list * exp
  | Bigor   of string list * set_exp list * exp
and set_exp =
  | Var  of string
  | Set   of GenSet.t
  | Union of set_exp * set_exp
  | Inter of set_exp * set_exp
  | Diff  of set_exp * set_exp
  | Range of int_exp * int_exp
and term_opt =
  | Str of string
  | Num of int_exp
