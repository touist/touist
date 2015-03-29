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
    | Empty
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
  | IVar   of string
  | Int    of int
  | Neg    of int_exp
  | Add    of int_exp * int_exp
  | Sub    of int_exp * int_exp
  | Mul    of int_exp * int_exp
  | Div    of int_exp * int_exp
  | Mod    of int_exp * int_exp
  | To_int of float_exp
  | Card   of set_exp
and float_exp =
  | FVar     of string
  | Float    of float
  | FNeg     of float_exp
  | FAdd     of float_exp * float_exp
  | FSub     of float_exp * float_exp
  | FMul     of float_exp * float_exp
  | FDiv     of float_exp * float_exp
  | Sqrt     of float_exp
  | To_float of int_exp
and bool_exp =
  | BVar              of string
  | Bool              of bool
  | BNot              of bool_exp
  | BAnd              of bool_exp  * bool_exp
  | BOr               of bool_exp  * bool_exp
  | BXor              of bool_exp  * bool_exp
  | BImplies          of bool_exp  * bool_exp
  | BEquiv            of bool_exp  * bool_exp
  | Equal             of int_exp   * int_exp
  | Not_equal         of int_exp   * int_exp
  | Lesser_than       of int_exp   * int_exp
  | Lesser_or_equal   of int_exp   * int_exp
  | Greater_than      of int_exp   * int_exp
  | Greater_or_equal  of int_exp   * int_exp
  | FEqual            of float_exp * float_exp
  | FNot_equal        of float_exp * float_exp
  | FLesser_than      of float_exp * float_exp
  | FLesser_or_equal  of float_exp * float_exp
  | FGreater_than     of float_exp * float_exp
  | FGreater_or_equal of float_exp * float_exp
  | In                of exp       * set_exp
  | Subset            of set_exp   * set_exp
  | SEqual            of set_exp   * set_exp
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
  | Bigand  of string list * set_exp list * bool_exp option * exp
  | Bigor   of string list * set_exp list * bool_exp option * exp
and set_exp =
  | SVar  of string
  | Set   of GenSet.t
  | Union of set_exp * set_exp
  | Inter of set_exp * set_exp
  | Diff  of set_exp * set_exp
  | Range of int_exp * int_exp
and term_opt =
  | Str of string
  | Exp of exp
