type prog = [
  | `Begin of set list option * clause_expr list
]

and set = [
  | `Affect of atom * expr
]

and expr = [
  | `Set_expr   of set_expr
  | `Int_expr   of int_expr
  | `Float_expr of float_expr
]

and bool_expr = [
  | `Bool             of bool
  | `In               of ident * set_expr
  | `Subset           of set_expr * set_expr
  | `Equal            of atom * atom
  | `Not_equal        of atom * atom
  | `Lesser_than      of atom * atom
  | `Leser_or_equal   of atom * atom
  | `Greater_than     of atom * atom
  | `Greater_or_equal of atom * atom
  | `Empty            of set_expr
  | `And              of bool_expr * bool_expr
  | `Or               of bool_expr * bool_expr
  | `Xor              of bool_expr * bool_expr
  | `Imply            of bool_expr * bool_expr
  | `Not              of bool_expr
]

and int_expr = [
  | `Integer   of integer
  | `Add       of int_expr * int_expr
  | `Multiply  of int_expr * int_expr
  | `Substract of int_expr * int_expr
  | `Divide    of int_expr * int_expr
  | `Modulo    of int_expr * int_expr
  | `Int       of float_expr
  | `Card      of set_expr
]

and float_expr = [
  | `Rational  of rational
  | `Add       of float_expr * float_expr
  | `Multiply  of float_expr * float_expr
  | `Substract of float_expr * float_expr
  | `Divide    of float_expr * float_expr
  | `Float     of int_expr
  | `Sqrt      of float_expr
]

and set_expr = [
  | `Set          of term list
  | `Union        of set_expr * set_expr
  | `Intersection of set_expr * set_expr
  | `Difference   of set_expr * set_expr
  | `Range        of int_expr * int_expr
  | `Dot          of set_expr * int_expr
  | `Upperset     of ident * set_expr
  | `If           of bool_expr * set_expr * set_expr
]

and clause_expr = [
  | `Trueclause
  | `Falseclause
  | `Not     of clause_expr
  | `And     of clause_expr * clause_expr
  | `Or      of clause_expr * clause_expr
  | `Xor     of clause_expr * clause_expr
  | `Imply   of clause_expr * clause_expr
  | `Bigand  of bigbody
  | `Bigor   of bigbody
  | `Exact   of integer * bigbody
  | `Atleast of integer * bigbody
  | `Atmost  of integer * bigbody
  | `If      of bool_expr * clause_expr * clause_expr
]

and term = [
  | `Ident    of ident
  | `Integer  of integer
  | `Rational of rational
]

and bigbody = ident * set_expr * bool_expr option * clause_expr

and atom = ident * term list option

and ident    = string
and integer  = int
and rational = float


