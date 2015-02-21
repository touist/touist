type prog =
  | Begin of (command list option * command list)

and command =
  | Affect of (string * aexp)
  | If     of (bexp * command * command)
  | Bigand of bigbody
  | Bigor  of bigbody
  | Clause of bexp

and bexp =
  | True
  | False
  | Trueclause
  | Falseclause
  | Term             of string
  | And              of (bexp * bexp)
  | Or               of (bexp * bexp)
  | Xor              of (bexp * bexp)
  | Imply            of (bexp * bexp)
  | Not              of bexp
  | Equal            of (aexp * aexp)
  | Not_equal        of (aexp * aexp)
  | Lesser_than      of (aexp * aexp)
  | Lesser_or_equal  of (aexp * aexp)
  | Greater_than     of (aexp * aexp)
  | Greater_or_equal of (aexp * aexp)
  | Exact            of (aexp * bigbody)
  | Atmost           of (aexp * bigbody)
  | Atleast          of (aexp * bigbody)
  | Empty            of sexp
  | Subset           of (sexp * sexp)
  | In               of (set_body * sexp) 

and aexp =
  | Var      of string
  | Int      of int
  | Float    of float
  | Set      of sexp
  | Add      of (aexp * aexp)
  | Sub      of (aexp * aexp)
  | Mul      of (aexp * aexp)
  | Div      of (aexp * aexp)
  | Mod      of (aexp * aexp)
  | Sqrt     of aexp
  | To_float of aexp
  | To_int   of aexp

and sexp = 
  | Set_body of set_body list
  | Union    of (sexp * sexp)
  | Inter    of (sexp * sexp)
  | Diff     of (sexp * sexp)
  | Upperset of (aexp * sexp)
  | Range    of (aexp * aexp)
  | Dot      of (sexp * aexp)
  | Card     of sexp

and set_body =
  | Num  of aexp
  | Prop of bexp

and bigbody = (string * sexp * bexp option * command)
