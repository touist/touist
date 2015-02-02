type token =
  (* boolean *)
  | True
  | False
  | Not
  | And
  | Or
  | Equal
  | NotEqual
  | LT
  | GT
  | LE
  | GE
  | Empty
  | In
  | Subset
  (* numeric *)
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Cardinal
  | IntPart
  | Sqrt
  | Int of int
  | Float of float
  (* set *)
  | Union
  | Intersect
  | Diff
  | If
  | Then
  | Else
  | Upperset
  | Range        (* 1..9          *)
  | Dot          (* s.(10)        *)
  | Set          (* s(?a; ?b; ?c) *)
  (* clause *)
  | Top
  | Bottom
  | Clause
  | Negation
  | LogicalAnd
  | LogicalOr
  | Arrow
  | BigAnd
  | BigOr
  (* other *)
  | Begin
  | End
  | Formula
  | Sets
  | With
  | LeftPar
  | RightPar
  | Semicolon
  | Affect
  | Var of string
  | Const of string
  | Eof
