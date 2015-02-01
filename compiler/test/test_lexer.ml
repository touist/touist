open Tokens

let string_of_token = function
  | True       -> "True"
  | False      -> "False"
  | Not        -> "Not"
  | And        -> "And"
  | Or         -> "Or"
  | Equal      -> "Equal"
  | NotEqual   -> "NotEqual"
  | LT         -> "LT"
  | GT         -> "GT"
  | LE         -> "LE"
  | GE         -> "GE"
  | Empty      -> "Empty"
  | In         -> "In"
  | Subset     -> "Subset"
  | Plus       -> "Plus"
  | Minus      -> "Minus"
  | Times      -> "Times"
  | Divide     -> "Divide"
  | Modulo     -> "Modulo"
  | Cardinal   -> "Cardinal"
  | IntPart    -> "IntPart"
  | Sqrt       -> "Sqrt"
  | Int n      -> "Int " ^ string_of_int n
  | Float f    -> "Float " ^ string_of_float f
  | Union      -> "Union"
  | Intersect  -> "Intersect"
  | Diff       -> "Diff"
  | If         -> "If"
  | Then       -> "Then"
  | Else       -> "Else"
  | Upperset   -> "Upperset"
  | Range      -> "Range"
  | Dot        -> "Dot"
  | Top        -> "Top"
  | Bottom     -> "Bottom"
  | Negation   -> "Negation"
  | LogicalAnd -> "LogicalAnd"
  | LogicalOr  -> "LogicalOr"
  | Arrow      -> "Arrow"
  | BigAnd     -> "BigAnd"
  | BigOr      -> "BigOr"
  | Begin      -> "Begin"
  | End        -> "End"
  | Formula    -> "Formula"
  | Sets       -> "Sets"
  | With       -> "With"
  | LeftPar    -> "LeftPar"
  | RightPar   -> "RightPar"
  | Affect     -> "Affect"
  | Var v      -> "Var " ^ v
  | Const c    -> "Const " ^ c
  | Eof        -> "Eof"

let lexbuf = Lexing.from_channel (open_in Sys.argv.(1))

let rec print_code lexbuf =
  let t = Lexer.lexer lexbuf in
  print_endline (string_of_token t);
  if t <> Eof then print_code lexbuf

let () = print_code lexbuf
