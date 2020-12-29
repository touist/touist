open Types
open Types.Ast

let arith_unop u x =
  match u with
  | Neg -> "(- " ^ x ^ ")"
  | Sqrt -> "sqrt(" ^ x ^ ")"
  | To_int -> "int(" ^ x ^ ")"
  | Abs -> "abs(" ^ x ^ ")"
  | To_float -> "float(" ^ x ^ ")"

let arith_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"

let logic_binop_base = function
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Implies -> "=>"
  | Equiv -> "<=>"

let logic_binop_utf8 = function
  | And -> "⋀"
  | Or -> "⋁"
  | Xor -> "xor"
  | Implies -> "⇒"
  | Equiv -> "⇔"

let logic_binop ?(utf8 = false) =
  if utf8 then logic_binop_utf8 else logic_binop_base

let set_binop_base = function
  | Union -> "union"
  | Inter -> "inter"
  | Diff -> "diff"

let set_binop_utf8 = function Union -> "∪" | Inter -> "∩" | Diff -> "\\"

let arith_binrel_base = function
  | Equal -> "=="
  | Not_equal -> "!="
  | Lesser_than -> "<"
  | Lesser_or_equal -> "<="
  | Greater_than -> ">"
  | Greater_or_equal -> ">="

let arith_binrel_utf8 = function
  | Equal -> "=="
  | Not_equal -> "≠"
  | Lesser_than -> "<"
  | Lesser_or_equal -> "≤"
  | Greater_than -> ">"
  | Greater_or_equal -> "≥"

let arith_binrel ?(utf8 = false) =
  if utf8 then arith_binrel_utf8 else arith_binrel_base

let layout ?(debug = false) x = function
  | Loc l -> (if debug then "loc " ^ Err.string_of_loc l ^ ":" else "") ^ x
  | Paren | NewlineBefore | NewlineAfter -> x

let layout_type ?(debug = false) x = function
  | Loc _ -> if debug then "location" else x
  | Paren -> x
  | NewlineBefore | NewlineAfter -> "newline"

let cardinality = function
  | Exact -> "exact"
  | Atmost -> "atmost"
  | Atleast -> "atleast"

let rec string_of_ast ?(utf8 = false) ?(show_var = fun _ -> "") ?(debug = false)
    ?(parenthesis = debug) ast =
  let of_ast = string_of_ast ~utf8 ~show_var ~parenthesis ~debug in
  let of_ast_list = string_of_ast_list ~utf8 ~show_var ~parenthesis ~debug in
  match ast with
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x
  | Top when utf8 -> "⟙"
  | Top -> "Top"
  | Bottom when utf8 -> "⟘"
  | Bottom -> "Bot"
  | Prop x | UnexpProp (x, None) -> x
  | UnexpProp (x, Some y) -> x ^ "(" ^ of_ast_list "," y ^ ")"
  | Var (x, None) -> x ^ show_var ast
  | Var (x, Some y) -> x ^ "(" ^ of_ast_list "," y ^ ")" ^ show_var ast
  | Set x -> "[" ^ of_ast_list "," (AstSet.elements x) ^ "]"
  | Set_decl x -> "[" ^ of_ast_list "," x ^ "]"
  | ArithUnop (u, x) -> arith_unop u (of_ast x)
  | ArithBinop (x, b, y) ->
      "(" ^ of_ast x ^ " " ^ arith_binop b ^ " " ^ of_ast y ^ ")"
  | Not x when utf8 -> "¬" ^ of_ast x
  | Not x -> "not " ^ of_ast x
  | LogicBinop (x, b, y) ->
      "(" ^ of_ast x ^ " " ^ logic_binop ~utf8 b ^ " " ^ of_ast y ^ ")"
  | ArithBinrel (x, b, y) when utf8 ->
      of_ast x ^ " " ^ arith_binrel_utf8 b ^ " " ^ of_ast y
  | ArithBinrel (x, b, y) ->
      of_ast x ^ " " ^ arith_binrel_base b ^ " " ^ of_ast y
  | SetBinop (x, b, y) when utf8 ->
      of_ast x ^ " " ^ set_binop_utf8 b ^ " " ^ of_ast y
  | SetBinop (x, b, y) ->
      set_binop_base b ^ "(" ^ of_ast x ^ ", " ^ of_ast y ^ ")"
      (* AS: previously no closing parenthesis. *)
  | Range (x, y) -> "[" ^ of_ast x ^ ".." ^ of_ast y ^ "]"
  | Subset (x, y) when utf8 -> of_ast x ^ " ⊆ " ^ of_ast y
  | Subset (x, y) -> "subset(" ^ of_ast x ^ "," ^ of_ast y
  | Powerset x -> "powerset(" ^ of_ast x ^ ")"
  | In (x, y) when utf8 -> of_ast x ^ " ∈ " ^ of_ast y
  | In (x, y) -> of_ast x ^ " in " ^ of_ast y
  | Empty x when utf8 -> of_ast x ^ "=∅"
  | Empty x -> "empty(" ^ of_ast x ^ ")"
  | Card x when utf8 -> "|" ^ of_ast x ^ "|"
  | Card x -> "card(" ^ of_ast x ^ ")"
  | If (x, y, z) ->
      "if " ^ of_ast x ^ " then\n" ^ of_ast y ^ "\nelse\n" ^ of_ast z
      ^ "\nend\n"
  | Bigand (x, y, None, z) ->
      "bigand " ^ of_ast_list "," x ^ " in " ^ of_ast_list "," y ^ ":\n"
      ^ of_ast z ^ "\nend\n"
  | Bigand (x, y, Some b, z) ->
      "bigand " ^ of_ast_list "," x ^ " in " ^ of_ast_list "," y ^ " when "
      ^ of_ast b ^ ":\n" ^ of_ast z ^ "\nend\n"
  | Bigor (x, y, None, z) ->
      "bigor " ^ of_ast_list "," x ^ " in " ^ of_ast_list "," y ^ ":\n"
      ^ of_ast z ^ "\nend\n"
  | Bigor (x, y, Some b, z) ->
      "bigor " ^ of_ast_list "," x ^ " in " ^ of_ast_list "," y ^ " when "
      ^ of_ast b ^ ":\n" ^ of_ast z ^ "\nend\n"
  | Cardinality (c, x, y) ->
      cardinality c ^ "(" ^ of_ast x ^ "," ^ of_ast y ^ ")"
  | Let (v, x, c) -> of_ast v ^ "=" ^ of_ast x ^ ": " ^ of_ast c
  | Affect (v, c) -> of_ast v ^ "=" ^ of_ast c
  | Touist_code f -> of_ast_list "\n" f
  | Layout (l, x) -> layout ~debug (of_ast x) l
  | Exists (v, f) when utf8 -> "∃" ^ of_ast v ^ "." ^ of_ast f
  | Exists (v, f) -> "exists " ^ of_ast v ^ ": " ^ of_ast f
  | Forall (v, f) when utf8 -> "∀" ^ of_ast v ^ "." ^ of_ast f
  | Forall (v, f) -> "forall " ^ of_ast v ^ ": " ^ of_ast f
  | For (v, c, f) -> "for " ^ of_ast v ^ " in " ^ of_ast c ^ ":" ^ of_ast f
  | Formula f -> "\"" ^ of_ast f ^ "\""
  | SetBuilder (f, vars, sets, cond) ->
      "[" ^ of_ast f ^ " for " ^ of_ast_list "," vars ^ " in "
      ^ of_ast_list "," sets
      ^ (match cond with Some c -> " when " ^ of_ast c | None -> "")
      ^ "]"

and string_of_ast_type ?(debug = false) (ast : Ast.t) : string =
  let of_ast_type ast = string_of_ast_type ~debug ast in
  match ast with
  | Int _ -> "int"
  | Float _ -> "float"
  | Bool _ -> "bool"
  | Top -> "Top"
  | Bottom -> "Bot"
  | UnexpProp (_, _) -> "unexpanded proposition"
  | Prop _ -> "proposition"
  | Var (_, None) -> "variable"
  | Var (_, Some _) -> "tuple-variable"
  | Set _ -> "set"
  | Set_decl _ -> "[ ] (set definition)"
  | ArithBinop (_, b, _) -> arith_binop b
  | ArithUnop (Neg, _) -> "-"
  | ArithUnop (Sqrt, _) -> "sqrt()"
  | ArithUnop (To_int, _) -> "int()"
  | ArithUnop (To_float, _) -> "float()"
  | ArithUnop (Abs, _) -> "abs()"
  | Not _ -> "not"
  | LogicBinop (_, b, _) -> logic_binop b
  | ArithBinrel (_, b, _) -> arith_binrel_base b
  | SetBinop (_, b, _) -> set_binop_base b
  | Range (_, _) -> ".."
  | Subset (_, _) -> "subset()"
  | Powerset _ -> "powerset()"
  | In (_, _) -> "in"
  | Empty _ -> "empty()"
  | Card _ -> "card()"
  | If (_, _, _) -> "if"
  | Bigand (_, _, None, _) -> "bigand"
  | Bigand (_, _, Some _, _) -> "bigand"
  | Bigor (_, _, None, _) -> "bigor"
  | Bigor (_, _, Some _, _) -> "bigor"
  | Cardinality (c, _, _) -> cardinality c
  | Let (_, _, _) -> "let"
  | Affect (_, _) -> "="
  | Touist_code _ -> "(touist code)"
  | Layout (l, x) -> layout_type ~debug (of_ast_type x) l
  | Exists (_, _) -> "exists"
  | Forall (_, _) -> "forall"
  | For (_, _, _) -> "for"
  | Formula _ -> "quoted formula"
  | SetBuilder (_, _, _, _) -> "set builder"

and string_of_ast_list ?(utf8 = false) ?(show_var = fun _ -> "")
    ?(debug = false) ?(parenthesis = debug) sep el =
  String.concat sep
    (List.map (string_of_ast ~utf8 ~show_var ~parenthesis ~debug) el)
