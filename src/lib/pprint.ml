open Types
open Types.Ast

let rec string_of_ast ?(utf8=false) ?(show_var=(fun _ -> "")) ?(debug=false) ?(parenthesis=debug) ast =
  let of_ast = string_of_ast ~utf8 ~show_var ~parenthesis ~debug in
  let of_ast_list = string_of_ast_list ~utf8 ~show_var ~parenthesis ~debug in
  match ast with
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top when utf8 -> "⟙"
  | Top    -> "Top"
  | Bottom when utf8 -> "⟘"
  | Bottom -> "Bot"
  | Prop x | UnexpProp (x, None)-> x
  | UnexpProp (x,Some y) -> x ^ "(" ^ of_ast_list "," y ^ ")"
  | Var (x,None)   -> x ^ (show_var ast)
  | Var (x,Some y) -> x ^ "(" ^ (of_ast_list "," y) ^ ")" ^ show_var ast
  | Set    x -> "[" ^ of_ast_list "," (AstSet.elements x) ^ "]"
  | Set_decl x -> "[" ^ of_ast_list "," x ^ "]"
  | Neg x     -> "(- " ^ of_ast x ^ ")"
  | Add (x,y) -> "(" ^ of_ast x ^ " + "   ^ of_ast y ^ ")"
  | Sub (x,y) -> "(" ^ of_ast x ^ " - "   ^ of_ast y ^ ")"
  | Mul (x,y) -> "(" ^ of_ast x ^ " * "   ^ of_ast y ^ ")"
  | Div (x,y) -> "(" ^ of_ast x ^ " / "   ^ of_ast y ^ ")"
  | Mod (x,y) -> "(" ^ of_ast x ^ " mod " ^ of_ast y ^ ")"
  | Sqrt     x -> "sqrt("  ^ of_ast x ^ ")"
  | To_int   x -> "int("   ^ of_ast x ^ ")"
  | Abs   x -> "abs("   ^ of_ast x ^ ")"
  | To_float x -> "float(" ^ of_ast x ^ ")"
  | Not     x when utf8    -> "¬" ^ of_ast x
  | Not     x     -> "not " ^ of_ast x
  | And     (x,y) when utf8 -> "(" ^ of_ast x ^ " ⋀ " ^ of_ast y ^ ")"
  | And     (x,y) -> "(" ^ of_ast x ^ " and " ^ of_ast y ^ ")"
  | Or      (x,y) when utf8 -> "(" ^ of_ast x ^ " ⋁ "  ^ of_ast y ^ ")"
  | Or      (x,y) -> "(" ^ of_ast x ^ " or "  ^ of_ast y ^ ")"
  | Xor     (x,y) -> "(" ^ of_ast x ^ " xor " ^ of_ast y ^ ")"
  | Implies (x,y) when utf8 -> "(" ^ of_ast x ^ " ⇒ "  ^ of_ast y ^ ")"
  | Implies (x,y) -> "(" ^ of_ast x ^ " => "  ^ of_ast y ^ ")"
  | Equiv   (x,y) when utf8 -> "(" ^ of_ast x ^ " ⇔ " ^ of_ast y ^ ")"
  | Equiv   (x,y) -> "(" ^ of_ast x ^ " <=> " ^ of_ast y ^ ")"
  | Equal            (x,y) -> of_ast x ^ " == " ^ of_ast y
  | Not_equal        (x,y) when utf8  -> of_ast x ^ " ≠ " ^ of_ast y
  | Not_equal        (x,y) -> of_ast x ^ " != " ^ of_ast y
  | Lesser_than      (x,y) -> of_ast x ^ " < "  ^ of_ast y
  | Lesser_or_equal  (x,y) when utf8 -> of_ast x ^ " ≤ " ^ of_ast y
  | Lesser_or_equal  (x,y) -> of_ast x ^ " <= " ^ of_ast y
  | Greater_than     (x,y) -> of_ast x ^ " > "  ^ of_ast y
  | Greater_or_equal (x,y) when utf8 -> of_ast x ^ " ≥ " ^ of_ast y
  | Greater_or_equal (x,y) -> of_ast x ^ " >= " ^ of_ast y
  | Union  (x,y) when utf8 -> of_ast x ^ " ∪ " ^ of_ast y
  | Union  (x,y) -> "union("  ^ of_ast x ^ "," ^ of_ast y
  | Inter  (x,y) when utf8 -> of_ast x ^ " ∩ " ^ of_ast y
  | Inter  (x,y) -> "inter("  ^ of_ast x ^ "," ^ of_ast y
  | Diff   (x,y) when utf8 -> of_ast x ^ "\\" ^ of_ast y
  | Diff   (x,y) -> "diff("   ^ of_ast x ^ "," ^ of_ast y
  | Range  (x,y) -> "["       ^ of_ast x ^ ".." ^ of_ast y ^ "]"
  | Subset (x,y) when utf8 -> of_ast x ^ " ⊆ " ^ of_ast y
  | Subset (x,y) -> "subset(" ^ of_ast x ^ "," ^ of_ast y
  | Powerset x   -> "powerset(" ^ of_ast x ^ ")"
  | In     (x,y) when utf8 -> of_ast x ^ " ∈ " ^ of_ast y
  | In     (x,y) -> of_ast x ^ " in " ^ of_ast y
  | Empty x when utf8 -> of_ast x ^ "=∅"
  | Empty x -> "empty(" ^ of_ast x ^ ")"
  | Card  x when utf8 -> "|"  ^ of_ast x ^ "|"
  | Card  x -> "card("  ^ of_ast x ^ ")"
  | If (x,y,z) ->
      "if " ^ of_ast x
      ^ " then\n" ^ of_ast y
      ^ "\nelse\n" ^ of_ast z
      ^ "\nend\n"
  | Bigand (x,y,None,z) ->
      "bigand " ^ (of_ast_list "," x)
       ^ " in " ^ (of_ast_list "," y)
       ^ ":\n"  ^ of_ast z
       ^ "\nend\n"
  | Bigand (x,y,Some b,z) ->
      "bigand " ^ (of_ast_list "," x)
       ^ " in "   ^ (of_ast_list "," y)
       ^ " when " ^ of_ast b
       ^ ":\n"    ^ of_ast z
       ^ "\nend\n"
  | Bigor (x,y,None,z) ->
      "bigor " ^ (of_ast_list "," x)
       ^ " in " ^ (of_ast_list "," y)
       ^ ":\n"  ^ of_ast z
       ^ "\nend\n"
  | Bigor (x,y,Some b,z) ->
      "bigor " ^ (of_ast_list "," x)
       ^ " in "   ^ (of_ast_list "," y)
       ^ " when " ^ of_ast b
       ^ ":\n"    ^ of_ast z
       ^ "\nend\n"
  | Exact (x,y) -> "exact(" ^ of_ast x ^ "," ^ of_ast y ^ ")"
  | Atmost (x,y) -> "atmost(" ^ of_ast x ^ "," ^ of_ast y ^ ")"
  | Atleast (x,y) -> "atleast(" ^ of_ast x ^ "," ^ of_ast y ^ ")"
  | Let (v,x,c) -> of_ast v ^ "=" ^ of_ast x ^ ": " ^ of_ast c
  | Affect (v,c) -> of_ast v ^ "=" ^ of_ast c
  | Touist_code (f) -> (of_ast_list "\n" f)
  | Loc (x,l) -> (if debug then "loc "^ Err.string_of_loc l ^":" else "") ^ of_ast x
  | Paren x -> of_ast x
  | Exists (v,f) when utf8 -> "∃"^ of_ast v ^"."^ of_ast f
  | Exists (v,f)             -> "exists "^ of_ast v ^": "^ of_ast f
  | Forall (v,f) when utf8 -> "∀" ^ of_ast v ^"."^ of_ast f
  | Forall (v,f)             -> "forall "^ of_ast v ^": "^ of_ast f
  | For (v,c,f)           -> "for "^of_ast v^" in "^of_ast c^":"^ of_ast f
  | NewlineBefore f | NewlineAfter f -> of_ast f
  | Formula f -> "\"" ^ of_ast f ^ "\""
  | SetBuilder (f, vars, sets, cond)   ->
    "[" ^ of_ast f ^ " for "
    ^ of_ast_list "," vars ^ " in " ^ of_ast_list "," sets
    ^ (match cond with Some c -> " when " ^ of_ast c | None -> "") ^"]"

and string_of_ast_type ?(debug=false) (ast:Ast.t) : string =
  let of_ast_type ast = string_of_ast_type ~debug ast in
  match ast with
  | Int    _               -> "int"
  | Float  _               -> "float"
  | Bool      _            -> "bool"
  | Top                    -> "Top"
  | Bottom                 -> "Bot"
  | UnexpProp (_,_)        -> "unexpanded proposition"
  | Prop _                 -> "proposition"
  | Var (_,None)         -> "variable"
  | Var (_,Some _)       -> "tuple-variable"
  | Set _                  -> "set"
  | Set_decl _             -> "[ ] (set definition)"
  | Neg _                  -> "-"
  | Add (_,_)              -> "+"
  | Sub (_,_)              -> "-"
  | Mul (_,_)              -> "*"
  | Div (_,_)              -> "/"
  | Mod (_,_)              -> "mod"
  | Sqrt     _             -> "sqrt()"
  | To_int   _             -> "int()"
  | To_float _             -> "float()"
  | Abs _                  -> "abs()"
  | Not _                  -> "not"
  | And     (_,_)          -> "and"
  | Or      (_,_)          -> "or"
  | Xor     (_,_)          -> "xor"
  | Implies (_,_)          -> "=>"
  | Equiv   (_,_)          -> "<=>"
  | Equal            (_,_) -> "=="
  | Not_equal        (_,_) -> "!="
  | Lesser_than      (_,_) -> "<"
  | Lesser_or_equal  (_,_) -> "<="
  | Greater_than     (_,_) -> ">"
  | Greater_or_equal (_,_) -> ">="
  | Union  (_,_)           -> "union"
  | Inter  (_,_)           -> "inter"
  | Diff   (_,_)           -> "diff"
  | Range  (_,_)           -> ".."
  | Subset (_,_)           -> "subset()"
  | Powerset _             -> "powerset()"
  | In     (_,_)           -> "in"
  | Empty _                -> "empty()"
  | Card  _                -> "card()"
  | If (_,_,_)             -> "if"
  | Bigand (_,_,None,_)    -> "bigand"
  | Bigand (_,_,Some _,_)  -> "bigand"
  | Bigor (_,_,None,_)     -> "bigor"
  | Bigor (_,_,Some _,_)   -> "bigor"
  | Exact (_,_)            -> "exact"
  | Atmost (_,_)           -> "atmost"
  | Atleast (_,_)          -> "atleast"
  | Let (_,_,_)            -> "let"
  | Affect (_,_)           -> "="
  | Touist_code (_)      -> "(touist code)"
  | Loc (x,_) -> if debug then "location" else of_ast_type x
  | Paren x -> of_ast_type x
  | Exists (_,_)           -> "exists"
  | Forall (_,_)           -> "forall"
  | For (_,_,_)            -> "for"
  | NewlineBefore _ | NewlineAfter _ -> "newline"
  | Formula _              -> "quoted formula"
  | SetBuilder (_,_,_,_)   -> "set builder"


and string_of_ast_list ?(utf8=false) ?(show_var=(fun _ -> "")) ?(debug=false) ?(parenthesis=debug) sep el =
    String.concat sep (List.map (string_of_ast ~utf8 ~show_var ~parenthesis ~debug) el)