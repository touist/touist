(** Helper functions for printing anything contained in the AST
    produced by the parser. *)

open Types
open Types.Ast

(** [string_of_ast] takes an abstract syntaxic tree.
    @param ast of type [Types.Ast.ast] *)
let rec string_of_ast ?(show_var=(fun ast -> "")) ?(debug=false) ?(parenthesis=debug) ast =
  let string_of_ast = string_of_ast ~show_var:show_var ~parenthesis:parenthesis ~debug:debug in
  match ast with
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top    -> "Top"
  | Bottom -> "Bot"
  | Prop x | UnexpProp (x, None)-> x
  | UnexpProp (x,Some y) -> x ^ "(" ^ (string_of_ast_list "," y) ^ ")"
  | Var (x,None)   -> x ^ (show_var ast)
  | Var (x,Some y) -> x ^ "(" ^ (string_of_ast_list "," y) ^ ")" ^ show_var ast
  | Set    x -> "[" ^ string_of_ast_list "," (Set.elements x) ^ "]"
  | Set_decl x -> "[" ^ (string_of_ast_list "," x) ^ "]"
  | Neg x     -> "(- " ^ (string_of_ast x) ^ ")"
  | Add (x,y) -> "(" ^ (string_of_ast x) ^ " + "   ^ (string_of_ast y) ^ ")"
  | Sub (x,y) -> "(" ^ (string_of_ast x) ^ " - "   ^ (string_of_ast y) ^ ")"
  | Mul (x,y) -> "(" ^ (string_of_ast x) ^ " * "   ^ (string_of_ast y) ^ ")"
  | Div (x,y) -> "(" ^ (string_of_ast x) ^ " / "   ^ (string_of_ast y) ^ ")"
  | Mod (x,y) -> "(" ^ (string_of_ast x) ^ " mod " ^ (string_of_ast y) ^ ")"
  | Sqrt     x -> "sqrt("  ^ (string_of_ast x) ^ ")"
  | To_int   x -> "int("   ^ (string_of_ast x) ^ ")"
  | Abs   x -> "abs("   ^ (string_of_ast x) ^ ")"
  | To_float x -> "float(" ^ (string_of_ast x) ^ ")"
  | Not     x     -> "not " ^ string_of_ast x
  | And     (x,y) -> "(" ^ (string_of_ast x) ^ " and " ^ (string_of_ast y) ^ ")"
  | Or      (x,y) -> "(" ^ (string_of_ast x) ^ " or "  ^ (string_of_ast y) ^ ")"
  | Xor     (x,y) -> "(" ^ (string_of_ast x) ^ " xor " ^ (string_of_ast y) ^ ")"
  | Implies (x,y) -> "(" ^ (string_of_ast x) ^ " => "  ^ (string_of_ast y) ^ ")"
  | Equiv   (x,y) -> "(" ^ (string_of_ast x) ^ " <=> " ^ (string_of_ast y) ^ ")"
  | Equal            (x,y) -> (string_of_ast x) ^ " == " ^ (string_of_ast y)
  | Not_equal        (x,y) -> (string_of_ast x) ^ " != " ^ (string_of_ast y)
  | Lesser_than      (x,y) -> (string_of_ast x) ^ " < "  ^ (string_of_ast y)
  | Lesser_or_equal  (x,y) -> (string_of_ast x) ^ " <= " ^ (string_of_ast y)
  | Greater_than     (x,y) -> (string_of_ast x) ^ " > "  ^ (string_of_ast y)
  | Greater_or_equal (x,y) -> (string_of_ast x) ^ " >= " ^ (string_of_ast y)
  | Union  (x,y) -> "union("  ^ (string_of_ast x) ^ "," ^ (string_of_ast y)
  | Inter  (x,y) -> "inter("  ^ (string_of_ast x) ^ "," ^ (string_of_ast y)
  | Diff   (x,y) -> "diff("   ^ (string_of_ast x) ^ "," ^ (string_of_ast y)
  | Range  (x,y) -> "["       ^ (string_of_ast x) ^ ".." ^ (string_of_ast y) ^ "]"
  | Subset (x,y) -> "subset(" ^ (string_of_ast x) ^ "," ^ (string_of_ast y)
  | Powerset x   -> "powerset(" ^ string_of_ast x ^ ")"
  | In     (x,y) -> (string_of_ast x) ^ " in " ^ (string_of_ast y)
  | Empty x -> "empty(" ^ (string_of_ast x) ^ ")"
  | Card  x -> "card("  ^ (string_of_ast x) ^ ")"
  | If (x,y,z) ->
      "if " ^ (string_of_ast x)
      ^ " then\n" ^ (string_of_ast y)
      ^ "\nelse\n" ^ (string_of_ast z)
      ^ "\nend\n"
  | Bigand (x,y,None,z) ->
      "bigand " ^ (string_of_ast_list "," x)
       ^ " in " ^ (string_of_ast_list "," y)
       ^ ":\n"  ^ (string_of_ast z)
       ^ "\nend\n"
  | Bigand (x,y,Some b,z) ->
      "bigand " ^ (string_of_ast_list "," x)
       ^ " in "   ^ (string_of_ast_list "," y)
       ^ " when " ^ (string_of_ast b)
       ^ ":\n"    ^ (string_of_ast z)
       ^ "\nend\n"
  | Bigor (x,y,None,z) ->
      "bigor " ^ (string_of_ast_list "," x)
       ^ " in " ^ (string_of_ast_list "," y)
       ^ ":\n"  ^ (string_of_ast z)
       ^ "\nend\n"
  | Bigor (x,y,Some b,z) ->
      "bigor " ^ (string_of_ast_list "," x)
       ^ " in "   ^ (string_of_ast_list "," y)
       ^ " when " ^ (string_of_ast b)
       ^ ":\n"    ^ (string_of_ast z)
       ^ "\nend\n"
  | Exact (x,y) -> "exact(" ^ (string_of_ast x) ^ "," ^ (string_of_ast y) ^ ")"
  | Atmost (x,y) -> "atmost(" ^ (string_of_ast x) ^ "," ^ (string_of_ast y) ^ ")"
  | Atleast (x,y) -> "atleast(" ^ (string_of_ast x) ^ "," ^ (string_of_ast y) ^ ")"
  | Let (v,x,c) -> (string_of_ast v) ^ "=" ^ (string_of_ast x) ^ ": " ^ (string_of_ast c)
  | Affect (v,c) -> (string_of_ast v) ^ "=" ^ (string_of_ast c)
  | Touist_code (f) -> (string_of_ast_list "\n" f)
  | Loc (x,l) -> (if debug then "("^(Msgs.string_of_loc l)^")" else "") ^ string_of_ast x
  | Paren x -> string_of_ast x

and string_of_ast_type = function
  | Int    x               -> "int"
  | Float  x               -> "float"
  | Bool      x            -> "bool"
  | Top                    -> "Top"
  | Bottom                 -> "Bot"
  | UnexpProp (_,_)        -> "unexpanded proposition"
  | Prop x                 -> "proposition"
  | Var (x,None)         -> "variable"
  | Var (x,Some y)       -> "tuple-variable"
  | Set s                  -> "set"
  | Set_decl x             -> "[ ] (set definition)"
  | Neg x                  -> "-"
  | Add (x,y)              -> "+"
  | Sub (x,y)              -> "-"
  | Mul (x,y)              -> "*"
  | Div (x,y)              -> "/"
  | Mod (x,y)              -> "mod"
  | Sqrt     x             -> "sqrt()"
  | To_int   x             -> "int()"
  | To_float x             -> "float()"
  | Abs x                  -> "abs()"
  | Not x                  -> "not"
  | And     (x,y)          -> "and"
  | Or      (x,y)          -> "or"
  | Xor     (x,y)          -> "xor"
  | Implies (x,y)          -> "=>"
  | Equiv   (x,y)          -> "<=>"
  | Equal            (x,y) -> "=="
  | Not_equal        (x,y) -> "!="
  | Lesser_than      (x,y) -> "<"
  | Lesser_or_equal  (x,y) -> "<="
  | Greater_than     (x,y) -> ">"
  | Greater_or_equal (x,y) -> ">="
  | Union  (x,y)           -> "union"
  | Inter  (x,y)           -> "inter"
  | Diff   (x,y)           -> "diff"
  | Range  (x,y)           -> ".."
  | Subset (x,y)           -> "subset()"
  | Powerset x             -> "powerset()"
  | In     (x,y)           -> "in"
  | Empty x                -> "empty()"
  | Card  x                -> "card()"
  | If (x,y,z)             -> "if"
  | Bigand (x,y,None,z)    -> "bigand"
  | Bigand (x,y,Some b,z)  -> "bigand"
  | Bigor (x,y,None,z)     -> "bigor"
  | Bigor (x,y,Some b,z)   -> "bigor"
  | Exact (x,y)            -> "exact"
  | Atmost (x,y)           -> "atmost"
  | Atleast (x,y)          -> "atleast"
  | Let (v,x,c)            -> "let"
  | Affect (_,_)           -> "="
  | Touist_code (_)      -> "(touist code)"
  | Loc (x,_) -> string_of_ast_type x
  | Paren x -> string_of_ast_type x

and string_of_ast_list sep el = String.concat sep (List.map string_of_ast el)