open Types
open Types.Ast

(* [traverse] for example for 'and' nodes, it will traverse every other embedded
   'and' and use 'f' on the leaf nodes (a leaf node is a node that is not
   'and'). The list returned. Example:
   ((a and (b or c)) and d) becomes list [a; b or c; d]. *)
type node = AndNode | OrNode
let rec traverse kind = function
  | And (And (x,y),z) | And (x,And (y, z)) when kind = AndNode -> traverse kind x @ traverse kind y @ traverse kind z
  | And (x,y) -> [x;y]
  | Or (Or (x,y),z) | Or (x,Or (y, z)) when kind = OrNode -> traverse kind x @ traverse kind y @ traverse kind z
  | Or (x,y) -> [x;y]
  | x -> [x]


let rec string_of_ast ?(utf8=false) ?(show_var=(fun ast -> "")) ?(debug=false) ?(parenthesis=debug) ast =
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
  | And     (x,y) -> "("^ (traverse AndNode ast |> of_ast_list (if utf8 then " ⋀ " else " and ")) ^")"
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
  | Assign' (x, y) when utf8 -> of_ast x ^ "←" ^ of_ast y
  | Assign' (x, y) -> of_ast x ^ "<-" ^ of_ast y
  | Test pr -> of_ast pr ^ "?"
  | Seq (pr1, pr2) -> of_ast pr1 ^ ";" ^ of_ast pr2
  | Union' (pr1, pr2) when utf8 -> "("^ of_ast pr1 ^ " ∪ " ^ of_ast pr2 ^")"
  | Union' (pr1, pr2) -> "("^ of_ast pr1 ^ " union " ^ of_ast pr2 ^")"
  | Inverse pr when utf8 -> of_ast pr ^"⁻¹"
  | Inverse pr -> of_ast pr ^"^-1"
  | Star pr -> of_ast pr ^ "*"
  | Box (pr, f) -> "[" ^ of_ast pr ^ "] " ^ of_ast f
  | Diamond (pr, f) -> "<" ^ of_ast pr ^ "> " ^ of_ast f
  | Add' p -> "+" ^ of_ast p
  | Remove p -> "-" ^ of_ast p

and string_of_ast_type ?(debug=false) (ast:Ast.t) : string =
  let of_ast_type ast = string_of_ast_type ~debug ast in
  match ast with
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
  | Loc (x,_) -> if debug then "location" else of_ast_type x
  | Paren x -> of_ast_type x
  | Exists (v,f)           -> "exists"
  | Forall (v,f)           -> "forall"
  | For (_,_,_)            -> "for"
  | NewlineBefore f | NewlineAfter f -> "newline"
  | Formula f              -> "quoted formula"
  | SetBuilder (_,_,_,_)   -> "set builder"
  | Assign' (_, _) -> "DL-PA assignment '<-'"
  | Test _ -> "DL-PA test '?'"
  | Seq (_, _) -> "DL-PA sequence ';'"
  | Union' (_, _) -> "DL-PA union"
  | Inverse _ -> "DL-PA inverse"
  | Star _ -> "DL-PA Kleene-star"
  | Diamond (_, _) -> "DL-PA diamond"
  | Box (_, _) -> "DL-PA box"
  | Add' _ -> "DL-PA true assigment"
  | Remove _ -> "DL-PA false assigment"


and string_of_ast_list ?(utf8=false) ?(show_var=(fun ast -> "")) ?(debug=false) ?(parenthesis=debug) sep el =
    String.concat sep (List.map (string_of_ast ~utf8 ~show_var ~parenthesis ~debug) el)