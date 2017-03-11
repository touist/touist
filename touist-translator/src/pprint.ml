(** Helper functions for printing anything contained in the AST
    produced by the parser. *)

open Syntax

(** [string_of_ast] takes an abstract syntaxic tree.
    @param ast of type [Syntax.ast] *)
let rec string_of_ast = function
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top    -> "top"
  | Bottom -> "bot"
  | Prop x | UnexpProp (x, None)-> x
  | UnexpProp (x,Some y) -> x ^ "(" ^ (string_of_ast_list ", " y) ^ ")"
  | Var (x,None)   -> x
  | Var (x,Some y) -> x ^ "(" ^ (string_of_ast_list ", " y) ^ ")"
  | Set    x -> string_of_set x
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
  | And     (x,y) -> (string_of_ast x) ^ " and " ^ (string_of_ast y)
  | Or      (x,y) -> (string_of_ast x) ^ " or "  ^ (string_of_ast y)
  | Xor     (x,y) -> (string_of_ast x) ^ " xor " ^ (string_of_ast y)
  | Implies (x,y) -> (string_of_ast x) ^ " => "  ^ (string_of_ast y)
  | Equiv   (x,y) -> (string_of_ast x) ^ " <=> " ^ (string_of_ast y)
  | Equal            (x,y) -> (string_of_ast x) ^ " == " ^ (string_of_ast y)
  | Not_equal        (x,y) -> (string_of_ast x) ^ " != " ^ (string_of_ast y)
  | Lesser_than      (x,y) -> (string_of_ast x) ^ " < "  ^ (string_of_ast y)
  | Lesser_or_equal  (x,y) -> (string_of_ast x) ^ " <= " ^ (string_of_ast y)
  | Greater_than     (x,y) -> (string_of_ast x) ^ " > "  ^ (string_of_ast y)
  | Greater_or_equal (x,y) -> (string_of_ast x) ^ " >= " ^ (string_of_ast y)
  | Union  (x,y) -> "union("  ^ (string_of_ast x) ^ ", " ^ (string_of_ast y)
  | Inter  (x,y) -> "inter("  ^ (string_of_ast x) ^ ", " ^ (string_of_ast y)
  | Diff   (x,y) -> "diff("   ^ (string_of_ast x) ^ ", " ^ (string_of_ast y)
  | Range  (x,y) -> "["       ^ (string_of_ast x) ^ ".." ^ (string_of_ast y) ^ "]"
  | Subset (x,y) -> "subset(" ^ (string_of_ast x) ^ ", " ^ (string_of_ast y)
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
  | Loc (x,_) -> string_of_ast x
  | Paren x -> string_of_ast x

and string_of_ast_type = function
  | Int    x               -> "int"
  | Float  x               -> "float"
  | Bool      x            -> "bool"
  | Top                    -> "top"
  | Bottom                 -> "bot"
  | UnexpProp (_,_)        -> "unexpanded proposition"
  | Prop x                 -> "proposition"
  | Var (x,None)         -> "variable"
  | Var (x,Some y)       -> "tuple-variable"
  | Set (EmptySet)     -> "empty set"
  | Set (FSet _)    -> "float-set"
  | Set (ISet _)    -> "int-set"
  | Set (SSet _)    -> "term-set"
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

and string_of_set = function
  | EmptySet  -> "[]"
  | ISet s -> (*string_of_a_list string_of_int (IntSet.elements s)*)
      string_of_intset s
  | FSet s -> (*string_of_a_list string_of_float (FloatSet.elements s)*)
      string_of_floatset s
  | SSet s -> (*string_of_a_list (fun x -> x) (PropSet.elements s)*)
      string_of_strset s

and string_of_ast_list sep el = String.concat sep (List.map string_of_ast el)

and string_of_a_list to_string il =
  "[" ^ (String.concat ", " (List.map to_string il)) ^ "]"

and string_of_intset s =
  "[" ^ (String.concat ", " (List.map string_of_int (IntSet.elements s))) ^ "]"

and string_of_floatset s =
  "[" ^ (String.concat ", " (List.map string_of_float (FloatSet.elements s))) ^ "]"

and string_of_strset s =
  "[" ^ (String.concat ", " (PropSet.elements s)) ^ "]"