open Syntax

let rec string_of_ast = function
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top    -> "top"
  | Bottom -> "bot"
  | Term (x,None)   -> x
  | Term (x,Some y) -> x ^ "(" ^ (string_of_ast_list ", " y) ^ ")"
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

and string_of_ast_type = function
  | Int    x               -> "int"
  | Float  x               -> "float"
  | Bool      x            -> "bool"
  | Top                    -> "top"
  | Bottom                 -> "bot"
  | Term (x,None)          -> "term"
  | Term (x,Some y)        -> "tuple-term"
  | Var (x,None)           -> "variable"
  | Var (x,Some y)         -> "tuple-variable"
  | Set (GenSet.Empty)     -> "empty set"
  | Set (GenSet.FSet _)    -> "float-set"
  | Set (GenSet.ISet _)    -> "int-set"
  | Set (GenSet.SSet _)    -> "term-set"
  | Set_decl x             -> "operator [_] (declaration of set)"
  | Neg x                  -> "operator -"
  | Add (x,y)              -> "operator +"
  | Sub (x,y)              -> "operator -"
  | Mul (x,y)              -> "operator *"
  | Div (x,y)              -> "operator /"
  | Mod (x,y)              -> "operator mod"
  | Sqrt     x             -> "operator sqrt()"
  | To_int   x             -> "operator int()"
  | To_float x             -> "operator float()"
  | Not x                  -> "operator not"
  | And     (x,y)          -> "operator and"
  | Or      (x,y)          -> "operator or"
  | Xor     (x,y)          -> "operator xor"
  | Implies (x,y)          -> "operator =>"
  | Equiv   (x,y)          -> "operator <=>"
  | Equal            (x,y) -> "operator =="
  | Not_equal        (x,y) -> "operator !="
  | Lesser_than      (x,y) -> "operator <"
  | Lesser_or_equal  (x,y) -> "operator <="
  | Greater_than     (x,y) -> "operator >"
  | Greater_or_equal (x,y) -> "operator >="
  | Union  (x,y)           -> "operator union"
  | Inter  (x,y)           -> "operator inter"
  | Diff   (x,y)           -> "operator diff"
  | Range  (x,y)           -> "operator .."
  | Subset (x,y)           -> "operator subset()"
  | In     (x,y)           -> "operator in"
  | Empty x                -> "operator empty()"
  | Card  x                -> "operator card()"
  | If (x,y,z)             -> "operator if"
  | Bigand (x,y,None,z)    -> "operator bigand"
  | Bigand (x,y,Some b,z)  -> "operator bigand"
  | Bigor (x,y,None,z)     -> "operator bigor"
  | Bigor (x,y,Some b,z)   -> "operator bigor"
  | Exact (x,y)            -> "operator exact"
  | Atmost (x,y)           -> "operator atmost"
  | Atleast (x,y)          -> "operator atleast"
  | Let (v,x,c)            -> "operator let"
  | Affect (_,_)           -> "operator ="

and string_of_set = function
  | GenSet.Empty  -> "[]"
  | GenSet.ISet s -> (*string_of_a_list string_of_int (IntSet.elements s)*)
      string_of_intset s
  | GenSet.FSet s -> (*string_of_a_list string_of_float (FloatSet.elements s)*)
      string_of_floatset s
  | GenSet.SSet s -> (*string_of_a_list (fun x -> x) (StringSet.elements s)*)
      string_of_strset s

and string_of_ast_list sep el = String.concat sep (List.map string_of_ast el)

and string_of_a_list to_string il =
  "[" ^ (String.concat ", " (List.map to_string il)) ^ "]"

and string_of_intset s =
  "[" ^ (String.concat ", " (List.map string_of_int (IntSet.elements s))) ^ "]"

and string_of_floatset s =
  "[" ^ (String.concat ", " (List.map string_of_float (FloatSet.elements s))) ^ "]"

and string_of_strset s =
  "[" ^ (String.concat ", " (StringSet.elements s)) ^ "]"
