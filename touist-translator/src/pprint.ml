open Syntax

let rec string_of_exp = function
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Var (x,None)   -> x
  | Var (x,Some y) -> x ^ "(" ^ (string_of_exp_list ", " y) ^ ")" 
  | Clause x -> string_of_clause x
  | Set    x -> string_of_set x
  | Set_decl x -> "<set-decl>"
  | Neg x     -> "(- " ^ (string_of_exp x) ^ ")"
  | Add (x,y) -> "(" ^ (string_of_exp x) ^ " + "   ^ (string_of_exp y) ^ ")"
  | Sub (x,y) -> "(" ^ (string_of_exp x) ^ " - "   ^ (string_of_exp y) ^ ")"
  | Mul (x,y) -> "(" ^ (string_of_exp x) ^ " * "   ^ (string_of_exp y) ^ ")"
  | Div (x,y) -> "(" ^ (string_of_exp x) ^ " / "   ^ (string_of_exp y) ^ ")"
  | Mod (x,y) -> "(" ^ (string_of_exp x) ^ " mod " ^ (string_of_exp y) ^ ")"
  | Sqrt     x -> "sqrt("  ^ (string_of_exp x) ^ ")"
  | To_int   x -> "int("   ^ (string_of_exp x) ^ ")"
  | To_float x -> "float(" ^ (string_of_exp x) ^ ")"
  | Not     x     -> "not " ^ string_of_exp x
  | And     (x,y) -> (string_of_exp x) ^ " and " ^ (string_of_exp y)
  | Or      (x,y) -> (string_of_exp x) ^ " or "  ^ (string_of_exp y)
  | Xor     (x,y) -> (string_of_exp x) ^ " xor " ^ (string_of_exp y)
  | Implies (x,y) -> (string_of_exp x) ^ " => "  ^ (string_of_exp y)
  | Equiv   (x,y) -> (string_of_exp x) ^ " <=> " ^ (string_of_exp y)
  | Equal            (x,y) -> (string_of_exp x) ^ " == " ^ (string_of_exp y)
  | Not_equal        (x,y) -> (string_of_exp x) ^ " != " ^ (string_of_exp y)
  | Lesser_than      (x,y) -> (string_of_exp x) ^ " < "  ^ (string_of_exp y)
  | Lesser_or_equal  (x,y) -> (string_of_exp x) ^ " <= " ^ (string_of_exp y)
  | Greater_than     (x,y) -> (string_of_exp x) ^ " > "  ^ (string_of_exp y)
  | Greater_or_equal (x,y) -> (string_of_exp x) ^ " >= " ^ (string_of_exp y)
  | Union  (x,y) -> "union("  ^ (string_of_exp x) ^ ", " ^ (string_of_exp y)
  | Inter  (x,y) -> "inter("  ^ (string_of_exp x) ^ ", " ^ (string_of_exp y)
  | Diff   (x,y) -> "diff("   ^ (string_of_exp x) ^ ", " ^ (string_of_exp y)
  | Range  (x,y) -> "["       ^ (string_of_exp x) ^ ".." ^ (string_of_exp y) ^ "]"
  | Subset (x,y) -> "subset(" ^ (string_of_exp x) ^ ", " ^ (string_of_exp y)
  | In     (x,y) -> (string_of_exp x) ^ " in " ^ (string_of_exp y)
  | Empty x -> "empty(" ^ (string_of_exp x) ^ ")"
  | Card  x -> "card("  ^ (string_of_exp x) ^ ")"
  | If (x,y,z) ->
      "if " ^ (string_of_exp x)
      ^ " then\n" ^ (string_of_exp y)
      ^ "\nelse\n" ^ (string_of_exp z)
      ^ "\nend\n"
and string_of_clause = function
  | CInt x -> string_of_int x
  | CFloat x -> string_of_float x
  | CNeg x -> "(-" ^ (string_of_clause x) ^ ")"
  | CAdd (x,y) -> "(" ^ (string_of_clause x) ^ " + " ^ (string_of_clause y) ^ ")"
  | CSub (x,y) -> "(" ^ (string_of_clause x) ^ " - " ^ (string_of_clause y) ^ ")"
  | CMul (x,y) -> "(" ^ (string_of_clause x) ^ " * " ^ (string_of_clause y) ^ ")"
  | CDiv (x,y) -> "(" ^ (string_of_clause x) ^ " / " ^ (string_of_clause y) ^ ")"
  | CEqual            (x,y) -> "(" ^ (string_of_clause x) ^ " == " ^ (string_of_clause y) ^ ")"
  | CNot_equal        (x,y) -> "(" ^ (string_of_clause x) ^ " != " ^ (string_of_clause y) ^ ")"
  | CLesser_than      (x,y) -> "(" ^ (string_of_clause x) ^ " < "  ^ (string_of_clause y) ^ ")"
  | CLesser_or_equal  (x,y) -> "(" ^ (string_of_clause x) ^ " <= " ^ (string_of_clause y) ^ ")"
  | CGreater_than     (x,y) -> "(" ^ (string_of_clause x) ^ " > "  ^ (string_of_clause y) ^ ")"
  | CGreater_or_equal (x,y) -> "(" ^ (string_of_clause x) ^ " >= " ^ (string_of_clause y) ^ ")"
  | Top    -> "top"
  | Bottom -> "bot"
  | CVar (x,None)   -> x
  | CVar (x,Some y) -> x ^ "(" ^ (string_of_exp_list ", " y) ^ ")"
  | Term (x,None)   -> x
  | Term (x,Some y) -> x ^ "(" ^ (string_of_exp_list ", " y) ^ ")"
  | CNot x -> "not " ^ (string_of_clause x)
  | CAnd     (x,y) -> "(" ^ (string_of_clause x) ^ " and " ^ (string_of_clause y) ^ ")"
  | COr      (x,y) -> "(" ^ (string_of_clause x) ^ " or "  ^ (string_of_clause y) ^ ")"
  | CXor     (x,y) -> (string_of_clause x) ^ " xor " ^ (string_of_clause y)
  | CImplies (x,y) -> (string_of_clause x) ^ " => "  ^ (string_of_clause y)
  | CEquiv   (x,y) -> (string_of_clause x) ^ " <=> " ^ (string_of_clause y)
  | Bigand (x,y,None,z) ->
      "bigand " ^ (String.concat "," x)
       ^ " in " ^ (string_of_exp_list "," y)
       ^ ":\n"  ^ (string_of_clause z)
       ^ "\nend\n"
  | Bigand (x,y,Some b,z) ->
      "bigand " ^ (String.concat "," x)
       ^ " in "   ^ (string_of_exp_list "," y)
       ^ " when " ^ (string_of_exp b)
       ^ ":\n"    ^ (string_of_clause z)
       ^ "\nend\n"
  | Bigor (x,y,None,z) ->
      "bigor " ^ (String.concat "," x)
       ^ " in " ^ (string_of_exp_list "," y)
       ^ ":\n"  ^ (string_of_clause z)
       ^ "\nend\n"
  | Bigor (x,y,Some b,z) ->
      "bigor " ^ (String.concat "," x)
       ^ " in "   ^ (string_of_exp_list "," y)
       ^ " when " ^ (string_of_exp b)
       ^ ":\n"    ^ (string_of_clause z)
       ^ "\nend\n"
  | CIf (x,y,z) ->
      "if " ^ (string_of_exp x)
      ^ " then\n" ^ (string_of_clause y)
      ^ "\nelse\n" ^ (string_of_clause z)
      ^ "\nend\n"
  | Exact (x,y) -> "exact(" ^ (string_of_exp x) ^ "," ^ (string_of_exp y) ^ ")"
  | Atmost (x,y) -> "atmost(" ^ (string_of_exp x) ^ "," ^ (string_of_exp y) ^ ")"
  | Atleast (x,y) -> "atleast(" ^ (string_of_exp x) ^ "," ^ (string_of_exp y) ^ ")"

and string_of_set = function
  | GenSet.Empty  -> "[]"
  | GenSet.ISet s -> (*string_of_a_list string_of_int (IntSet.elements s)*)
      string_of_intset s
  | GenSet.FSet s -> (*string_of_a_list string_of_float (FloatSet.elements s)*)
      string_of_floatset s
  | GenSet.SSet s -> (*string_of_a_list (fun x -> x) (StringSet.elements s)*)
      string_of_strset s

and string_of_exp_list sep el = String.concat sep (List.map string_of_exp el)

and string_of_a_list to_string il =
  "[" ^ (String.concat ", " (List.map to_string il)) ^ "]"

and string_of_intset s =
  "[" ^ (String.concat ", " (List.map string_of_int (IntSet.elements s))) ^ "]"

and string_of_floatset s =
  "[" ^ (String.concat ", " (List.map string_of_float (FloatSet.elements s))) ^ "]"

and string_of_strset s =
  "[" ^ (String.concat ", " (StringSet.elements s)) ^ "]"


