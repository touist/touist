module IntSet = Set_ext.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

module FloatSet = Set_ext.Make(struct
  type t = float
  let compare = Pervasives.compare
end)

module StringSet = Set_ext.Make(String)

module GenSet = struct
  type t =
    | Empty
    | ISet of IntSet.t
    | FSet of FloatSet.t
    | SSet of StringSet.t
end

type prog =
  | Prog of affect list option * clause list
and affect =
  | Affect of var * exp
and var = string * exp list option
and exp =
  | Int              of int
  | Float            of float
  | Bool             of bool
  | Var              of var
  | Set              of GenSet.t
  | Set_decl         of exp list
  | Clause           of clause
  | Neg              of exp
  | Add              of exp * exp
  | Sub              of exp * exp
  | Mul              of exp * exp
  | Div              of exp * exp
  | Mod              of exp * exp
  | Sqrt             of exp
  | To_int           of exp
  | To_float         of exp
  | Not              of exp
  | And              of exp * exp
  | Or               of exp * exp
  | Xor              of exp * exp
  | Implies          of exp * exp
  | Equiv            of exp * exp
  | Equal            of exp * exp
  | Not_equal        of exp * exp
  | Lesser_than      of exp * exp
  | Lesser_or_equal  of exp * exp
  | Greater_than     of exp * exp
  | Greater_or_equal of exp * exp
  | Union            of exp * exp
  | Inter            of exp * exp
  | Diff             of exp * exp
  | Range            of exp * exp
  | Empty            of exp
  | Card             of exp
  | Subset           of exp * exp
  | In               of exp * exp
  | If               of exp * exp * exp
and clause =
  | CInt              of int
  | CFloat            of float
  | CAdd              of clause * clause 
  | CSub              of clause * clause 
  | CMul              of clause * clause 
  | CDiv              of clause * clause 
  | CEqual            of clause * clause 
  | CNot_equal        of clause * clause 
  | CLesser_than      of clause * clause 
  | CLesser_or_equal  of clause * clause 
  | CGreater_than     of clause * clause 
  | CGreater_or_equal of clause * clause 
  | Top
  | Bottom
  | Term     of var
  | CVar     of var
  | CNot     of clause
  | CAnd     of clause * clause
  | COr      of clause * clause
  | CXor     of clause * clause
  | CImplies of clause * clause
  | CEquiv   of clause * clause
  | Exact    of exp * exp
  | Atleast  of exp * exp
  | Atmost   of exp * exp
  | Bigand   of string list * exp list * exp option * clause
  | Bigor    of string list * exp list * exp option * clause
  | CIf      of exp * clause * clause

let rec string_of_exp = function
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Var (x,None)   -> x
  | Var (x,Some y) -> x ^ "(" ^ (string_of_exp_list ", " y) ^ ")" 
  | Clause x -> string_of_clause x
  | Set    x -> string_of_set x
  | Set_decl x -> "<set-decl>"
  | Neg x     -> "- " ^ (string_of_exp x)
  | Add (x,y) -> (string_of_exp x) ^ " + "   ^ (string_of_exp y)
  | Sub (x,y) -> (string_of_exp x) ^ " - "   ^ (string_of_exp y)
  | Mul (x,y) -> (string_of_exp x) ^ " * "   ^ (string_of_exp y)
  | Div (x,y) -> (string_of_exp x) ^ " / "   ^ (string_of_exp y)
  | Mod (x,y) -> (string_of_exp x) ^ " mod " ^ (string_of_exp y)
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

