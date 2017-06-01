(** Transform AST to latex.

    Note that the headers are not included; if you want to compile
    the resulting latex text in a real latex document, you would need
    to add (fr example):
    {[
      \documentclass[11pt]{report}
      \usepackage{mathtools}
      \begin{document}
        % thing produced by latex_of_ast
      \end{document}
    ]}

    Some details on the latex code produced:
    - tuple-propositions [p(a,b,c)] turn into p_{a,b,c}
    - variables are displayed in bold font and '$' is removed
    - we use [\mathbf{}] for setting bold font on variables.
      We could use [\bm{}] (which is a more appropriate way
      of using bold-font in the math env as it keeps the 'italic'
      way of displaying math) but [\usepackage{bm}] does not work
      with most tools: MathJax (javascript), jlatexmath (java).
*)

open Types.Ast
open Types
open Pprint

let rm_dollar x = String.sub x 1 (String.length x - 1)

let rec latex_of_ast = function
  | Touist_code (f) -> (latex_of_commalist "\\\\\n" f)
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top    -> "\\top "
  | Bottom -> "\\bot "
  | Prop x | UnexpProp (x, None)-> escape_underscore x
  | UnexpProp (x,Some y) -> x ^ "_{" ^ (latex_of_commalist "," y) ^ "}"
  | Var (name,ind)   -> "\\mathbf{" ^ escape_underscore (rm_dollar name) ^ "}" ^
    (match ind with Some ind -> ("("^ (latex_of_commalist "," ind) ^")") | None->"")
  | Set    x -> "["^latex_of_commalist "," (AstSet.elements x) ^ "]"
  | Set_decl x -> "[" ^ (latex_of_commalist "," x) ^ "]"
  | Neg x     -> "-" ^ (latex_of_ast x)
  | Add (x,y) -> (latex_of_ast x) ^ " + "   ^ (latex_of_ast y)
  | Sub (x,y) -> (latex_of_ast x) ^ " - "   ^ (latex_of_ast y)
  | Mul (x,y) -> (latex_of_ast x) ^ " \\times "   ^ (latex_of_ast y)
  | Div (x,y) -> "\\frac{" ^ (latex_of_ast x) ^ "}{" ^ (latex_of_ast y) ^"}"
  | Mod (x,y) -> (latex_of_ast x) ^ " \\textrm{mod} " ^ (latex_of_ast y)
  | Sqrt     x -> "\\sqrt{"  ^ (latex_of_ast x) ^ "}"
  | To_int   x -> "\\textrm{int}("   ^ (latex_of_ast x) ^ ")"
  | Abs   x -> "\\|"   ^ (latex_of_ast x) ^ "\\|"
  | To_float x -> "\\textrm{float}(" ^ (latex_of_ast x) ^ ")"
  | Not     x     -> "\\neg " ^ latex_of_ast x
  | And     (x,y) -> (latex_of_ast x) ^ " \\wedge " ^ (latex_of_ast y)
  | Or      (x,y) -> (latex_of_ast x) ^ " \\vee "  ^ (latex_of_ast y)
  | Xor     (x,y) -> (latex_of_ast x) ^ " \\oplus " ^ (latex_of_ast y)
  | Implies (x,y) -> (latex_of_ast x) ^ " \\Rightarrow "  ^ (latex_of_ast y)
  | Equiv   (x,y) -> (latex_of_ast x) ^ " \\Leftrightarrow " ^ (latex_of_ast y)
  | Equal            (x,y) -> (latex_of_ast x) ^ " = " ^ (latex_of_ast y)
  | Not_equal        (x,y) -> (latex_of_ast x) ^ " \\neq " ^ (latex_of_ast y)
  | Lesser_than      (x,y) -> (latex_of_ast x) ^ " < "  ^ (latex_of_ast y)
  | Lesser_or_equal  (x,y) -> (latex_of_ast x) ^ " \\leq " ^ (latex_of_ast y)
  | Greater_than     (x,y) -> (latex_of_ast x) ^ " > "  ^ (latex_of_ast y)
  | Greater_or_equal (x,y) -> (latex_of_ast x) ^ " \\geq " ^ (latex_of_ast y)
  | Union  (x,y) -> (latex_of_ast x) ^ "\\cup" ^ (latex_of_ast y)
  | Inter  (x,y) -> (latex_of_ast x) ^ "\\cap " ^ (latex_of_ast y)
  | Diff   (x,y) -> (latex_of_ast x) ^ "\\setminus " ^ (latex_of_ast y)
  | Range  (x,y) -> "["       ^ (latex_of_ast x) ^ ".." ^ (latex_of_ast y) ^ "]"
  | Subset (x,y) -> (latex_of_ast x) ^ "\\subset " ^ (latex_of_ast y)
  | Powerset x   -> "\\textrm{powerset}(" ^ latex_of_ast x ^ ")"
  | In     (x,y) -> (latex_of_ast x) ^ " \\in " ^ (latex_of_ast y)
  | Empty x -> "\\textrm{empty}(" ^ (latex_of_ast x) ^ ")"
  | Card  x -> "\\textrm{card}("  ^ (latex_of_ast x) ^ ")"
  | If (x,y,z) ->
      "\\textrm{if}\\;" ^ (latex_of_ast x)
      ^ "\\;\\textrm{then}\\;" ^ (latex_of_ast y)
      ^ "\\;\\textrm{else}\\;" ^ (latex_of_ast z)
  | Bigand (x,y,b,z) ->
       "\\bigwedge\\limits_{\\substack{" ^ (latex_of_commalist "," x) ^
       "\\in " ^ (latex_of_commalist "," y) ^
       (match b with None -> "" | Some b -> "\\\\" ^ (latex_of_ast b)) ^ "}}" ^
       (latex_of_ast z)
  | Bigor (x,y,b,z) ->
      "\\bigvee\\limits_{\\substack{" ^ (latex_of_commalist "," x) ^
       "\\in " ^ (latex_of_commalist "," y) ^
       (match b with None -> "" | Some b -> "\\\\" ^ (latex_of_ast b)) ^ "}}" ^
       (latex_of_ast z)
  | Exact (x,y) -> "\\textrm{exact}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Atmost (x,y) -> "\\textrm{atmost}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Atleast (x,y) -> "\\textrm{atleast}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Let (v,x,c) -> (latex_of_ast v) ^ " \\leftarrow " ^ (latex_of_ast x) ^ "\\\\" ^ (latex_of_ast c)
  | Affect (v,c) -> (latex_of_ast v) ^ " \\leftarrow " ^ (latex_of_ast c)
  | Loc (x,_) -> latex_of_ast x
  | Paren x -> "(" ^ latex_of_ast x ^ ")"
  | Exists (v,f) -> "\\exists "^(latex_of_ast v) ^". "^ (latex_of_ast f)
  | Forall (v,f) -> "\\forall "^(latex_of_ast v) ^". "^ (latex_of_ast f)
  | For (v,c,f)  -> "\\textrm{for} "^latex_of_ast v^" \\in "^latex_of_ast c^":"^ latex_of_ast f

  and latex_of_commalist sep el = String.concat sep (List.map latex_of_ast el)
  and escape_underscore txt =
    Str.global_replace (Str.regexp "_") "\\_" txt