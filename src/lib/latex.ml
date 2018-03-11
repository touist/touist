open Types
open Types.Ast
open Pprint

let rm_dollar x = String.sub x 1 (String.length x - 1)

(** Idea for adding the needed parenthesis (for understanding the priorities)
    in the latex:
    - As long as the top formula finds 'and', we don't need parenthesis
    - If the top formula only contains 'and', we can omit the parenthesis
    - Big operators should have inside parenthesis when it contains anything
      non-unary: or, and, =>, <=>
    - Big operators should not have inside parenthesis when they contain
      something unary except for 'not' (?):
      bigand, bigor, exists, forall, not
    {[
    a and b or c               ->          (a and b or c)
    a and b and c              ->          <same>
    bigand x: x                ->          <same>
    bigand x: x and y          ->          bigand x: (x and y)
    a and bigand x: x          ->          <same>
    a or bigand x: x           ->           (a or bigand x: x)
    a or bigand x: x and y end ->           (a or bigand x: (x and y))
    ]}
*)

let rec latex_of_ast ~full ast =
  let latex_of_ast ast = latex_of_ast ~full ast in
  let latex_of_commalist = latex_of_commalist ~full in
  match ast with
  (* TODO: If a top-formula contains any binary operator that have lesser
     precedence than 'and', then the whole top-formula should be
     parenthesized. *)
  | Touist_code (f) -> latex_of_commalist "\\\\\n" f ^ "\\\\"
  | Int    x -> string_of_int x
  | Float  x -> string_of_float x
  | Bool   x -> string_of_bool x
  | Top    -> "\\top "
  | Bottom -> "\\bot "
  | Prop x | UnexpProp (x, None)-> escape_underscore x
  | UnexpProp (x,Some y) -> escape_underscore x ^ "_{" ^ (latex_of_commalist "," y) ^ "}"
  | Var (name,ind)   -> "\\mathbf{" ^ escape_underscore (rm_dollar name) ^ "}" ^
    (match ind with Some ind -> ("_{"^ (latex_of_commalist "," ind) ^"}") | None->"")
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
  | Subset (x,y) -> (latex_of_ast x) ^ "\\subseteq " ^ (latex_of_ast y)
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
      latex_of_ast (if z |> Eval.ast_without_loc |> is_binary_op then (Paren z) else z)
  | Bigor (x,y,b,z) ->
      "\\bigvee\\limits_{\\substack{" ^ (latex_of_commalist "," x) ^
       "\\in " ^ (latex_of_commalist "," y) ^
       (match b with None -> "" | Some b -> "\\\\" ^ (latex_of_ast b)) ^ "}}" ^
       latex_of_ast (if z |> Eval.ast_without_loc |> is_binary_op then (Paren z) else z)
  | Exact (x,y) -> "\\textrm{exact}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Atmost (x,y) -> "\\textrm{atmost}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Atleast (x,y) -> "\\textrm{atleast}(" ^ (latex_of_ast x) ^ "," ^ (latex_of_ast y) ^ ")"
  | Let (v,x,c) -> (latex_of_ast v) ^ " \\leftarrow " ^ (latex_of_ast x) ^ "\\\\" ^ (latex_of_ast c)
  | Affect (v,c) -> (latex_of_ast v) ^ " \\leftarrow " ^ (latex_of_ast c)
  | Loc (x,_) -> latex_of_ast x
  | Paren x -> if full && contains_newline x
      (* \left( and \right) must be on the same line in latex.
          If there is a \\ in latex between two parenthesis, we use
          a pmatrix instead. *)
      then "\\begin{pmatrix*}[l]" ^ latex_of_ast x ^ "\\end{pmatrix*}"
      else "\\left(" ^ latex_of_ast x ^ "\\right)"
  | Exists (v,f) -> "\\exists "^(latex_of_ast v) ^". "^ (latex_of_ast f)
  | Forall (v,f) -> "\\forall "^(latex_of_ast v) ^". "^ (latex_of_ast f)
  | For (var,set,above_f) ->
    let op, prop, f = match Eval.ast_without_loc above_f with
    | Forall (prop,f) -> ("\\forall ", prop, f)
    | Exists (prop,f) -> ("\\exists ", prop, f)
    | f -> failwith ("[shoudlnt happen] only exists and forall allowed with \
    'for' statement. This is an '"^Pprint.string_of_ast_type f^"': "^Pprint.string_of_ast ~debug:true above_f)
    in
        "\\displaystyle\\mathop{"^ op ^ latex_of_ast prop ^"}_{"
        ^ (latex_of_ast var)^ "\\in "^latex_of_ast set ^"} ."^ latex_of_ast f
  | NewlineBefore f -> "\\\\\n" ^ latex_of_ast f
  | NewlineAfter f -> latex_of_ast f ^ "\\\\\n"
  | Formula f -> latex_of_ast f
  | SetBuilder (f, vars, sets, cond) ->
    "[" ^ latex_of_ast f ^ "~|~" ^ latex_of_commalist "," vars ^ "\\in "
    ^ latex_of_commalist " \\times " sets
    ^ (match cond with Some c -> ", "^ latex_of_ast c | _ -> "") ^ "]"

  and latex_of_commalist ~full sep el = String.concat sep (List.map (latex_of_ast ~full) el)
  and escape_underscore txt =
    Re_str.global_replace (Re_str.regexp "_") "\\\\_" txt

(** [ast_fun] will apply f on all *formula*-related elements of the AST where
    cond is true. The tranversal order should not be considered.
    Whenever a non-formula is given, acc will be immediatly returned. *)
and ast_fun (f:('a -> Ast.t -> 'a)) (acc:'a) ast : 'a =
  let acc = f acc ast in
  let ast_fun' ast acc = ast_fun f acc ast in
  match ast with
  | Touist_code listf
      -> listf |> List.fold_left (fun acc f -> acc |> ast_fun' f) acc
  | Neg f | Sqrt f | To_int f | To_float f | Abs f | Not f
  | Bigand (_,_,_,f) | Bigor  (_,_,_,f) | Let (_,_,f) | Loc (f,_)
  | Paren f | Exists (_,f) | Forall (_,f) | For (_,_,f)
  | NewlineBefore f | NewlineAfter  f
      -> acc |> ast_fun' f
  | Add (x,y) | Sub (x,y) | Mul (x,y) | Div (x,y)
  | If (_,x,y) | And (x,y) | Or (x,y) | Xor (x,y) | Implies (x,y) | Equiv (x,y)
  | Equal (x,y) | Not_equal (x,y) | Lesser_than (x,y) | Lesser_or_equal (x,y)
  | Greater_than (x,y) | Greater_or_equal (x,y)
      -> acc |> ast_fun' x |> ast_fun' y
  | Int _ | Float _ | Bool _ | Top | Bottom | Prop _ | UnexpProp _ | Var _
  | Set _ | Set_decl _ | Card  _ | Exact _ | Atmost _ | Atleast _ | Affect  _
  | Formula _ | SetBuilder _
      -> acc
  (* non-formulas *)
  | Mod _ | Union _ | Inter _ | Diff _ | Range _ | Subset _ | Powerset _
  | In _ | Empty _ -> acc

and contains_newline ast =
  ast |> ast_fun (fun acc ast -> match ast with
    NewlineAfter _ | NewlineBefore _ -> true | _ -> acc) false

and is_binary_op = function
    | Add _ | Sub _ | Mul _ | Div _ | And _ | Or _ | Xor _ | Implies _ | Equiv _
    | Equal _ | Not_equal _ | Lesser_than _ | Lesser_or_equal _
    | Greater_than _ | Greater_or_equal _ -> true
    | _ -> false

and contains_binary_op ast =
  ast |> ast_fun (fun acc ast -> if is_binary_op ast then true else acc) false