open Types
open Types.Ast

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

let arith_unop u x =
  match u with
  | Neg -> "-" ^ x
  | Sqrt -> "\\sqrt{" ^ x ^ "}"
  | To_int -> "\\textrm{int}(" ^ x ^ ")"
  | Abs -> "\\|" ^ x ^ "\\|"
  | To_float -> "\\textrm{float}(" ^ x ^ ")"

let arith_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "\\times"
  | Div -> "\\div"
  | Mod -> "\\textrm{mod}"

let logic_binop = function
  | And -> "\\wedge"
  | Or -> "\\vee"
  | Xor -> "\\oplus"
  | Implies -> "\\Rightarrow"
  | Equiv -> "\\Leftrightarrow"

let set_binop = function
  | Union -> "\\cup"
  | Inter -> "\\cap"
  | Diff -> "\\setminus"

let arith_binrel = function
  | Equal -> "="
  | Not_equal -> "\\neq"
  | Lesser_than -> "<"
  | Lesser_or_equal -> "\\leq"
  | Greater_than -> "<"
  | Greater_or_equal -> "\\geq"

let layout full has_newlines x = function
  | Loc _ -> x
  | Paren ->
      if
        full && has_newlines
        (* \left( and \right) must be on the same line in latex.
            If there is a \\ in latex between two parenthesis, we use
            a pmatrix instead. *)
      then "\\begin{pmatrix*}[l]" ^ x ^ "\\end{pmatrix*}"
      else "\\left(" ^ x ^ "\\right)"
  | NewlineBefore -> "\\\\\n" ^ x
  | NewlineAfter -> x ^ "\\\\\n"

let cardinality = function
  | Exact -> "\\textrm{exact}"
  | Atmost -> "\\textrm{atmost}"
  | Atleast -> "\\textrm{atleast}"

let rec latex_of_ast ?(matrix_instead_of_substack = false) ~full ast =
  let latex_of_ast ast = latex_of_ast ~matrix_instead_of_substack ~full ast in
  let latex_of_commalist =
    latex_of_commalist ~matrix_instead_of_substack ~full
  in
  match ast with
  (* TODO: If a top-formula contains any binary operator that have lesser
     precedence than 'and', then the whole top-formula should be
     parenthesized. *)
  | Touist_code f -> latex_of_commalist "\\\\\n" f ^ "\\\\"
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x
  | Top -> "\\top "
  | Bottom -> "\\bot "
  | Prop x | UnexpProp (x, None) -> escape_underscore x
  | UnexpProp (x, Some y) ->
      escape_underscore x ^ "_{" ^ latex_of_commalist "," y ^ "}"
  | Var (name, ind) -> (
      "\\mathbf{"
      ^ escape_underscore (rm_dollar name)
      ^ "}"
      ^
      match ind with
      | Some ind -> "_{" ^ latex_of_commalist "," ind ^ "}"
      | None -> "")
  | Set x -> "[" ^ latex_of_commalist "," (AstSet.elements x) ^ "]"
  | Set_decl x -> "[" ^ latex_of_commalist "," x ^ "]"
  | ArithUnop (u, y) -> arith_unop u (latex_of_ast y)
  | ArithBinop (x, Div, y) ->
      "\\frac{" ^ latex_of_ast x ^ "}{" ^ latex_of_ast y ^ "}"
      (* AS: I'd prefer to use \\div, but leaving things as they are now. *)
  | ArithBinop (x, b, y) ->
      latex_of_ast x ^ " " ^ arith_binop b ^ " " ^ latex_of_ast y
  | Not x -> "\\neg " ^ latex_of_ast x
  | LogicBinop (x, b, y) ->
      latex_of_ast x ^ " " ^ logic_binop b ^ " " ^ latex_of_ast y
  | ArithBinrel (x, b, y) ->
      latex_of_ast x ^ " " ^ arith_binrel b ^ " " ^ latex_of_ast y
  | SetBinop (x, b, y) -> latex_of_ast x ^ set_binop b ^ latex_of_ast y
  | Range (x, y) -> "[" ^ latex_of_ast x ^ ".." ^ latex_of_ast y ^ "]"
  | Subset (x, y) -> latex_of_ast x ^ "\\subseteq " ^ latex_of_ast y
  | Powerset x -> "\\textrm{powerset}(" ^ latex_of_ast x ^ ")"
  | In (x, y) -> latex_of_ast x ^ " \\in " ^ latex_of_ast y
  | Empty x -> "\\textrm{empty}(" ^ latex_of_ast x ^ ")"
  | Card x -> "\\textrm{card}(" ^ latex_of_ast x ^ ")"
  | If (x, y, z) ->
      "\\textrm{if}\\;" ^ latex_of_ast x ^ "\\;\\textrm{then}\\;"
      ^ latex_of_ast y ^ "\\;\\textrm{else}\\;" ^ latex_of_ast z
  | Bigand (x, y, b, z) ->
      "\\bigwedge\\limits_{"
      ^ (if matrix_instead_of_substack then "\\begin{matrix}"
        else "\\substack{")
      ^ latex_of_commalist "," x ^ "\\in " ^ latex_of_commalist "," y
      ^ (match b with None -> "" | Some b -> "\\\\ " ^ latex_of_ast b)
      ^ (if matrix_instead_of_substack then "\\end{matrix}" else "}")
      ^ "}"
      ^ latex_of_ast
          (if z |> Eval.ast_without_layout |> is_binary_op then Layout (Paren, z)
          else z)
  | Bigor (x, y, b, z) ->
      "\\bigvee\\limits_{"
      ^ (if matrix_instead_of_substack then "\\begin{matrix}"
        else "\\substack{")
      ^ latex_of_commalist "," x ^ "\\in " ^ latex_of_commalist "," y
      ^ (match b with None -> "" | Some b -> "\\\\ " ^ latex_of_ast b)
      ^ (if matrix_instead_of_substack then "\\end{matrix}" else "}")
      ^ "}"
      ^ latex_of_ast
          (if z |> Eval.ast_without_layout |> is_binary_op then Layout (Paren, z)
          else z)
  | Cardinality (c, x, y) ->
      cardinality c ^ "(" ^ latex_of_ast x ^ "," ^ latex_of_ast y ^ ")"
  | Let (v, x, c) ->
      let rec unwrap acc ast : string =
        match ast with
        | Let (v', x', c') -> unwrap ((v', x') :: acc) c'
        | Layout (Loc _, x) -> unwrap acc x
        | c' ->
            "\\left(" ^ latex_of_ast c' ^ "\\right)_{\\begin{Bmatrix}"
            ^ (acc
              |> List.fold_left
                   (fun acc (v, x) ->
                     latex_of_ast v ^ " \\leftarrow " ^ latex_of_ast x ^ "\\\\ "
                     ^ acc)
                   "")
            ^ "\\end{Bmatrix}}"
      in
      unwrap [ (v, x) ] c
  | Affect (v, c) -> latex_of_ast v ^ " \\leftarrow " ^ latex_of_ast c
  | Layout (l, x) -> layout full (contains_newline x) (latex_of_ast x) l
  | Exists (v, f) -> "\\exists " ^ latex_of_ast v ^ ". " ^ latex_of_ast f
  | Forall (v, f) -> "\\forall " ^ latex_of_ast v ^ ". " ^ latex_of_ast f
  | For (var, set, above_f) ->
      let op, prop, f =
        match Eval.ast_without_layout above_f with
        | Forall (prop, f) -> ("\\forall ", prop, f)
        | Exists (prop, f) -> ("\\exists ", prop, f)
        | f ->
            failwith
              ("[shoudlnt happen] only exists and forall allowed with 'for' \
                statement. This is an '"
              ^ Pprint.string_of_ast_type f
              ^ "': "
              ^ Pprint.string_of_ast ~debug:true above_f)
      in
      "\\displaystyle\\mathop{" ^ op ^ latex_of_ast prop ^ "}_{"
      ^ latex_of_ast var ^ "\\in " ^ latex_of_ast set ^ "} ." ^ latex_of_ast f
  | Formula f -> latex_of_ast f
  | SetBuilder (f, vars, sets, cond) ->
      "[" ^ latex_of_ast f ^ "~|~"
      ^ latex_of_commalist "," vars
      ^ "\\in "
      ^ latex_of_commalist " \\times " sets
      ^ (match cond with Some c -> ", " ^ latex_of_ast c | _ -> "")
      ^ "]"

and latex_of_commalist ~matrix_instead_of_substack ~full sep el =
  String.concat sep
    (List.map (latex_of_ast ~matrix_instead_of_substack ~full) el)

and escape_underscore txt =
  Re.Str.global_replace (Re.Str.regexp "_") "\\\\_" txt

(** [ast_fun] will apply f on all *formula*-related elements of the AST where
    cond is true. The tranversal order should not be considered.
    Whenever a non-formula is given, acc will be immediatly returned. *)
and ast_fun (f : 'a -> Ast.t -> 'a) (acc : 'a) ast : 'a =
  let acc = f acc ast in
  let ast_fun' ast acc = ast_fun f acc ast in
  match ast with
  | Touist_code listf ->
      listf |> List.fold_left (fun acc f -> acc |> ast_fun' f) acc
  | ArithUnop (_, f)
  | Not f
  | Bigand (_, _, _, f)
  | Bigor (_, _, _, f)
  | Let (_, _, f)
  | Layout (_, f)
  | Exists (_, f)
  | Forall (_, f)
  | For (_, _, f) ->
      acc |> ast_fun' f
  | ArithBinop (_, Mod, _) -> acc
  | ArithBinop (x, _, y)
  | If (_, x, y)
  | LogicBinop (x, _, y)
  | ArithBinrel (x, _, y) ->
      acc |> ast_fun' x |> ast_fun' y
  | Int _ | Float _ | Bool _ | Top | Bottom | Prop _ | UnexpProp _ | Var _
  | Set _ | Set_decl _ | Card _ | Cardinality _ | Affect _ | Formula _
  | SetBuilder _ ->
      acc
  (* non-formulas *)
  | SetBinop _ | Range _ | Subset _ | Powerset _ | In _ | Empty _ -> acc

and contains_newline ast =
  ast
  |> ast_fun
       (fun acc ast ->
         match ast with
         | Layout (NewlineAfter, _) | Layout (NewlineBefore, _) -> true
         | _ -> acc)
       false

and is_binary_op = function
  | ArithBinop (_, _, _) | LogicBinop (_, _, _) | ArithBinrel _ -> true
  | _ -> false

and contains_binary_op ast =
  ast |> ast_fun (fun acc ast -> if is_binary_op ast then true else acc) false
