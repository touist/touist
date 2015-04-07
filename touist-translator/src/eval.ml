open Syntax

exception TypeError     of string
exception NameError     of string
exception ArgumentError of string

(* return the list of integers between min and max-1
 * with an increment of step
 *)
let range min max step =
  let rec loop acc cpt =
    if cpt = max then
      acc
    else
      loop (cpt::acc) (cpt+1)
  in loop [] min |> List.rev

let rec set_bin_op iop fop sop repr s1 s2 =
  match s1, s2 with
  | GenSet.Empty, GenSet.Empty -> GenSet.Empty
  | GenSet.ISet _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.ISet IntSet.empty)
  | GenSet.Empty, GenSet.ISet _ ->
      set_bin_op iop fop sop repr (GenSet.ISet IntSet.empty) s2
  | GenSet.Empty, GenSet.FSet _ ->
      set_bin_op iop fop sop repr (GenSet.FSet FloatSet.empty) s2
  | GenSet.FSet _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.FSet FloatSet.empty)
  | GenSet.Empty, GenSet.SSet _ ->
      set_bin_op iop fop sop repr (GenSet.SSet StringSet.empty) s2
  | GenSet.SSet _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.SSet StringSet.empty)
  | GenSet.ISet a, GenSet.ISet b -> GenSet.ISet (iop a b)
  | GenSet.FSet a, GenSet.FSet b -> GenSet.FSet (fop a b)
  | GenSet.SSet a, GenSet.SSet b -> GenSet.SSet (sop a b)
  | _,_ -> raise (TypeError ("unsupported set type(s) for '" ^ repr  ^ "'"))

let rec set_pred_op ipred fpred spred repr s1 s2 =
  match s1, s2 with
  | GenSet.Empty, GenSet.Empty -> true
  | GenSet.Empty, GenSet.ISet _ ->
      set_pred_op ipred fpred spred repr (GenSet.ISet IntSet.empty) s2
  | GenSet.ISet _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.ISet IntSet.empty)
  | GenSet.Empty, GenSet.FSet _ ->
      set_pred_op ipred fpred spred repr (GenSet.FSet FloatSet.empty) s2
  | GenSet.FSet _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.FSet FloatSet.empty)
  | GenSet.Empty, GenSet.SSet _ ->
      set_pred_op ipred fpred spred repr (GenSet.SSet StringSet.empty) s2
  | GenSet.SSet _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.SSet StringSet.empty)
  | GenSet.ISet a, GenSet.ISet b -> ipred a b
  | GenSet.FSet a, GenSet.FSet b -> fpred a b
  | GenSet.SSet a, GenSet.SSet b -> spred a b
  | _,_ -> raise (TypeError ("unsupported set type(s) for '" ^ repr ^ "'"))

let num_pred_op n1 n2 ipred fpred repr =
  match n1,n2 with
  | Int x, Int y     -> Bool (ipred x y)
  | Float x, Float y -> Bool (fpred x y)
  | _,_ -> raise (TypeError ("unsupported operand types for '" ^ repr ^ "'"))

let num_bin_op n1 n2 iop fop repr =
  match n1,n2 with
  | Int x, Int y     -> Int   (iop x y)
  | Float x, Float y -> Float (fop x y)
  | _,_ -> raise (TypeError ("unsupported operand types for '" ^ repr ^ "'"))

let bool_bin_op b1 b2 op repr =
  match b1,b2 with
  | Bool x, Bool y -> Bool (op x y)
  | _,_ -> raise (TypeError ("unsupported operand types for '" ^ repr ^ "'"))

let unwrap_int = function
  | Int x -> x
  | x -> raise (TypeError ("expected int, got " ^ (string_of_exp x)))

let unwrap_float = function
  | Float x -> x
  | x -> raise (TypeError ("expected float, got " ^ (string_of_exp x)))

let unwrap_str = function
  | Clause (Term (x,None))   -> x
  | Clause (Term (x,Some _)) -> failwith ("unevaluated term: " ^ x)
  | x -> raise (TypeError ("expected term, got " ^ (string_of_exp x)))


let extenv = Hashtbl.create 10

let rec eval exp env =
  List.fold_left (fun acc x -> CAnd (acc, x)) Top (eval_prog exp env)

and eval_prog exp env =
  match exp with
  | Prog (None, clauses) -> List.map (fun x -> eval_clause x env) clauses
  | Prog (Some decl, clauses) ->
      List.iter (fun x -> eval_affect x env) decl;
      List.map (fun x -> eval_clause x env) clauses

and eval_affect exp env =
  match exp with
  | Affect (x,y) -> Hashtbl.replace extenv x (eval_exp y env)

and eval_exp exp env =
  match exp with
  | Int x   -> Int x
  | Float x -> Float x
  | Bool x  -> Bool x
  | Var x ->
      begin
        try List.assoc x env
        with Not_found ->
          try Hashtbl.find extenv x
          with Not_found -> raise (NameError (string_of_exp exp))
      end
  | Set x -> Set x
  | Set_decl x -> eval_set x env
  | Clause x -> Clause (eval_clause x env)
  | Neg x ->
      begin
        match eval_exp x env with
        | Int x'   -> Int   (- x')
        | Float x' -> Float (-. x')
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | Add (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (+) (+.) "+"
  | Sub (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (-) (-.) "-"
  | Mul (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) ( * ) ( *. ) "*"
  | Div (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (/) (/.) "/"
  | Mod (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y' -> Int (x' mod y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Sqrt x ->
      begin
        match eval_exp x env with
        | Float x' -> Float (sqrt x')
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | To_int x ->
      begin
        match eval_exp x env with
        | Float x' -> Int (int_of_float x')
        | Int x'   -> Int x'
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | To_float x ->
      begin
        match eval_exp x env with
        | Int x'   -> Float (float_of_int x')
        | Float x' -> Float x'
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | Not x ->
      begin
        match eval_exp x env with
        | Bool x' -> Bool (not x')
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | And (x,y) -> bool_bin_op (eval_exp x env) (eval_exp y env) (&&) "and"
  | Or (x,y) -> bool_bin_op (eval_exp x env) (eval_exp y env) (||) "or"
  | Xor (x,y) ->
      bool_bin_op (eval_exp x env)
                  (eval_exp y env)
                  (fun p q -> (p || q) && (not (p && q))) "xor"
  | Implies (x,y) ->
      bool_bin_op (eval_exp x env) (eval_exp y env) (fun p q -> not p || q) "=>"
  | Equiv (x,y) ->
      bool_bin_op (eval_exp x env)
                  (eval_exp y env)
                  (fun p q -> (not p || q) && (not q || p)) "<=>"
  | If (x,y,z) ->
      let test =
        match eval_exp x env with
        | Bool true  -> true
        | Bool false -> false
        | _ -> raise (TypeError (string_of_exp exp))
      in
      if test then eval_exp y env else eval_exp z env
  | Union (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.union)
                            (FloatSet.union)
                            (StringSet.union) "union" x' y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Inter (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.inter)
                            (FloatSet.inter)
                            (StringSet.inter) "inter" x' y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Diff (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.diff)
                            (FloatSet.diff)
                            (StringSet.diff) "diff" x' y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Range (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y' -> Set (GenSet.ISet (IntSet.of_list (range x' y' 1)))
        | _,_ -> raise (ArgumentError (string_of_exp exp))
      end
  | Empty x ->
      begin
        match eval_exp x env with
        | Set x' ->
            begin
              match x' with
              | GenSet.Empty    -> Bool true
              | GenSet.ISet x'' -> Bool (IntSet.is_empty x'')
              | GenSet.FSet x'' -> Bool (FloatSet.is_empty x'')
              | GenSet.SSet x'' -> Bool (StringSet.is_empty x'')
            end
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | Card x ->
      begin
        match eval_exp x env with
        | Set x' ->
            begin
              match x' with
              | GenSet.Empty    -> Int 0
              | GenSet.ISet x'' -> Int (IntSet.cardinal x'')
              | GenSet.FSet x'' -> Int (FloatSet.cardinal x'')
              | GenSet.SSet x'' -> Int (StringSet.cardinal x'')
            end
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | Subset (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Bool (set_pred_op (IntSet.subset)
                              (FloatSet.subset)
                              (StringSet.subset) "subset" x' y')
        | _ -> raise (TypeError (string_of_exp exp))
      end
  | In (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Set (GenSet.ISet y') -> Bool (IntSet.mem x' y')
        | Float x', Set (GenSet.FSet y') -> Bool (FloatSet.mem x' y')
        | Clause (Term (x',None)), Set (GenSet.SSet y') -> Bool (StringSet.mem x' y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Equal (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y' -> Bool (x' = y')
        | Float x', Float y' -> Bool (x' = y')
        | Set x', Set y' ->
            Bool (set_pred_op (IntSet.equal)
                              (FloatSet.equal)
                              (StringSet.equal) "=" x' y')
        | _,_ -> raise (TypeError (string_of_exp exp))
      end
  | Not_equal        (x,y) -> eval_exp (Not (Equal (x,y))) env
  | Lesser_than      (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (<) (<) "<"
  | Lesser_or_equal  (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (<=) (<=) "<="
  | Greater_than     (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (>) (>) ">"
  | Greater_or_equal (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (>=) (>=) ">="

and eval_set set_decl env =
  let eval_form = List.map (fun x -> eval_exp x env) set_decl in
  match eval_form with
  | (Int _)::xs ->
      Set (GenSet.ISet (IntSet.of_list (List.map unwrap_int eval_form)))
  | (Float _)::xs ->
      Set (GenSet.FSet (FloatSet.of_list (List.map unwrap_float eval_form)))
  | (Clause (Term (_,None)))::xs ->
      Set (GenSet.SSet (StringSet.of_list (List.map unwrap_str eval_form)))
  | _ -> raise (TypeError "malformed set")

and eval_clause exp env =
  match exp with
  | Top    -> Top
  | Bottom -> Bottom
  | Term (x,None)   -> Term (x,None)
  | Term (x,Some y) -> Term (x ^ "(" ^ string_of_exp (eval_exp y env) ^ ")", None)
  | CNot     x     -> CNot     (eval_clause x env)
  | CAnd     (x,y) -> CAnd     (eval_clause x env, eval_clause y env)
  | COr      (x,y) -> COr      (eval_clause x env, eval_clause y env)
  | CXor     (x,y) -> CXor     (eval_clause x env, eval_clause y env)
  | CImplies (x,y) -> CImplies (eval_clause x env, eval_clause y env)
  | CEquiv   (x,y) -> CEquiv   (eval_clause x env, eval_clause y env)
  | Bigand (v,s,t,e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> raise (ArgumentError (string_of_clause exp))
        | [x],[y] ->
            begin
              match eval_exp y env with
              | Set (GenSet.Empty) ->
                  raise (ArgumentError ("empty set: " ^ (string_of_clause exp)))
              | Set (GenSet.ISet a) -> bigand_int   env x (IntSet.elements a)    test e
              | Set (GenSet.FSet a) -> bigand_float env x (FloatSet.elements a)  test e
              | Set (GenSet.SSet a) -> bigand_str   env x (StringSet.elements a) test e
              | _ -> raise (ArgumentError (string_of_clause exp))
            end
        | x::xs,y::ys ->
            eval_clause (Bigand ([x],[y],None,(Bigand (xs,ys,t,e)))) env
      end
  | Bigor (v,s,t,e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> raise (ArgumentError (string_of_clause exp))
        | [x],[y] ->
            begin
              match eval_exp y env with
              | Set (GenSet.Empty) ->
                  raise (ArgumentError ("empty set: " ^ (string_of_clause exp)))
              | Set (GenSet.ISet a) -> bigor_int   env x (IntSet.elements a)    test e
              | Set (GenSet.FSet a) -> bigor_float env x (FloatSet.elements a)  test e
              | Set (GenSet.SSet a) -> bigor_str   env x (StringSet.elements a) test e
              | _ -> raise (ArgumentError (string_of_clause exp))
            end
        | x::xs,y::ys ->
            eval_clause (Bigor ([x],[y],None,(Bigor (xs,ys,t,e)))) env
      end
  | CIf (x,y,z) ->
      let test = eval_test x env in
      if test then eval_clause y env else eval_clause z env

and bigand_int env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      CAnd (acc, eval_clause exp ((var, Int x)::env))
    else
      acc) Top values
and bigand_float env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      CAnd (acc, eval_clause exp ((var, Float x)::env))
    else
      acc) Top values
and bigand_str env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      CAnd (acc, eval_clause exp ((var, Clause (Term (x,None)))::env))
    else
      acc) Top values
and bigor_int env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      COr (acc, eval_clause exp ((var, Int x)::env))
    else
      acc) Bottom values
and bigor_float env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      COr (acc, eval_clause exp ((var, Float x)::env))
    else
      acc) Bottom values
and bigor_str env var values test exp =
  List.fold_left (fun acc x ->
    if eval_test test env then
      COr (acc, eval_clause exp ((var, Clause (Term (x, None)))::env))
    else
      acc) Bottom values

and eval_test exp env =
  match eval_exp exp env with
  | Bool x -> x
  | _ -> raise (TypeError (string_of_exp exp))
