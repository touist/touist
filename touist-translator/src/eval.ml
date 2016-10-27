(*
 * eval.ml: semantic analysis of the abstract syntaxic tree produced by the parser.
 *          [eval] is the main function.
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available
 * under the terms of the GNU Lesser General Public License (LGPL)
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *)

open Syntax
open Pprint

exception Error of string

(* Return the list of integers between min and max
 * with an increment of step
 *)
let irange min max step =
  let rec loop acc cpt =
    if cpt = max+1 then
      acc
    else
      loop (cpt::acc) (cpt+1)
  in loop [] min |> List.rev

let frange min max step =
  let rec loop acc cpt =
    if cpt = max +. 1. then
      acc
    else
      loop (cpt::acc) (cpt+.1.)
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
  | _,_ -> raise (Error (
      "mismatch types for set operator '" ^ repr ^ "' in the statement\n"^
      "    "^(string_of_set s1) ^ repr ^ (string_of_set s2)^"\n"^
      "Left operand has type '"^(string_of_exp_type (Set s1))^"'\n"^
      "    "^(string_of_exp_type (Set s1))^"\n"^
      "and right operand has type '"^(string_of_exp_type (Set s2))^"'\n"^
      "    "^(string_of_exp_type (Set s2))^"\n"^
      ""))
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
  | _,_ -> raise (Error (
      "mismatch types for set operator '" ^ repr ^ "' in the statement\n"^
      "    "^(string_of_set s1) ^ repr ^ (string_of_set s2)^"\n"^
      "Left operand has type '"^(string_of_exp_type (Set s1))^"'\n"^
      "    "^(string_of_exp_type (Set s1))^"\n"^
      "and right operand has type '"^(string_of_exp_type (Set s2))^"'\n"^
      "    "^(string_of_exp_type (Set s2))^"\n"^
      ""))

let num_pred_op n1 n2 ipred fpred repr =
  match n1,n2 with
  | Int x, Int y     -> Bool (ipred x y)
  | Float x, Float y -> Bool (fpred x y)
  | _,_ -> raise (Error (
      "mismatch types for number operator '" ^ repr ^ "' in the statement\n"^
      "    "^(string_of_exp n1) ^ repr ^ (string_of_exp n2)^"\n"^
      "Left operand has type '"^(string_of_exp_type n1)^"':\n"^
      "    "^(string_of_exp n1)^"\n"^
      "and right operand has type '"^(string_of_exp_type n2)^"':\n"^
      "    "^(string_of_exp n2)^"\n"^
      ""))
let num_bin_op n1 n2 iop fop repr =
  match n1,n2 with
  | Int x, Int y     -> Int   (iop x y)
  | Float x, Float y -> Float (fop x y)
  | _,_ -> raise (Error (
      "mismatch types for number operator '" ^ repr ^ "' in the statement\n"^
      "    "^(string_of_exp n1) ^ repr ^ (string_of_exp n2)^"\n"^
      "Left operand has type '"^(string_of_exp_type n1)^"':\n"^
      "    "^(string_of_exp n1)^"\n"^
      "and right operand has type '"^(string_of_exp_type n2)^"':\n"^
      "    "^(string_of_exp n2)^"\n"^
      ""))

let bool_bin_op b1 b2 op repr =
  match b1,b2 with
  | Bool x, Bool y -> Bool (op x y)
  | _,_ -> raise (Error (
      "mismatch types for boolean operator '" ^ repr ^ "' in the statement\n"^
      "    "^(string_of_exp b1) ^ repr ^ (string_of_exp b2)^"\n"^
      "Left operand has type '"^(string_of_exp_type b1)^"':\n"^
      "    "^(string_of_exp b1)^"\n"^
      "and right operand has type '"^(string_of_exp_type b2)^"':\n"^
      "    "^(string_of_exp b2)^"\n"^
      ""))


(* Two structures are used to store the information on variables.

   [env] a simple list [(name,content),...] passed as recursive argument that
   stores int & floats. It allows 'local' variables (i.e., local to the block).
   It is used for bigand and bigor and let

   [extenv] is a global hashtable with (name, content)

*)
let extenv = Hashtbl.create 10

let rec eval exp env =
  eval_prog exp env

and eval_prog exp env =
  let rec loop = function
    | []    -> raise (Error ("no formulas"))
    | [x]   -> x
    | x::xs -> And (x, loop xs)
  in
  match exp with
  | Prog (formulas, None) -> eval_exp_no_expansion (loop formulas) env
  | Prog (formulas, Some decl) ->
      List.iter (fun x -> eval_affect x env) decl;
      eval_exp_no_expansion (loop formulas) env

and eval_affect exp env =
  match exp with
  | Affect (Var x,y) ->
    Hashtbl.replace extenv (expand_var_name x env) (eval_exp y env)
  | e -> raise (Error ("this does not seem to be an affectation: " ^ string_of_exp e))


and eval_exp exp env =
  match exp with
  | Int x   -> Int x
  | Float x -> Float x
  | Bool x  -> Bool x
  | Var x ->
    let name = expand_var_name x env in
      begin
        try List.assoc name env
        with Not_found ->
          try Hashtbl.find extenv name
          with Not_found -> raise (Error (
              "variable '" ^ name ^"' does not seem to be known. Either you forgot\n"^
              "to declare it globally or it has been previously declared locally\n"^
              "(with bigand, bigor or let) and you are out of its scope."))
      end
  | Set x -> Set x
  | Set_decl x -> eval_set x env
  | Neg x ->
      begin
        match eval_exp x env with
        | Int x'   -> Int   (- x')
        | Float x' -> Float (-. x')
        | x' -> raise (Error (
            "In the following statement, the operator '-' should only\n"^
            "be used on a number:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "The following statement is not a number:\n"^
            "    "^(string_of_exp x')))
      end
  | Add (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (+) (+.) "+"
  | Sub (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (-) (-.) "-"
  | Mul (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) ( * ) ( *. ) "*"
  | Div (x,y) -> num_bin_op (eval_exp x env) (eval_exp y env) (/) (/.) "/"
  | Mod (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y' -> Int (x' mod y')
        | x',y' -> raise (Error (
            "the operator 'mod' expects int as operands. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Mod (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Sqrt x ->
      begin
        match eval_exp x env with
        | Float x' -> Float (sqrt x')
        | x' -> raise (Error (
            "the operator 'sqrt(_)' expects float as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Sqrt (x')))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
      end
  | To_int x ->
      begin
        match eval_exp x env with
        | Float x' -> Int (int_of_float x')
        | Int x'   -> Int x'
        | x' -> raise (Error (
            "the operator 'int(_)' expects float or int as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (To_int (x')))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
      end
  | To_float x ->
      begin
        match eval_exp x env with
        | Int x'   -> Float (float_of_int x')
        | Float x' -> Float x'
        | x' -> raise (Error (
            "the operator 'float(_)' expects float or int as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Sqrt (x')))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
      end
  | Not x ->
      begin
        match eval_exp x env with
        | Bool x' -> Bool (not x')
        | x' -> raise (Error (
            "the operator 'float(_)' expects a boolean as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Not (x')))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
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
        | x' -> raise (Error (
            "the 'if' statement expects a boolean in its condition. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (If (x',y,z)))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
      in
      if test then eval_exp y env else eval_exp z env
  | Union (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.union)
                            (FloatSet.union)
                            (StringSet.union) "union" x' y')
        | x',y' -> raise (Error (
            "the operator 'union' expects sets of same types as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Mod (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Inter (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.inter)
                            (FloatSet.inter)
                            (StringSet.inter) "inter" x' y')
        | x',y' -> raise (Error (
            "In the following statement, the operator 'inter' should only\n"^
            "be used on sets:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "One of the two following statement is not a set:\n"^
            "    "^(string_of_exp x')^"\n"^
            "    "^(string_of_exp y')))
      end
  | Diff (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Set (set_bin_op (IntSet.diff)
                            (FloatSet.diff)
                            (StringSet.diff) "diff" x' y')
        | x',y' -> raise (Error (
            "the operator 'diff' expects sets of same types as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Diff (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Range (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y'     -> Set (GenSet.ISet (IntSet.of_list (irange x' y' 1)))
        | Float x', Float y' -> Set (GenSet.FSet (FloatSet.of_list (frange x' y' 1.)))
        | x',y' -> raise (Error (
            "the operator '..' expects int as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Range (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
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
        | x' -> raise (Error (
            "the operator 'empty(_)' expects a set as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Empty x'))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
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
        | x' -> raise (Error (
            "the operator 'card(_)' expects a set as operand. In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Card (x')))^"\n"^
            "the operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"))
      end
  | Subset (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Set x', Set y' ->
            Bool (set_pred_op (IntSet.subset)
                              (FloatSet.subset)
                              (StringSet.subset) "subset" x' y')
        | x',y' -> raise (Error (
            "the operator 'subset(_,_)' expects sets of same types as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Subset (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | In (x,y) ->
    begin
      let x,y = eval_exp x env, eval_exp y env in
      match x,y with
      | _, Set (GenSet.Empty) -> Bool false (* nothing can be in an empty set!*)
      | Int x', Set (GenSet.ISet y') -> Bool (IntSet.mem x' y')
      | Float x', Set (GenSet.FSet y') -> Bool (FloatSet.mem x' y')
      | Term (x',None), Set (GenSet.SSet y') -> Bool (StringSet.mem x' y')
      | x',y' -> raise (Error (
          "the operator 'in' expects a scalar and a set of same types as operands.\n"^
          "In the statement:\n"^
          "    "^(string_of_exp exp)^"\n"^
          "which has been expanded to:\n"^
          "    "^(string_of_exp (In (x',y')))^"\n"^
          "left operand, expected to be a scalar (number or term), has type '"^(string_of_exp_type x')^"':\n"^
          "    "^(string_of_exp x')^"\n"^
          "and right-operand, expected to be a set, has type '"^(string_of_exp_type x')^"':\n"^
          "    "^(string_of_exp y')))
    end
  | Equal (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x', Int y' -> Bool (x' = y')
        | Float x', Float y' -> Bool (x' = y')
        | Term (x',None), Term (y',None) -> Bool (x' = y')
        | Set x', Set y' ->
            Bool (set_pred_op (IntSet.equal)
                              (FloatSet.equal)
                              (StringSet.equal) "=" x' y')
        | x',y' -> raise (Error (
            "the operator '==' expects scalars or sets of same types as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Equal (x',y')))^"\n"^
            "left operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Not_equal        (x,y) -> eval_exp (Not (Equal (x,y))) env
  | Lesser_than      (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (<) (<) "<"
  | Lesser_or_equal  (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (<=) (<=) "<="
  | Greater_than     (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (>) (>) ">"
  | Greater_or_equal (x,y) -> num_pred_op (eval_exp x env) (eval_exp y env) (>=) (>=) ">="
  | Term (prefix,indices) -> Term (prefix,indices)
  | e -> raise (Error ("this expression cannot be expanded: " ^ string_of_exp e))

and eval_set set_decl env =
  let set_as_list = List.map (fun x -> eval_exp x env) set_decl in
  (* [unwrap] needs a function exp -> 'a to transform, for example, 
     'Int 1' to '1'; expected is a dummy variable to tell which type the set is;
     given is the actual element to unwrap.
  *)
  let check_types (expected:exp) (given:exp) : exp = match expected,given with
    (*expected, given*)
    | Int _, Int x -> Int x
    | Float _, Float x -> Float x
    | Term (_,None), Term (x,None) -> Term (x,None)
    | expected, given -> raise (Error (
        "elements do not have the same type in set declaration:\n"^
        "    ["^(string_of_exp_list ", " set_decl)^"]\n"^
        "which has been expanded to:\n"^
        "    ["^(string_of_exp_list ", " set_as_list)^"]\n"^
        "the set elements have type '"^(string_of_exp_type expected)^"',\n"^
        "but the following element has type '"^(string_of_exp_type given)^"':\n"^
        "    "^(string_of_exp given)))
  in 
  let unwrap_int exp = match check_types (Int 0) exp with 
    | Int x -> x | _ -> failwith "[check_types] already cheched"
  and unwrap_float exp = match check_types (Float 0.0) exp with  
    | Float x -> x | _ -> failwith "[check_types] already cheched"
  and unwrap_str exp = match check_types (Term ("",None)) exp with  
    | Term (x,None) -> x | _ -> failwith "[check_types] already cheched"
  in match set_as_list with
  | [] -> Set (GenSet.Empty) (*   (fun -> function Int x->x   *)
  | (Int _)::xs -> Set (GenSet.ISet (IntSet.of_list (List.map unwrap_int set_as_list)))
  | (Float _)::xs -> Set (GenSet.FSet (FloatSet.of_list (List.map unwrap_float set_as_list)))
  | (Term (_,None))::xs -> Set (GenSet.SSet (StringSet.of_list (List.map unwrap_str set_as_list)))
  | x::xs -> raise (Error (
      "sets declarations can only contain scalars (float, int or term). In the statement:\n"^
      "    ["^(string_of_exp_list ", " set_decl)^"]\n"^
      "which has been expanded to:\n"^
      "    ["^(string_of_exp_list ", " set_as_list)^"]\n"^
      "the following element has type '"^(string_of_exp_type x)^"':\n"^
      "    "^(string_of_exp x)))
and eval_exp_no_expansion exp env =
  match exp with
  | Int x   -> Int x
  | Float x -> Float x
  | Neg x ->
      begin
        match eval_exp_no_expansion x env with
        | Int   x' -> Int   (- x')
        | Float x' -> Float (-. x')
        | x' -> Neg x'
        (*| _ -> raise (Error (string_of_exp exp))*)
      end
  | Add (x,y) ->
      begin
        match eval_exp_no_expansion x env, eval_exp_no_expansion y env with
        | Int x', Int y'     -> Int   (x' +  y')
        | Float x', Float y' -> Float (x' +. y')
        | Int _, Term _
        | Term _, Int _ -> Add (x,y)
        | x', y' -> Add (x', y')
        (*| _,_ -> raise (Error (string_of_exp exp))*)
      end
  | Sub (x,y) ->
      begin
        match eval_exp_no_expansion x env, eval_exp_no_expansion y env with
        | Int x', Int y'     -> Int   (x' -  y')
        | Float x', Float y' -> Float (x' -. y')
        (*| Term x', Term y' -> Sub (Term x', Term y')*)
        | x', y' -> Sub (x', y')
        (*| _,_ -> raise (Error (string_of_exp exp))*)
      end
  | Mul (x,y) ->
      begin
        match eval_exp_no_expansion x env, eval_exp_no_expansion y env with
        | Int x', Int y'     -> Int   (x' *  y')
        | Float x', Float y' -> Float (x' *. y')
        | x', y' -> Mul (x', y')
        (*| _,_ -> raise (Error (string_of_exp exp))*)
      end
  | Div (x,y) ->
      begin
        match eval_exp_no_expansion x env, eval_exp_no_expansion y env with
        | Int x', Int y'     -> Int   (x' /  y')
        | Float x', Float y' -> Float (x' /. y')
        | x', y' -> Div (x', y')
        (*| _,_ -> raise (Error (string_of_exp exp))*)
      end
  | Equal            (x,y) -> Equal            (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Not_equal        (x,y) -> Not_equal        (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Lesser_than      (x,y) -> Lesser_than      (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Lesser_or_equal  (x,y) -> Lesser_or_equal  (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Greater_than     (x,y) -> Greater_than     (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Greater_or_equal (x,y) -> Greater_or_equal (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Top    -> Top
  | Bottom -> Bottom
  | Term x -> Term ((expand_var_name x env), None)
  | Var x ->
    (* name = prefix + indices. 
       Example with $v(a,b,c):
       name is '$v(a,b,c)', prefix is '$v' and indices are '(a,b,c)' *)
    let name = expand_var_name x env in
    begin
      (* Case 1. Check if this variable name has been affected locally 
         (recursive-wise) in bigand, bigor or let. *)
      try let content = List.assoc name env in
        match content with
        | Int x' -> Int x'
        | Float x' -> Float x'
        | Term x' -> Term x'
        | _ -> raise (Error (
            "'" ^ name ^ "' has been declared locally (in bigand, bigor or let)\n" ^
            "Locally declared variables must be scalar (float, int or term).\n" ^
            "Instead, the content of the variable has type '"^(string_of_exp_type content)^"':\n"^
            "    "^(string_of_exp content)))
      with Not_found ->  
      (* Case 2. Check if this variable name has been affected globally, i.e.,
         in the 'data' section *)
      try let content = Hashtbl.find extenv name in
        match content with
        | Int x' -> Int x'
        | Float x' -> Float x'
        | Term x' -> Term x'
        | _ -> raise (Error (
            "the global variable '" ^ name ^ "' should be a scalar (number or term).\n" ^
            "Instead, the content of the variable has type '"^(string_of_exp_type content)^"':\n"^
            "    "^(string_of_exp content)))
      with Not_found ->
      try
        match x with
        (* Case 3. The variable is a non-tuple of the form '$v' => name=prefix only.
           As it has not been found in the Case 1 or 2, this means that this variable
           has not been declared. *)
        | prefix, None -> raise Not_found (* trick to go to the Case 5. error *)
        (* Case 4. The var is a tuple-variable of the form '$v(1,2,3)' and has not
           been declared.
           But maybe we are in the following special case where the parenthesis
           in $v(a,b,c) that should let think it is a tuple-variable is actually
           a 'reconstructed' term, e.g. the content of $v should be expanded.
           Example of use:
            $F = [a,b,c]
            bigand $i in [1..3]:
              bigand $f in $F:     <- $f is defined as non-tuple variable (= no indices)
                $f($i)             <- here, $f looks like a tuple-variable but NO!
              end                     It is simply used to form the proposition
            end                       a(1), a(2)..., b(1)...    *)
        | prefix, Some indices ->
          let term = match List.assoc prefix env with
            | Int x'' -> Int x''
            | Float x'' -> Float x''
            | Term x'' -> Term x''
            | x'' -> raise (Error (
                "'" ^ name ^ "' has not been declared; maybe you wanted '"^prefix^"' to expand\n" ^
                "in order to produce an expanded version <"^prefix^"-content>("^(string_of_exp_list "," indices)^")." ^
                "But the content of the variable '"^prefix^"' has type '"^(string_of_exp_type x'')^"':\n"^
                "    "^(string_of_exp x'')^"\n'"^
                "which is not a term or a number, so it cannot be expanded as explained above."))
          in eval_exp_no_expansion (Term ((string_of_exp term), Some indices)) env
      (* Case 5. the variable was of the form '$v(1,2,3)' and was not declared
         and '$v' is not either declared, so we can safely guess that this var has not been declared. *)
      with Not_found -> raise (Error ("'" ^ name ^ "' has not been declared"))
    end
  | Not Top    -> Bottom
  | Not Bottom -> Top
  | Not x      -> Not (eval_exp_no_expansion x env)
  | And (Bottom, _) | And (_, Bottom) -> Bottom
  | And (Top,x)
  | And (x,Top) -> eval_exp_no_expansion x env
  | And     (x,y) -> And (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Or (Top, _) | Or (_, Top) -> Top
  | Or (Bottom,x)
  | Or (x,Bottom) -> eval_exp_no_expansion x env
  | Or      (x,y) -> Or  (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Xor     (x,y) -> Xor (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Implies (_,Top)
  | Implies (Bottom,_) -> Top
  | Implies (x,Bottom) -> eval_exp_no_expansion (Not x) env
  | Implies (Top,x) -> eval_exp_no_expansion x env
  | Implies (x,y) -> Implies (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Equiv   (x,y) -> Equiv (eval_exp_no_expansion x env, eval_exp_no_expansion y env)
  | Exact (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x, Set (GenSet.SSet s) ->
            exact_str (StringSet.exact x s)
        | x',y' -> raise (Error (
            "the operator 'exact(_,_)' expects an int and a term-set as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Exact (x',y')))^"\n"^
            "left operand that should be an 'int' has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand that should be a set has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Atleast (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x, Set (GenSet.SSet s) -> atleast_str (StringSet.atleast x s)
        | x',y' -> raise (Error (
            "the operator 'atleast(_,_)' expects an int and a term-set as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Atleast (x',y')))^"\n"^
            "left operand that should be an 'int' has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand that should be a set has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  | Atmost (x,y) ->
      begin
        match eval_exp x env, eval_exp y env with
        | Int x,Set (GenSet.SSet s) -> atmost_str (StringSet.atmost x s)
        | x',y' -> raise (Error (
            "the operator 'atmost(_,_)' expects an int and a term-set as operands.\n"^
            "In the statement:\n"^
            "    "^(string_of_exp exp)^"\n"^
            "which has been expanded to:\n"^
            "    "^(string_of_exp (Atmost (x',y')))^"\n"^
            "left operand that should be an 'int' has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp x')^"\n"^
            "and right-operand that should be a set has type '"^(string_of_exp_type x')^"':\n"^
            "    "^(string_of_exp y')))
      end
  (* What is returned by bigand or bigor when they do not
     generate anything? A direct solution would have been to
     return the 'neutral' element of the containing type, e.g.,
         ... and (bigand $i in []: p($i) end)
     would have to transform into
         ... and Top
     And we would have to know in what is the 'bigor/bigand'.
     Maybe we could bypass this problem: return Nothing when
     the bigand is empty; during the evaluation, Nothing will
     act like '... and Top' or '... or Bot'. *)
  | Bigand (v,s,t,e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> raise (Error (
            "In the 'bigand' statement\n"^
            "    "^(string_of_exp exp)^"\n"^
            "the number of variables and the number of sets are not the same."))
        | [Var x],[y] ->
            begin
              match eval_exp y env with
              | Set (GenSet.Empty)  -> bigand_empty env x [] test e
              | Set (GenSet.ISet a) -> bigand_int   env x (IntSet.elements a)    test e
              | Set (GenSet.FSet a) -> bigand_float env x (FloatSet.elements a)  test e
              | Set (GenSet.SSet a) -> bigand_str   env x (StringSet.elements a) test e
              | y' -> raise (Error (
                  "the 'bigand _ in _ :' statement expects a comma-separated list of\n"^
                  "sets as second operand. In the statement:\n"^
                  "    "^(string_of_exp exp)^"\n"^
                  "which has been expanded to:\n"^
                  "    "^(string_of_exp (Bigand (v,s,t,e)))^"\n"^
                  "the following statement, expected to be a set, has type '"^(string_of_exp_type y')^"':\n"^
                  "    "^(string_of_exp y')^"\n"))
            end
        | x::xs,y::ys ->
            eval_exp_no_expansion (Bigand ([x],[y],None,(Bigand (xs,ys,t,e)))) env
      end
  | Bigor (v,s,t,e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> raise (Error (
            "In the 'bigor' statement\n"^
            "    "^(string_of_exp exp)^"\n"^
            "the number of variables and the number of sets are not the same."))
        | [Var x],[y] ->
            begin
              match eval_exp y env with
              | Set (GenSet.Empty)  -> bigor_empty env x [] test e
              | Set (GenSet.ISet a) -> bigor_int   env x (IntSet.elements a)    test e
              | Set (GenSet.FSet a) -> bigor_float env x (FloatSet.elements a)  test e
              | Set (GenSet.SSet a) -> bigor_str   env x (StringSet.elements a) test e
              | y' -> raise (Error (
                  "the 'bigor _ in _ :' statement expects a comma-separated list of\n"^
                  "sets as second operand. In the statement:\n"^
                  "    "^(string_of_exp exp)^"\n"^
                  "which has been expanded to:\n"^
                  "    "^(string_of_exp (Bigand (v,s,t,e)))^"\n"^
                  "the following statement, expected to be a set, has type '"^(string_of_exp_type y')^"':\n"^
                  "    "^(string_of_exp y')^"\n"))
            end
        | x::xs,y::ys ->
            eval_exp_no_expansion (Bigor ([x],[y],None,(Bigor (xs,ys,t,e)))) env
      end
  | If (x,y,z) ->
      let test = eval_test x env in
      if test then eval_exp_no_expansion y env else eval_exp_no_expansion z env
  | Let (Var v,x,c) -> eval_exp_no_expansion c (((expand_var_name v env),x)::env)
  | e -> raise (Error ("this expression is not a formula: " ^ string_of_exp e))


and exact_str lst =
  let rec go = function
    | [],[]       -> Top
    | t::ts,[]    -> And (And (Term (t,None), Top), go (ts,[]))
    | [],f::fs    -> And (And (Top, Not (Term (f,None))), go ([],fs))
    | t::ts,f::fs -> And (And (Term (t,None), Not (Term (f,None))), go (ts,fs))
  in
  match lst with
  | []    -> Bottom
  | x::xs -> Or (go x, exact_str xs)

and atleast_str lst =
  List.fold_left (fun acc str -> Or (acc, formula_of_string_list str)) Bottom lst

and atmost_str lst =
  List.fold_left (fun acc str ->
    Or (acc, List.fold_left (fun acc' str' ->
      And (acc', Not (Term (str',None)))) Top str)) Bottom lst

and formula_of_string_list =
  List.fold_left (fun acc str -> And (acc, Term (str,None))) Top

and and_of_term_list =
  List.fold_left (fun acc t -> And (acc, t)) Top

and bigand_empty env var values test exp = Top (* XXX what if bigand in a or?*)
and bigand_int env var values test exp =
  let exp' = If (test,exp,Top) and (name,_) = var in
  match values with
  | []    -> Top
  | [x]   -> eval_exp_no_expansion exp' ((name, Int x)::env)
  | x::xs -> And (eval_exp_no_expansion exp' ((name, Int x)::env) ,bigand_int env var xs test exp)
and bigand_float env var values test exp =
  let exp' = If (test,exp,Top) and (name,_) = var in
  match values with
  | []    -> Top
  | [x]   -> eval_exp_no_expansion exp' ((name, Float x)::env)
  | x::xs -> And (eval_exp_no_expansion exp' ((name, Float x)::env) ,bigand_float env var xs test exp)
and bigand_str env var values test exp =
  let exp' = If (test,exp,Top) and (name,_) = var in
  match values with
  | []    -> Top
  | [x]   -> eval_exp_no_expansion exp' ((name, Term  (x,None))::env)
  | x::xs ->
      And (eval_exp_no_expansion exp' ((name, Term  (x,None))::env), bigand_str env var xs test exp)
and bigor_empty env var values test exp = Bottom
and bigor_int env var values test exp =
  let exp' = If (test,exp,Bottom) and (name,_) = var in
  match values with
  | []    -> Bottom
  | [x]   -> eval_exp_no_expansion exp' ((name, Int x)::env)
  | x::xs -> Or (eval_exp_no_expansion exp' ((name, Int x)::env), bigor_int env var xs test exp)
and bigor_float env (var:var) values test exp =
  let exp' = If (test,exp,Bottom) and (name,_) = var in
  match values with
  | []    -> Bottom
  | [x]   -> eval_exp_no_expansion exp' ((name, Float x)::env)
  | x::xs -> Or (eval_exp_no_expansion exp' ((name, Float x)::env), bigor_float env var xs test exp)
and bigor_str env var values test exp =
  let exp' = If (test,exp,Bottom) and (name,_) = var in
  match values with
  | []    -> Bottom
  | [x]   -> eval_exp_no_expansion exp' ((name, Term  (x,None))::env)
  | x::xs ->
      Or (eval_exp_no_expansion exp' ((name, Term (x,None))::env), bigor_str env var xs test exp)

and eval_test exp env =
  match eval_exp exp env with
  | Bool x -> x
  | _ -> raise (Error (
      "the following expression is expected to be a boolean:\n"^
      "    "^(string_of_exp exp)))

and expand_var_name (var:var) env =
  match var with
  | (x,None)   -> x
  | (x,Some y) ->
       x ^ "("
         ^ (string_of_exp_list ", " (List.map (fun e -> eval_exp e env) y))
         ^ ")"
