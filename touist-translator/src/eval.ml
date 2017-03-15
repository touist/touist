(** Functions for semantic analysis of the abstract syntaxic tree produced by [Parse.parse].
    
    [eval] is the main function. *)

(* Project TouIST, 2015. Easily formalize and solve real-world sized problems
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
open Msgs


(* Variables are stored in two data structures (global and local scopes). *)

(* [env] is for local variables (for bigand,bigor and let constructs).
   It is a simple list [(name,description),...] passed as recursive argument.
   The name is the variable name (e.g., '$var' or '$var(a,1,c)').
   The description is a couple (content, location) *)
type env = (string * (ast * loc)) list

(* [extenv] is for global variables (defined after 'data'). It is a hashtable
   accessible from anywhere where the elements are (name, description):
   The name is the variable name (e.g., '$var' or '$var(a,1,c)').
   The description is a couple (content, location) *)
type extenv = (string, (ast * loc)) Hashtbl.t

let get_loc (ast:ast) : loc = match ast with 
    | Loc (_,loc) -> loc 
    | _ -> (Lexing.dummy_pos,Lexing.dummy_pos)

let warning msgs (ast:ast) (message:string) =
  add_msg msgs (Warning,Eval,message,get_loc ast)

(* [ast_whithout_loc] removes the location attached by the parser to the ast
   node. This location 'Loc (ast,loc)' allows to give the location in error 
   messages. [ast_whithout_loc] must be called before any 
        match ast with | Inter (x,y) -> ... *)
let ast_whithout_loc (ast:ast) : ast = match ast with
  | Loc (ast,_) -> ast
  | ast -> ast

(* [raise_with_loc] takes an ast that may contains a Loc (Loc is added in
   parser.mly) and raise an exception with the given message.
   The only purpose of giving 'ast' is to get the Loc thing.
   [ast_whithout_loc] should not have been previously applied to [ast]
   because ast_whithout_loc will remove the Loc thing. *)
let raise_with_loc msgs (ast:ast) (message:string) = match ast with
  | Loc (ast,loc) -> add_fatal msgs (Error,Eval,message,loc)
  | _ -> add_fatal msgs (Error,Eval,message,(Lexing.dummy_pos,Lexing.dummy_pos))

(* [raise_type_error] raises the errors that come from one-parameter functions.
   operator is the non-expanded (expand = eval_ast) operator.
   Example: in 'To_int x', 'operand' is the non-expanded parameter 'x',
   'expanded' is the expanded parameter 'x'.
   Expanded means that eval_ast has been applied to x.
   [expected_types] contain a string that explain what is expected, e.g.,
   'an integer or a float'. *)
let raise_type_error msgs operator operand expanded (expected_types:string) = 
  raise_with_loc msgs operator (
    "'"^(string_of_ast_type operator)^"' expects "^expected_types^".\n"^
    "The operand:\n"^
    "    "^(string_of_ast operand)^"\n"^
    "has been expanded to something of type '"^(string_of_ast_type expanded)^"':\n"^
    "    "^(string_of_ast expanded)^"")

(* Same as above but for functions of two parameters. Example: with And (x,y),
   operator is And (x,y),
   op1 and op2 are the non-expanded parameters x and y,
   exp1 and exp2 are the expanded parameters x and y. *)
let raise_type_error2 msgs operator op1 exp1 op2 exp2 (expected_types:string) =
  raise_with_loc msgs operator
    ("incorrect types with '"^(string_of_ast_type operator)^"'; expects "^expected_types^".\n"^
    "In statement:\n"^
    "    "^(string_of_ast operator)^"\n"^
    "Left-hand operand has type '"^(string_of_ast_type exp1)^"':\n"^
    "    "^(string_of_ast exp1)^"\n"^
    "Right-hand operand has type '"^(string_of_ast_type exp2)^"':\n"^
    "    "^(string_of_ast exp2)^""^
    "")

(* [raise_set_decl] is the same as [raise_type_error2] but between one element
   and the set this element is supposed to be added to. *)
let raise_set_decl msgs ast elmt elmt_expanded set set_expanded (expected_types:string) =
  raise_with_loc msgs ast
    ("Ill-formed set declaration. It expects "^expected_types^".\n"^
    "One of the elements is of type '"^(string_of_ast_type elmt_expanded)^"':\n"^
    "    "^(string_of_ast elmt)^"\n"^
    "This element has been expanded to\n"^
    "    "^(string_of_ast elmt_expanded)^"\n"^
    "Up to now, the set declaration\n"^
    "    "^(string_of_ast set)^"\n"^
    "has been expanded to:\n"^
    "    "^(string_of_ast set_expanded)^"")


let check_nb_vars_same_as_nb_sets msgs (ast:ast) (vars: ast list) (sets: ast list) : unit =
  let loc = match (List.nth vars 0), List.nth sets ((List.length sets)-1) with
    | Loc (_,(startpos,_)), Loc (_,(_,endpos)) -> startpos,endpos 
    | _-> failwith "[shouldn't happen] missing locations in vars/sets"
  in
  match (List.length vars) == (List.length sets) with
  | true -> ()
  | false -> add_fatal msgs (Error,Eval,
    "Ill-formed '"^(string_of_ast_type ast)^"'. The number of variables and sets must be the same.\n"^
    "You defined "^(string_of_int (List.length vars))^" variables:\n"^
    "    "^(string_of_ast_list "," vars)^"\n"^
    "but you gave "^(string_of_int (List.length sets))^" sets:\n"^
    "    "^(string_of_ast_list "," sets)^""
    ,loc)


(* [process_empty] is necessary because of how 'clunky' have been implemented
   the set capabilities (type 'set', EmptySet, ISet, IntSet.empty.....).
   If 'set' is EmptySet, transform it into a typed IntSet.empty,
   FloatSet.empty or PropSet.empty, depending on the type of 'set_type'.
   If 'set' isn't an empty set, then return 'set'.*)
let process_empty (set:ast) (set_type:ast) : ast = match set,set_type with
  | Set EmptySet, Set (ISet _) -> Set (ISet IntSet.empty)
  | Set EmptySet, Set (FSet _) -> Set (FSet FloatSet.empty)
  | Set EmptySet, Set (SSet _) -> Set (SSet PropSet.empty)
  | Set EmptySet, Set (EmptySet)  -> Set (ISet IntSet.empty) (* arbitrary *)
  | _,_ -> set

let extenv = ref (Hashtbl.create 0)
let check_only = ref false

(* [check_only] allows to only 'check the types'. It prevents the bigand,
    bigor, exact, atmost, atleast and range to expand completely(as it
    may take a lot of time to do so). *)
let check_only = ref false

(* By default, we are in 'SAT' mode. When [smt] is true,
   some type checking (variable expansion mostly) is different
   (formulas can be 'int' or 'float' for example). *)
let smt = ref false

(** Main function for checking the types and evaluating the touistl expressions
    (variables, bigand, bigor, let...).

    @param ast is the AST given by [Parse.parse] 
    @param onlychecktypes will limit the evaluation to its minimum in
           order to get type errors as fast as possible.
    @param smt enables the SMT mode. By default, the SAT mode is used.

    @raise Eval.Error (msg,loc) *)
let rec eval ?smt:(smt_mode=false) ?(onlychecktypes=false) (ast,msgs) : ast * Msgs.t ref =
  check_only := onlychecktypes;
  smt := smt_mode;
  extenv := Hashtbl.create 50; (* extenv must be re-init between two calls to [eval] *)
  eval_touist_code msgs [] ast, msgs

and eval_touist_code msgs (env:env) ast : ast =
  let rec affect_vars = function
    | [] -> []
    | Loc (Affect (Loc (Var (p,i),var_loc),y),affect_loc)::xs ->
      Hashtbl.replace !extenv (expand_var_name msgs env (p,i)) (eval_ast msgs env y, var_loc);
        affect_vars xs
    | x::xs -> x::(affect_vars xs)
  in
  let rec process_formulas = function
    | []    -> raise_with_loc msgs ast ("no formulas")
    | x::[] -> x
    | x::xs -> And (x, process_formulas xs)
  in
  match ast_whithout_loc ast with
  | Touist_code (formulas) -> 
    (match cleantoremove (eval_ast_formula msgs env (process_formulas (affect_vars formulas))) with
    | ToRemove -> raise_with_loc msgs ast ("your formulas have expanded to nothing")
    | evaluated -> evaluated)
  | e -> raise_with_loc msgs ast ("this does not seem to be a touist code structure: " ^ string_of_ast e)

(* [eval_ast] evaluates (= expands) numerical, boolean and set expresions that
   are not directly in formulas. For example, in 'when $a!=a' or 'if 3>4',
   the boolean values must be computed: eval_ast will do exactly that.*)
and eval_ast (msgs:Msgs.t ref) (env:env) (ast:ast) : ast =
  let eval_ast = eval_ast msgs env in
  match ast_whithout_loc ast with
  | Int x   -> Int x
  | Float x -> Float x
  | Bool x  -> Bool x
  | Var (p,i) -> (* p,i = prefix, indices *)
    let name = expand_var_name msgs env (p,i) in
    begin
      try let (content,loc) = List.assoc name env in content
      with Not_found ->
      try let (content,_) = Hashtbl.find !extenv name in content
      with Not_found -> raise_with_loc msgs ast
          ("variable '" ^ name ^"' does not seem to be known. Either you forgot\n"^
          "to declare it globally or it has been previously declared locally\n"^
          "(with bigand, bigor or let) and you are out of its scope.")
    end
  | Set x -> Set x
  | Set_decl x -> eval_set_decl msgs env ast
  | Neg x -> (match eval_ast x with
      | Int x'   -> Int   (- x')
      | Float x' -> Float (-. x')
      | x' -> raise_type_error msgs ast x x' "'float' or 'int'")
  | Add (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Int (x + y)
      | Float x, Float y -> Float (x +. y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "'float' or 'int'")
  | Sub (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Int (x - y)
      | Float x, Float y -> Float (x -. y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "'float' or 'int'")
  | Mul (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Int (x * y)
      | Float x, Float y -> Float (x *. y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Div (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Int (x / y)
      | Float x, Float y -> Float (x /. y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Mod (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Int (x mod y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Sqrt x -> (match eval_ast x with
      | Float x -> Float (sqrt x)
      | x' -> raise_type_error msgs ast x x' "a float")
  | To_int x -> (match eval_ast x with
      | Float x -> Int (int_of_float x)
      | Int x   -> Int x
      | x' -> raise_type_error msgs ast x x' "a 'float' or 'int'")
  | To_float x -> (match eval_ast x with
      | Int x   -> Float (float_of_int x)
      | Float x -> Float x
      | x' -> raise_type_error msgs ast x x' "a 'float' or 'int'")
  | Abs x -> (match eval_ast x with
      | Int x   -> Int (abs x)
      | Float x -> Float (abs_float x)
      | x' -> raise_type_error msgs ast x x' "a 'float' or 'int'")
  | Not x -> (match eval_ast x with
      | Bool x -> Bool (not x)
      | x' -> raise_type_error msgs ast x x' "a 'bool'")
  | And (x,y) -> (match eval_ast x, eval_ast y with
      | Bool x,Bool y -> Bool (x && y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'bool'")
  | Or (x,y) -> (match eval_ast x, eval_ast y with
      | Bool x,Bool y -> Bool (x || y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'bool'")
  | Xor (x,y) -> (match eval_ast x, eval_ast y with
      | Bool x,Bool y -> Bool ((x || y) && (not (x && y)))
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'bool'")
  | Implies (x,y) -> (match eval_ast x, eval_ast y with
      | Bool x,Bool y -> Bool (not x || y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'bool'")
  | Equiv (x,y) -> (match eval_ast x, eval_ast y with
      | Bool x,Bool y -> Bool ((not x || y) && (not x || y))
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'bool'")
  | If (x,y,z) ->
    let test =
      match eval_ast x with
      | Bool true  -> true
      | Bool false -> false
      | x' -> raise_type_error msgs ast x x' "a 'bool'"
    in
    if test then eval_ast y else eval_ast z
  | Union (x,y) -> begin
      let x',y' = eval_ast x, eval_ast y in
      match process_empty x' y', process_empty y' x' with
      | Set (ISet a), Set (ISet b) -> Set (ISet (IntSet.union a b))
      | Set (FSet a), Set (FSet b) -> Set (FSet (FloatSet.union a b))
      | Set (SSet a), Set (SSet b) -> Set (SSet (PropSet.union a b))
      | _,_ -> raise_type_error2 msgs ast x x' y y' "a 'float-set', 'int-set' or 'prop-set'"
    end
  | Inter (x,y) -> begin
      let x',y' = eval_ast x, eval_ast y in
      match process_empty x' y', process_empty y' x' with
      | Set (ISet a), Set (ISet b) -> Set (ISet (IntSet.inter a b))
      | Set (FSet a), Set (FSet b) -> Set (FSet (FloatSet.inter a b))
      | Set (SSet a), Set (SSet b) -> Set (SSet (PropSet.inter a b))
      | _,_ -> raise_type_error2 msgs ast x x' y y' "a 'float-set', 'int-set' or 'prop-set'"
    end
  | Diff (x,y) -> begin
      let x',y' = eval_ast x, eval_ast y in
      match process_empty x' y', process_empty y' x' with
      | Set (ISet a), Set (ISet b) -> Set (ISet (IntSet.diff a b))
      | Set (FSet a), Set (FSet b) -> Set (FSet (FloatSet.diff a b))
      | Set (SSet a), Set (SSet b) -> Set (SSet (PropSet.diff a b))
      | _,_ -> raise_type_error2 msgs ast x x' y y' "a 'float-set', 'int-set' or 'prop-set'"
    end
  | Range (x,y) -> (* !check_only will simplify [min..max] to [min..min] *)
    (* [irange] generates a list of int between min and max with an increment of step. *)
    let irange min max step =
      let rec loop acc = function i when i=max+1 -> acc | i -> loop (i::acc) (i+step)
      in loop [] min |> List.rev
    and frange min max step =
      let rec loop acc = function i when i=max+.1. -> acc | i -> loop (i::acc) (i+.step)
      in loop [] min |> List.rev
    in begin
      match eval_ast x, eval_ast y with
      | Int x, Int y     -> Set (ISet (IntSet.of_list (irange x (if !check_only then x else y)  1)))
      | Float x, Float y -> Set (FSet (FloatSet.of_list (frange x (if !check_only then x else y) 1.)))
      | x',y' -> raise_type_error2 msgs ast x x' y y' "two integers or two floats"
    end
  | Empty x -> begin
      match eval_ast x with
      | Set (EmptySet)    -> Bool true
      | Set (ISet x) -> Bool (IntSet.is_empty x)
      | Set (FSet x) -> Bool (FloatSet.is_empty x)
      | Set (SSet x) -> Bool (PropSet.is_empty x)
      | x' -> raise_type_error msgs ast x x' "a 'float-set', 'int-set' or 'prop-set'"
    end
  | Card x -> begin
      match eval_ast x with
      | Set (EmptySet)    -> Int 0
      | Set (ISet x) -> Int (IntSet.cardinal x)
      | Set (FSet x) -> Int (FloatSet.cardinal x)
      | Set (SSet x) -> Int (PropSet.cardinal x)
      | x' -> raise_type_error msgs ast x x' "a 'float-set', 'int-set' or 'prop-set'"
    end
  | Subset (x,y) -> begin
      let x',y' = eval_ast x, eval_ast y in
      match process_empty x' y', process_empty y' x' with
      | Set (ISet a), Set (ISet b) -> Bool (IntSet.subset a b)
      | Set (FSet a), Set (FSet b) -> Bool (FloatSet.subset a b)
      | Set (SSet a), Set (SSet b) -> Bool (PropSet.subset a b)
      | _,_ -> raise_type_error2 msgs ast x x' y y' "a 'float-set', int or prop"
    end
  | In (x,y) ->
    begin match eval_ast x, eval_ast y with
      | _, Set (EmptySet) -> Bool false (* nothing can be in an empty set!*)
      | Int x, Set (ISet y) -> Bool (IntSet.mem x y)
      | Float x', Set (FSet y') -> Bool (FloatSet.mem x' y')
      | Prop x', Set (SSet y') -> Bool (PropSet.mem x' y')
      | x',y' -> raise_type_error2 msgs ast x x' y y' "\nan 'int', 'float' or 'prop' on the left-hand and a 'set' on the right-hand"
    end
  | Equal (x,y) -> begin let x',y' = eval_ast x, eval_ast y in
      match process_empty x' y', process_empty y' x' with
      | Int x, Int y -> Bool (x = y)
      | Float x, Float y -> Bool (x = y)
      | Prop x, Prop y -> Bool (x = y)
      | Set (ISet a), Set (ISet b) -> Bool (IntSet.equal a b)
      | Set (FSet a), Set (FSet b) -> Bool (FloatSet.equal a b)
      | Set (SSet a), Set (SSet b) -> Bool (PropSet.equal a b)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "an int, float, prop or set"
    end
  | Not_equal (x,y) -> eval_ast (Not (Equal (x,y)))
  | Lesser_than (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Bool (x < y)
      | Float x, Float y -> Bool (x < y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Lesser_or_equal (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Bool (x <= y)
      | Float x, Float y -> Bool (x <= y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Greater_than     (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Bool (x > y)
      | Float x, Float y -> Bool (x > y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | Greater_or_equal (x,y) -> (match eval_ast x, eval_ast y with
      | Int x, Int y -> Bool (x >= y)
      | Float x, Float y -> Bool (x >= y)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "a 'float' or 'int'")
  | UnexpProp (p,i) -> expand_prop_with_set msgs env  p i
  | Prop x -> Prop x
  | Loc (x,l) -> eval_ast x
  | Paren x -> eval_ast x
  | e -> raise_with_loc msgs ast ("this expression cannot be expanded: " ^ string_of_ast e)

and eval_set_decl (msgs:Msgs.t ref) (env:env) (set_decl:ast) =
  let sets = (match ast_whithout_loc set_decl with Set_decl sets -> sets | _ -> failwith "shoulnt happen: non-Set_decl in eval_set_decl") in
  let sets_expanded = List.map (fun x -> eval_ast msgs env x) sets in
  let unwrap_int elmt elmt_expanded = match elmt_expanded with
    | Int x -> x
    | _ -> raise_set_decl msgs set_decl elmt elmt_expanded
             (Set_decl sets) (Set_decl sets_expanded)
             "at this point a\ncomma-separated list of integers, because previous elements\nof the list had this type"
  and unwrap_float elmt elmt_expanded = match elmt_expanded with
    | Float x -> x
    | _ -> raise_set_decl msgs set_decl elmt elmt_expanded
             (Set_decl sets) (Set_decl sets_expanded)
             "at this point a\ncomma-separated list of floats, because previous elements\nof the list had this type"
  and unwrap_str elmt elmt_expanded = match elmt_expanded with
    | Prop x -> x
    | _ -> raise_set_decl msgs set_decl elmt elmt_expanded
             (Set_decl sets) (Set_decl sets_expanded)
             "at this point a\ncomma-separated list of propositions, because previous elements\nof the list had this type"

  in (* this match-with uses the first element of the list to set the set type
        (ISet, FSet, SSet...)*)
  match sets, sets_expanded with
  | [],[] -> Set (EmptySet) (*   (fun -> function Int x->x   *)
  | _,(Int _)::_ -> Set (ISet (IntSet.of_list (List.map2 unwrap_int sets sets_expanded)))
  | _,(Float _)::_ -> Set (FSet (FloatSet.of_list (List.map2 unwrap_float sets sets_expanded)))
  | _,(Prop _)::_ -> Set (SSet (PropSet.of_list (List.map2 unwrap_str sets sets_expanded)))
  | x::_,x'::_ -> raise_set_decl msgs set_decl x x'
                    (Set_decl sets) (Set_decl sets_expanded)
                    "elements of type int,\nfloat or propositon"
  | [],x::_ | x::_,[] -> failwith "shouldn't happen: len(sets)!=len(sets_expanded)" 


(* [eval_ast_formula] evaluates formulas; nothing in formulas should be
   expanded, except for variables, bigand, bigor, let, exact, atleast,atmost. *)
and eval_ast_formula (msgs:Msgs.t ref) (env:env) (ast:ast) : ast =
  let eval_ast_formula = eval_ast_formula msgs env
  and eval_ast_formula_env = eval_ast_formula msgs
  and eval_ast = eval_ast msgs env in
  match ast_whithout_loc ast with
  | Int x   -> Int x
  | Float x -> Float x
  | Neg x ->
    begin
      match eval_ast_formula x with
      | Int   x' -> Int   (- x')
      | Float x' -> Float (-. x')
      | x' -> Neg x'
      (*| _ -> raise (Error (string_of_ast ast))*)
    end
  | Add (x,y) ->
    begin
      match eval_ast_formula x, eval_ast_formula y with
      | Int x', Int y'     -> Int   (x' +  y')
      | Float x', Float y' -> Float (x' +. y')
      | Int _, Prop _
      | Prop _, Int _ -> Add (x,y)
      | x', y' -> Add (x', y')
      (*| _,_ -> raise (Error (string_of_ast ast))*)
    end
  | Sub (x,y) ->
    begin
      match eval_ast_formula x, eval_ast_formula y with
      | Int x', Int y'     -> Int   (x' -  y')
      | Float x', Float y' -> Float (x' -. y')
      (*| Prop x', Prop x' -> Sub (Prop x', Prop x')*)
      | x', y' -> Sub (x', y')
      (*| _,_ -> raise (Error (string_of_ast ast))*)
    end
  | Mul (x,y) ->
    begin
      match eval_ast_formula x, eval_ast_formula y with
      | Int x', Int y'     -> Int   (x' *  y')
      | Float x', Float y' -> Float (x' *. y')
      | x', y' -> Mul (x', y')
      (*| _,_ -> raise (Error (string_of_ast ast))*)
    end
  | Div (x,y) ->
    begin
      match eval_ast_formula x, eval_ast_formula y with
      | Int x', Int y'     -> Int   (x' /  y')
      | Float x', Float y' -> Float (x' /. y')
      | x', y' -> Div (x', y')
      (*| _,_ -> raise (Error (string_of_ast ast))*)
    end
  | Equal            (x,y) -> Equal            (eval_ast_formula x, eval_ast_formula y)
  | Not_equal        (x,y) -> Not_equal        (eval_ast_formula x, eval_ast_formula y)
  | Lesser_than      (x,y) -> Lesser_than      (eval_ast_formula x, eval_ast_formula y)
  | Lesser_or_equal  (x,y) -> Lesser_or_equal  (eval_ast_formula x, eval_ast_formula y)
  | Greater_than     (x,y) -> Greater_than     (eval_ast_formula x, eval_ast_formula y)
  | Greater_or_equal (x,y) -> Greater_or_equal (eval_ast_formula x, eval_ast_formula y)
  | Top    -> Top
  | Bottom -> Bottom
  | UnexpProp (p,i) -> Prop (expand_var_name msgs env (p,i))
  | Prop x -> Prop x
  | Var (p,i) -> (* p,i = prefix,indices *)
    (* name = prefix + indices. 
       Example with $v(a,b,c):
       name is '$v(a,b,c)', prefix is '$v' and indices are '(a,b,c)' *)
    let name = expand_var_name msgs env (p,i) in
    begin
      (* Case 1. Check if this variable name has been affected locally
         (recursive-wise) in bigand, bigor or let.
         To be accepted, this variable must contain a proposition. *)
      try let content,loc_affect = List.assoc name env in
        match content with
        | Prop x -> Prop x
        | Int x when !smt -> Int x
        | Float x when !smt -> Float x
        | _ -> raise_with_loc msgs ast
            ("local variable '" ^ name ^ "' (defined in bigand, bigor or let)\n"^
            "cannot be expanded into a 'prop' because its content\n"^
            "is of type '"^(string_of_ast_type content)^"' instead of "^
              (if !smt then "'prop', 'int' or 'float'" else "'prop'") ^ ".\n"^
            "Why? Because this variable is part of a formula, and thus is expected\n"^
            "to be a proposition. Here is the content of '" ^name^"':\n"^
            "    "^(string_of_ast content))
      with Not_found ->
      (* Case 2. Check if this variable name has been affected globally, i.e.,
         in the 'data' section. To be accepted, this variable must contain
         a proposition. *)
      try let content,loc_affect = Hashtbl.find !extenv name in
        match content with
        | Prop x -> Prop x
        | Int x when !smt -> Int x
        | Float x when !smt -> Float x
        | _ -> raise_with_loc msgs ast
            ("global variable '" ^ name ^ "' cannot be expanded into a 'prop'\n"^
            "because its content is of type '"^(string_of_ast_type content)^"' instead of "^
               (if !smt then "'prop', 'int' or 'float'" else "'prop'") ^ ".\n"^
            "Why? Because this variable is part of a formula, and thus is expected\n"^
            "to be a proposition. Here is the content of '" ^name^"':\n"^
            "    "^(string_of_ast content))
      with Not_found ->
      try
        match (p,i) with
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
          let (content,loc_affect) = List.assoc prefix env in
          let term = match content with
            | Prop x -> Prop x
            | wrong -> add_fatal msgs (Error,Eval,
                "the proposition '" ^ name ^ "' cannot be expanded because '"^prefix^"' is of type '"^(string_of_ast_type wrong)^"'.\n" ^
                "In order to produce an expanded proposition of this kind, '"^prefix^"' must be a proposition.\n"^
                "Why? Because this variable is part of a formula, and thus is expected\n"^
                "to be a proposition. Here is the content of '" ^prefix^"':\n"^
                "    "^(string_of_ast content), loc_affect)
          in eval_ast_formula (UnexpProp ((string_of_ast term), Some indices))
      (* Case 5. the variable was of the form '$v(1,2,3)' and was not declared
         and '$v' is not either declared, so we can safely guess that this var has not been declared. *)
      with Not_found -> raise_with_loc msgs ast ("'" ^ name ^ "' has not been declared")
    end
  | Not ToRemove -> ToRemove
  | Not Top    -> Bottom
  | Not Bottom -> Top
  | Not x      -> Not (eval_ast_formula x)
  | And (ToRemove,x) | And (x,ToRemove) -> x
  | And (Bottom, _) | And (_, Bottom) -> Bottom
  | And (Top,x)
  | And (x,Top) -> eval_ast_formula x
  | And     (x,y) -> And (eval_ast_formula x, eval_ast_formula y)
  | Or (ToRemove,x) | Or (x,ToRemove) -> x
  | Or (Top, _) | Or (_, Top) -> Top
  | Or (Bottom,x)
  | Or (x,Bottom) -> eval_ast_formula x
  | Or      (x,y) -> Or  (eval_ast_formula x, eval_ast_formula y)
  | Xor (ToRemove,x) | Xor (x,ToRemove) -> x
  | Xor     (x,y) -> Xor (eval_ast_formula x, eval_ast_formula y)
  | Implies (ToRemove,x) | Implies (x,ToRemove) -> ToRemove
  | Implies (_,Top)
  | Implies (Bottom,_) -> Top
  | Implies (x,Bottom) -> eval_ast_formula (Not x)
  | Implies (Top,x) -> eval_ast_formula x
  | Implies (x,y) -> Implies (eval_ast_formula x, eval_ast_formula y)
  | Equiv (x,ToRemove) | Equiv (ToRemove,x) -> ToRemove 
  | Equiv   (x,y) -> Equiv (eval_ast_formula x, eval_ast_formula y)
  | Exact (x,y) -> begin (* !check_only simplifies by returning a dummy proposition *)
      match eval_ast x, eval_ast y with
      | Int v, _ when v<=0 -> ToRemove 
      | _, Set (EmptySet) -> ToRemove
      | Int x, Set (SSet s) -> if !check_only then Prop "dummy" else exact_str (PropSet.exact x s)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "'int' (left-hand)\nand a 'prop-set' (right-hand)"
    end
  | Atleast (x,y) -> begin
      match eval_ast x, eval_ast y with
      | Int v, _ when v<=0 -> ToRemove 
      | _, Set (EmptySet) -> ToRemove
      | Int x, Set (SSet s) -> if !check_only then Prop "dummy" else atleast_str (PropSet.atleast x s)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "'int' (left-hand)\nand a 'prop-set' (right-hand)"
    end
  | Atmost (x,y) ->begin
      match eval_ast x, eval_ast y with
      | Int v, _ when v<=0 -> ToRemove 
      | _, Set (EmptySet) -> ToRemove
      | Int x, Set (SSet s) -> if !check_only then Prop "dummy" else atmost_str (PropSet.atmost x s)
      | x',y' -> raise_type_error2 msgs ast x x' y y' "'int' (left-hand)\nand a 'prop-set' (right-hand)"
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
  | Bigand (vars,sets,when_optional,body) ->
    let when_cond = match when_optional with Some x -> x | None -> Bool true in
    begin check_nb_vars_same_as_nb_sets msgs ast vars sets;
      match vars,sets with
      | [],[] | _,[] | [],_ -> failwith "shouln't happen: non-variable in big construct"
      | [Loc (Var (name,_),loc)],[set] -> (* we don't need the indices because bigand's vars are 'simple' *)
        (* If [when_cond] has never been satisfied, [process_list_set] will return
             (_,false). In the opposite case, it will return (ast,true). *)
        let rec process_list_set env (set_list:ast list) : ast * bool =
          match set_list with
          | []   -> ToRemove,false (* what if bigand in a or? We give a warning (see below) *)
          | x::xs ->
            let env = (name,(x,loc))::env in
            match ast_to_bool msgs env when_cond with
            | true when xs != [] -> let next,b = process_list_set env xs in And (eval_ast_formula_env env body, next),true
            | true  -> eval_ast_formula_env env body,true
            | false -> process_list_set env xs
        in
        let list_ast_set = set_to_ast_list msgs env set in
        if (List.length list_ast_set) == 0 then ToRemove
        else
          let evaluated_ast,when_satisfied = process_list_set env list_ast_set in
          if not when_satisfied then ToRemove
          else cleantoremove evaluated_ast
      | x::xs,y::ys ->
        (* only the last couple of var-set in the var-set list is going to
           have the [when_optional] evaluated. *)
        eval_ast_formula (Bigand ([x],[y],None,(Bigand (xs,ys,when_optional,body))))
    end
  | Bigor (vars,sets,when_optional,body) ->
    let when_cond = match when_optional with Some x -> x | None -> Bool true
    in
    begin check_nb_vars_same_as_nb_sets msgs ast vars sets;
      match vars,sets with
      | [],[] | _,[] | [],_ -> failwith "shouln't happen: non-variable in big construct"
      | [Loc (Var (name,_),loc)],[set] ->
          (* If [when_cond] has never been satisfied, [process_list_set] will return
             (_,false). In the opposite case, it will return (ast,true). *)
          let rec process_list_set env (set_list:ast list) : ast * bool =
            match set_list with
            | []    -> ToRemove, false
            | x::xs ->
              let env = (name,(x,loc))::env in
              match ast_to_bool msgs env when_cond with
              | true when xs != [] -> let next,b = process_list_set env xs in Or (eval_ast_formula_env env body, next),true
              | true  -> eval_ast_formula_env env body,true
              | false -> process_list_set env xs
          in
          let list_ast_set = set_to_ast_list msgs env set in
          if (List.length list_ast_set) == 0 then ToRemove
          else
            let evaluated_ast,when_satisfied = process_list_set env list_ast_set in
            if not when_satisfied then ToRemove
            else cleantoremove evaluated_ast
      | x::xs,y::ys ->
        eval_ast_formula (Bigor ([x],[y],None,(Bigor (xs,ys,when_optional,body))))
    end
  | If (c,y,z) ->
    let test = match eval_ast c with Bool c -> c | c' -> raise_type_error msgs ast c c' "boolean"
    in if test then eval_ast_formula y else eval_ast_formula z
  | Let (Loc (Var (p,i),loc),content,formula) ->
    let name = (expand_var_name msgs env (p,i)) and desc = (eval_ast content,loc)
    in eval_ast_formula_env ((name,desc)::env) formula
  | Paren x -> eval_ast_formula x
  | ToRemove -> ToRemove
  | e -> raise_with_loc msgs ast ("this expression is not a formula: " ^ string_of_ast e)

and exact_str lst =
  let rec go = function
    | [],[]       -> ToRemove
    | t::ts,[]    -> And (And (Prop t, ToRemove), go (ts,[]))
    | [],f::fs    -> And (And (ToRemove, Not (Prop f)), go ([],fs))
    | t::ts,f::fs -> And (And (Prop t, Not (Prop f)), go (ts,fs))
  in
  match lst with
  | []    -> ToRemove
  | x::xs -> cleantoremove (Or (go x, exact_str xs))

and atleast_str lst =
  List.fold_left (fun acc str -> Or (acc, formula_of_string_list str)) ToRemove lst

and atmost_str lst =
  List.fold_left (fun acc str ->
      Or (acc, List.fold_left (fun acc' str' ->
          And (acc', Not (Prop str'))) ToRemove str)) ToRemove lst

and formula_of_string_list =
  List.fold_left (fun acc str -> And (acc, Prop str)) ToRemove

and and_of_term_list =
  List.fold_left (fun acc t -> And (acc, t)) ToRemove

(* [expand_prop] will expand a proposition containing a set as index, e.g.,
   time([1,2],[a,b]) will become [time(1,a),time(1,b)...time(b,2)]. This is useful when 
   generating sets. *)
and expand_prop_with_set msgs env name indices_optional =
  let rec eval_indices env (l:ast list) : ast list = match l with
    | [] -> []
    | x::xs -> (eval_ast msgs env x)::(eval_indices env xs)
  in
  let rec has_nonempty_set = function
    | []         -> false
    | (Set (EmptySet))::_ -> false
    | (Set _)::_ -> true
    | _::next    -> has_nonempty_set next
  in
  let indices, generated_props = match indices_optional with
    | None   -> [], [UnexpProp (name,None)]
    | Some x -> let indices = eval_indices env x in
            indices, expand_prop_with_set' msgs [UnexpProp (name,None)] indices env
  in
  let eval_unexpprop acc cur = match cur with 
    | UnexpProp (p,i) -> (expand_var_name msgs env (p,i))::acc | _->failwith "shouldnt happen"
  in let props_evaluated = List.fold_left eval_unexpprop [] generated_props in
  if (let x = has_nonempty_set indices in x) then Set (SSet (PropSet.of_list props_evaluated))
  else Prop (let a = List.nth props_evaluated 0 in a)

and expand_prop_with_set' msgs proplist indices env =
  match indices with (* at this point, indice is either a Prop or a Set *)
  | [] -> proplist
  | i::next ->
    match i with
    | Set (EmptySet) -> expand_prop_with_set' msgs proplist next env
    | Set s -> let new_proplist = (expand_proplist proplist (set_to_ast_list msgs env (Set s))) in
        expand_prop_with_set' msgs new_proplist next env
    | x -> expand_prop_with_set' msgs (expand_proplist proplist [x]) next env
and expand_proplist proplist ind = match proplist with
  | [] -> []
  | x::xs -> (expand_prop x ind) @ (expand_proplist xs ind)
and expand_prop prop ind = match prop with
  | UnexpProp (name, None) -> List.fold_left (fun acc i -> (UnexpProp (name,Some ([i])))::acc) [] ind
  | UnexpProp (name, Some cur) -> List.fold_left (fun acc i -> (UnexpProp (name,Some (cur @ [i])))::acc) [] ind
  | x -> failwith ("[shouldnt happen] proplist contains smth that is not UnexpProp: "^string_of_ast_type x)
and expand_var_name (msgs:Msgs.t ref) (env:env) (prefix,indices:string * ast list option) =
  match (prefix,indices) with
  | (x,None)   -> x
  | (x,Some y) ->
    x ^ "("
    ^ (string_of_ast_list "," (List.map (fun e -> eval_ast msgs env e) y))
    ^ ")"

(* [set_to_ast_list] evaluates one element  of the list of things after
   the 'in' of bigand/bigor. 
   If this element is a set, it turns this Set (.) into a list of Int,
   Float or Prop.
   
   WARNING: this function reverses the order of the elements of the set;
   we could use fold_right in order to keep the original order, but 
   it would mean that it is not tail recursion anymore (= uses much more heap) 
   
   If [!check_only] is true, then the lists *)
and set_to_ast_list (msgs:Msgs.t ref) (env:env) (ast:ast) : ast list =
  let lst = match ast_whithout_loc (eval_ast msgs env ast) with
  | Set (EmptySet)-> []
  | Set (ISet a) -> List.fold_left (fun acc v -> (Int v)::acc)   [] (IntSet.elements a)
  | Set (FSet a) -> List.fold_left (fun acc v -> (Float v)::acc) [] (FloatSet.elements a)
  | Set (SSet a) -> List.fold_left (fun acc v -> (Prop v)::acc)  [] (PropSet.elements a)
  | ast' -> raise_with_loc msgs ast (
      "after 'in', only sets are allowed, but got '"^(string_of_ast_type ast')^"':\n"^
      "    "^(string_of_ast ast')^"\n"^
      "This element has been expanded to\n"^
      "    "^(string_of_ast ast')^"")
  in match !check_only, lst with (* useful when you only want to check types *)
          | false,      _      -> lst
          | true,       []     -> []
          | true,        x::xs -> [x]

  (* [ast_to_bool] evaluates the 'when' condition when returns 'true' or 'false'
     depending on the result. 
     This function is used in Bigand and Bigor statements. *)
  and ast_to_bool msgs env (ast:ast) : bool =
    match eval_ast msgs env ast with 
    | Bool b -> b 
    | ast' -> raise_with_loc msgs ast (
      "'when' expects a 'bool' but got '"^(string_of_ast_type ast')^"':\n"^
      "    "^(string_of_ast ast')^"\n"^
      "This element has been expanded to\n"^
      "    "^(string_of_ast ast')^"")
  (* To_int, To_float, Var, Int... all these cannot contain ToRemove because
     ToRemove can only be generated by exact, atleast, atmost, bigand and bigor.
     I only need to match the items that can potentially be produced by the 
     above mentionned. And because "produced" means that everything has already
     been evaluated, all If, Var... have already disapeared. *)
  and hastoremove = function
    | ToRemove -> true
    | Not x                  -> hastoremove x
    | And     (x,y)          -> hastoremove x || hastoremove y
    | Or      (x,y)          -> hastoremove x || hastoremove y
    | Xor     (x,y)          -> hastoremove x || hastoremove y
    | Implies (x,y)          -> hastoremove x || hastoremove y
    | Equiv   (x,y)          -> hastoremove x || hastoremove y
     (* the following items are just here because of SMT that
        allows ==, <, >, +, -, *... in formulas. *)
    | Neg x                  -> hastoremove x
    | Add (x,y)              -> hastoremove x || hastoremove y
    | Sub (x,y)              -> hastoremove x || hastoremove y
    | Mul (x,y)              -> hastoremove x || hastoremove y
    | Div (x,y)              -> hastoremove x || hastoremove y
    | Equal            (x,y) -> hastoremove x || hastoremove y
    | Not_equal        (x,y) -> hastoremove x || hastoremove y
    | Lesser_than      (x,y) -> hastoremove x || hastoremove y
    | Lesser_or_equal  (x,y) -> hastoremove x || hastoremove y
    | Greater_than     (x,y) -> hastoremove x || hastoremove y
    | Greater_or_equal (x,y) -> hastoremove x || hastoremove y
    | _ -> false
  and cleantoremove ast =
    if ast != ToRemove && hastoremove ast
    then cleantoremove (eval_ast_formula (ref Msgs.empty) [] ast)
    else ast