open Ast

exception Type_error of string

let type_error msg =
  raise (Type_error msg)

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

(* binary boolean operators on int and float:
 * <, <=
 * >, >=
 * =, <>
 *)
let arith_pred f frepr x y =
  match eval_arith x, eval_arith y with
  | Int a, Int b -> f a b
  | Float a, Float b -> f a b
  | (Int _, Float _) ->
      type_error "unsupported operand type(s) for '" ^ frepr ^ "': 'int' and 'float'"
  | (Float _), (Int _) ->
      type_error "unsupported operand type(s) for '" ^ frepr ^ "': 'float' and 'int'"

(* binary operators on int or float:
 * + and +.
 * - and -.
 * * and *.
 * / and /.
 *)
let arith_basic_op iop fop repr x y =
  match eval_arith x, eval_arith y with
  | Int a, Int b       -> Int (iop a b)
  | Float a, Float b   -> Float (fop a b)
  | (Int _), (Float _) ->
      type_error "unsupported operand type(s) for '" ^ repr ^ "': 'int' and 'float'"
  | (Float _), (Int _) ->
      type_error "unsupported operand type(s) for '" ^ repr "': 'float' and 'int'"

(* binary operators on sets of int, float or string:
  * union
  * inter
  * diff
  * ...
  *)
let set_bin_op iop fop sop repr s1 s2 =
  match eval_set s1, eval_set s2 with
  | GenSet.IS a, GenSet.IS b -> GenSet.IS (iop a b)
  | GenSet.FS a, GenSet.FS b -> GenSet.FS (fop a b)
  | GenSet.SS a, GenSet.SS b -> GenSet.SS (sop a b)
  | _,_ -> type_error "unsupported set type(s) for '" ^ repr "'"


(********************************************************************************
 *                             Eval functions                                   *
 ********************************************************************************)

let toplevel = Hashtbl.create 10

let rec eval_prog = function
  | Begin (None, exp) -> List.map eval_exp exp
  | Begin (Some sets, exp) ->
      List.iter eval_affect sets; List.map eval_exp exp

and eval_affect = function
  | Affect (str, exp) -> Hashtbl.replace toplevel str (eval_exp exp)

and eval_exp = function
  | Scalar    x -> eval_arith x
  | SetExp    x -> eval_set x
  | BoolExp   x -> BoolExp (Bool (eval_bool x))
  | ClauseExp x -> eval_clause x
  | ListExp   x -> eval_list x
  | Dot (x, y) ->
      begin
        match eval_set x, eval_arith y with
        | GenSet.IS a, Int b ->
            try Scalar (Int (IntSet.find b a))
            with Not_found -> failwith "element " ^ (string_of_int b) ^ " not in set " ^ x
        | GenSet.FS a, Int b ->
            try Scalar (Float (FloatSet.find b a))
            with Not_found -> failwith "element " ^ (string_of_float b) ^ " not in set " ^ x
        | GenSet.SS a, Int b ->
            try ClauseExp (Var ((StringSet.find b a), None))
            with Not_found -> failwith "element " ^ b ^ " not in set " ^ x
        | _,_ -> type_error "unsupported types for '.' operator"
      end

and eval_arith = function
  | Int   _ as i -> i
  | Float _ as f -> f
  | Add (x, y) -> arith_basic_op (+)   (+.)   "+" x y
  | Sub (x, y) -> arith_basic_op (-)   (-.)   "-" x y
  | Mul (x, y) -> arith_basic_op ( * ) ( *. ) "*" x y 
  | Div (x, y) -> arith_basic_op (/)   (/.)   "/" x y
  | Mod (x, y) ->
      begin
        match eval_arith x, eval_arith y with
        | Int a, Int b         -> Int (a mod b)
        | (Float _), (Float _) -> type_error "unsupported operand type(s) for 'mod': 'float' and 'float'"
        | (Int _), (Float _)   -> type_error "unsupported operand type(s) for 'mod': 'int' and 'float'"
        | (Float _), (Int _)   -> type_error "unsupported operand type(s) for 'mod': 'float' and 'int'"
      end
  | Sqrt x ->
      begin
        match eval_arith x with
        | Float a -> Float (sqrt a)
        | Int _   -> type_error "unsupported operand type for 'sqrt': int"
      end
  | To_int x ->
      begin
        match eval_arith x with
        | Int   a -> Int a
        | Float a -> Int (int_of_float a)
      end
  | To_float x ->
      begin
        match x with
        | Float a -> Float a
        | Int   a -> Float (float_of_int a)
      end
  | Card x ->
      begin
        match eval_set x with
        | GenSet.IS a -> Int (IntSet.cardinal    a)
        | GenSet.FS a -> Int (FloatSet.cardinal  a)
        | GenSet.SS a -> Int (StringSet.cardinal a)
      end

and eval_bool = function 
  | Bool b     -> b
  | Not  b     -> not (eval_bool b)
  | And (p, q) -> (eval_bool p) && (eval_bool q)
  | Or (p, q)  -> (eval_bool p) && (eval_bool q)
  | Xor (x, y) ->
      let p = eval_bool x in
      let q = eval_bool y in
      (p || q) && (not (p && q))
  | Implies (p, q) -> (not (eval_bool p)) || (eval_bool q)
  | Equiv (x, y)   -> not (eval_bool (Xor (x, y)))
  | Equal            (x, y) -> arith_pred (=)  "="  x y
  | Not_equal        (x, y) -> arith_pred (<>) "<>" x y
  | Lesser_than      (x, y) -> arith_pred (<)  "<"  x y
  | Lesser_or_equal  (x, y) -> arith_pred (<=) "<=" x y
  | Greater_than     (x, y) -> arith_pred (>)  ">"  x y
  | Greater_or_equal (x, y) -> arith_pred (>=) ">=" x y
  | Empty x ->
      begin
        match eval_set x with
        | Set y ->
            begin
              match y with
              | IS i -> IntSet.empty i
              | FS f -> FloatSet.empty f
              | SS s -> StringSet.empty s
            end
      end

and eval_set = function
  | Set _ as s -> s
  | Union (x, y) -> set_bin_op (IntSet.union) (FloatSet.union) (StringSet.union) "union" x y
  | Inter (x, y) -> set_bin_op (IntSet.inter) (FloatSet.inter) (StringSet.inter) "union" x y
  | Diff  (x, y) -> set_bin_op (IntSet.diff)  (FloatSet.diff)  (StringSet.diff)  "union" x y

and eval_list = function
  | List _ as l  -> l
  | Range (x, y) ->
      begin
        match eval_arith x, eval_arith y with
        | Int a, Int b -> List (range a b 1)
        | _,_ -> type_error "unsupported types for operator '..'"
      end

