open Ast

exception Type_error of string

let type_error msg =
  raise (Type_error msg)

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


let toplevel = Hashtbl.create 10

let rec eval = function
  | Begin (None, exp) -> List.map eval_exp exp
  | Begin (Some sets, exp) ->
      List.iter eval_affect sets; List.map eval_exp exp

and eval_affect = function
  | Affect (str, exp) -> Hashtbl.replace toplevel str (eval_exp exp)

and eval_exp = function
  | Scalar    x -> eval_arith x
  | SetExp    x -> eval_set    x
  | BoolExp   x -> Bool (eval_bool x)
  | ClauseExp x -> eval_clause x

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
  (* arith boolean operators *)
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




    

