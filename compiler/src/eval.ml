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

let set_bin_op iop fop sop repr s1 s2 =
  match s1, s2 with
  | (GenSet.IS a), (GenSet.IS b) -> (GenSet.IS (iop a b))
  | (GenSet.FS a), (GenSet.FS b) -> (GenSet.FS (fop a b))
  | (GenSet.SS a), (GenSet.SS b) -> (GenSet.SS (sop a b))
  | _,_ -> type_error ("unsupported set type(s) for '" ^ repr  ^ "'")


(********************************************************************************
 *                             Eval functions                                   *
 ********************************************************************************)

let toplevel = Hashtbl.create 10

let rec eval_prog = function
  | Begin (None, exp) -> List.map eval_exp exp
  | Begin (Some sets, exp) ->
      List.iter eval_affect sets; List.map eval_exp exp

and eval_affect : affect -> unit = function
  | Affect (str, exp) -> Hashtbl.replace toplevel str (eval_exp exp)

and eval_exp = function
  | Var x ->
      begin
        try Hashtbl.find toplevel x
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | IntExp    x -> IntExp (Int (eval_int x))
  | FloatExp  x -> FloatExp (Float (eval_float x))
  | SetExp    x -> SetExp (Set (eval_set x))
  | BoolExp   x -> BoolExp (Bool (eval_bool x))
  | ClauseExp x -> ClauseExp (eval_clause x)
  | Dot (x, y) ->
      begin
        match eval_set x, eval_exp y with
        | (GenSet.IS a), IntExp (Int b) ->
            begin
              try IntExp (Int (IntSet.find b a))
              with Not_found -> failwith ("element " ^ (string_of_int b) ^ " not in set")
            end
        | (GenSet.FS a), FloatExp (Float b) ->
            begin
              try FloatExp (Float (FloatSet.find b a))
              with Not_found -> failwith ("element " ^ (string_of_float b) ^ " not in set")
            end
        | (GenSet.SS a), ClauseExp (Term (b, None)) ->
            begin
              try ClauseExp (Term ((StringSet.find b a), None))
              with Not_found -> failwith ("element " ^ b ^ " not in set")
            end
        | _,_ -> type_error "unsupported types for '.' operator"
      end
  | If (x, y, z) ->
      if (eval_bool x) then
        eval_exp y
      else
        eval_exp z

and eval_int = function
  | Var x ->
      begin
        try
          begin
            match Hashtbl.find toplevel x with
            | IntExp (Int a) -> a
            | _ -> type_error ("variable '" ^ x ^ "' is not an int")
          end
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | Int i -> i
  | Add (x, y) -> (eval_int x) + (eval_int y)
  | Sub (x, y) -> (eval_int x) - (eval_int y)
  | Mul (x, y) -> (eval_int x) * (eval_int y)
  | Div (x, y) -> (eval_int x) / (eval_int y)
  | Mod (x, y) -> (eval_int x) mod (eval_int y)
  | To_int x   -> int_of_float (eval_float x)

and eval_float = function
  | Var x ->
      begin
        try
          begin
            match Hashtbl.find toplevel x with
            | FloatExp (Float a) -> a
            | _ -> type_error ("variable '" ^ x ^ "' is not a float")
          end
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | Float f -> f
  | Add (x, y) -> (eval_float x) +. (eval_float y)
  | Sub (x, y) -> (eval_float x) -. (eval_float y)
  | Mul (x, y) -> (eval_float x) *. (eval_float y)
  | Div (x, y) -> (eval_float x) /. (eval_float y)
  | Sqrt x -> sqrt (eval_float x)
  | To_float x -> float_of_int (eval_int x)

and eval_bool = function
  | Var x ->
      begin
        try 
          begin
            match Hashtbl.find toplevel x with
            | BoolExp (Bool a) -> a
            | _         -> type_error ("variable '" ^ x ^ "' is not a bool")
          end
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
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
  | Equal             (x, y) -> (eval_int x)   =  (eval_int y)
  | Not_equal         (x, y) -> (eval_int x)   <> (eval_int y)
  | Lesser_than       (x, y) -> (eval_int x)   <  (eval_int y)
  | Lesser_or_equal   (x, y) -> (eval_int x)   <= (eval_int y)
  | Greater_than      (x, y) -> (eval_int x)   >  (eval_int y)
  | Greater_or_equal  (x, y) -> (eval_int x)   >= (eval_int y)
  | FEqual            (x, y) -> (eval_float x) =  (eval_float y)
  | FNot_equal        (x, y) -> (eval_float x) <> (eval_float y)
  | FLesser_than      (x, y) -> (eval_float x) <  (eval_float y)
  | FLesser_or_equal  (x, y) -> (eval_float x) <= (eval_float y)
  | FGreater_than     (x, y) -> (eval_float x) >  (eval_float y)
  | FGreater_or_equal (x, y) -> (eval_float x) >= (eval_float y)
  | Empty x ->
      begin
        match eval_set x with
        | (GenSet.IS i) -> IntSet.is_empty i
        | (GenSet.FS f) -> FloatSet.is_empty f
        | (GenSet.SS s) -> StringSet.is_empty s
      end

and eval_set = function
  | Var x ->
      begin
        try
          begin
            match Hashtbl.find toplevel x with
            | SetExp (Set a) -> a
            | _ -> type_error ("variable '" ^ x ^ "' is not a set")
          end
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | Set s -> s
  | Union (x, y) -> set_bin_op (IntSet.union) (FloatSet.union) (StringSet.union) "union" (eval_set x) (eval_set y)
  | Inter (x, y) -> set_bin_op (IntSet.inter) (FloatSet.inter) (StringSet.inter) "union" (eval_set x) (eval_set y)
  | Diff  (x, y) -> set_bin_op (IntSet.diff)  (FloatSet.diff)  (StringSet.diff)  "union" (eval_set x) (eval_set y)
  | Range (x, y) -> GenSet.IS (IntSet.of_list (range (eval_int x) (eval_int y) 1))

and eval_clause = fun x -> x

