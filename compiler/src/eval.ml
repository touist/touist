open Ast

exception TypeError of string

let type_error msg =
  raise (TypeError msg)

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
  | GenSet.IS _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.IS IntSet.empty)
  | GenSet.Empty, GenSet.IS _ ->
      set_bin_op iop fop sop repr (GenSet.IS IntSet.empty) s2
  | GenSet.Empty, GenSet.FS _ ->
      set_bin_op iop fop sop repr (GenSet.FS FloatSet.empty) s2
  | GenSet.FS _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.FS FloatSet.empty)
  | GenSet.Empty, GenSet.SS _ ->
      set_bin_op iop fop sop repr (GenSet.SS StringSet.empty) s2
  | GenSet.SS _, GenSet.Empty ->
      set_bin_op iop fop sop repr s1 (GenSet.SS StringSet.empty)
  | GenSet.IS a, GenSet.IS b -> GenSet.IS (iop a b)
  | GenSet.FS a, GenSet.FS b -> GenSet.FS (fop a b)
  | GenSet.SS a, GenSet.SS b -> GenSet.SS (sop a b)
  | _,_ -> type_error ("unsupported set type(s) for '" ^ repr  ^ "'")

let rec set_pred_op ipred fpred spred repr s1 s2 =
  match s1, s2 with
  | GenSet.Empty, GenSet.IS _ ->
      set_pred_op ipred fpred spred repr (GenSet.IS IntSet.empty) s2
  | GenSet.IS _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.IS IntSet.empty)
  | GenSet.Empty, GenSet.FS _ ->
      set_pred_op ipred fpred spred repr (GenSet.FS FloatSet.empty) s2
  | GenSet.FS _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.FS FloatSet.empty)
  | GenSet.Empty, GenSet.SS _ ->
      set_pred_op ipred fpred spred repr (GenSet.SS StringSet.empty) s2
  | GenSet.SS _, GenSet.Empty ->
      set_pred_op ipred fpred spred repr s1 (GenSet.SS StringSet.empty)
  | GenSet.IS a, GenSet.IS b -> ipred a b
  | GenSet.FS a, GenSet.FS b -> fpred a b
  | GenSet.SS a, GenSet.SS b -> spred a b
  | _,_ -> type_error ("unsupported set type(s) for '" ^ repr  ^ "'")


(********************************************************************************
 *                             Eval functions                                   *
 ********************************************************************************)

let toplevel = Hashtbl.create 10

let rec eval_prog = function
  | Begin (None, exp) -> List.map (eval_exp ~env:[]) exp
  | Begin (Some sets, exp) ->
      List.iter eval_affect sets; List.map (eval_exp ~env:[]) exp

and eval_affect = function
  | Affect (str, exp) -> Hashtbl.replace toplevel str (eval_exp exp)

and eval_exp ?(env=[]) = function
  | Var x ->
    begin
      try List.assoc x env
      with Not_found ->
        try Hashtbl.find toplevel x
        with Not_found -> failwith ("unbound variable: " ^ x)
    end
  | IntExp    x -> IntExp (Int (eval_int ~env:env x))
  | FloatExp  x -> FloatExp (Float (eval_float x))
  | SetExp    x -> SetExp (Set (eval_set x))
  | BoolExp   x -> BoolExp (Bool (eval_bool x))
  | ClauseExp x -> ClauseExp (eval_clause ~env:env x)
  | Dot (x, y) ->
      begin
        match eval_set x, eval_exp ~env:env y with
        | GenSet.Empty, _ -> failwith "empty set"
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
        eval_exp ~env:env y
      else
        eval_exp ~env:env z

and eval_int ?(env=[]) = function
  | IVar x ->
      begin
        try
          begin
            match List.assoc x env with
            | IntExp (Int a) -> a
            | _ -> type_error ("variable '" ^ x ^ "' is not an int")
          end
        with Not_found ->
          try
            begin
              match Hashtbl.find toplevel x with
              | IntExp (Int a) -> a
              | _ -> type_error ("variable '" ^ x ^ "' is not an int")
            end
          with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | Int i -> i
  | Neg i -> - (eval_int ~env:env i)
  | Add (x, y) -> (eval_int ~env:env x) + (eval_int ~env:env y)
  | Sub (x, y) -> (eval_int ~env:env x) - (eval_int ~env:env y)
  | Mul (x, y) -> (eval_int ~env:env x) * (eval_int ~env:env y)
  | Div (x, y) -> (eval_int ~env:env x) / (eval_int ~env:env y)
  | Mod (x, y) -> (eval_int ~env:env x) mod (eval_int ~env:env y)
  | To_int x   -> int_of_float (eval_float x)
  | Card x ->
      begin
        match eval_set x with
        | GenSet.Empty -> 0
        | GenSet.IS a  -> IntSet.cardinal    a
        | GenSet.FS a  -> FloatSet.cardinal  a
        | GenSet.SS a  -> StringSet.cardinal a
      end

and eval_float = function
  | FVar x ->
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
  | FNeg   f -> -. (eval_float f)
  | FAdd (x, y) -> (eval_float x) +. (eval_float y)
  | FSub (x, y) -> (eval_float x) -. (eval_float y)
  | FMul (x, y) -> (eval_float x) *. (eval_float y)
  | FDiv (x, y) -> (eval_float x) /. (eval_float y)
  | Sqrt x -> sqrt (eval_float x)
  | To_float x -> float_of_int (eval_int x)

and eval_bool ?env = function
  | BVar x ->
      begin
        try 
          begin
            match Hashtbl.find toplevel x with
            | BoolExp (Bool a) -> a
            | _ -> type_error ("variable '" ^ x ^ "' is not a bool")
          end
        with Not_found -> failwith ("unbound variable: " ^ x)
      end
  | Bool b     -> b
  | BNot  b     -> not (eval_bool b)
  | BAnd (p, q) -> (eval_bool p) && (eval_bool q)
  | BOr (p, q)  -> (eval_bool p) && (eval_bool q)
  | BXor (x, y) ->
      let p = eval_bool x in
      let q = eval_bool y in
      (p || q) && (not (p && q))
  | BImplies (p, q) -> (not (eval_bool p)) || (eval_bool q)
  | BEquiv (x, y)   -> not (eval_bool (BXor (x, y)))
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
  | SEqual (x, y) -> set_pred_op (IntSet.equal) (FloatSet.equal) (StringSet.equal) "=" (eval_set x) (eval_set y)
  | Subset (x, y) -> set_pred_op (IntSet.subset) (FloatSet.subset) (StringSet.subset) "subset" (eval_set x) (eval_set y)
  | In (x, y) ->
      begin
        match eval_exp x, eval_set y with
        | IntExp    (Int a),          GenSet.IS b -> IntSet.mem    a b
        | FloatExp  (Float a),        GenSet.FS b -> FloatSet.mem  a b
        | ClauseExp (Term (a, None)), GenSet.SS b -> StringSet.mem a b
        | _,_ -> type_error "unexpected types for operator 'in'"
      end
  | Empty x ->
      begin
        match eval_set x with
        | GenSet.Empty -> true
        | GenSet.IS i  -> IntSet.is_empty i
        | GenSet.FS f  -> FloatSet.is_empty f
        | GenSet.SS s  -> StringSet.is_empty s
      end

and eval_set = function
  | SVar x ->
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

and eval_clause ?(env=[]) = function
  | Top    -> Top
  | Bottom -> Bottom
  | Term (x, None)  -> Term (x, None)
  | Term (x, Some (Str y)) -> Term ((x ^ "(" ^ y ^ ")"), None)
  | Term (x, Some (Exp y)) ->
      let opt =
        match eval_exp ~env:env y with
        | IntExp (Int a) -> string_of_int a
        | FloatExp (Float a) -> string_of_float a
        | ClauseExp (Term (a, None)) -> a
        | _ -> type_error "unsupported expression format"
      in Term ((x ^ "(" ^ opt ^ ")"), None)
  | Not     x      -> Not     (eval_clause ~env:env x)
  | And     (x, y) -> And     (eval_clause ~env:env x, eval_clause ~env:env y)
  | Or      (x, y) -> Or      (eval_clause ~env:env x, eval_clause ~env:env y)
  | Xor     (x, y) -> Xor     (eval_clause ~env:env x, eval_clause ~env:env y)
  | Implies (x, y) -> Implies (eval_clause ~env:env x, eval_clause ~env:env y)
  | Equiv   (x, y) -> Equiv   (eval_clause ~env:env x, eval_clause ~env:env y)
  | Bigand (v, s, t, e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> failwith "malformed bigand expression"
        | [x],[y] ->
            begin
              match eval_set y with
              | GenSet.Empty -> failwith "bigand: empty set"
              | GenSet.IS a  -> bigand_int env x (IntSet.elements a) test e
              | GenSet.FS a  -> bigand_float env x (FloatSet.elements a) test e
              | GenSet.SS a  -> bigand_str env x (StringSet.elements a) test e
            end
        | x::xs,y::ys ->
            eval_clause ~env:env (Bigand ([x],[y],None,ClauseExp(Bigand (xs,ys,t,e))))
      end
  | Bigor (v, s, t, e) ->
      let test =
        match t with
        | Some x -> x
        | None   -> Bool true
      in
      begin
        match v,s with
        | [],[] | _,[] | [],_ -> failwith "malformed bigor expression"
        | [x],[y] ->
            begin
              match eval_set y with
              | GenSet.Empty -> failwith "bigor: empty set"
              | GenSet.IS a  -> bigor_int env x (IntSet.elements a) test e
              | GenSet.FS a  -> bigor_float env x (FloatSet.elements a) test e
              | GenSet.SS a  -> bigor_str env x (StringSet.elements a) test e
            end
        | x::xs,y::ys ->
            eval_clause ~env:env (Bigor ([x],[y],None,ClauseExp (Bigor (xs,ys,t,e))))
      end
and bigand_int env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      And (acc, eval_bigbody ((var, IntExp (Int x))::env) exp)
    else
      acc) Top values
and bigand_float env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      And (acc, eval_bigbody ((var, FloatExp (Float x))::env) exp)
    else
      acc) Top values
and bigand_str env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      And (acc, eval_bigbody ((var, ClauseExp (Term (x, None)))::env) exp)
    else
      acc) Top values
and bigor_int env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      Or (acc, eval_bigbody ((var, IntExp (Int x))::env) exp)
    else
      acc) Bottom values
and bigor_float env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      Or (acc, eval_bigbody ((var, FloatExp (Float x))::env) exp)
    else
      acc) Bottom values
and bigor_str env var values test exp =
  List.fold_left (fun acc x ->
    if eval_bool test then
      Or (acc, eval_bigbody ((var, ClauseExp (Term (x, None)))::env) exp)
    else
      acc) Bottom values
and eval_bigbody env exp =
  match eval_exp ~env:env exp with
  | ClauseExp a -> a
  | _ -> failwith "expected clause expression"

let eval exp =
  List.fold_left (fun acc x ->
                    match x with
                    | ClauseExp a -> And (acc, a)
                    | _ -> type_error "expected clause_exp") Top (eval_prog exp)
