open Types.Ast
open Printf

let write_int x = if x < 0 then sprintf "(- %d)" (-x) else string_of_int x

let write_float x =
  let x' = string_of_float x in
  let x'' = if x'.[String.length x' - 1] = '.' then x' ^ "0" else x' in
  if x''.[0] = '-' then "(- " ^ String.sub x'' 1 (String.length x'' - 1) ^ ")"
  else x''

let sanitize_var name =
  String.map
    (fun c -> if c = '(' || c = ')' || c = ',' || c = ' ' then '_' else c)
    name

let to_smt2 logic formula =
  let vtbl = Hashtbl.create 10 in
  let out = Buffer.create 1000 in
  Buffer.add_string out ("(set-logic " ^ logic ^ ")\n");

  let write_to_buf = Buffer.add_string out in

  let add_var name typ =
    if not (Hashtbl.mem vtbl name) then Hashtbl.replace vtbl name typ
  in

  let decl_var name typ = sprintf "(declare-fun %s () %s)\n" name typ in
  let decl_assert body = sprintf "(assert %s)\n" body in
  let decl_bin_op op x y = sprintf "(%s %s %s)" op x y in
  let decl_un_op op x = sprintf "(%s %s)" op x in

  let rec term_expr = function
    | Prop _ -> true
    | ArithUnop (Neg, x) -> term_expr x
    | ArithBinop (x, _, y) -> term_expr x && term_expr y
    | _ -> false
  in

  let rec gen_var typ = function
    | Prop x -> add_var x typ
    | ArithBinop (Prop x, _, Int _)
    | ArithBinop (Int _, _, Prop x)
    | ArithBinrel (Prop x, _, Int _)
    | ArithBinrel (Int _, _, Prop x) ->
        add_var x "Int"
    | ArithBinop (Prop x, _, Float _)
    | ArithBinop (Float _, _, Prop x)
    | ArithBinrel (Prop x, _, Float _)
    | ArithBinrel (Float _, _, Prop x) ->
        add_var x "Real"
    | ArithBinop (Prop x, _, Prop y) | ArithBinrel (Prop x, _, Prop y) -> (
        try
          let x_type = Hashtbl.find vtbl x in
          add_var y x_type
        with Not_found -> (
          try
            let y_type = Hashtbl.find vtbl y in
            add_var x y_type
          with Not_found ->
            add_var x typ;
            add_var y typ))
    | Not x -> gen_var typ x
    | LogicBinop (x, _, y) ->
        gen_var "Bool" x;
        gen_var "Bool" y
    | ArithBinop (x, _, Int _)
    | ArithBinop (Int _, _, x)
    | ArithBinrel (x, _, Int _)
    | ArithBinrel (Int _, _, x) ->
        let rec go = function
          | Prop x -> add_var x "Int"
          | ArithBinop (x, _, y) ->
              go x;
              go y
          | _ -> failwith "not a term exp"
        in
        if term_expr x then go x else ()
    | ArithBinop (x, _, Float _)
    | ArithBinop (Float _, _, x)
    | ArithBinrel (x, _, Float _)
    | ArithBinrel (Float _, _, x) ->
        let rec go = function
          | Prop x -> add_var x "Real"
          | ArithBinop (x, _, y) ->
              go x;
              go y
          | _ -> failwith "not a term exp"
        in
        if term_expr x then go x else ()
    | ArithBinop (x, _, y) | ArithBinrel (x, _, y) ->
        gen_var typ x;
        gen_var typ y
    | _ -> ()
  in

  let parse = function
    | ArithBinrel (x, _, Int _) | ArithBinrel (Int _, _, x) ->
        gen_var "Int" x (*; Equal (x, Int y)*)
    | ArithBinrel (x, _, Float _) | ArithBinrel (Float _, _, x) ->
        gen_var "Real" x (*; Equal (x, Float y)*)
    | LogicBinop (x, _, y) ->
        gen_var "Bool" x;
        gen_var "Bool" y (*; And (x, y)*)
    | Prop x -> add_var x "Bool"
    | x ->
        failwith
          ("this cannot be transformed into SMT2: "
          ^ Pprint.string_of_ast ~debug:true x)
  in

  let rec write = function
    | Top -> "true"
    | Bottom -> "false"
    | Prop x -> sanitize_var x
    | Int x -> write_int x
    | Float x -> write_float x
    | Not x -> decl_un_op "not" (write x)
    | LogicBinop (x, Equiv, y) ->
        write
          (LogicBinop
             (LogicBinop (x, Implies, y), And, LogicBinop (y, Implies, x)))
    | LogicBinop (x, b, y) ->
        decl_bin_op (Pprint.logic_binop b) (write x) (write y)
    | ArithUnop (Neg, Int x) -> "(- " ^ string_of_int (-x) ^ ")"
    | ArithUnop (Neg, Float x) -> "(- " ^ string_of_float (-.x) ^ ")"
    | ArithBinop (x, b, y) ->
        decl_bin_op (Pprint.arith_binop b) (write x) (write y)
    | ArithBinrel (x, Equal, y) -> decl_bin_op "=" (write x) (write y)
    | ArithBinrel (x, Not_equal, y) -> write (Not (ArithBinrel (x, Equal, y)))
    | ArithBinrel (x, b, y) ->
        decl_bin_op (Pprint.arith_binrel b) (write x) (write y)
    | Layout (_, x) -> write x
    | x -> failwith ("error smt write: " ^ Pprint.string_of_ast ~debug:true x)
  in

  parse formula;
  Hashtbl.iter (fun k v -> write_to_buf (decl_var (sanitize_var k) v)) vtbl;
  decl_assert (write formula) |> write_to_buf;
  write_to_buf "(check-sat)\n(get-value (";
  Hashtbl.iter (fun k _ -> write_to_buf (sanitize_var k ^ " ")) vtbl;
  write_to_buf "))";
  out
