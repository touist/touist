open Types.Ast

let to_smt2 logic formula =
  let vtbl = Hashtbl.create 10 in
  let out  = Buffer.create 1000 in
  Buffer.add_string out ("(set-logic " ^ logic ^ ")\n");

  let write_to_buf = Buffer.add_string out in

  let add_var name typ =
    if not (Hashtbl.mem vtbl name) then
      Hashtbl.replace vtbl name typ
  in

  let decl_var name typ =
    "(declare-fun " ^ name ^ " () " ^ typ ^ ")\n"
  in
  let decl_assert body   = "(assert " ^ body ^ ")\n" in
  let decl_bin_op op x y = "(" ^ op ^ " " ^ x ^ " " ^ y ^ ")" in
  let decl_un_op  op x   = "(" ^ op ^ " " ^ x ^ ")" in

  let sanitize_var name =
    String.map (fun c ->
        if (c = '(') ||
           (c = ')') ||
           (c = ',') || (c = ' ') then '_' else c) name
  in

  let rec term_expr = function
    | Prop _
    | Neg (Prop _)
    | Add (Prop _, Prop _)
    | Sub (Prop _, Prop _)
    | Mul (Prop _, Prop _)
    | Div (Prop _, Prop _) -> true
    | Neg x -> term_expr x
    | Add (x,y)
    | Sub (x,y)
    | Mul (x,y)
    | Div (x,y) -> term_expr x && term_expr y
    | _ -> false
  in

  let rec gen_var typ = function
    | Prop x -> add_var x typ
    | Add              (Prop x, Int _) | Add              (Int _, Prop x)
    | Sub              (Prop x, Int _) | Sub              (Int _, Prop x)
    | Mul              (Prop x, Int _) | Mul              (Int _, Prop x)
    | Div              (Prop x, Int _) | Div              (Int _, Prop x)
    | Lesser_than      (Prop x, Int _) | Lesser_than      (Int _, Prop x)
    | Lesser_or_equal  (Prop x, Int _) | Lesser_or_equal  (Int _, Prop x)
    | Greater_than     (Prop x, Int _) | Greater_than     (Int _, Prop x)
    | Greater_or_equal (Prop x, Int _) | Greater_or_equal (Int _, Prop x)
    | Equal            (Prop x, Int _) | Equal            (Int _, Prop x)
    | Not_equal        (Prop x, Int _) | Not_equal        (Int _, Prop x)
      -> add_var x "Int"
    | Add              (Prop x, Float _) | Add              (Float _, Prop x)
    | Sub              (Prop x, Float _) | Sub              (Float _, Prop x)
    | Mul              (Prop x, Float _) | Mul              (Float _, Prop x)
    | Div              (Prop x, Float _) | Div              (Float _, Prop x)
    | Lesser_than      (Prop x, Float _) | Lesser_than      (Float _, Prop x)
    | Lesser_or_equal  (Prop x, Float _) | Lesser_or_equal  (Float _, Prop x)
    | Greater_than     (Prop x, Float _) | Greater_than     (Float _, Prop x)
    | Greater_or_equal (Prop x, Float _) | Greater_or_equal (Float _, Prop x)
    | Equal            (Prop x, Float _) | Equal            (Float _, Prop x)
    | Not_equal        (Prop x, Float _) | Not_equal        (Float _, Prop x)
      -> add_var x "Real"
    | Add              (Prop x, Prop y)
    | Sub              (Prop x, Prop y)
    | Mul              (Prop x, Prop y)
    | Div              (Prop x, Prop y)
    | Lesser_than      (Prop x, Prop y)
    | Lesser_or_equal  (Prop x, Prop y)
    | Greater_than     (Prop x, Prop y)
    | Greater_or_equal (Prop x, Prop y)
    | Equal            (Prop x, Prop y)
    | Not_equal        (Prop x, Prop y) ->
      begin
        try
          let x_type = Hashtbl.find vtbl x in
          add_var y x_type
        with Not_found ->
        try
          let y_type = Hashtbl.find vtbl y in
          add_var x y_type
        with Not_found ->
          add_var x typ;
          add_var y typ
      end
    | Not x -> gen_var typ x
    | And     (x,y)
    | Or      (x,y)
    | Xor     (x,y)
    | Implies (x,y)
    | Equiv   (x,y) -> gen_var "Bool" x; gen_var "Bool" y
    | Add              (x, Int _) | Add              (Int _, x)
    | Sub              (x, Int _) | Sub              (Int _, x)
    | Mul              (x, Int _) | Mul              (Int _, x)
    | Div              (x, Int _) | Div              (Int _, x)
    | Equal            (x, Int _) | Equal            (Int _, x)
    | Not_equal        (x, Int _) | Not_equal        (Int _, x)
    | Lesser_than      (x, Int _) | Lesser_than      (Int _, x)
    | Lesser_or_equal  (x, Int _) | Lesser_or_equal  (Int _, x)
    | Greater_than     (x, Int _) | Greater_than     (Int _, x)
    | Greater_or_equal (x, Int _) | Greater_or_equal (Int _, x)
      ->
      let rec go = function
        | Prop x -> add_var x "Int"
        | Add (x,y)
        | Sub (x,y)
        | Mul (x,y)
        | Div (x,y) -> go x; go y
        | _ -> failwith "not a term exp"
      in
      if term_expr x then go x else ()
    | Add              (x, Float _) | Add              (Float _, x)
    | Sub              (x, Float _) | Sub              (Float _, x)
    | Mul              (x, Float _) | Mul              (Float _, x)
    | Div              (x, Float _) | Div              (Float _, x)
    | Equal            (x, Float _) | Equal            (Float _, x)
    | Not_equal        (x, Float _) | Not_equal        (Float _, x)
    | Lesser_than      (x, Float _) | Lesser_than      (Float _, x)
    | Lesser_or_equal  (x, Float _) | Lesser_or_equal  (Float _, x)
    | Greater_than     (x, Float _) | Greater_than     (Float _, x)
    | Greater_or_equal (x, Float _) | Greater_or_equal (Float _, x)
      ->
      let rec go = function
        | Prop x -> add_var x "Real"
        | Add (x,y)
        | Sub (x,y)
        | Mul (x,y)
        | Div (x,y) -> go x; go y
        | _ -> failwith "not a term exp"
      in
      if term_expr x then go x else ()
    | Add              (x, y)
    | Sub              (x, y)
    | Mul              (x, y)
    | Div              (x, y)
    | Equal            (x, y)
    | Not_equal        (x, y)
    | Lesser_than      (x, y)
    | Lesser_or_equal  (x, y)
    | Greater_than     (x, y)
    | Greater_or_equal (x, y) -> gen_var typ x; gen_var typ y
    | _ -> ()
  in

  (* [parse] will 'infer' the right type. If both operands are propositions,
     1) we check if these propositions have already been used somewhere; if
        yes, we use their type
     2) if these propositions have not been already seen, then
        use the "most general" type:
        - in QF_LRA (or any real-based coding), Real is going to be used;
        - in QF_LIA (or any integer-based codings), Int will be used. *)
  let rec parse = function
    | Equal            (x, Int _) | Equal            (Int _, x)
    | Not_equal        (x, Int _) | Not_equal        (Int _, x)
    | Lesser_than      (x, Int _) | Lesser_than      (Int _, x)
    | Lesser_or_equal  (x, Int _) | Lesser_or_equal  (Int _, x)
    | Greater_than     (x, Int _) | Greater_than     (Int _, x)
    | Greater_or_equal (x, Int _) | Greater_or_equal (Int _, x)
      -> gen_var "Int" x
    | Equal            (x, Float _) | Equal            (Float _, x)
    | Not_equal        (x, Float _) | Not_equal        (Float _, x)
    | Lesser_than      (x, Float _) | Lesser_than      (Float _, x)
    | Lesser_or_equal  (x, Float _) | Lesser_or_equal  (Float _, x)
    | Greater_than     (x, Float _) | Greater_than     (Float _, x)
    | Greater_or_equal (x, Float _) | Greater_or_equal (Float _, x)
      -> gen_var "Real" x
    | And (x, y) | Or (x, y) | Xor (x, y) | Implies (x, y) | Equiv (x, y)
      -> gen_var "Bool" x; gen_var "Bool" y
    | Prop x -> add_var x "Bool"
    | x -> failwith ("this cannot be transformed into SMT2: "^(Pprint.string_of_ast ~debug:true x))
  in

  let rec write = function
    | Top                        -> "true"
    | Bottom                     -> "false"
    | Prop              x -> sanitize_var x
    | Int x ->
      if x < 0 then
        "(- " ^ string_of_int (-x) ^ ")"
      else
        string_of_int   x
    | Float x ->
      let x'  = string_of_float x in
      let x'' = if x'.[String.length x' - 1] = '.' then x' ^ "0" else x' in
      if x''.[0] = '-' then
        "(- " ^ String.sub x'' 1 (String.length x'' - 1) ^ ")"
      else
        x''
    | Not              x        -> decl_un_op  "not" (write x)
    | And              (x,y)    -> decl_bin_op "and" (write x) (write y)
    | Or               (x,y)    -> decl_bin_op "or"  (write x) (write y)
    | Xor              (x,y)    -> decl_bin_op "xor" (write x) (write y)
    | Implies          (x,y)    -> decl_bin_op "=>" (write x) (write y)
    | Equiv            (x,y)    -> write (And (Implies (x,y), Implies (y,x)))
    | Neg              (Int x) -> "(- " ^ string_of_int (-x) ^ ")"
    | Neg              (Float x) -> "(- " ^ string_of_float (-.x) ^ ")"
    | Add              (x,y)    -> decl_bin_op "+" (write x) (write y)
    | Sub              (x,y)    -> decl_bin_op "-" (write x) (write y)
    | Mul              (x,y)    -> decl_bin_op "*" (write x) (write y)
    | Div              (x,y)    -> decl_bin_op "/" (write x) (write y)
    | Equal            (x,y)    -> decl_bin_op "=" (write x) (write y)
    | Not_equal        (x,y)    -> write (Not (Equal (x,y)))
    | Lesser_than      (x,y)    -> decl_bin_op "<" (write x) (write y)
    | Lesser_or_equal  (x,y)    -> decl_bin_op "<=" (write x) (write y)
    | Greater_than     (x,y)    -> decl_bin_op ">" (write x) (write y)
    | Greater_or_equal (x,y)    -> decl_bin_op ">=" (write x) (write y)
    | Loc (x,_) -> write x
    | x -> failwith ("error smt write: "^(Pprint.string_of_ast ~debug:true x))
  in

  parse formula;
  Hashtbl.iter (fun k v -> write_to_buf (decl_var (sanitize_var k) v)) vtbl;
  decl_assert (write formula) |> write_to_buf;
  write_to_buf "(check-sat)\n(get-value (";
  Hashtbl.iter (fun k _ -> write_to_buf (sanitize_var k ^ " ")) vtbl;
  write_to_buf "))";
  out
