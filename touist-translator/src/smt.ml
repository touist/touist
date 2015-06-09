open Syntax

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
    try
      let lparen_index = String.index name '(' in
      let rparen_index = String.index name ')' in
      Bytes.set name lparen_index '_';
      Bytes.set name rparen_index '_';
      String.iteri (fun i c -> if (c = ',') then Bytes.set name i '_';) name;
      String.iteri (fun i c -> if (c = ' ') then Bytes.set name i '_';)  name;
      name
    with Not_found -> name
  in
  
  let rec term_expr = function
    | Term (_,None)
    | CNeg (Term _)
    | CAdd (Term _, Term _)
    | CSub (Term _, Term _)
    | CMul (Term _, Term _)
    | CDiv (Term _, Term _) -> true
    | CNeg x -> term_expr x
    | CAdd (x,y)
    | CSub (x,y)
    | CMul (x,y)
    | CDiv (x,y) -> term_expr x && term_expr y
    | _ -> false
  in

  let rec gen_var typ = function
    | Term (x,None) -> add_var x typ
    | CAdd              (Term (x,None), CInt _)
    | CAdd              (CInt _, Term (x,None))
    | CSub              (Term (x,None), CInt _)
    | CSub              (CInt _, Term (x,None))
    | CMul              (Term (x,None), CInt _)
    | CMul              (CInt _, Term (x,None))
    | CDiv              (Term (x,None), CInt _)
    | CDiv              (CInt _, Term (x,None))
    | CLesser_than      (Term (x,None), CInt _)
    | CLesser_than      (CInt _, Term (x,None))
    | CLesser_or_equal  (Term (x,None), CInt _)
    | CLesser_or_equal  (CInt _, Term (x,None))
    | CGreater_than     (Term (x,None), CInt _)
    | CGreater_than     (CInt _, Term (x,None))
    | CGreater_or_equal (CInt _, Term (x,None))
    | CGreater_or_equal (Term (x,None), CInt _)
    | CEqual            (Term (x,None), CInt _)
    | CEqual            (CInt _, Term (x,None))
    | CNot_equal        (Term (x,None), CInt _)
    | CNot_equal        (CInt _, Term (x,None)) -> add_var x "Int"
    | CAdd              (Term (x,None), CFloat _)
    | CAdd              (CFloat _, Term (x,None))
    | CSub              (Term (x,None), CFloat _)
    | CSub              (CFloat _, Term (x,None))
    | CMul              (Term (x,None), CFloat _)
    | CMul              (CFloat _, Term (x,None))
    | CDiv              (Term (x,None), CFloat _)
    | CDiv              (CFloat _, Term (x,None))
    | CLesser_than      (Term (x,None), CFloat _)
    | CLesser_than      (CFloat _, Term (x,None))
    | CLesser_or_equal  (Term (x,None), CFloat _)
    | CLesser_or_equal  (CFloat _, Term (x,None))
    | CGreater_than     (Term (x,None), CFloat _)
    | CGreater_than     (CFloat _, Term (x,None))
    | CGreater_or_equal (Term (x,None), CFloat _)
    | CGreater_or_equal (CFloat _, Term (x,None))
    | CEqual            (Term (x,None), CFloat _)
    | CEqual            (CFloat _, Term (x,None))
    | CNot_equal        (Term (x,None), CFloat _)
    | CNot_equal        (CFloat _, Term (x,None)) -> add_var x "Float"
    | CAdd              (Term (x,None), Term (y,None))
    | CSub              (Term (x,None), Term (y,None))
    | CMul              (Term (x,None), Term (y,None))
    | CDiv              (Term (x,None), Term (y,None))
    | CLesser_than      (Term (x,None), Term (y,None))
    | CLesser_or_equal  (Term (x,None), Term (y,None))
    | CGreater_than     (Term (x,None), Term (y,None))
    | CGreater_or_equal (Term (x,None), Term (y,None))
    | CEqual            (Term (x,None), Term (y,None))
    | CNot_equal        (Term (x,None), Term (y,None)) ->
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
    | CNot x -> gen_var typ x
    | CAnd     (x,y)
    | COr      (x,y)
    | CXor     (x,y)
    | CImplies (x,y)
    | CEquiv   (x,y) -> gen_var "Bool" x; gen_var "Bool" y
    | CAdd              (x, CInt _) 
    | CAdd              (CInt _, x) 
    | CSub              (x, CInt _) 
    | CSub              (CInt _, x) 
    | CMul              (x, CInt _) 
    | CMul              (CInt _, x) 
    | CDiv              (x, CInt _) 
    | CDiv              (CInt _, x)
    | CEqual            (x, CInt _)
    | CEqual            (CInt _, x)
    | CNot_equal        (x, CInt _)
    | CNot_equal        (CInt _, x)
    | CLesser_than      (x, CInt _)
    | CLesser_than      (CInt _, x)
    | CLesser_or_equal  (x, CInt _)
    | CLesser_or_equal  (CInt _, x)
    | CGreater_than     (x, CInt _)
    | CGreater_than     (CInt _, x)
    | CGreater_or_equal (x, CInt _)
    | CGreater_or_equal (CInt _, x)->
        let rec go = function
          | Term (x,None) -> add_var x "Int"
          | CAdd (x,y)
          | CSub (x,y)
          | CMul (x,y)
          | CDiv (x,y) -> go x; go y
          | _ -> failwith "not a term exp"
        in
        if term_expr x then go x else ()
    | CAdd              (x, CFloat _) 
    | CAdd              (CFloat _, x) 
    | CSub              (x, CFloat _) 
    | CSub              (CFloat _, x) 
    | CMul              (x, CFloat _) 
    | CMul              (CFloat _, x) 
    | CDiv              (x, CFloat _) 
    | CDiv              (CFloat _, x)
    | CEqual            (x, CFloat _)
    | CEqual            (CFloat _, x)
    | CNot_equal        (x, CFloat _)
    | CNot_equal        (CFloat _, x)
    | CLesser_than      (x, CFloat _)
    | CLesser_than      (CFloat _, x)
    | CLesser_or_equal  (x, CFloat _)
    | CLesser_or_equal  (CFloat _, x)
    | CGreater_than     (x, CFloat _)
    | CGreater_than     (CFloat _, x)
    | CGreater_or_equal (x, CFloat _)
    | CGreater_or_equal (CFloat _, x) ->
        let rec go = function
          | Term (x,None) -> add_var x "Real"
          | CAdd (x,y)
          | CSub (x,y)
          | CMul (x,y)
          | CDiv (x,y) -> go x; go y
          | _ -> failwith "not a term exp"
        in
        if term_expr x then go x else ()
    | CAdd              (x, y) 
    | CSub              (x, y) 
    | CMul              (x, y) 
    | CDiv              (x, y) 
    | CEqual            (x, y)
    | CNot_equal        (x, y)
    | CLesser_than      (x, y)
    | CLesser_or_equal  (x, y)
    | CGreater_than     (x, y)
    | CGreater_or_equal (x, y) -> gen_var typ x; gen_var typ y
    | _ -> ()
  in

  let rec parse = function
    | CEqual            (x, CInt y) -> gen_var "Int" x(*; CEqual (x, CInt y)*)
    | CEqual            (CInt x, y) -> gen_var "Int" y(*; CEqual (CInt x, y)*)
    | CNot_equal        (x, CInt y) -> gen_var "Int" x(*; CNot_equal (x, CInt
    y)*)
    | CNot_equal        (CInt x, y) -> gen_var "Int" y(*; CNot_equal (CInt x,
    y)*)
    | CLesser_than      (x, CInt y) -> gen_var "Int" x(*; CLesser_than (x, CInt
    y)*)
    | CLesser_than      (CInt x, y) -> gen_var "Int" y(*; CLesser_than (CInt x,
    y)*)
    | CLesser_or_equal  (x, CInt y) -> gen_var "Int" x(*; CLesser_or_equal (x,
    CInt y)*)
    | CLesser_or_equal  (CInt x, y) -> gen_var "Int" y(*; CLesser_or_equal (CInt
    x, y)*)
    | CGreater_than     (x, CInt y) -> gen_var "Int" x(*; CGreater_than (x, CInt
    y)*)
    | CGreater_than     (CInt x, y) -> gen_var "Int" y(*; CGreater_than (CInt x,
    y)*)
    | CGreater_or_equal (x, CInt y) -> gen_var "Int" x(*; CGreater_or_equal (x,
    CInt y)*)
    | CGreater_or_equal (CInt x, y) -> gen_var "Int" y(*; CGreater_or_equal
    (CInt x, y)*)
    | CEqual            (x, CFloat y) -> gen_var "Real" x(*; CEqual (x, CFloat
    y)*)
    | CEqual            (CFloat x, y) -> gen_var "Real" y(*; CEqual (CFloat x,
    y)*)
    | CNot_equal        (x, CFloat y) -> gen_var "Real" x(*; CNot_equal (x,
    CFloat y)*)
    | CNot_equal        (CFloat x, y) -> gen_var "Real" y(*; CNot_equal (CFloat
    x, y)*)
    | CLesser_than      (x, CFloat y) -> gen_var "Real" x(*; CLesser_than (x,
    CFloat y)*)
    | CLesser_than      (CFloat x, y) -> gen_var "Real" y(*; CLesser_than
    (CFloat x, y)*)
    | CLesser_or_equal  (x, CFloat y) -> gen_var "Real" x(*; CLesser_or_equal
    (x, CFloat y)*)
    | CLesser_or_equal  (CFloat x, y) -> gen_var "Real" y(*; CLesser_or_equal
    (CFloat x, y)*)
    | CGreater_than     (x, CFloat y) -> gen_var "Real" x(*; CGreater_than (x,
    CFloat y)*)
    | CGreater_than     (CFloat x, y) -> gen_var "Real" y(*; CGreater_than
    (CFloat x, y)*)
    | CGreater_or_equal (x, CFloat y) -> gen_var "Real" x(*; CGreater_or_equal
    (x, CFloat y)*)
    | CGreater_or_equal (CFloat x, y) -> gen_var "Real" y(*; CGreater_or_equal
    (CFloat x, y)*)
    | CAnd     (x, y) -> gen_var "Bool" x; gen_var "Bool" y(*; CAnd (x, y)*)
    | COr      (x, y) -> gen_var "Bool" x; gen_var "Bool" y(*; COr  (x, y)*)
    | CXor     (x, y) -> gen_var "Bool" x; gen_var "Bool" y(*; CXor (x, y)*)
    | CImplies (x, y) -> gen_var "Bool" x; gen_var "Bool" y(*; CImplies (x, y)*)
    | CEquiv   (x, y) -> gen_var "Bool" x; gen_var "Bool" y(*; CEquiv (x, y)*)
    | _ -> failwith "parse error"
  in
  (*
  let rec gen = function
    | CAdd              (x, CInt y) -> gen_var x; CAdd (x, CInt y) 
    | CAdd              (CInt x, y) -> gen_var y; CAdd (CInt x, y)
    | CSub              (x, CInt y) -> gen_var x; CSub (x, CInt y)
    | CSub              (CInt x, y) -> gen_var y; CSub (CInt x, y)
    | CMul              (x, CInt y) -> gen_var x; CMul (x, CInt y) 
    | CMul              (CInt x, y) -> gen_var y; CMul (CInt x, y)
    | CDiv              (x, CInt y) -> gen_var x; CDiv (x, CInt y) 
    | CDiv              (CInt x, y) -> gen_var y; CDiv (CInt x, y)
    | CEqual            (x, CInt y) -> gen_var x; CEqual (x, CInt y)
    | CEqual            (CInt x, y) -> gen_var y; CEqual (CInt x, y)
    | CNot_equal        (x, CInt y) -> gen_var x; CNot_equal (x, CInt y)
    | CNot_equal        (CInt x, y) -> gen_var y; CNot_equal (CInt x, y)
    | CLesser_than      (x, CInt y) -> gen_var x; CLesser_than (x, CInt y)
    | CLesser_than      (CInt x, y) -> gen_var y; CLesser_than (CInt x, y)
    | CLesser_or_equal  (x, CInt y) -> gen_var x; CLesser_or_equal (x, CInt y)
    | CLesser_or_equal  (CInt x, y) -> gen_var y; CLesser_or_equal (CInt x, y)
    | CGreater_than     (x, CInt y) -> gen_var x; CGreater_than (x, CInt y)
    | CGreater_than     (CInt x, y) -> gen_var y; CGreater_than (CInt x, y)
    | CGreater_or_equal (x, CInt y) -> gen_var x; CGreater_or_equal (x, CInt y)
    | CGreater_or_equal (CInt x, y) -> gen_var y; CGreater_or_equal (CInt x, y)
    | CAdd              (x, CFloat y) -> gen_var x; CAdd (x, CFloat y) 
    | CAdd              (CFloat x, y) -> gen_var y; CAdd (CFloat x, y)
    | CSub              (x, CFloat y) -> gen_var x; CSub (x, CFloat y)
    | CSub              (CFloat x, y) -> gen_var y; CSub (CFloat x, y)
    | CMul              (x, CFloat y) -> gen_var x; CMul (x, CFloat y) 
    | CMul              (CFloat x, y) -> gen_var y; CMul (CFloat x, y)
    | CDiv              (x, CFloat y) -> gen_var x; CDiv (x, CFloat y) 
    | CDiv              (CFloat x, y) -> gen_var y; CDiv (CFloat x, y)
    | CEqual            (x, CFloat y) -> gen_var x; CEqual (x, CFloat y)
    | CEqual            (CFloat x, y) -> gen_var y; CEqual (CFloat x, y)
    | CNot_equal        (x, CFloat y) -> gen_var x; CNot_equal (x, CFloat y)
    | CNot_equal        (CFloat x, y) -> gen_var y; CNot_equal (CFloat x, y)
    | CLesser_than      (x, CFloat y) -> gen_var x; CLesser_than (x, CFloat y)
    | CLesser_than      (CFloat x, y) -> gen_var y; CLesser_than (CFloat x, y)
    | CLesser_or_equal  (x, CFloat y) -> gen_var x; CLesser_or_equal (x, CFloat y)
    | CLesser_or_equal  (CFloat x, y) -> gen_var y; CLesser_or_equal (CFloat x, y)
    | CGreater_than     (x, CFloat y) -> gen_var x; CGreater_than (x, CFloat y)
    | CGreater_than     (CFloat x, y) -> gen_var y; CGreater_than (CFloat x, y)
    | CGreater_or_equal (x, CFloat y) -> gen_var x; CGreater_or_equal (x, CFloat y)
    | CGreater_or_equal (CFloat x, y) -> gen_var y; CGreater_or_equal (CFloat x, y)
    | x -> gen_var x; x
  in*)

  let rec write = function
    | Top                        -> "true"
    | Bottom                     -> "false"
    | Term              (x,None) -> sanitize_var x
    | CInt              x        -> string_of_int   x
    | CFloat x ->
        let x' = string_of_float x in
        if x'.[String.length x' - 1] = '.' then x' ^ "0" else x'
    | CNot              x        -> decl_un_op  "not" (write x)
    | CAnd              (x,y)    -> decl_bin_op "and" (write x) (write y)
    | COr               (x,y)    -> decl_bin_op "or"  (write x) (write y)
    | CXor              (x,y)    -> decl_bin_op "xor" (write x) (write y)
    | CImplies          (x,y)    -> decl_bin_op "=>" (write x) (write y)
    | CEquiv            (x,y)    -> write (CAnd (CImplies (x,y), CImplies (y,x)))
    | CAdd              (x,y)    -> decl_bin_op "+" (write x) (write y)
    | CSub              (x,y)    -> decl_bin_op "-" (write x) (write y)
    | CMul              (x,y)    -> decl_bin_op "*" (write x) (write y)
    | CDiv              (x,y)    -> decl_bin_op "/" (write x) (write y)
    | CEqual            (x,y)    -> decl_bin_op "=" (write x) (write y)
    | CNot_equal        (x,y)    -> write (CNot (CEqual (x,y)))
    | CLesser_than      (x,y)    -> decl_bin_op "<" (write x) (write y)
    | CLesser_or_equal  (x,y)    -> decl_bin_op "<=" (write x) (write y)
    | CGreater_than     (x,y)    -> decl_bin_op ">" (write x) (write y)
    | CGreater_or_equal (x,y)    -> decl_bin_op ">=" (write x) (write y)
    | _ -> failwith "error smt write"
  in
  
  (*gen_var formula;*)
  (*gen formula;*)
  parse formula;
  Hashtbl.iter (fun k v -> write_to_buf (decl_var (sanitize_var k) v)) vtbl;
  decl_assert (write formula) |> write_to_buf;
  write_to_buf "(check-sat)\n(get-value (";
  Hashtbl.iter (fun k _ -> write_to_buf (k ^ " ")) vtbl;
  write_to_buf "))";
  out
