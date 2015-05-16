open Syntax

let to_smt2 logic formula =
  let vtbl = Hashtbl.create 10 in
  let out  = Buffer.create 1000 in
  Buffer.add_string out ("(set-logic " ^ logic ^ ")\n");
 
  let write_to_buf = Buffer.add_string out in

  let add_var name typ =
    if not (Hashtbl.mem vtbl name) then
      Hashtbl.add vtbl name typ
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
      name
    with Not_found -> name
  in
  
  let rec gen_var = function
    | Term (x,None) -> add_var (sanitize_var x) "Bool"
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
    | CNot_equal        (CFloat _, Term (x,None)) -> add_var x "Real"
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
            with Not_found -> failwith ("unknown type: " ^ x ^ ", " ^ y)
        end
    | CNot x -> gen_var x
    | CAnd     (x,y)
    | COr      (x,y)
    | CXor     (x,y)
    | CImplies (x,y)
    | CEquiv   (x,y) -> gen_var x; gen_var y
    | _ -> ()
  in

  let rec write = function
    | Top                        -> "true"
    | Bottom                     -> "false"
    | Term              (x,None) -> sanitize_var x
    | CInt              x        -> string_of_int   x
    | CFloat            x        -> string_of_float x
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
  
  gen_var formula;
  Hashtbl.iter (fun k v -> write_to_buf (decl_var k v)) vtbl;
  decl_assert (write formula) |> write_to_buf;
  write_to_buf "(check-sat)\n(get-value (";
  Hashtbl.iter (fun k _ -> write_to_buf (k ^ " ")) vtbl;
  write_to_buf "))";
  out
