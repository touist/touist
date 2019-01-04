open Touist.Types
open Touist.Types.Ast
open Yices2

let ast_to_yices formula : term * (string,term) Hashtbl.t =
  let vtbl = Hashtbl.create 10 in
  let add_term name term =
    if not (Hashtbl.mem vtbl name) then
      Hashtbl.replace vtbl name term
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

  (* 'above' means the type that is expected from the expression that
     is recursively above (or 'before'). *)
  let rec process_terms above_type = function
    | Prop x -> add_term x (Term.new_uninterpreted above_type)
    | Add              (Prop x, Int _)
    | Add              (Int _, Prop x)
    | Sub              (Prop x, Int _)
    | Sub              (Int _, Prop x)
    | Mul              (Prop x, Int _)
    | Mul              (Int _, Prop x)
    | Div              (Prop x, Int _)
    | Div              (Int _, Prop x)
    | Lesser_than      (Prop x, Int _)
    | Lesser_than      (Int _, Prop x)
    | Lesser_or_equal  (Prop x, Int _)
    | Lesser_or_equal  (Int _, Prop x)
    | Greater_than     (Prop x, Int _)
    | Greater_than     (Int _, Prop x)
    | Greater_or_equal (Int _, Prop x)
    | Greater_or_equal (Prop x, Int _)
    | Equal            (Prop x, Int _)
    | Equal            (Int _, Prop x)
    | Not_equal        (Prop x, Int _)
    | Not_equal        (Int _, Prop x) -> add_term x (Term.new_uninterpreted (Type.int ()))
    | Add              (Prop x, Float _)
    | Add              (Float _, Prop x)
    | Sub              (Prop x, Float _)
    | Sub              (Float _, Prop x)
    | Mul              (Prop x, Float _)
    | Mul              (Float _, Prop x)
    | Div              (Prop x, Float _)
    | Div              (Float _, Prop x)
    | Lesser_than      (Prop x, Float _)
    | Lesser_than      (Float _, Prop x)
    | Lesser_or_equal  (Prop x, Float _)
    | Lesser_or_equal  (Float _, Prop x)
    | Greater_than     (Prop x, Float _)
    | Greater_than     (Float _, Prop x)
    | Greater_or_equal (Prop x, Float _)
    | Greater_or_equal (Float _, Prop x)
    | Equal            (Prop x, Float _)
    | Equal            (Float _, Prop x)
    | Not_equal        (Prop x, Float _)
    | Not_equal        (Float _, Prop x) -> add_term x (Term.new_uninterpreted (Type.real ()))
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
          let x_type = Type.of_term (Hashtbl.find vtbl x) in
          add_term y (Term.new_uninterpreted (x_type))
        with Not_found ->
        try
          let y_type = Type.of_term (Hashtbl.find vtbl y) in
          add_term x (Term.new_uninterpreted (y_type))
        with Not_found ->
          add_term x (Term.new_uninterpreted (above_type));
          add_term y (Term.new_uninterpreted (above_type))
      end
    | Not x -> process_terms above_type x
    | And     (x,y)
    | Or      (x,y)
    | Xor     (x,y)
    | Implies (x,y)
    | Equiv   (x,y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y
    | Add              (x, Int _)
    | Add              (Int _, x)
    | Sub              (x, Int _)
    | Sub              (Int _, x)
    | Mul              (x, Int _)
    | Mul              (Int _, x)
    | Div              (x, Int _)
    | Div              (Int _, x)
    | Equal            (x, Int _)
    | Equal            (Int _, x)
    | Not_equal        (x, Int _)
    | Not_equal        (Int _, x)
    | Lesser_than      (x, Int _)
    | Lesser_than      (Int _, x)
    | Lesser_or_equal  (x, Int _)
    | Lesser_or_equal  (Int _, x)
    | Greater_than     (x, Int _)
    | Greater_than     (Int _, x)
    | Greater_or_equal (x, Int _)
    | Greater_or_equal (Int _, x)->
      let rec go = function
        | Prop x -> add_term x (Term.new_uninterpreted (Type.int ()))
        | Add (x,y)
        | Sub (x,y)
        | Mul (x,y)
        | Div (x,y) -> go x; go y
        | _ -> failwith "not a term exp"
      in
      if term_expr x then go x else ()
    | Add              (x, Float _)
    | Add              (Float _, x)
    | Sub              (x, Float _)
    | Sub              (Float _, x)
    | Mul              (x, Float _)
    | Mul              (Float _, x)
    | Div              (x, Float _)
    | Div              (Float _, x)
    | Equal            (x, Float _)
    | Equal            (Float _, x)
    | Not_equal        (x, Float _)
    | Not_equal        (Float _, x)
    | Lesser_than      (x, Float _)
    | Lesser_than      (Float _, x)
    | Lesser_or_equal  (x, Float _)
    | Lesser_or_equal  (Float _, x)
    | Greater_than     (x, Float _)
    | Greater_than     (Float _, x)
    | Greater_or_equal (x, Float _)
    | Greater_or_equal (Float _, x) ->
      let rec go = function
        | Prop x -> add_term x (Term.new_uninterpreted (Type.real ()))
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
    | Greater_or_equal (x, y) -> process_terms above_type x; process_terms above_type y
    | _ -> ()
  in
  let parse = function
    | Equal            (x, Int _) -> process_terms (Type.int ()) x
    | Equal            (Int _, y) -> process_terms (Type.int ()) y
    | Not_equal        (x, Int _) -> process_terms (Type.int ()) x
    | Not_equal        (Int _, y) -> process_terms (Type.int ()) y
    | Lesser_than      (x, Int _) -> process_terms (Type.int ()) x
    | Lesser_than      (Int _, y) -> process_terms (Type.int ()) y
    | Lesser_or_equal  (x, Int _) -> process_terms (Type.int ()) x
    | Lesser_or_equal  (Int _, y) -> process_terms (Type.int ()) y
    | Greater_than     (x, Int _) -> process_terms (Type.int ()) x
    | Greater_than     (Int _, y) -> process_terms (Type.int ()) y
    | Greater_or_equal (x, Int _) -> process_terms (Type.int ()) x
    | Greater_or_equal (Int _, y) -> process_terms (Type.int ()) y
    | Equal            (x, Float _) -> process_terms (Type.real ()) x
    | Equal            (Float _, y) -> process_terms (Type.real ()) y
    | Not_equal        (x, Float _) -> process_terms (Type.real ()) x
    | Not_equal        (Float _, y) -> process_terms (Type.real ()) y
    | Lesser_than      (x, Float _) -> process_terms (Type.real ()) x
    | Lesser_than      (Float _, y) -> process_terms (Type.real ()) y
    | Lesser_or_equal  (x, Float _) -> process_terms (Type.real ()) x
    | Lesser_or_equal  (Float _, y) -> process_terms (Type.real ()) y
    | Greater_than     (x, Float _) -> process_terms (Type.real ()) x
    | Greater_than     (Float _, y) -> process_terms (Type.real ()) y
    | Greater_or_equal (x, Float _) -> process_terms (Type.real ()) x
    | Greater_or_equal (Float _, y) -> process_terms (Type.real ()) y
    | And     (x, y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y(*; And (x, y)*)
    | Or      (x, y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y(*; Or  (x, y)*)
    | Xor     (x, y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y(*; Xor (x, y)*)
    | Implies (x, y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y(*; Implies (x, y)*)
    | Equiv   (x, y) -> process_terms (Type.bool ()) x; process_terms (Type.bool ()) y(*; Equiv (x, y)*)
    | Prop x -> add_term x (Term.new_uninterpreted (Type.bool ()))
    | x -> failwith ("this cannot be transformed into SMT2: "^(Touist.Pprint.string_of_ast ~debug:true x))
  in
  let rec write (ast:Ast.t) : term = match ast with
    | Top          -> Term.Bool.true_ ()
    | Bottom       -> Term.Bool.false_ ()
    | Prop x       -> Hashtbl.find vtbl x
    | Int x        -> Term.Int.of_int x
    | Float x      -> Term.Ratio.parse_float (string_of_float x) (* TODO this is dumb... *)
    | Not              x        -> Term.Bool.not (write x)
    | And              (x,y)    -> Term.Bool.and2 (write x) (write y)
    | Or               (x,y)    -> Term.Bool.or2  (write x) (write y)
    | Xor              (x,y)    -> Term.Bool.xor2 (write x) (write y)
    | Implies          (x,y)    -> Term.Bool.implies (write x) (write y)
    | Equiv            (x,y)    -> Term.Bool.iff (write x) (write y)
    | Neg              (Int x)  -> Term.Int.of_int (-x)
    | Neg              (Float x)-> Term.Ratio.parse_float (string_of_float (-.x))
    | Add              (x,y)    -> Term.Arith.add (write x) (write y)
    | Sub              (x,y)    -> Term.Arith.sub (write x) (write y)
    | Mul              (x,y)    -> Term.Arith.mul (write x) (write y)
    | Div              (x,y)    -> Term.Arith.div (write x) (write y)
    | Equal            (x,y)    -> Term.Arith.eq (write x) (write y)
    | Not_equal        (x,y)    -> Term.Arith.neq (write x) (write y)
    | Lesser_than      (x,y)    -> Term.Arith.lt (write x) (write y)
    | Lesser_or_equal  (x,y)    -> Term.Arith.leq (write x) (write y)
    | Greater_than     (x,y)    -> Term.Arith.gt (write x) (write y)
    | Greater_or_equal (x,y)    -> Term.Arith.geq (write x) (write y)
    | Loc (x,_) -> write x
    | x -> failwith ("error smt write: "^(Touist.Pprint.string_of_ast ~debug:true x))
  in
  parse formula;
  (write formula), vtbl

let string_of_modelterm model term = match term with
  | x when Term.is_bool x -> let b = Model.get_bool model x in if b then "1" else "0"
  | x when Term.is_int x -> string_of_int (Model.get_int model x)
  | x when Term.is_real x -> string_of_float (Model.get_float model x)
  | _ -> failwith "cannot output the value of the term"

let string_of_model ?(value_sep="\n") vtbl model =
  Hashtbl.fold (fun name term acc ->
      acc ^ (match acc with ""->"" | _->value_sep) ^ (string_of_modelterm model term) ^" "^ name) vtbl ""

let solve (logic: string) (formula: Yices2.term) : Yices2.model option =
  let config = Context.Config.create () in
  Context.Config.default_for_logic config logic;
  let ctx = Context.create ~config () in
  Context.assert_formula ctx formula;
  match Context.check ctx with
  | SAT -> Some (Context.get_model ctx)
  | UNSAT -> None
  | s -> failwith ("the status of the yices solver is unexpected: "^string_of_status s)

let logic_supported (logic:string) =
  try Context.Config.default_for_logic (Context.Config.create ()) logic; true
  with YicesError (Error.CTX_UNKNOWN_LOGIC,_) -> false
     | YicesError (Error.CTX_LOGIC_NOT_SUPPORTED,_) -> false
     | YicesError (_code,report) ->
       failwith ("[shouldnt happen] instead of returning CTX_UNKNOWN_LOGIC,"^
                 "code returned was "^ report.Error.name)

let () = Yices2.register_exn ()

let enabled = true