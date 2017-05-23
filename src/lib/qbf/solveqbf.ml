open Qbf
open Qbf.Formula
open Qbf.QFormula
open Types.Ast

let rec ocamlqbf_of_ast (ast:ast) : Qbf.QFormula.t * (Qbf.Lit.t,string) Hashtbl.t =
  let str_to_lit = Hashtbl.create 500 in
  let lit_to_str = Hashtbl.create 500 in (* this duplicate is for the return value *)
  let add_lit (name:string) : Qbf.Lit.t =
    let lit = Lit.fresh () in
    try Hashtbl.find str_to_lit name
    with Not_found ->
    (Hashtbl.add str_to_lit name lit; Hashtbl.add lit_to_str lit name);
    lit
  in
  let rec of_quant_ast (ast:ast) : QFormula.t = match ast with
    | Exists (Prop name, f) -> exists [add_lit name] (if Qbf_of_ast.is_unquant f then prop (of_ast f) else of_quant_ast f)
    | Forall (Prop name, f) -> forall [add_lit name] (if Qbf_of_ast.is_unquant f then prop (of_ast f) else of_quant_ast f)
    | unquant -> prop (of_ast unquant)
  and of_ast = function
    | Prop x        -> atom (add_lit x)
    | Top           -> true_
    | Bottom        -> false_
    | Not f         -> neg (of_ast f)
    | And     (f,g) -> and_l [of_ast f; of_ast g]
    | Or      (f,g) -> or_l [of_ast f; of_ast g]
    | Xor     (f,g) -> xor_l [of_ast f; of_ast g]
    | Implies (f,g) -> imply (of_ast f) (of_ast g)
    | Equiv   (f,g) -> equiv_l [of_ast f; of_ast g]
    | wrong -> failwith ("[shouldnt happen] when applying 'of_ast', formula still had quantors: "^Pprint.string_of_ast wrong)
  in let ocamlqbf = of_quant_ast ast
  in (simplify ocamlqbf,lit_to_str)

let string_of_assign (assign:assignment) : string = match assign with
  | True -> "1"
  | False -> "0"
  | Undef -> "?"

let solve (f,table:QFormula.t * (Qbf.Lit.t,string) Hashtbl.t) =
  let qcnf = QFormula.cnf f in
  let res = solve Quantor.solver qcnf
  in match res with
  | Unknown
  | Timeout
  | Spaceout -> failwith "error"
  | Unsat -> print_endline "unsat"
  | Sat assign -> print_endline (Hashtbl.fold (fun lit name acc -> (if acc="" then "" else acc^"\n")^ (assign lit |> string_of_assign) ^" "^ name) table "")