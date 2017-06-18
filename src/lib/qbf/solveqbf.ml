open Qbf
open Qbf.Formula
open Qbf.QFormula
open Types.Ast


(* [ocamlqbf_of_ast] transforms a touist quantified formula to a Qbf.QFormula.
   The formula does no need to be in cnf. After calling this function, you
   should use [QFormula.cnf].
   This function had been written before [Qbf_of_ast.cnf] existed, this is why
   I wrote a second function [qcnf_of_cnf] which allows me to use my own
   CNF function (i.e., [Qbf_of_ast.cnf]). *)
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
    | wrong -> failwith ("[shouldnt happen] when applying 'of_ast', formula still had quantors: "^Pprint.string_of_ast ~debug:true wrong)
  in let ocamlqbf = of_quant_ast ast
  in (simplify ocamlqbf,lit_to_str)

(* [qcnf_of_cnf] does the same as [ocamlqbf_of_ast] except that its input ast
   needs to be already in CNF form.
   I should have skipped the call to Qbf.QFormula.cnf (as the formula is already
   in cnf, it is useless) and instead I should transform from ast to QCNF but
   you know... I already had done [ocamlqbf_of_ast], so I didn't bother... *)
let rec qcnf_of_cnf (cnf_ast:ast) : Qbf.QCNF.t * (Qbf.Lit.t,string) Hashtbl.t =
  let qformula,table = ocamlqbf_of_ast cnf_ast in
  let qcnf = Qbf.QFormula.cnf qformula in
  qcnf,table


let string_of_assign (assign:assignment) : string = match assign with
  | True -> "1"
  | False -> "0"
  | Undef -> "?"

(* hidden=false allows to display the tseitlin-added variables. *)
let solve ?(hidden=false) (f,table:QCNF.t * (Qbf.Lit.t,string) Hashtbl.t) : string option =
  let res = solve Quantor.solver f
  in match res with
  | Unknown -> failwith "the quantor solver returned an unknown error"
  | Timeout -> failwith "the quantor solver took too much time to solve"
  | Spaceout -> failwith "the quantor solver ran out of memory"
  | Unsat -> None
  | Sat assign ->
    Some (Hashtbl.fold
      (fun lit name acc -> if not hidden && (String.get name 0) = '&' then acc
        else (if acc="" then "" else acc^"\n")^(assign lit |> string_of_assign)^" "^ name) table "")