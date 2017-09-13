open Qbf
open Qbf.Formula
open Qbf.QFormula
open TouistTypes
open TouistTypes.Ast
open TouistQbf

let depqbf_clauses_of_cnf ast =
  let quants, inner = TouistQbf.regroup_quantors ast [] in
  let clauses,lit_to_str,str_to_lit = TouistCnf.clauses_of_cnf Qbf.Lit.neg Qbf.Lit.fresh inner in
  let quantlist = quants |> List.fold_left (fun acc lst ->
      let res = match lst with
        | TouistQbf.A l -> TouistQbf.A (List.fold_right (fun p acc -> Hashtbl.find str_to_lit p :: acc) l [])
        | TouistQbf.E l -> TouistQbf.E (List.fold_right (fun p acc -> Hashtbl.find str_to_lit p :: acc) l [])
      in res::acc
    ) []
  in List.rev quantlist, clauses, lit_to_str

let string_of_assign (assign:assignment) : string = match assign with
  | True -> "1"
  | False -> "0"
  | Undef -> "?"

let depqbf_of_cnf ast =
  let d = Depqbf.create () in
  let quantlist, clauses, lit_to_str = depqbf_clauses_of_cnf ast in
  quantlist |> List.iter (fun ql ->
      let lst = match ql with
        | A l -> Depqbf.new_scope d Qbf.Forall |> ignore; l
        | E l -> Depqbf.new_scope d Qbf.Exists |> ignore; l
      in
      lst |> List.iter (fun q -> Depqbf.add d q); Depqbf.add0 d);
  clauses |> List.iter (fun lst ->
      lst |> List.iter (fun v -> Depqbf.add d v); Depqbf.add0 d);
  d, lit_to_str

let solve_depqbf ?(hidden=false) ast =
  let depqbf, tbl = depqbf_of_cnf ast in
  Depqbf.configure depqbf "--no-dynamic-nenofex";
  match Depqbf.check depqbf with
  | Unknown -> failwith "the depqbf solver returned an unknown error"
  | Timeout -> failwith "the depqbf solver took too much time to solve"
  | Spaceout -> failwith "the depqbf solver ran out of memory"
  | Unsat -> None
  | Sat assign ->
    Some (Hashtbl.fold
            (fun lit name acc -> if not hidden && (String.get name 0) = '&' then acc
              else (if acc="" then "" else acc^"\n")^(assign lit |> string_of_assign)^" "^ name) tbl "")