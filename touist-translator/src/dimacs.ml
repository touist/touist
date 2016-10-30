(*
 * dimacs.ml: processes the CNF-compliant version of the abstract syntaxic tree and
 *            produces a string in DIMACS format.
 *            [to_dimacs] is the main function.
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available
 * under the terms of the GNU Lesser General Public License (LGPL)
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *)

open Syntax
open Pprint
open Minisat

(* [print_lit2str] gives a string where each like contain 'p(1,2) 98'
   where 98 is the literal id number (given automatically) of the DIMACS format
   and 'p(1,2)' is the name of the literal (given by the user).
   NOTE: you can add a prefix to 'p(1,2) 98', e.g.
     string_of_table table ~prefix:"c "
   in order to have all lines beginning by 'c' (=comment) in order to comply to
   the DIMACS format. *)
let print_lit2str (out:out_channel) (table:(Lit.t,string) Hashtbl.t) ?prefix:(prefix="") =
  let print_lit_and_name lit name = Printf.fprintf out "%s %d\n" name (Lit.to_int lit)
  in Hashtbl.iter print_lit_and_name table

(* [minisatclauses_of_cnf] translates the expression into an instance of
   list of list of literals. [[clause1],[clause2]...] *)
let minisatclauses_of_cnf (ast:ast) : Lit.t list list * (Lit.t,string) Hashtbl.t * int = (* int = nb of literals *)
  (* num = a number that will serve to identify a literal
     lit = a literal that has a number inside it to identify it *)
  let str_to_lit = Hashtbl.create 500 in
  let lit_to_str = Hashtbl.create 500 in (* this duplicate is for the return value *)
  let num_lit = ref 1 in
  let rec process_cnf ast : Minisat.Lit.t list list = match ast with
    | And  (x,y) -> (process_cnf x) @ (process_cnf y)
    | x when Cnf.is_clause x -> begin 
        match process_clause x with
        | [] -> let lit=(gen_lit ("&bot"^(string_of_int !num_lit))) 
          in [[lit]] @ [[Lit.neg lit]]
        (* This [] is caused by Bottom that was alone in the clause. We must 
           trigger the Unsat case; to do so, we add '&bot12 and not &bot12'*)
        | x -> [x]
      end
    | _ -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_ast ast) ^ "'")
  and process_clause (ast:ast) : Minisat.Lit.t list = match ast with
    | Term (str, None)        -> (gen_lit str)::[]
    | Not (Term (str, None)) -> (Minisat.Lit.neg (gen_lit str))::[]
    | Bottom -> [] (* if Bot is the only one in the clause, then the whole formula is false *)
    | Top -> (* The clause shouldn't be added because Top is found. Instead of
                not adding the clause, we translate it by '&top12 or not &top12'*)
      let lit=(gen_lit ("&top"^(string_of_int !num_lit))) in lit::(Lit.neg lit)::[]
    | Or (x,y) ->
        (match process_clause x,process_clause y with 
          | [],x | x,[] -> x (* [] is created by Bottom; remove [] as soon as another literal exists in the clause *)
          | x,y -> x @ y)
    | _ -> failwith ("CNF: was expecting a clause but got '" ^ (string_of_ast ast) ^ "'")
  and gen_lit (s:string) : Lit.t =
    try Hashtbl.find str_to_lit s
    with Not_found -> 
      (let lit = Minisat.Lit.make !num_lit in
        Hashtbl.add str_to_lit s lit; Hashtbl.add lit_to_str lit s;
        incr num_lit;
        lit)
  in let clauses = process_cnf ast in clauses, lit_to_str, !num_lit - 1

let instance_of_minisatclauses (clauses:Lit.t list list) : Minisat.t * bool =
  let inst = Minisat.create () in
  let rec add_clauses inst (l:Lit.t list list) : bool =
    match l with
    | [] -> true
    | cur::next -> (Minisat.Raw.add_clause_a inst (Array.of_list cur)) && (add_clauses inst next)
  in let has_next_model = add_clauses inst clauses
  in inst, has_next_model

let dimacs_of_minisatclauses (out:out_channel) nblits (clauses:Lit.t list list) : unit =
  let nbclauses = List.length clauses in
  let rec string_of_clause (cl:Lit.t list) = match cl with
    | [] -> "0"
    | cur::next -> (Lit.to_string cur) ^" "^ (string_of_clause next)
  and print_listclause (cl:Lit.t list list) = match cl with
    | [] -> ()
    | cur::next -> Printf.fprintf out "%s\n" (string_of_clause cur); print_listclause next
  in Printf.fprintf out "c CNF format file\np cnf %d %d\n" nblits nbclauses;
  print_listclause clauses
  

(* for printing the Minisat.value type *)
let string_of_value = function
  | V_true -> "1"
  | V_false -> "0"
  | _ -> "?"

(* A container for remembering a model *)
module Model =
struct
  type t = (Minisat.Lit.t * Minisat.value) list
  let compare l1 l2 = Pervasives.compare l1 l2
  (* [dump] gives a string under the form (0,1)(1,2)(1,3)(0,4)... *)
  let dump l = List.fold_left (fun acc x -> match x with a,b -> ("("^(Lit.to_string a) ^ "," ^ (string_of_value b) ^ ")" ^ acc)) "" l
  (* [pprint] gives a string under the form
     1 prop(1,2,9)
     O prop(1,4,2)... *)
  let pprint table model = List.fold_left (fun acc x -> match x with a,b -> ((Hashtbl.find table a) ^ " " ^ (string_of_value b) ^ "\n" ^ acc)) "" model
  (* [model_of_instance] retrieves the valuations from a current 'solve' of a
     Minisat.t and put them into a Model. *)
  let model_of_instance instance (table:(Lit.t,string) Hashtbl.t) =
    let model (lit:Lit.t) name acc = match name.[0] with
    | '&' -> acc | _ -> (lit, Minisat.value instance lit)::acc
    in let model = (Hashtbl.fold model table []) in
    model
end

(* A set that contains all the models already found. *)
module ModelSet = struct
  include Set.Make(Model)
  let dump models = print_endline (fold (fun m acc -> (Model.dump m) ^ "\n" ^ acc) models "")
  let pprint table models = print_endline (fold (fun m acc -> (Model.pprint table m) ^ "=====\n" ^ acc) models "")
end

(* 1. Prevent current model from reappearing
   =========================================
   We must prevent the current model to reappear in future models;
   to do so, we add a clause that take the negation of the valuations
   E.g: with the model a=1, b=0 we must add the clause -a or b.
   [counter_clause] will produce a list of literals that corresponds
   to this clause. [counter_current_model] then adds the clause to the problem.
   IMPORTANT: When adding the counter-clause, the problem can become unsat.
   [counter_current_model] returns false if the added clause makes the
   formula unsat right away. *)

(* 2. Avoid duplicates caused by fake literals (of the form '&6')
   ==============================================================
   Our issue here: the models contain fake '&12' literals. We don't
   want to see these fake literals in our models; we also want to
   remove the duplicate models linked to these fake literals.
   To avoid those duplicates, we store the models (without the fake
   literals) in a set.

   To use this function, you need a ModelSet ref already initialized, e.g. with
    let models = ref ModelSet.empty *)

(* 3. Fetch the models
   ===================
   Basically, we
    (1) compute a model with Minisat.solve
    (2) check if we already saw this model (duplicates because of &23 literals)
    (3) prevent the same model from reappearing in adding the clause where all
        lits are the negation of the valuation in the model
    (4) go on with (1)
*)
(* limit is the limit of number of models you allow to be fetched.
   When limit = 0, all models will be fetched. *)
let count = ref 0
let find_models ?limit:(limit=0) cnf : (Lit.t,string) Hashtbl.t * (ModelSet.t ref) =
  let counter_current_model instance (table:(Lit.t,string) Hashtbl.t) : bool =
    let counter_clause (l:Lit.t) _ acc = match Minisat.value instance l with
      | V_true -> (Minisat.Lit.neg l)::acc | V_false -> l::acc | _ -> acc
    in let counter_clause = Hashtbl.fold counter_clause table []
    in Minisat.Raw.add_clause_a instance (Array.of_list counter_clause)
  in
  let clauses,table,_ = minisatclauses_of_cnf cnf in
  let instance,_ = instance_of_minisatclauses clauses in
  let models = ref ModelSet.empty in (* for returning the models found *)
  (* searching for duplicate is slow on ModelSet. For checking a model hasn't
     appeared already, I use a way faster Hashtbl, ass it won't check on every
     single literal but compute a hash of the model) *)
  let models_hash = (Hashtbl.create 100) in
  let rec solve_loop limit i =
    if not (i<limit || limit==0) (* limit=0 means no limit *)
    || not (Minisat.Raw.simplify instance)
    || not (Minisat.Raw.solve instance [||])
    then models
    else
      let model = Model.model_of_instance instance table (* no &1 literals in it *)
      and has_next_model = counter_current_model instance table in
      let is_duplicate = Hashtbl.mem models_hash model in 
      match is_duplicate,has_next_model with
      | true,false -> models (* is duplicate and no next model *)
      | true,true  -> solve_loop limit i (* is duplicate but has next *)
      | false, true ->  (* both not duplicate and has next *)
        models := ModelSet.add model !models;
        Hashtbl.add models_hash model ();
        solve_loop  limit (i+1)
      | false, false -> (* is not duplicate and no next model *)
        models := ModelSet.add model !models;
        models
  in table, solve_loop limit 0
