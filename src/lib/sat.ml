 (** Processes the CNF-compliant version of the abstract syntaxic tree given by [Cnf.ast_to_cnf]
     and produces a string in DIMACS format.
     
     [cnf_to_clauses] is the main function. *)

(**
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

open Types.Ast
open Pprint
open Minisat

(* [print_table] prints the correspondance table between literals (= numbers)
   and user-defined proposition names, e.g.,
       'p(1,2) 98'
   where 98 is the literal id number (given automatically) and 'p(1,2)' is the
   name of this proposition.
   NOTE: you can add a prefix to 'p(1,2) 98', e.g.
     string_of_table ~prefix:"c " table
   in order to have all lines beginning by 'c' (=comment) in order to comply to
   the DIMACS format. *)
let print_table (out:out_channel) ?(prefix="") (table:(Lit.t,string) Hashtbl.t) =
  let print_lit_and_name lit name = Printf.fprintf out "%s%s %d\n" prefix name (Lit.to_int lit)
  in Hashtbl.iter print_lit_and_name table

(* [cnf_to_clauses] translates the cnf into a list of clauses (clause = list of
   literals). Returns
   - the list of clauses,
   - the table literal-to-name (Lit.t doesn't store the proposition name)
   Note that the total number of literals is exactly equal to the table size. *)
let cnf_to_clauses (ast:ast) : Lit.t list list * (Lit.t,string) Hashtbl.t =
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
    | _ -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_ast ~debug:true ast) ^ "'")
  and process_clause (ast:ast) : Minisat.Lit.t list = match ast with
    | Prop str        -> (gen_lit str)::[]
    | Not (Prop str) -> (Minisat.Lit.neg (gen_lit str))::[]
    | Bottom -> [] (* if Bot is the only one in the clause, then the whole formula is false *)
    | Top -> (* The clause shouldn't be added because Top is found. Instead of
                not adding the clause, we translate it by '&top12 or not &top12'*)
      let lit=(gen_lit ("&top"^(string_of_int !num_lit))) in lit::(Lit.neg lit)::[]
    | Or (x,y) ->
        (match process_clause x,process_clause y with 
          | [],x | x,[] -> x (* [] is created by Bottom; remove [] as soon as another literal exists in the clause *)
          | x,y -> x @ y)
    | _ -> failwith ("CNF: was expecting a clause but got '" ^ (string_of_ast ~debug:true ast) ^ "'")
  and gen_lit (s:string) : Lit.t =
    try Hashtbl.find str_to_lit s
    with Not_found -> 
      (let lit = Minisat.Lit.make !num_lit in
        Hashtbl.add str_to_lit s lit; Hashtbl.add lit_to_str lit s;
        incr num_lit;
        lit)
  in let clauses = process_cnf ast in clauses, lit_to_str

(* [clauses_to_solver] takes a list of clauses (clause = list of literals)
   and generates an intance of minisat solver. *)
let clauses_to_solver (clauses:Lit.t list list) : Minisat.t * bool =
  let solver = Minisat.create () in
  let rec add_clauses solver (l:Lit.t list list) : bool =
    match l with
    | [] -> true
    | cur::next -> (Minisat.Raw.add_clause_a solver (Array.of_list cur)) && (add_clauses solver next)
  in let has_next_model = add_clauses solver clauses
  in solver, has_next_model

let print_clauses_to_dimacs (out:out_channel) nblits (clauses:Lit.t list list) : unit =
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
  let dump l = List.fold_left (fun acc x -> match x with l,v -> ("("^(string_of_value v) ^ "," ^ (Lit.to_string l) ^ ")" ^ acc)) "" l
  (* [pprint] gives a string under the form
     1 prop(1,2,9)
     O prop(1,4,2)... *)
  let pprint ?(sep="\n") table model = List.fold_left 
    (fun acc (n,v) -> let str = (string_of_value v)^" "^(Hashtbl.find table n)
      in match acc with "" -> str | _ -> str ^ sep ^ acc)
     "" model
end

(* A set that contains all the models already found. *)
module ModelSet = struct
  include Set.Make(Model)
  let dump models = fold (fun m acc -> (Model.dump m) ^ "\n" ^ acc) models ""
  let pprint table models = fold (fun m acc -> (Model.pprint table m) ^ "=====\n" ^ acc) models ""
end

(* [get_model] retrieves the valuations from the current state of the solver
   and put them into a Model.t.
   [discard] must return true if the literal that is mapped to the given
   proposition name shouldn't be in the model. *)
let get_model solver (table:(Lit.t,string) Hashtbl.t) (discard:string->bool): Model.t =
  Hashtbl.fold (fun lit name acc -> 
      if not (discard name) then (lit,Minisat.value solver lit)::acc else acc) 
    table []

(* [is_dummy] filters the 'dummy' literals that were introduced during
   cnf conversion; these literals are identified by their prefix '&'.
   Returns true if the given name corresponds to is a dummy literal *)
let is_dummy (name:string) : bool = name.[0] = '&'

(* [print_clause] dumps the clause in its literal-number form:
   e.g., 1 -5 3 9 -2 -7 *)
let print_clause (clause:Lit.t list) : unit =
  let str = List.fold_left (fun acc lit -> (Lit.to_string lit) ^" "^ acc) "" clause
  in Printf.fprintf stderr "%s\n=====\n" str

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

(** [solve_clauses] finds the models for the given clauses. 
    @param print is a function that will print a model as soon as it is found.
      [i] is the number of the model.
      It can be useful to print the models as they appear because finding all
      models (if [limit] is large) can be extremely long.
      Example: [~print:(Sat.Model.pprint table model)]
    @param limit is the limit of number of models you allow to be fetched.
      When limit = 0, all models will be fetched. *)
let solve_clauses
    ?(print: Model.t -> int -> unit = fun m i ->())
    ?(limit: int = 0)
    (clauses,table : Lit.t list list * (Lit.t,string) Hashtbl.t)
  : (ModelSet.t ref) =
  let counter_current_model solver (table:(Lit.t,string) Hashtbl.t) : bool =
    let counter_clause (l:Lit.t) _ acc = match Minisat.value solver l with
      | V_true -> (Minisat.Lit.neg l)::acc | V_false -> l::acc | _ -> acc
    in let counter_clause = Hashtbl.fold counter_clause table []
    in Minisat.Raw.add_clause_a solver (Array.of_list counter_clause)
  in
  let solver,_ = clauses_to_solver clauses in
  let models = ref ModelSet.empty in (* for returning the models found *)
  (* searching for duplicate is slow on ModelSet. For checking a model hasn't
     appeared already, I use a way faster Hashtbl, ass it won't check on every
     single literal but compute a hash of the model) *)
  let models_hash = (Hashtbl.create 100) in
  let rec solve_loop limit i =
    if not (i<limit || limit==0) (* limit=0 means no limit *)
    || not (Minisat.Raw.simplify solver)
    || not (Minisat.Raw.solve solver [||])
    then models
    else
      let model = get_model solver table (is_dummy) (* is_dummy removes &1 lits *)
      and has_next_model = counter_current_model solver table in
      let is_duplicate = Hashtbl.mem models_hash model in
      match is_duplicate,has_next_model with
      | true,false -> models (* is duplicate and no next model *)
      | true,true  -> solve_loop limit i (* is duplicate but has next *)
      | false, true ->  (* both not duplicate and has next *)
        models := ModelSet.add model !models; print model i;
        Hashtbl.add models_hash model ();
        solve_loop limit (i+1)
      | false, false -> (* is not duplicate and no next model *)
        models := ModelSet.add model !models; print model i;
        models
  in solve_loop limit 0

(** [print_solve] outputs the result of the solver. But it is much more recommanded
    to use the parameter '~print_model' of [solve_clauses] in order to output the
    models as soon as they are found; if looking for a large number of models,
    [print_solve] will have to wait [solve_clauses] is done.

    @param output is the [out_channel] you want to solutions to be written into.
    @param show_hidden indicates that the hidden literals introduced during 
      [ast_to_cnf].

    CNF conversion should be shown. *)
let print_solve ?(show_hidden=false) output (solver:Minisat.t) (table:(string, Minisat.Lit.t) Hashtbl.t) =
  let string_of_value solver (lit:Minisat.Lit.t) = match Minisat.value solver lit with
    | V_true -> "1" | V_false -> "0" | V_undef -> "?"
  in let print_value_and_name name lit = if show_hidden || name.[0] != '&'
       then Printf.fprintf output "%s %s\n" (string_of_value solver lit) name
  in Hashtbl.iter print_value_and_name table