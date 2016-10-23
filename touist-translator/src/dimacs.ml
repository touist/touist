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

let to_dimacs (prop:Syntax.exp) : string * (string,int) Hashtbl.t =
  let table    = Hashtbl.create 10
  and num_sym  = ref 1
  and nbclause = ref 0 in
  let rec go acc = function
    | Top -> failwith "Clause is always true"
    | Bottom -> failwith "Clause is alway false"
    | Term (x, None)        -> acc ^ string_of_int (gensym x)
    | Term (x, _)           -> failwith ("unevaluated term: " ^ x)
    | Not (Term (x, None)) -> acc ^ string_of_int (- (gensym x))
    | And (x, y) -> incr nbclause; (go acc x) ^ " 0\n" ^ (go acc y)
    | Or  (x, y) -> (go acc x) ^ " " ^ (go acc y)
    | _ -> failwith "non CNF clause"
  and gensym x =
    try Hashtbl.find table x
    with Not_found ->
      let n = !num_sym in
      Hashtbl.add table x n; incr num_sym; n
  in
  let str = (go "" prop) ^ " 0\n" in
  let header =
    "c CNF format file\np cnf " ^ string_of_int (Hashtbl.length table)
                                ^ " "
                                ^ string_of_int (!nbclause+1)
                                ^ "\n"
  in
  header ^ str, table

let to_text prop =
  let table    = Hashtbl.create 10
  and nbclause = ref 0 in
  let rec go acc = function
    | Top -> (*failwith "Clause is always true"*) "VTop"
    | Bottom -> (*failwith "Clause is alway false"*) "VBot"
    | Term (x, None)        -> acc ^ x
    | Term (x, _)           -> failwith ("unevaluated term: " ^ x)
    | Not (Term (x, None)) -> acc ^ "-" ^ x
    | And (x, y) -> incr nbclause; (go acc x) ^ " \n" ^ (go acc y)
    | Or  (x, y) -> (go acc x) ^ " v " ^ (go acc y)
    | _ -> failwith "non CNF clause"
  in
  let str = (go "" prop) ^ " 0\n" in
  let header =
    "c CNF format file\np cnf " ^ string_of_int (Hashtbl.length table)
                                ^ " "
                                ^ string_of_int (!nbclause+1)
                                ^ "\n"
  in
  header ^ str, table

let string_of_table table ?(prefix="") =
  Hashtbl.fold (fun k v acc -> acc ^ prefix ^ k ^ " " ^ (string_of_int v) ^ "\n") table ""

open Minisat
(* [minisat_of_cnf] translates the expression into an instance of Minisat.t,
   which can then be used for solving the SAT problem with Minisat.Solve
   In utop, you can test Minisat with
       #require "minisat";;
       open Minisat
*)
(*    ((((not &2) or a) and ((not &2) or b)) and (((not &1) or c) and ((not &1) or (not a)))) *)
let minisat_of_cnf (exp:exp) : Minisat.t * (string, Minisat.Lit.t) Hashtbl.t =
  let inst = Minisat.create () in
  (* num = a number that will serve to identify a literal
     lit = a literal that has a number inside it to identify it *)
  let str_to_lit = Hashtbl.create 500 in
  let num_lit = ref 1 in
  let rec process_cnf exp : Minisat.Lit.t list list = match exp with
    | And  (x,y) when Cnf.is_clause x -> [process_clause x] @ process_cnf y
    | And  (y,x) when Cnf.is_clause x -> [process_clause x] @ process_cnf y
    | And  (x,y) -> (process_cnf x) @ (process_cnf y)
    | x when Cnf.is_clause x -> [process_clause x]
    | _ -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_exp exp) ^ "'")
  and process_clause (exp:exp) : Minisat.Lit.t list = match exp with
    | Term (str, None)        -> (gen_lit str)::[]
    | Not (Term (str, None)) -> (Minisat.Lit.neg (gen_lit str))::[]
    | Or (x,y)               -> (process_clause x) @ (process_clause y)
    | _ -> failwith ("CNF: was expecting a clause but got '" ^ (string_of_exp exp) ^ "'")
  and gen_lit (s:string) : Minisat.Lit.t =
    try Hashtbl.find str_to_lit s
    with Not_found ->
      let lit = Minisat.Lit.make !num_lit in
      Hashtbl.add str_to_lit s lit;
      incr num_lit;
      lit
  and add_clauses inst (l:Minisat.Lit.t list list) : unit =
  match l with
  | [] -> ()
  | cur::next ->
    Minisat.add_clause_l inst cur;
    add_clauses inst next
  in
  process_cnf exp |> add_clauses inst; inst, str_to_lit

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
  let print l = List.fold_left (fun acc x -> match x with a,b -> ("("^(Lit.to_string a) ^ "," ^ (string_of_value b) ^ ")" ^ acc)) "" l
end

(* A set that contains all the models already found. *)
module ModelSet = struct
  include Set.Make(Model)
  let print models = print_endline (fold (fun m acc -> (Model.print m) ^ "\n" ^ acc) models "");
end

(* 1. Fetch the models
   ===================
   This is done calling multiple times [fetch_model]. If the first call
   returns false, it means that this problem is unsat. If the second call
   (or any subsequent call) returns false, there is no more models.
   The general structure of the model fetching must be:
      fetch_model -> next_model -> fetch_model -> next_model -> fetch_model...

*)
let fetch_model instance : bool =
  (Minisat.Raw.simplify instance) && (Minisat.Raw.solve instance [||])


(* 1. Prevent current model from reappearing
   =========================================
   We must prevent the current model to reappear in future models;
   to do so, we add a clause that take the negation of the valuations
   E.g: with the model a=1, b=0 we must add the clause -a or b.
   [counter_clause] will produce a list of literals that corresponds
   to this clause.
   IMPORTANT: When adding the counter-clause, the problem can become unsat.
   [next_model] returns false if the added clause makes the formula unsat. *)
let next_model instance table : bool =
 let counter_clause _ (l:Lit.t) acc = match Minisat.value instance l with
  | V_true -> (Minisat.Lit.neg l)::acc | V_false -> l::acc | _ -> acc
 in let counter_clause = Hashtbl.fold counter_clause table []
 in Minisat.Raw.add_clause_a instance (Array.of_list counter_clause)

(* 2. Avoid duplicates caused by fake literals (of the form '&6')
   ==============================================================
   Our issue here: the models contain fake '&12' literals. We don't
   want to see these fake literals in our models; we also want to
   remove the duplicate models linked to these fake literals.
   To avoid those duplicates, we store the models (without the fake
   literals) in a set.

   To use this function, you need a ModelSet ref already initialized, e.g. with
    let models = ref ModelSet.empty *)
let is_duplicate instance table (prev_models:ModelSet.t ref) : bool =
  let model name (l:Lit.t) acc = match name.[0] with
    | '&' -> acc | _ -> (l, Minisat.value instance l)::acc
  in let model = (Hashtbl.fold model table []) in
  (* 'mem' checks if this model is in the set of already-seen models *)
  let is_duplicate = (ModelSet.mem model !prev_models) in
  prev_models := (ModelSet.add model !prev_models);
  is_duplicate



(*let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))*)
