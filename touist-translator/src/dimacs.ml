(*
 * dimacs.ml: processes the CNF-compliant version of the abstract syntaxic tree and
 *            produces a string in DIMACS format.
 *            [to_dimacs] is the main function.
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/FredMaris/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available 
 * under the terms of the GNU Lesser General Public License (LGPL) 
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *)

open Syntax

let to_dimacs prop =
  let table    = Hashtbl.create 10
  and num_sym  = ref 1
  and nbclause = ref 0 in
  let rec go acc = function
    | Top -> failwith "Clause is always true"
    | Bottom -> failwith "Clause is alway false"
    | Term (x, None)        -> acc ^ string_of_int (gensym x)
    | Term (x, _)           -> failwith ("unevaluated term: " ^ x)
    | CNot (Term (x, None)) -> acc ^ string_of_int (- (gensym x))
    | CAnd (x, y) -> incr nbclause; (go acc x) ^ " 0\n" ^ (go acc y)
    | COr  (x, y) -> (go acc x) ^ " " ^ (go acc y)
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
    | CNot (Term (x, None)) -> acc ^ "-" ^ x
    | CAnd (x, y) -> incr nbclause; (go acc x) ^ " \n" ^ (go acc y)
    | COr  (x, y) -> (go acc x) ^ " v " ^ (go acc y)
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

let string_of_table table =
  Hashtbl.fold (fun k v acc -> acc ^ k ^ " " ^ (string_of_int v) ^ "\n") table ""
