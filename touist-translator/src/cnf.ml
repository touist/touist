(*
 * cnf.ml: processes the "semantically correct" abstract syntax tree (ast) given by [eval]
 *         to produce a CNF-compliant version of the abstract syntax tree.
 *         [to_cnf] is the main function.
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

(*  Vocabulary:
    - Literal:
      a possibly negated proposition; we denote them as a, b... and
      their type is homogenous to Term _ or CNot(Term _) or Top or Bottom. Exples:
          a                                         is a literal
          not b                                     is a literal
    - Clause:
      a disjunction (= separated by "or") of possibly negated literals.
      Example of clause:
          a or not b or c or d                      is a clause
      WARNING: Syntax.clause isn't actually a clause as defined here; it can
      hold CImplies, CEquiv, CXor. Its naming isn't really appropriate...
    - Conjunction:
      literals separated by "and"; example:
          a and b and not and not d                 is a conjunction
    - AST:
      abstract syntax tree; it is homogenous to Syntax.clause
      and is a recursive tree representing a formula, using COr, CAnd, CImplies...
      Example: the formula (1) has the abstract syntax tree (2):
          (a or b) and not c                          (1) natural language
          CAnd (COr (Term a, Term b),CNot (Term c))   (2) abstract syntax tree
    - CNF:
      a Conjunctive Normal Form is an AST that has a special structure with
      is a conjunction of disjunctions of literals. For example:
          (a or not b) and (not c and d)            is a CNF form
          (a and b) or not (c or d)                 is not a CNF form
 *)

(* [is_clause] checks that the given AST is a clause. This function can only
   be called on an AST containing COr, CAnd or CNot. No CEquiv or CImplies! *)
let rec is_clause (ast: clause) : bool = match ast with
  | Top | Bottom | Term _ | CNot (Term _) -> true
  | CAnd _ -> false
  | COr (x,y) -> is_clause x && is_clause y
  | x -> failwith ("is_clause: unexpected value " ^ (string_of_clause x))

(* [push_lit] allows to translate into CNF the non-CNF disjunction `d or cnf`
   (`d` is the literal we want to add, `cnf` is the existing CNF form).
   For example:
          d  or  ((a or not b) and (not c))            <- is not in CNF
    <=>   push_lit (d) ((a or not b) and not c)
    <=>   (d or a or b) and (d or not c)               <- is in CNF
    This function is necessary because `d or cnf` (with `cnf` an arbitrary CNF
    form) is not a CNF form and must be modified. Conversely, the form
          `d  and  ((a or not b) and (not c))`
    doesn't need to be modified because it is already in CNF.  *)
let rec push_lit (lit:clause) (cnf: clause) : clause = match cnf with
  | Top           -> Top
  | Bottom        -> lit
  | Term x        -> COr (lit, Term x)
  | CNot (Term x) -> COr (lit, CNot (Term x))
  | CAnd (x,y)    -> CAnd (push_lit lit x, push_lit lit y)
  | COr (x,y)     -> COr (lit, COr (x,y))
  | x -> failwith ("Cnf.push_lit: unexpected " ^ (string_of_clause x))


(* [genterm] generates a (Term &i) with i being a self-incrementing index.
 * This function allows to speed up and simplify the translation of some
 * forms of COr.
 * NOTE: OCaml's functions can't have 0 param: we use the unit `()`. *)
let dummy_term_count = ref 0
let genterm () =
  incr dummy_term_count; Term ("&" ^ (string_of_int !dummy_term_count), None)

let debug = ref false (* The debug flag activated by --debug-cnf *)
let rec indent = function 0 -> "" | i -> (indent (i-1))^"\t"
type limit = No | Yes of int
let print_debug (prefix:string) depth (clauses:clause list) : unit =
  let rec string_of_clauses = function
    | [] -> ""
    | cur::[] -> string_of_clause cur
    | cur::next -> (string_of_clause cur)^", "^(string_of_clauses next)
  in print_endline ((indent depth) ^ (string_of_int depth) ^ " " ^ prefix
                    ^ (string_of_clauses clauses))

(* [to_cnf] translates the syntaxic tree made of COr, CAnd, CImplies, CEquiv...
 * COr, CAnd and CNot; moreover, it can only be in a conjunction of clauses
 * (see a reminder of their definition above).
 * For example (instead of CAnd, COr we use "and" and "or" and "not"):
 *     (a or not b or c) and (not a or b or d) and (d)
 * The matching abstract syntax tree (ast) is
 *     CAnd (COr a,(Cor (CNot b),c)), (CAnd (COr (COr (CNot a),b),d), d)
 *
 * The `depth` variable tells what is the current level of recursion and
 * helps for debugging the translation to CNF.
 * The `stop` boolean tells to_cnf if it should stop or continue the recursion
 *
 * (1) When transforming to CNF, we want to make sure that "outer" to_cnf
 *     transformations are made before inner ones. For example, in
 *          to_cnf (CNot (to_cnf ((a and b) => c)))
 *     we want to limit the inner `to_cnf` expansion to let the possibily for
 *     the outer to_cnf to "simplify" with the CNot as soon as possible.
 *     For inner `to_cnf`, we simply use `to_cnf_once` to prevent the inner
 *     `to_cnf` from recursing more than once.
 * *)
let rec to_cnf depth stop (ast:clause) : clause =
  if !debug then print_debug "in:  " depth [ast];
  if (match stop with Yes 0 -> true | _ -> false) then ast else (* See (1) above*)
      let to_cnf_once = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->Yes 1) in
      let to_cnf = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->No) in
    let cnf = begin match ast with
    | Top    -> Top
    | Bottom -> Bottom
    | Term a -> Term a
    | CAnd (x,y) -> let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Top,x | x,Top     -> x
        | Bottom,_|_,Bottom -> Bottom
        | x,y               -> CAnd (x,y)
      end
    | CNot x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | Term a     -> CNot (Term a)
        | CNot x     -> to_cnf x
        | CAnd (x,y) -> to_cnf (COr (CNot x, CNot y))           (* De Morgan *)
        | COr (x,y)  -> CAnd (to_cnf (CNot x), to_cnf (CNot y)) (* De Morgan *)
        | _ -> to_cnf (CNot (to_cnf_once x)) (* See (1) above*)
      end
    | COr (x,y) -> if !debug then print_debug "COr: " depth [x;y];
      let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Bottom, z | z, Bottom   -> z
        | Top, _ | _, Top         -> Top
        | Term a, z | z, Term a   -> push_lit (Term a) z
        | CNot (Term a),z | z,CNot (Term a) -> push_lit (CNot (Term a)) z
        | x,y when is_clause x && is_clause y -> COr (x, y)
        | x,y -> (* At this point, either x or y is a conjunction
                    => Tseytin transform (see explanations below) *)
          let (new1, new2) = (genterm (), genterm ()) in
          CAnd (COr (new1, new2), CAnd (push_lit (CNot new1) x,
                                        push_lit (CNot new2) y))
      end
        (* Note on `COr` and the Tseytin transform:
           When translating `x or y` into CNF and that either x or y is a
           conjunction (= isn't a clause), we must avoid the 'natural' translation
           that should occur when translating (1) into (2): (2) would have an
           exponential number of clauses. Instead, we use arbitrary variables
           created by [genterm], denoted by &1, &2... and use the Tseytin
           transform (3) which yields a linear number of clauses.
                (x1 and y1)  or  (x2 and y2)                                (1)
                (x1 or x2) and (x1 or y2) and (y1 or x2) or (etc...)        (2)
                (&1 or &2) and (not &1 or x1) and (not &1 or y1)            (3)
                          and (not &2 or x2) and (not &2 or y2)
        *)
    | CImplies (x,y) -> to_cnf (COr (CNot x, y))
    | CEquiv (x,y) -> to_cnf (CAnd (CImplies (x,y), CImplies (y,x)))
    | CXor (x,y) -> to_cnf (CAnd (COr (x,y), COr (CNot x, CNot y)))
    | _ -> failwith("Cnf.to_cnf failed on: " ^ (string_of_clause ast))
    end in
    if !debug then print_debug "out: " depth [cnf];
    cnf

(* [transform_to_cnf] is the entry point  for [to_cnf] *)
let transform_to_cnf (ast:clause) debug_mode : clause =
  debug := debug_mode;
  to_cnf 0 No ast
