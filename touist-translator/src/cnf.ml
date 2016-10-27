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
exception Error of string

(*  Vocabulary:
    - Literal:
      a possibly negated proposition; we denote them as a, b... and
      their type is homogenous to Term _ or Not(Term _) or Top or Bottom. Exples:
          a                                         is a literal
          not b                                     is a literal
    - Clause:
      a disjunction (= separated by "or") of possibly negated literals.
      Example of clause:
          a or not b or c or d                      is a clause
    - Conjunction:
      literals separated by "and"; example:
          a and b and not and not d                 is a conjunction
    - AST:
      abstract syntax tree; it is homogenous to Syntax.exp
      and is a recursive tree representing a formula, using Or, And, Implies...
      Example: the formula (1) has the abstract syntax tree (2):
          (a or b) and not c                          (1) natural language
          And (Or (Term a, Term b),Not (Term c))   (2) abstract syntax tree
    - CNF:
      a Conjunctive Normal Form is an AST that has a special structure with
      is a conjunction of disjunctions of literals. For example:
          (a or not b) and (not c and d)            is a CNF form
          (a and b) or not (c or d)                 is not a CNF form
 *)

(* [is_clause] checks that the given AST is a clause. This function can only
   be called on an AST containing Or, And or Not. No Equiv or Implies! *)
let rec is_clause (ast: exp) : bool = match ast with
  | Top | Bottom | Term _ | Not (Term _) -> true
  | Or (x,y) -> is_clause x && is_clause y
  | And _ -> false
  | x -> false

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
let rec push_lit (lit:exp) (cnf:exp) : exp = match cnf with
  | Top           -> Top
  | Bottom        -> lit
  | Term x        -> Or (lit, Term x)
  | Not (Term x) -> Or (lit, Not (Term x))
  | And (x,y)    -> And (push_lit lit x, push_lit lit y)
  | Or (x,y)     -> Or (lit, Or (x,y))
  | x -> raise (Error ("this doesn't seem to be a formula: '" ^ (string_of_exp x) ^ "'"))


(* [genterm] generates a (Term &i) with i being a self-incrementing index.
 * This function allows to speed up and simplify the translation of some
 * forms of Or.
 * NOTE: OCaml's functions can't have 0 param: we use the unit `()`. *)
let dummy_term_count = ref 0
let genterm () =
  incr dummy_term_count; Term ("&" ^ (string_of_int !dummy_term_count), None)


let debug = ref false (* The debug flag activated by --debug-cnf *)

(* [indent] creates a string that contains N indentations *)
let rec indent = function 0 -> "" | i -> (indent (i-1))^"\t"

(* Just a function for printing debug info in [to_cnf] *)
let print_debug (prefix:string) depth (formulas:exp list) : unit =
  let rec string_of_exps = function
    | [] -> ""
    | cur::[] -> string_of_exp cur
    | cur::next -> (string_of_exp cur)^", "^(string_of_exps next)
  in print_endline ((indent depth) ^ (string_of_int depth) ^ " " ^ prefix
                    ^ (string_of_exps formulas))

(* `strop` is a type is used in [to_cnf] in order to stop it after a number of
   recursions. See (1) below *)
type stop = No | Yes of int

(* [to_cnf] translates the syntaxic tree made of Or, And, Implies, Equiv...
 * Or, And and Not; moreover, it can only be in a conjunction of formulas
 * (see a reminder of their definition above).
 * For example (instead of And, Or we use "and" and "or" and "not"):
 *     (a or not b or c) and (not a or b or d) and (d)
 * The matching abstract syntax tree (ast) is
 *     And (Or a,(Cor (Not b),c)), (And (Or (Or (Not a),b),d), d)
 *
 * The `depth` variable tells what is the current level of recursion and
 * helps for debugging the translation to CNF.
 * The `stop` boolean tells to_cnf if it should stop or continue the recursion
 *
 * (1) When transforming to CNF, we want to make sure that "outer" to_cnf
 *     transformations are made before inner ones. For example, in
 *          to_cnf (Not (to_cnf ((a and b) => c)))
 *     we want to limit the inner `to_cnf` expansion to let the possibily for
 *     the outer to_cnf to "simplify" with the Not as soon as possible.
 *     For inner `to_cnf`, we simply use `to_cnf_once` to prevent the inner
 *     `to_cnf` from recursing more than once.
 * *)
let rec to_cnf depth (stop:stop) (ast:exp) : exp =
  if !debug then print_debug "in:  " depth [ast];
  if (match stop with Yes 0 -> true | _ -> false) then ast else (* See (1) above*)
    let to_cnf_once = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->Yes 1) in
    let to_cnf = to_cnf (depth+1) (match stop with Yes i->Yes (i-1) | No->No) in
    let cnf = begin match ast with
    | Top    -> Top
    | Bottom -> Bottom
    | Term a -> Term a
    | And (x,y) -> let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Top,x | x,Top     -> x
        | Bottom,_|_,Bottom -> Bottom
        | x,y               -> And (x,y)
      end
    | Not x ->
      begin
        match x with
        | Top        -> Bottom
        | Bottom     -> Top
        | Term a     -> Not (Term a)
        | Not x     -> to_cnf x
        | And (x,y) -> to_cnf (Or (Not x, Not y))           (* De Morgan *)
        | Or (x,y)  -> And (to_cnf (Not x), to_cnf (Not y)) (* De Morgan *)
        | _ -> to_cnf (Not (to_cnf_once x)) (* See (1) above*)
      end
    | Or (x,y) -> if !debug then print_debug "Or: " depth [x;y];
      let (x,y) = (to_cnf x, to_cnf y) in
      begin
        match x,y with
        | Bottom, z | z, Bottom   -> z
        | Top, _ | _, Top         -> Top
        | Term a, z | z, Term a   -> push_lit (Term a) z
        | Not (Term a),z | z,Not (Term a) -> push_lit (Not (Term a)) z
        | x,y when is_clause x && is_clause y -> Or (x, y)
        | x,y -> (* At this point, either x or y is a conjunction
                    => Tseytin transform (see explanations below) *)
          let (new1, new2) = (genterm (), genterm ()) in
          And (Or (new1, new2), And (push_lit (Not new1) x,
                                        push_lit (Not new2) y))
      end
        (* Note on `Or` and the Tseytin transform:
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
    | Implies (x,y) -> to_cnf (Or (Not x, y))
    | Equiv (x,y) -> to_cnf (And (Implies (x,y), Implies (y,x)))
    | Xor (x,y) -> to_cnf (And (Or (x,y), Or (Not x, Not y)))
    | _ -> raise (Error ("this doesn't seem to be a formula: '" ^ (string_of_exp ast) ^ "'"))
    end in
    if !debug then print_debug "out: " depth [cnf];
    cnf

(* [transform_to_cnf] is the entry point  for [to_cnf] *)
let transform_to_cnf (ast:exp) debug_mode : exp =
  debug := debug_mode;
  to_cnf 0 No ast
