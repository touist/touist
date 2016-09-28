(*
 * cnf.ml: processes the "semantically correct" abstract syntaxic tree given by [eval]
 *         to produce a CNF-compliant version of the abstract syntaxic tree.
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

let dummy_term_count = ref 0
let genterm () =
  incr dummy_term_count; Term ("&" ^ (string_of_int !dummy_term_count), None)

let rec is_clause = function
  | Top | Bottom | Term _ | CNot (Term _) -> true
  | CNot x -> is_conj x
  | CAnd _ -> false
  | COr (x,y) -> is_clause x && is_clause y
  | x -> failwith ("is_clause: unexpected value " ^ (string_of_clause x))
and is_conj = function
  | Top | Bottom | Term _ | CNot (Term _) -> true
  | CNot x -> is_clause x
  | COr _  -> false
  | CAnd (x,y) -> is_conj x && is_conj y
  | x -> failwith ("is_conj: unexpected value " ^ (string_of_clause x))

let rec push_lit lit = function
  | Top           -> Top
  | Bottom        -> lit
  | Term x        -> COr (lit, Term x)
  | CNot (Term x) -> COr (lit, CNot (Term x))
  | CAnd (x,y)    -> CAnd (push_lit lit x, push_lit lit y)
  | COr (x,y)     -> COr (lit, COr (x,y))
  | x -> failwith ("push_lit: unexpected value " ^ (string_of_clause x)) 

let rec to_cnf = function
  | Top    -> Top
  | Bottom -> Bottom
  | Term x -> Term x
  | CAnd (Top,x) | CAnd (x,Top) -> to_cnf x
  | CAnd (Bottom,_) | CAnd (_,Bottom) -> Bottom
  | CAnd (x,y) ->
      begin
        let x' = to_cnf x in
        let y' = to_cnf y in
        match x',y' with 
        | Top,_ -> y'
        | _,Top -> x'
        | _,_   -> CAnd (x',y')
      end
  | CNot x ->
      begin
        match x with
        | Top -> Bottom
        | Bottom -> Top
        | Term a -> CNot (Term a)
        | CNot y -> to_cnf y
        | CAnd (x',y') -> to_cnf (COr (CNot x', CNot y'))
        | COr (x',y') -> CAnd (to_cnf (CNot x'), to_cnf (CNot y'))
        | CImplies (x',y') -> CAnd (to_cnf x', CNot (to_cnf y'))
        | x -> failwith ("Failed to transform to CNF: " ^ (string_of_clause x))
      end
  | COr (x,y) ->
      begin
        match x,y with
        | Bottom, x' | x', Bottom -> to_cnf x'
        | Top, _ | _, Top -> Top
        | Term a, Term b               -> COr (Term a, Term b)
        | CNot (Term a), Term b        -> COr (CNot (Term a), Term b)
        | CNot (Term a), CNot (Term b) -> COr (CNot (Term a), CNot (Term b))
        | Term a, CNot (Term b)        -> COr (Term a, CNot (Term b))
        | Term a, y' | y', Term a -> push_lit (Term a) (to_cnf y')
        | CNot (Term a), y' | y', CNot (Term a) -> push_lit (CNot (Term a)) (to_cnf y')
        | _,_ ->
            let x' = to_cnf x in
            let y' = to_cnf y in
            match x',y' with
            | Bottom,_ -> y'
            | _,Bottom -> x'
            | _,_ ->
                if is_clause x && is_clause y then
                  COr (to_cnf x, to_cnf y)
                else
                  let (new1, new2) = (genterm (), genterm ()) in
                  CAnd (COr (new1, new2), CAnd (push_lit (CNot new1) (to_cnf x),
                                                push_lit (CNot new2) (to_cnf y)))
      end
  | CImplies (x,y) -> to_cnf (COr (CNot x, y))
  | CEquiv (x,y) -> to_cnf (CAnd (CImplies (x,y), CImplies (y,x)))
  | CXor (x,y) -> to_cnf (CAnd (COr (x,y), COr (CNot x, CNot y)))
  | _ -> failwith "Failed to transform to CNF"

