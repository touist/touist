open Syntax
open Pprint

let dummy_term_count = ref 0
let genterm () =
  incr dummy_term_count; Term ("&" ^ (string_of_int !dummy_term_count), None)

let rec push_lit lit = function
  | Term x -> COr (lit, Term x)
  | CNot (Term x) -> COr (lit, CNot (Term x))
  | CAnd (x,y) -> CAnd (push_lit lit x, push_lit lit y)
  | COr (x,y) -> COr (lit, COr (x,y))
  | x -> failwith ("push_lit: unexpected value " ^ (string_of_clause x)) 

let rec to_cnf = function
  | Top    -> failwith "Formula is always true"
  | Bottom -> failwith "Formula is always false"
  | Term x -> Term x
  | CAnd (Top,x) | CAnd (x,Top) -> to_cnf x
  | CAnd (Bottom,_) | CAnd (_,Bottom) -> Bottom
  | CAnd (x,y) -> CAnd (to_cnf x, to_cnf y)
  | CNot x ->
      begin
        match x with
        | Top -> Bottom
        | Bottom -> Top
        | Term a -> CNot (Term a)
        | CNot y -> to_cnf y
        | CAnd (x',y') -> to_cnf (COr (CNot x', CNot y'))
        | COr (x',y') -> CAnd (to_cnf (CNot x'), to_cnf (CNot y'))
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
            let (new1, new2) = (genterm (), genterm ()) in
            CAnd (COr (new1, new2), CAnd (push_lit (CNot new1) (to_cnf x),
                                          push_lit (CNot new2) (to_cnf y)))
      end
  | CImplies (x,y) -> to_cnf (COr (CNot x, y))
  | CEquiv (x,y) -> to_cnf (CAnd (CImplies (x,y), CImplies (y,x)))
  | CXor (x,y) -> to_cnf (CAnd (COr (x,y), COr (CNot x, CNot y)))
  | _ -> failwith "Failed to transform to CNF"

