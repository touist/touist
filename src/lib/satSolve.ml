open Minisat

let minisat_clauses_of_cnf ast =
  let num_lit = ref 1 in
  let fresh_lit () =
    let lit = !num_lit in
    incr num_lit;
    Minisat.Lit.make lit
  in
  let clauses, lit_to_str, _ =
    Cnf.clauses_of_cnf Minisat.Lit.neg fresh_lit ast
  in
  (clauses, lit_to_str)

let clauses_to_solver ?(verbose = false) (clauses : Lit.t list list) :
    Minisat.t option =
  let solver = Minisat.create () in
  if verbose then set_verbose solver 10;
  let rec add_clauses solver (l : Lit.t list list) : Minisat.t option =
    match l with
    | [] -> Some solver (* -> the formula did not become unsat while adding *)
    | cur :: next ->
        let a = Array.of_list cur in
        if Minisat.Raw.add_clause_a solver a then add_clauses solver next
        else None
  in
  add_clauses solver clauses

(** for printing the Minisat.value type *)
let string_of_value = function V_true -> "1" | V_false -> "0" | _ -> "?"

(** A container for remembering a model *)
module Model = struct
  type t = (Minisat.Lit.t * Minisat.value) list

  let compare l1 l2 = Stdlib.compare l1 l2

  (* [dump] gives a string under the form (0,1)(1,2)(1,3)(0,4)... *)
  let dump l =
    List.fold_left
      (fun acc x ->
        match x with
        | l, v -> "(" ^ string_of_value v ^ "," ^ Lit.to_string l ^ ")" ^ acc)
      "" l

  (* [pprint] gives a string under the form
     1 prop(1,2,9)
     O prop(1,4,2)... *)
  let pprint ?(sep = "\n") table model =
    List.fold_left
      (fun acc (n, v) ->
        let str = string_of_value v ^ " " ^ Hashtbl.find table n in
        match acc with "" -> str | _ -> str ^ sep ^ acc)
      "" model
end

(** A set that contains all the models already found. *)
module ModelSet = struct
  include Set.Make (Model)

  let dump models = fold (fun m acc -> Model.dump m ^ "\n" ^ acc) models ""

  let pprint table models =
    fold (fun m acc -> Model.pprint table m ^ "=====\n" ^ acc) models ""
end

(** [get_model] retrieves the valuations from the current state of the solver
    and put them into a Model.t.
    [discard] must return true if the literal that is mapped to the given
    proposition name shouldn't be in the model. *)
let get_model solver (table : (Lit.t, string) Hashtbl.t)
    (discard : string -> bool) : Model.t =
  Hashtbl.fold
    (fun lit name acc ->
      if not (discard name) then (lit, Minisat.value solver lit) :: acc else acc)
    table []

(* [string_of_clause] dumps the clause in its literal-number form:
   e.g., 1 -5 3 9 -2 -7 *)
let string_of_clause (clause : Lit.t list) : string =
  List.fold_left (fun acc lit -> Lit.to_string lit ^ " " ^ acc) "" clause

let string_of_clauses =
  List.fold_left
    (fun acc v -> (if acc = "" then "" else acc ^ "\n") ^ string_of_clause v)
    ""

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

let solve_clauses ?(verbose = false)
    ?(print : (Lit.t, string) Hashtbl.t -> Model.t -> int -> unit =
      fun _ _ _ -> ()) ?(continue : Model.t -> int -> bool = fun _ _ -> true)
    ((clauses, table) : Lit.t list list * (Lit.t, string) Hashtbl.t) :
    ModelSet.t =
  let counter_current_model solver (table : (Lit.t, string) Hashtbl.t) : bool =
    let counter_clause (l : Lit.t) name acc =
      if Cnf.is_dummy name then acc
      else
        match Minisat.value solver l with
        | V_true -> Minisat.Lit.neg l :: acc
        | V_false -> l :: acc
        | _ -> acc
    in
    let counter_clause = Hashtbl.fold counter_clause table [] in
    Minisat.Raw.add_clause_a solver (Array.of_list counter_clause)
  in
  (* already_unsat means that during the adding of clauses, the formula
     was found unsat. So we don't even need to solve it. *)
  let models = ModelSet.empty in
  match clauses_to_solver ~verbose clauses with
  | None -> models
  | Some solver ->
      let rec solve_loop i =
        (* i is the model counter *)
        if
          (not (Minisat.Raw.simplify solver))
          || not (Minisat.Raw.solve solver [||])
        then models
        else
          let model = get_model solver table Cnf.is_dummy
          (* is_dummy removes &1 lits *)
          and has_next_model = counter_current_model solver table in
          let models = ModelSet.add model models in
          print table model i;
          if has_next_model && continue model i then solve_loop (i + 1)
          else models
      in
      solve_loop 1

let print_dimacs ?(line_begin = "") ?(debug_dimacs = false) (clauses, tbl)
    ?out_table (out : out_channel) =
  (match out_table with
  | None -> ()
  | Some out_table ->
      (* Display the mapping table. *)
      tbl
      |> Cnf.print_table Minisat.Lit.to_int out_table
           ~prefix:(if out = out_table then line_begin ^ "c " else line_begin));
  (* Display the dimacs' preamble line. *)
  Printf.fprintf out "%sp cnf %d %d\n" line_begin (Hashtbl.length tbl)
    (List.length clauses);
  (* Display the dimacs clauses. *)
  let print_lit l =
    if debug_dimacs then
      (if Minisat.Lit.sign l then "" else "-")
      ^ (Minisat.Lit.abs l |> Hashtbl.find tbl)
    else Minisat.Lit.to_string l
  in
  clauses |> Cnf.print_clauses out ~prefix:line_begin print_lit
