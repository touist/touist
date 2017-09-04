(** Transform a semantically correct (=returned by [Eval.eval]) ast into Prenex
    form, CNF and QDIMACS.
*)
open Types.Ast

let add_suffix name =
  let open Str in
  try let _ = Str.search_backward (Str.regexp "_[0-9]+$") name (String.length name) in ();
      name |> Str.substitute_first (Str.regexp "[0-9]+$")
      (fun str -> ((str |> matched_string |> int_of_string)+1) |> string_of_int)
  with Not_found -> name ^ "_1"

(** [to_prenex] applies the 'transformation rules' listed on wikipedia (fr) in
    order to transform a valid (= output of [Eval.eval]) AST into Prenex
    Normal Form (PNF). Because we do not know any to transform 'xor' and '<=>',
    these two connectors will be re-written using the other connectors.

    [quant_l] is the list of previously quantified propositions. We need this
    to rename any overlapping quantor scope when transforming to prenex form.
    [conflict_t] is the list of must-currenlty-be-renamed list of props (as
    strings).
    [only_rename] allows us to skip the subsequent transformations if one
    of the 14 transformations has been performed in the recursion. We
    only want one exists/forall transformation per to_prenex full recursion
    because of the variable renaming.
*)
let rec to_prenex debug quant_l conflict_l only_rename ast : ast =
  let string_of_prop prop = match prop with
    | Prop name -> name
    | e -> failwith ("[shouldnt happen] a quantor must be a proposition, not a '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast e)
  in
  (* [to_prenew_new] is just going to add a newly quantified variable to the
     list. But if the newly quantified variable is already in quant_l, then
     an error is raised. *)
  let to_prenex_new prop ast_inner =
    if List.exists (fun y -> y=(string_of_prop prop)) quant_l
    then Msgs.add_fatal (Msgs.Error,Msgs.Prenex,
      "the prop '"^Pprint.string_of_ast prop^"' has been quantified twice. \
      For all the quantifiers quantifying on '"^Pprint.string_of_ast prop^"' \
      in the following code, you should rename their variables:\n    "^Pprint.string_of_ast ast^"\n",None)
    else ast_inner |> to_prenex debug ((string_of_prop prop)::quant_l) conflict_l only_rename in
  (* [to_prenex_rn] will recursively launch a prenex where all propositions that
     have the same name as 'prop' will be renamed by adding a suffix
     (x1,x2...). *)
  let to_prenex_rn prop ast =
    ast |> to_prenex debug quant_l ((string_of_prop prop)::conflict_l) true in
  (* [to_prenex] is the same as the outer 'to_prenex' except that the
     non-changing arguments are already given. *)
  let to_prenex ast = ast |> to_prenex debug quant_l conflict_l only_rename in
  if debug then Printf.printf "to_prenex_in  (%s): %s\n"
    (if only_rename then "traversing  " else "transforming")
    (Pprint.string_of_ast ~utf8:true ast);

  (* To transform into prenex, I want to traverse recursively the AST so that
     in every traversal of each branch, only ONE transformation can happen.
     Do do that, we use the variable [only_rename] which is true if one
     transformation has already happened previously in the recursion.
     We call recursively [process] -> [transform] (if not only_rename) -> [traverse].
     As soon as a [transform] has been completed, all inner recursions of
     [to_prenex_rn] will disable any subsequent transformation to avoid any
     colision between renaming and transforming (the subsequent calls
     of [to_prenex_new] or [to_prenex] will still be able to run [transform]).
     *)
  let transform = function
  | Not Forall (x,f) -> Forall (to_prenex x,Not (to_prenex_new x f)) (* 1 *)
  | And (f,Forall (x,g)) | And (Forall (x,g),f)  -> Forall (to_prenex x,And (to_prenex_rn x f,to_prenex_new x g)) (* 2,5 *)
  | Or (f,Forall (x,g)) | Or (Forall (x,g),f)  -> Forall (to_prenex x,And (to_prenex_rn x f,to_prenex_new x g)) (* 3,6 *)
  | Implies (Forall (x,f),g) -> Exists (to_prenex x,Implies (to_prenex_new x f,to_prenex_rn x g)) (* 4 *)
  | Implies (f,Forall (x,g)) -> Forall (to_prenex x,Implies (to_prenex_rn x f,to_prenex_new x g)) (* 7 *)
  | Not Exists (x,f) -> Forall (to_prenex x,Not (to_prenex_new x f)) (* 8 *)
  | And (f,Exists (x,g)) | And (Exists (x,g),f)  -> Exists (to_prenex x,And (to_prenex_rn x f,to_prenex_new x g)) (* 9,12 *)
  | Or (f,Exists (x,g)) | Or (Exists (x,g),f)  -> Exists (to_prenex x,Or (to_prenex_rn x f,to_prenex_new x g)) (* 10,13 *)
  | Implies (Exists (x,f),g) -> Forall (to_prenex x,Implies (to_prenex_new x f,to_prenex_rn x g)) (* 11 *)
  | Implies (f,Exists (x,g)) -> Exists (to_prenex x,Implies (to_prenex_rn x f,to_prenex_new x g)) (* 14 *)
  | _ -> raise Not_found
  in
  let traverse = function
  | Top -> Top
  | Bottom -> Bottom
  | Not x -> Not (to_prenex x)
  | And (x,y) -> And (to_prenex x, to_prenex y)
  | Or (x,y) -> Or (to_prenex x, to_prenex y)
  | Xor (x,y) -> to_prenex (And (Or (x,y), Or (Not x, Not y)))
  | Implies (x,y) -> Implies (to_prenex x, to_prenex y)
  (* ∃x ⇔ y   ≡   (∃x ⇒ y)⋀(y ⇒ ∃x)  ≡  ∀x.(x ⇒ y) ⋀ ∃x1.(y ⇒ x1), and thus
     we cannot translate to prenex and keep the equivalence notation: x is used
     twice. *)
  | Equiv (x,y) -> to_prenex (And (Implies (x,y),Implies (y,x)))
  | Prop x -> if List.exists (fun y -> y=x) conflict_l then Prop (add_suffix x) else Prop x
  | e -> failwith ("[shouldnt happen] a qbf formula shouldn't contain '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast ~debug:true e)
  in
  let process = function
  | Forall (x,f) -> Forall (to_prenex x, to_prenex_new x f)
  | Exists (x,f) -> Exists (to_prenex x, to_prenex_new x f)
  | v -> if only_rename then traverse v
    else try transform v with Not_found -> traverse v
  in
  let new_ast = process ast in
  if debug then Printf.printf "to_prenex_out (%s): %s\n"
    (if only_rename then "traversing  " else "transforming")
    (Pprint.string_of_ast ~utf8:true ast);
  new_ast

(** [is_unquant] checks that the given formula does not contain any quantors. *)
let rec is_unquant = function
  | Exists (_,_) | Forall (_,_) -> false
  | Prop _ | Top | Bottom -> true
  | Not x                  -> is_unquant x
  | And     (x,y)          -> is_unquant x && is_unquant y
  | Or      (x,y)          -> is_unquant x && is_unquant y
  | Xor     (x,y)          -> is_unquant x && is_unquant y
  | Implies (x,y)          -> is_unquant x && is_unquant y
  | Equiv   (x,y)          -> is_unquant x && is_unquant y
  | e -> failwith ("[shouldnt happen] a qbf formula shouldn't contain '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast ~debug:true e)
let rec is_prenex = function
  | Exists (_,f) | Forall (_,f) -> is_prenex f
  | f -> is_unquant f

(* [] takes a prenex form and quantifies existentially any free variable
   in the innermost way possible. *)
let rec quantify_free_variables env ast =
  let rec search_free env ast =
    match ast with
    | Prop x -> if List.exists (fun y -> y=x) env then [] else [x]
    | Top | Bottom  -> []
    | Not x         -> search_free env x
    | And     (x,y) -> search_free env x @ search_free env y
    | Or      (x,y) -> search_free env x @ search_free env y
    | Xor     (x,y) -> search_free env x @ search_free env y
    | Implies (x,y) -> search_free env x @ search_free env y
    | Equiv   (x,y) -> search_free env x @ search_free env y
    | e -> failwith ("quantify_free_variables(): a qbf formula shouldn't \
      contain '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast ~debug:true e) in
  let rec remove_dups = function
    | [] -> []
    | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
  in match ast with
  | Exists (Prop x,f) -> Exists (Prop x,quantify_free_variables (x::env) f)
  | Forall (Prop x,f) -> Forall (Prop x,quantify_free_variables (x::env) f)
  | other -> let free = search_free env other in
    free |> remove_dups |> List.fold_left (fun acc x -> Exists (Prop x,acc)) other

(* [prenex] loops over [to_prenex] as long as the formula is not in prenex
   form. *)
let prenex ?(debug=false) ast : ast =
  let rec to_prenex_loop ast =
    if debug then Printf.printf "step: %s\n" (Pprint.string_of_ast ~utf8:true ast);
    if is_prenex ast then ast else ast |> to_prenex debug [] [] false |> to_prenex_loop
  in let intermediate = to_prenex_loop ast in
   if debug then Printf.printf "before bounding free vars: %s\n" (Pprint.string_of_ast ~utf8:true intermediate);
  let final = intermediate |> quantify_free_variables [] in
  final

(* [cnf] calls Cnf.cnf on the inner formula (with no quantifiers) and
   existentially quantifies any tseitlin variable in an innermost way. *)
let cnf ?(debug=false) ast =
  let rec process = function
  | Forall (x,f) -> Forall (x, process f)
  | Exists (x,f) -> Exists (x, process f)
  | inner -> Cnf.ast_to_cnf ~debug inner
  in ast |> process |> quantify_free_variables []

(* [regroup_quantors] gathers all succeeding Forall and Exists to a list
   of list such that each sublist only contains one type of quantor.
   Example:
      Forall ("a",Forall ("b",Exists ("c", Forall ("d",_)))
   becomes
      [A of ["a";"b"]; E of ["c"]; A of ["d"]]
   NOTE: I had to reverse the lists each time because the lists were
   constructed the wrong way around.
*)
type 'a quantlist = A of 'a list | E of 'a list
let rec regroup_quantors ast quantlist = match ast with
  | Forall (Prop x,f) ->
    let rec process_forall ast l = match ast with
      | Forall (Prop x',f') -> process_forall f' (x'::l)
      | f' -> (l,f')
    in
    let foralls,inner = process_forall f [x] in
    regroup_quantors inner (A (List.rev foralls) :: quantlist)
  | Exists (Prop x,f) ->
    let rec process_exists ast l = match ast with
      | Exists (Prop x',f') -> process_exists f' (x'::l)
      | f' -> (l,f')
    in
    let exists,inner = process_exists f [x] in
    regroup_quantors inner (E (List.rev exists) :: quantlist)
  | inner -> (List.rev quantlist,inner)

(**
    [qbfclauses_of_cnf] translates an AST (which is in CNF) to three
    structures:
    1) a list of quantlist which reprensents the grouped quantifiers in the
       Prenex Normal Form.
    2) a list of lists of integers which represents the CNF formula embedded in
    the Prenex Normal Form.
    3) a correspondance table 'int -> litteral names'
    NOTE: I use fold_right (which is non-tail recursive, thus less performant)
    to avoid the mess yielded by the reversing of the lists with fold_left.
*)
let qbfclauses_of_cnf ast =
  let quants, inner = regroup_quantors ast [] in
  let num_lit = ref 1 in
  let fresh_lit () = let lit = !num_lit in incr num_lit; lit in
  let clauses_int,int_to_str,str_to_int = Cnf.clauses_of_cnf (fun v -> -v) fresh_lit inner in
  let quantlist_int = quants |> List.fold_left (fun acc lst ->
    let res = match lst with
    | A l -> A (List.fold_right (fun p acc -> Hashtbl.find str_to_int p :: acc) l [])
    | E l -> E (List.fold_right (fun p acc -> Hashtbl.find str_to_int p :: acc) l [])
    in res::acc
  ) []
  in List.rev quantlist_int, clauses_int, int_to_str

(* [print_qdimacs] prints the following:
   1) dimacs header line ('p cnf 3 2')
   2) the quantifiers lines grouped (one quantifier per line,
   beginning with 'e' or 'a' and ending by 0)
   3) the clauses (one conjunction per line, one line is a disjunction,
   minus means 'not'). *)
let print_qdimacs out out_table ast =
  let quantlist_int,clauses_int,int_to_str = qbfclauses_of_cnf ast in
  (* Display the mapping table (propositional names -> int) in dimacs comments *)
  int_to_str |> Cnf.print_table (fun x->x) out_table ~prefix:"c ";
  (* Display the dimacs' preamble line. *)
  Printf.fprintf out "p cnf %d %d\n" (Hashtbl.length int_to_str) (List.length clauses_int);
  (* Display the quantifiers lines *)
  quantlist_int |> List.iter (fun quantlist ->
      let open List in let open Printf in
      match quantlist with
      | A l -> fprintf out "a%s 0\n" (l |> fold_left (fun acc s -> acc^" "^ string_of_int s) "")
      | E l -> fprintf out "e%s 0\n" (l |> fold_left (fun acc s -> acc^" "^ string_of_int s) "")
    );
  (* Display the clauses in dimacs way *)
  clauses_int |> Cnf.print_clauses_to_dimacs out string_of_int;