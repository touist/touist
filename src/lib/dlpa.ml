open Types.Ast
open Types
open Pprint
open Err

(* [propositions] browses the DL-PA formula and searches all its propositions.
   Duplicates? *)
let rec propositions = function
    | Bottom | Top -> []
    | Prop name -> [name]
    | Box (prog, f) -> propositions prog @ propositions f
    | Diamond (prog, f) -> propositions prog @ propositions f
    | Test f -> propositions f
    | Seq (pr1, pr2) -> propositions pr1 @ propositions pr2
    | Union' (pr1, pr2) -> propositions pr1 @ propositions pr2
    | Inverse prog -> propositions prog
    | Star prog -> propositions prog
    | Add' prop -> propositions prop
    | Remove prop -> propositions prop
    | Assign' (prop, form) -> propositions prop @ propositions form
    | Not x -> propositions x
    | And (x,y) | Or (x,y) | Equiv (x,y) | Implies (x,y)
      -> propositions x @ propositions y
    | ast -> failwith ("DL-PA: wrong thing: '" ^ (string_of_ast ~debug:true ast) ^ "'")

let prop name suffix nth = Prop (name ^"_"^suffix^ string_of_int nth)

let (&&) x y = And (x, y)
let (=>) x y = Implies (x, y)
let (||) x y = Or (x, y)
let (<=>) x y = Equiv (x, y)
let forall props formula = props |> List.fold_left (fun acc p -> Forall (p, acc))
let exists x y = Exists (x, y)

let propositions ast = propositions ast |> List.sort_uniq compare

let add_suffix name =
  let open Re_str in
  try let _ = Re_str.search_backward (Re_str.regexp "_[0-9]+$") name (String.length name) in ();
      name |> Re_str.substitute_first (Re_str.regexp "[0-9]+$")
      (fun str -> ((str |> matched_string |> int_of_string)+1) |> string_of_int)
  with Not_found -> name ^ "_1"



(* Notations:
   - f, g and h are functions
   - p is a proposition
   - phi is a formula
   - pi and prog is are programs *)

let t, x, y, z = ref 1, ref 1, ref 1, ref 1
let fresh_t () = incr t; !t
let fresh_x () = incr x; !t
let fresh_y () = incr y; !t
let fresh_z () = incr z; !t

let max_star_recursion = ref 0
let rec f ast x = match ast with
  | Prop p -> prop p "x" x
  | Bottom -> Bottom
  | Top -> Top
  | Not phi -> Not (f phi x)
  | And (f1,f2) -> And (f f1 x, f f2 x)
  | Or (f1,f2) -> Or (f f1 x, f f2 x)
  | Implies (f1,f2) -> Implies (f f1 x, f f2 x)
  | Equiv (f1,f2) -> Equiv (f f1 x, f f2 x)
  | Diamond (prog, form) ->
    let y = fresh_y () in
    List.fold_left (fun acc p -> Exists (prop p "y" y, acc)) (And (g x y prog, f form y)) (propositions ast)
  | Box _ -> failwith ("DL-PA: can't deal with "^string_of_ast_type ast^" yet: '"^string_of_ast ~debug:true ast^ "'")
  | ast -> failwith ("DL-PA: this formula is not yet supported in DL-PA ("^string_of_ast_type ast^"): '" ^ (string_of_ast ~debug:true ast) ^ "'")

(* TODO: instead of creating x,y and z by incrementing, use a global variable
   instead because it might create conflicting names in the end. *)
and g x y ast = match ast with
  | Add' (Prop p) -> g x y (Assign' (Prop p, Top))
  | Remove (Prop p) -> g x y (Assign' (Prop p, Bottom))
  | Assign' (Prop p, form) ->
    List.fold_left (fun acc q -> if q!=p then And (Equiv (prop q "x" x, prop q "x" x), acc) else acc)
      (Equiv (prop p "x" x, f form x))
      (propositions ast) (*TODO: cache the props list instead of fetching it each time *)
  | Test phi ->
    List.fold_left
      (fun acc p -> And (Equiv (prop p "y" y, prop p "x" x), acc))
      (f phi x)
      (propositions ast)
  | Seq (prog1, prog2) ->
    let z = fresh_z () in
    List.fold_left (fun acc p -> Exists (prop p "y" y, acc))
      (And (g x z prog1, g z y prog2))
      (propositions ast)
  | Union' (prog1, prog2) -> Or (g x y prog1, g x y prog2)
  | Inverse prog -> failwith "inverse not implemented"
  | Star prog -> h x y prog !max_star_recursion
  | _ -> failwith ("DL-PA: I want a program but got "^string_of_ast_type ast^": '"^string_of_ast ~debug:true ast^ "'")

and h x y prog n =
  let props = propositions prog in
  match n with
  | 0 ->
    List.fold_left (fun acc q -> And (Or (Equiv (prop q "y" y, prop q "x" x), g x y prog), acc))
      Top
      props
  | n -> let z, x1, y1, t = fresh_z (), fresh_x (), fresh_y (), "t_"^string_of_int (fresh_t ())  in
    props |> List.fold_left (fun acc p -> exists (prop p "z" z) acc)
      (Forall (Prop t,
        props |> List.fold_left (fun acc p -> Exists (prop p "x" x1, acc))
        (
          props |> List.fold_left (fun acc p -> Exists (prop p "y" y1, acc))
            (And
            (
              And
                (
                  h x1 y1 prog (n-1),
                  Implies (Prop t, props |> List.fold_left (fun acc p -> And (And (Equiv (prop p "x" x1, prop p "x" x), Equiv (prop p "y" y1, prop p "z" z)), acc)) Top)
                ),
                Implies (Prop t, props |> List.fold_left (fun acc p -> And (And (Equiv (prop p "x" x1, prop p "x" x), Equiv (prop p "y" y1, prop p "z" z)), acc)) Top)
            ))
        )
      ))

let to_dlpa ast n = max_star_recursion := n; f ast (fresh_x ())