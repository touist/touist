open Types.Ast

let add_suffix name =
  let open Str in
  let regex = regexp "[0-9]$" in
  if string_match regex name 0 then
    name |> substitute_first regex (fun str -> string_of_int ((int_of_string str)+1))
  else
    name ^ "1"

(** I used the 'transformation rules' listed on wikipedia (fr). I know,
    this is not serious!
    'quantified' is the list of previously quantified propositions. We need this
    to rename any overlapping quantor scope when transforming to prenex form. *)
let rec to_prenex quant_prop_l ast : ast =
  let to_prenex' = to_prenex quant_prop_l in
  match ast with
  | Forall (Prop x,f) -> Forall (to_prenex' (Prop x), to_prenex (x::quant_prop_l) f)
  | Exists (Prop x,f) -> Exists (to_prenex' (Prop x), to_prenex (x::quant_prop_l) f)
  | Not Forall (x,f) -> Forall (to_prenex' x,Not f) (* 1 *)
  | And (f,Forall (x,g)) | And (Forall (x,g),f)  -> Forall (to_prenex' x,And (f,g)) (* 2,5 *)
  | Or (f,Forall (x,g)) | Or (Forall (x,g),f)  -> Forall (to_prenex' x,And (f,g)) (* 3,6 *)
  | Implies (Forall (x,f),g) -> Exists (to_prenex' x,Implies (f,g)) (* 4 *)
  | Implies (f,Forall (x,g)) -> Forall (to_prenex' x,Implies (f,g)) (* 7 *)
  | Not Exists (x,f) -> Forall (to_prenex' x,Not f) (* 8 *)
  | And (f,Exists (x,g)) | And (Exists (x,g),f)  -> Exists (to_prenex' x,And (f,g)) (* 9,12 *)
  | Or (f,Exists (x,g)) | Or (Exists (x,g),f)  -> Exists (to_prenex' x,Or (f,g)) (* 10,13 *)
  | Implies (Exists (x,f),g) -> Forall (to_prenex' x,Implies (f,g)) (* 11 *)
  | Implies (f,Exists (x,g)) -> Exists (to_prenex' x,Implies (f,g)) (* 14 *)
  | Top -> Top
  | Bottom -> Bottom
  | Not x -> Not (to_prenex' x)
  | And (x,y) -> And (to_prenex' x, to_prenex' y)
  | Or (x,y) -> Or (to_prenex' x, to_prenex' y)
  | Xor (x,y) -> failwith "TODO: xor has not been implemented yet for use with qbf"
  | Implies (x,y) -> Implies (to_prenex' x, to_prenex' y)
  | Equiv (x,y) -> failwith "TODO: xor has not been implemented yet for use with qbf"
  | Prop x -> if List.exists (fun y -> y=x) quant_prop_l then Prop (add_suffix x) else Prop x
  | e -> failwith ("[shouldnt happen] a qbf formula shouldn't contain '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast e)

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
  | e -> failwith ("[shouldnt happen] a qbf formula shouldn't contain '"^Pprint.string_of_ast_type e^"' in " ^ Pprint.string_of_ast e)
let rec is_prenex = function
  | Exists (_,f) | Forall (_,f) -> is_prenex f
  | f -> is_unquant f

let prenex ast =
  let rec to_prenex_loop ast =
    if is_prenex ast then ast else ast |> to_prenex [] |> to_prenex_loop
  in to_prenex_loop ast