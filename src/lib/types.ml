(** Definition of types [Ast.ast] and [AstSet] constituting the abstract syntaxic tree *)

(* Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available
 * under the terms of asthe GNU Lesser General Public License (LGPL)
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html *)

open Msgs

(* Do you think this file is wierd, with this 'module rec' thing?
   This is because we want the type 'ast' to be used in 'Set' and
   also we want 'Set' to be inside an 'ast', I had to come up with
   this recursive (and quite confusing) module thing. I got
   the idea from http://stackoverflow.com/questions/8552589
   Notes:
   (1) This is a trick that allows me to avoid repeating the type
       definitions in sig .. end and in struct .. end (explained
       in above link)
   (2) I don't know why but Set.elt wouldn't be Ast.ast... So
       I tried this and now it works...
*)
module rec Ast : sig
  type var = string * ast list option
  and ast = (* Touist_code is the entry point *)
    | Touist_code      of ast list
    | Int              of int
    | Float            of float
    | Bool             of bool
    | Var              of var
    | Set              of AstSet.t
    | Set_decl         of ast list
    | Neg              of ast
    | Add              of ast * ast
    | Sub              of ast * ast
    | Mul              of ast * ast
    | Div              of ast * ast
    | Mod              of ast * ast
    | Sqrt             of ast
    | To_int           of ast
    | To_float         of ast
    | Abs              of ast
    | Top
    | Bottom
    | Not              of ast
    | And              of ast * ast
    | Or               of ast * ast
    | Xor              of ast * ast
    | Implies          of ast * ast
    | Equiv            of ast * ast
    | Equal            of ast * ast
    | Not_equal        of ast * ast
    | Lesser_than      of ast * ast
    | Lesser_or_equal  of ast * ast
    | Greater_than     of ast * ast
    | Greater_or_equal of ast * ast
    | Union            of ast * ast
    | Inter            of ast * ast
    | Diff             of ast * ast
    | Range            of ast * ast
    | Empty            of ast
    | Card             of ast
    | Subset           of ast * ast
    | Powerset         of ast
    | In               of ast * ast
    | If               of ast * ast * ast
    | Exact            of ast * ast
    | Atleast          of ast * ast
    | Atmost           of ast * ast
    | Bigand           of ast list * ast list * ast option * ast
    | Bigor            of ast list * ast list * ast option * ast
    | Let              of ast * ast * ast
    | Affect           of ast * ast
    | UnexpProp        of string * ast list option (* Unexp = unexpanded *)
    | Prop             of string
    (* UnexpProp is a proposition that contains unexpanded variables; we cannot
      tranform UnexpProp into Prop before knowing what is the content of asthe
      variables. Examples:
          abcd(1,$d,$i,a)       <- not a full-string yet
      Prop contains the actual proposition after the evaluation has been run.
      Example: if $d=foo and $i=123, then Prop is:
          abcd(1,foo,123,a)     <- an actual string that represents an actual
                                    logical proposition
    *)
    | Loc              of ast * loc
    (* Loc is a clever way of keeping the locations in the text of asthe t elements.
      In parser.mly, each production rule gives its location in the original text;
      for example, instead of simply returning
          Inter (x,y)
      the parser will return
          Loc (Inter (x,y), ($startpos,$endpos))
      Loc is used in eval.ml when checking the types; it allows to give precise
      locations.
      *)
    | Paren of ast
    (* [Paren] keeps track of asthe parenthesis in the AST in order to print latex *)
    | Exists           of ast * ast
    | Forall           of ast * ast
    | For              of ast * ast * ast
end = Ast (* see (1) *)

(* From the type [Ast], we create an ordered type [AstOrdered] in order to be able
   to create a set. *)
and AstOrdered : Set.OrderedType = struct
  include Ast
  type t = Ast.ast
  let compare ast ast2 = match ast,ast2 with
      | Int x, Int y -> Pervasives.compare x y
      | Float x, Float y -> Pervasives.compare x y
      | Prop x, Prop y -> Pervasives.compare x y
      | Set x, Set y -> Pervasives.compare x y
      | _ -> failwith "cannot compare"
      let of_list =
      List.fold_left (fun acc x -> add x acc) empty
end


and AstSet : sig
  include Set.S
  (** Return the different ways to choose k elements among a set of n
      elements *)
  val combinations : int -> t -> elt list list
  (** Return a list of tuples. The first member is a combination of k
     elements in the set and the second member is the list of every other
     set elements not in the combination *)
  val exact: int -> t -> (elt list * elt list) list
  (** Actually an alias for the combinations function:
      combinations k set *)
  val atleast: int -> t -> elt list list
  (** Equivalent to:
      combinations (n-k) set, where n = card(set)  *)
  val atmost: int -> t -> elt list list
  end with type elt = Ast.ast (* see (2) *)
= 
struct 
  include Set.Make(AstOrdered)

  (* Inefficient implementation - Found on Rosetta Code ^_^ *)
  let combinations k set =
    let rec comb k lst =
      match k,lst with
      | 0,_     -> [[]]
      | _,[]    -> []
      | k,x::xs -> List.map (fun y -> x::y) (comb (pred k) xs) @ comb k xs
    in comb k (elements set)

  let exact k set =
    let rec go k l =
      match k,l with
      | 0,_     -> [([],l)] (* exact 0 -> all terms in the list must be 'not' *)
      | _,[]    -> []       (* exact on empty set -> no couple at all *)
      | k,x::xs ->
          List.map (fun (comb,rest) ->
            (x::comb, elements (diff set (of_list (x::comb)))))
          (go (pred k) xs) @ go k xs
    in go k (elements set)

  let atleast = combinations

  let atmost k set =
    let n = cardinal set in
    combinations (n-k) set
end