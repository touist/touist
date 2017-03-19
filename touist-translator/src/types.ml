(** Defition of asthe types constituting the abstract syntaxic tree (t) *)

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
   This is because we want the type 't' to be used in 'Set' and
   also we want 'Set' to be inside an 't', I had to come up with
   this recursive (and quite confusing) module thing. I got
   the idea from http://stackoverflow.com/questions/8552589
   Notes:
   (1) I don't know why but Set.elt wouldn't be Ast.ast... So
       I tried this and now it works...
   (2) This is a trick that allows me to avoid repeating the type
       definitions in sig .. end and in struct .. end (explained
       in above link)
*)
module rec Set : Set_ext.S
  with type elt = Ast.ast = (*1*)
  Set_ext.Make(
struct
  include Ast
  type t = ast
  let compare ast ast2 = match ast,ast2 with
    | Int x, Int y -> Pervasives.compare x y
    | Float x, Float y -> Pervasives.compare x y
    | Prop x, Prop y -> Pervasives.compare x y
    | Set x, Set y -> Pervasives.compare x y
    | _ -> failwith "cannot compare"
end)

and Ast : sig
  type var = string * ast list option
  and ast = (* Touist_code is the entry point *)
    | Touist_code      of ast list
    | Int              of int
    | Float            of float
    | Bool             of bool
    | Var              of var
    | Set              of Set.t
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
  val compare : ast -> ast -> int
end = Ast (*2*)
