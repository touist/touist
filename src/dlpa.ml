open Types.Ast
open Types
open Pprint
open Err

(*
type form =
  | Dia of form * form
  | Box of form * form
and prop = string
and prog =
  | Affect of prop * form (* x ← φ? *)
  | Test of form (* φ? *)
  | Seq of prog * prog
  | Union of prog * prog
  | Inverse of prog
  | Star of prog

open TouistTypes
open TouistTypes.Ast
let f form x = Top (* TODO: write it *)
let g x y prog = match prog with
  | Affect (prop,form) -> And (Equiv (Prop (prop^"_y"), f form x),
                               Bigand (Var "q", Var ))
  | Test f ->
  | Seq (p1,p2) ->
  | Union of form * form
  | Inverse of prog
  | Star of prog
*)

let to_qbf ast = match ast with
| Test prog -> Test (eval_ast_prog prog)
| Seq (pr1, pr2) -> Seq (eval_ast_prog pr1, eval_ast_prog pr2)
| Union' (pr1, pr2) -> Union' (eval_ast_prog pr1, eval_ast_prog pr2)
| Inverse prog -> Inverse (eval_ast_prog prog)
| Star prog -> Star (eval_ast_prog prog)
| Add' prop -> Add' (eval_ast_formula env prop)
| Remove prop -> Remove (eval_ast_formula env prop)
| ast -> failwith ("CNF: was expecting a conjunction of clauses but got '" ^ (string_of_ast ~debug:true ast) ^ "'")

