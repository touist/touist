(*
 * translation.ml: all the function about translating touistl to cnf, smt...
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
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 *
 * Some of the code has been inspired by cparser/Lexer.mll contained in
 * the project AbsInt/CompCert. Here are the terms:
 *
 * The Compcert verified compiler
 * Jacques-Henri Jourdan, INRIA Paris-Rocquencourt
 * Copyright Institut National de Recherche en Informatique et en
 * Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.  This file is also distributed
 * under the terms of the INRIA Non-Commercial License Agreement.
 *)

open Lexer
open Lexing
open MenhirLib.General
open Parser.MenhirInterpreter
open Arg (* Parses the arguments *)
open FilePath (* Operations on file names *)
open Minisat

exception Error of string
let debug_syntax = ref false
let debug_cnf = ref false
let detailed_position = ref false
let verbose = ref false

(* [print_position] will print the position of the error; the two positions
   correspond to where the error starts and where it ends. Example of call:
       print_position Lexing.dummy_pos ()          <- the () is necessary
       print_position Lexing.dummy_pos (Lexing.dummy_pos,Lexing.dummy_pos) ()
   Optionnal argument 'area', combined with the boolean !detailed_position,
   ativates the output of the area where the error is.
   If you only ONE position for an error, just call [print_position] with the
   same position on the two parameters.*)
let print_position (err:position) ?area:(start_err,end_err=err,err) (): string =
  let simple = Printf.sprintf "%d:%d:" err.pos_lnum (err.pos_cnum - err.pos_bol+1) in
  (* The detailed version of the position is 'num_line:num_col:token_start:token_end:' 
     (token positions are absolute).*)
  if !detailed_position then
    simple ^ Printf.sprintf "%d:%d:" start_err.pos_cnum end_err.pos_cnum
  else
    simple

(* [lexer] is used [invoke_parser] in order to get the next token of the input
   stream. It is an intermediate to the [Lexer.token] function (in lexer.mll);
   - Rationale: the parser only accepts Parser.token; but [Lexer.token] returns
     Parser.token list. [lexer] acts as a buffer, returning one by one the list
     of tokens returned by [Lexer.token].
   - Drawback: ALL tokens must be returned as a list, even though most token
     case returns a single token, e.g.,
       "=>" { IMPLIES }    must be translated into     { [IMPLIES] }
   - Note: see details in [Lexer.token] (file lexer.mll)
*)
let lexer buffer : (Lexing.lexbuf -> Parser.token) =
  let tokens = ref [] in (* tokens stored to be processed (see above) *)
  fun lexbuf ->
    match !tokens with
    | x::xs -> tokens := xs; x (* tokens isn't empty, use one of its tokens *)
    | [] -> (* tokens is empty, we can read a new token *)
      try 
        let t = Lexer.token lexbuf in
        buffer := ErrorReporting.update !buffer (lexbuf.lex_start_p, lexbuf.lex_curr_p);
        match t with
        | [] -> failwith "One token at least must be returned in 'token rules' "
        | x::xs -> tokens := xs; x
      with Lexer.Error (msg,lexbuf) -> 
        raise (Error (Printf.sprintf "%s %s\n" (print_position lexbuf.lex_curr_p ()) msg))


(*  [invoke_parser] is used by [parse_to_ast] for calling the parser. It uses
    the incremental API of menhirLib, which allows us to do our own error handling.
    parser is the 'entry point' of the parser that is defined in parser.mly,e.g.,
      %start <Syntax.ast> touist_simple, touist_smt
    WARNING: for now, the `pos_fname` that should contain the filename
    needed by menhirlib (just for error handling) contains
    "foo.touistl"... For now, the name of the input file name is not
    indicated to the user: useless because we only handle a single touistl file 
    
    'detailed_pos' allows to get beginning and end of error area, instead of
    a single position error. *)
let invoke_parser (parser) (text:string) (lexer:Lexing.lexbuf -> Parser.token) (buffer) : Syntax.ast =
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "foo.touistl"; pos_lnum = 1};
  let checkpoint = parser lexbuf.lex_curr_p
  and supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier lexer lexbuf
  and succeed ast = ast
  and fail checkpoint =
    let msg = (ErrorReporting.report text !buffer checkpoint !debug_syntax)
    and exactpos = ErrorReporting.exact_pos !buffer (* exact position of error*)
    and firstpos,lastpos = ErrorReporting.area_pos !buffer (* error area *)
    in raise (Error (Printf.sprintf "%s %s" (print_position exactpos ~area:(firstpos,lastpos) ()) msg))
  in
  Parser.MenhirInterpreter.loop_handle succeed fail supplier checkpoint

(* [print_solve] outputs the result of the solver.
   'show_hidden' indicates that the hidden literals introduced during
   CNF conversion should be shown. *)
let print_solve output (solver:Minisat.t) (table:(string, Minisat.Lit.t) Hashtbl.t) show_hidden =
  let string_of_value solver (lit:Minisat.Lit.t) = match Minisat.value solver lit with
    | V_true -> "1" | V_false -> "0" | V_undef -> "?"
  in let print_value_and_name name lit = if show_hidden || name.[0] != '&'
       then Printf.fprintf output "%s %s\n" (string_of_value solver lit) name
  in Hashtbl.iter print_value_and_name table

(* [string_of_file] takes an opened file and returns a string of its content. *)
let rec string_of_file (input:in_channel) : string =
  let text = ref "" in
  try
    while true do
      text := !text ^ (input_line input) ^ "\n"
    done; ""
  with End_of_file -> !text

(* [parse_to_ast] takes a text containing touistl code and return
   its Abstract Syntaxic Tree.*)
let parse_to_ast parser (text_input:string) : Syntax.ast =
  if !verbose then print_endline "parsing begins";
  let buffer = ref ErrorReporting.Zero in
  let ast = invoke_parser parser text_input (lexer buffer) buffer in
  if !verbose then print_endline "parsing finished";
  ast


let parse_simple_to_ast input = parse_to_ast Parser.Incremental.touist_simple (string_of_file input)
let parse_smt_to_ast input = parse_to_ast Parser.Incremental.touist_smt (string_of_file input)
let parse_str_simple_to_ast text = parse_to_ast Parser.Incremental.touist_simple text
let parse_str_smt_to_ast text = parse_to_ast Parser.Incremental.touist_smt text

(* [eval_ast] expands the bigand, bigor, exact... of an ast to produce a valid
   formula. This function handles exceptions when calling the evaluation 
   function [Eval.eval].
   Eval.eval takes an abstract syntaxic tree and check that it is semantically 
   correct, creates the variables, expands variables/bigand/bigor/exact/etc...
   The result of [Eval.eval] is a correct logical formula.
   NOTE: ast = Syntax.ast, i.e. the "root" type in lexer.mll *)
let eval_ast (ast:Syntax.ast) : Syntax.ast =
  if !verbose then print_endline "evaluation begins";
  try let expanded = Eval.eval ast in
    if !verbose then print_endline "evaluation finished";
    expanded
  with
  | Eval.Error msg -> 
    raise (Error (Printf.sprintf "%s\n" msg))
  | Eval.ErrorWithLoc (msg,(startpos,endpos))  ->
    raise (Error (Printf.sprintf "%s %s\n" (print_position startpos ~area:(startpos,endpos) ()) msg))

(* [ast_to_cnf] transforms the evaluated ast to cnf.
   It wraps Cnf.ast_to_cnf to handle the error exceptions and
   return nice error messages. *)
let ast_to_cnf (evaluated_ast:Syntax.ast) : Syntax.ast =
  if !verbose then print_endline "cnf transformation begins";
  try
    let cnf = Cnf.ast_to_cnf evaluated_ast !debug_cnf in
    if !verbose then print_endline "cnf transformation finished";
    cnf
  with Cnf.Error msg ->
    raise (Error (Printf.sprintf "%s\n" msg))

(* [cnf_to_clauses] takes a cnf and transforms it to list of clauses.
   tbl contains the literal-to-name correspondance table. 
   The number of literals is (Hashtbl.length tbl) *)
let cnf_to_clauses ?verbose:(bool=false) cnf =
  if !verbose then print_endline "cnf to clauses begins";
  let clauses,tbl =  Dimacs.cnf_to_clauses cnf in
  if !verbose then print_endline "cnf to clauses finished";
  clauses,tbl