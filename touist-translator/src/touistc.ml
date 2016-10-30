(*
 * touistc.ml: Entry point of touistc
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


type mode = SMTLIB2 | SAT_DIMACS
type error =
  | OK
  | COMPILE_WITH_LINE_NUMBER_ERROR
  | COMPILE_NO_LINE_NUMBER_ERROR
  | OTHER

(* COMPILE_WITH_LINE_NUMBER_ERROR == `file_name:num_row:num_col:message`*)
(* COMPILE_NO_LINE_NUMBER_ERROR == `Any other message format` *)
let get_code (e : error) : int = match e with
  | OK -> 0
  | COMPILE_WITH_LINE_NUMBER_ERROR -> 1
  | COMPILE_NO_LINE_NUMBER_ERROR -> 2
  | OTHER -> 3

(* Here is the list of hard-coded accepted logics. There are many
 * other logics that can be accepted. *)
let smt_logic_avail = ["QF_IDL";"QF_LIA";"QF_LRA";"QF_RDL"]

let sat_mode = ref false
let version_asked = ref false
let smt_logic = ref ""
let input_file_path = ref ""
let output_file_path = ref ""
let output_table_file_path = ref ""
let output_file_basename = ref ""
let use_stdin = ref false
let output = ref stdout
let output_table = ref stdout
let input = ref stdin
let debug_cnf = ref false
let debug_syntax = ref false
let solve_sat = ref false
let limit = ref 1
let only_count = ref false
let show_hidden_lits = ref false
let debug_formula_expansion = ref false
let equiv_file_path = ref ""
let input_equiv = ref stdin
let verbose = ref false
let linter = ref false (* for displaying syntax errors (during parse only) *)
let linter_and_expand = ref false (* same but with semantic errors (during eval)*)
let detailed_position = ref false (* display absolute position of error *)

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
        Printf.fprintf stderr "%s %s\n" (print_position lexbuf.lex_curr_p ()) msg;
        exit (get_code COMPILE_WITH_LINE_NUMBER_ERROR)


(*  [invoke_parser] is used by [parse_to_ast] for calling the parser. It uses
    the incremental API of menhirLib, which allows us to do our own error handling.
    WARNING: for now, the `pos_fname` that should contain the filename
    needed by menhirlib (just for error handling) contains
    "foo.touistl"... For now, the name of the input file name is not
    indicated to the user: useless because we only handle a single touistl file *)
let invoke_parser (text:string) (lexer:Lexing.lexbuf -> Parser.token) (buffer) : Syntax.ast =
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "foo.touistl"; pos_lnum = 1};
  let checkpoint = Parser.Incremental.touist_code lexbuf.lex_curr_p
  and supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier lexer lexbuf
  and succeed ast = ast
  and fail checkpoint =
    let msg = (ErrorReporting.report text !buffer checkpoint !debug_syntax)
    and exactpos = ErrorReporting.exact_pos !buffer (* exact position of error*)
    and firstpos,lastpos = ErrorReporting.area_pos !buffer (* error area *)
    in Printf.fprintf stderr "%s %s\n" (print_position exactpos ~area:(firstpos,lastpos) ()) msg;
    exit (get_code COMPILE_WITH_LINE_NUMBER_ERROR)
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

(* [parse_to_ast] takes an opened file and return its Abstract Syntaxic Tree.*)
let parse_to_ast (input:in_channel) : Syntax.ast =
  if !verbose then print_endline "parsing begins";
  let text_input = string_of_file input in
  let buffer = ref ErrorReporting.Zero in
  let ast = invoke_parser text_input (lexer buffer) buffer in
  if !verbose then print_endline "parsing finished";
  ast

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
    Printf.fprintf stderr "%s\n" msg;
    exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)

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
    (Printf.fprintf stderr "%s\n" msg;
     exit (get_code COMPILE_NO_LINE_NUMBER_ERROR))

(* [cnf_to_clauses] takes a cnf and transforms it to list of clauses.
   tbl contains the literal-to-name correspondance table. 
   The number of literals is (Hashtbl.length tbl) *)
let cnf_to_clauses cnf =
  if !verbose then print_endline "cnf to clauses begins";
  let clauses,tbl =  Dimacs.cnf_to_clauses cnf in
  if !verbose then print_endline "cnf to clauses finished";
  clauses,tbl

(* [process_arg_alone] is the function called by the command-line argument
   parser when it finds an argument with no preceeding -flag (-f, -x...).
   In our case, an argument not preceeded by a flag is the touistl input file. *)
let process_arg_alone (file_path:string) : unit = input_file_path := file_path

(* The main program *)
let () =
  let cmd = (FilePath.basename Sys.argv.(0)) in (* ./touistl exec. name *)
  let argspecs = [ (* This list enumerates the different flags (-x,-f...)*)
    (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
    ("-o", Arg.Set_string (output_file_path), "OUTPUT is the translated file");
    ("-table", Arg.Set_string (output_table_file_path),
     "TABLE (-sat only) The output file that contains the literals table.
      By default, prints to stdout.");
    ("-sat", Arg.Set sat_mode, "Select the SAT solver");
    ("-smt2", Arg.Set_string (smt_logic), (
        "LOGIC Select the SMT solver with the specified LOGIC:
        QF_IDL allows to deal with boolean and integer, E.g, x - y < b
        QF_RDL is the same as QF_IDL but with reals
        QF_LIA (not documented)
        QF_LRA (not documented)
       See http://smtlib.cs.uiowa.edu/logics.shtml for more info."
      ));
    ("--version", Arg.Set version_asked, "Display version number");
    ("-", Arg.Set use_stdin,"reads from stdin instead of file");
    ("--debug-syntax", Arg.Set debug_syntax, "Print information for debugging
    syntax errors given by parser.messages");
    ("--debug-cnf", Arg.Set debug_cnf,"Print step by step CNF transformation");
    ("--verbose", Arg.Set verbose,"Print info on what is happening step by step");
    ("--solve", Arg.Set solve_sat,"Solve the problem and print the first model if it exists");
    ("--limit", Arg.Set_int limit,"(with --solve) Instead of one model, return N models if they exist.
                                            With 0, return every possible model.");
    ("--count", Arg.Set only_count,"(with --solve) Instead of displaying models, return the number of models");
    ("--show-hidden", Arg.Set show_hidden_lits,"(with --solve) Show the hidden '&a' literals used when translating to CNF");
    ("--equiv", Arg.Set_string equiv_file_path,"INPUT2 (with --solve) Check that INPUT2 has the same models as INPUT (equivalency)");
    ("--debug-formula-expansion", Arg.Set debug_formula_expansion,"Print how the formula is expanded (bigand...)");
    ("--linter", Arg.Set linter,"Display parse errors and exit");
    ("--linter-expand", Arg.Set linter_and_expand,"Same as --linter but with semantic errors");
    ("--detailed-position", Arg.Set detailed_position,"Detailed position with 'num_line:num_col:token_start:token_end: '");
  ]
  in
  let usage =
    "TouistL compiles files from the TouIST Language to SAT-DIMACS/SMT-LIB2.\n"^
    "Usage: " ^ cmd ^ " -sat [-o OUTPUT] [-table TABLE] (INPUT | -)\n"^
    "Usage: " ^ cmd ^ " -smt2 (QF_IDL|QF_RDL|QF_LIA|QF_LRA) [-o OUTPUT] (INPUT | -)\n"^
    "Note: in -sat mode, if TABLE and OUTPUT aren't given, both output will be mixed in stdout."
  in
  (* Step 1: we parse the args. If an arg. is "alone", we suppose
   * it is the touistl input file (this is handled by [process_arg_alone]) *)
  Arg.parse argspecs process_arg_alone usage; (* parses the arguments *)

  (* Step 1.5: if we are asked the version number
   * NOTE: !version_asked means like in C, *version_asked.
   * It doesn't mean "not version_asked" *)
  if !version_asked then (
    print_endline (Version.version);
    exit (get_code OK)
  );

  (* Step 2: we see if we got every parameter we need *)

  (* Check (file | -) and open input and output *)
  if (!input_file_path = "") && not !use_stdin (* NOTE: !var is like *var in C *)
  then (
    print_endline (cmd^": you must give an input file (try --help)");
    exit (get_code OTHER)
  );
  if !use_stdin then (input := stdin) else (input := open_in !input_file_path);

  if !output_file_path <> "" && (!sat_mode || (!smt_logic <> ""))
  then output := open_out !output_file_path;

  if !output_table_file_path <> "" && !sat_mode
  then output_table := open_out !output_table_file_path;

  if !equiv_file_path <> ""
  then input_equiv := open_in !equiv_file_path;

  (* Check that either -smt2 or -sat have been selected *)
  if (!sat_mode && (!smt_logic <> "")) then begin
    print_endline (cmd^": cannot use both SAT and SMT solvers (try --help)");
    exit (get_code OTHER) end;
  if (not !sat_mode) && (!smt_logic = "") then begin
    print_endline (cmd^": you must choose a solver to use: -sat or -smt2 (try --help)");
    exit (get_code OTHER) end;

  (* SMT Mode: check if one of the available QF_? has been given after -smt2 *)
  if (not !sat_mode) && (not (List.exists (fun x->x=(String.uppercase !smt_logic)) smt_logic_avail)) then
    (print_endline (cmd^": you must specify the logic used (-smt2 logic_name) (try --help)");
     print_endline ("Example: -smt2 QF_IDL");
     exit (get_code OTHER));

  (* linter = only show syntax errors *)  
  if !linter then 
    (let _ = parse_to_ast !input in (); exit (get_code OK));
  if !linter_and_expand then (* same but adds the semantic (using [eval_ast]) *)
    (let _ = parse_to_ast !input |> eval_ast in (); exit (get_code OK));

  (* Step 3: translation *)
  if (!sat_mode) then
    (* A. solve has been asked *)
    if !solve_sat then
      if !equiv_file_path <> "" then begin
        let models = parse_to_ast !input |> eval_ast |> ast_to_cnf |> cnf_to_clauses |> Dimacs.solve_clauses
        and models2 = parse_to_ast !input_equiv |> eval_ast |> ast_to_cnf |> cnf_to_clauses |> Dimacs.solve_clauses in
        match Dimacs.ModelSet.equal !models !models2 with
        | true -> Printf.fprintf !output "Equivalent\n"; exit 0
        | false -> Printf.fprintf !output "Not equivalent\n"; exit 1
      end
      else
        let clauses,table = parse_to_ast !input |> eval_ast |> ast_to_cnf |> cnf_to_clauses in
        let models =
          (if !only_count then Dimacs.solve_clauses (clauses,table)
           else 
             let print_model model i = Printf.fprintf !output "==== model %d\n%s" i (Dimacs.Model.pprint table model)
              in Dimacs.solve_clauses ~limit:!limit ~print:print_model (clauses,table))
        in
        match Dimacs.ModelSet.cardinal !models with
        | i when !only_count -> Printf.fprintf !output "%d\n" i; exit 0
        | 0 -> Printf.fprintf stderr "Unsat\n"; exit 1
        | i -> (* case where we already printed models in [solve_clause] *)
          Printf.fprintf !output "==== Found %d models, limit is %d (--limit N for more models)\n" !limit i; exit 0
    else
      (* B. solve not asked: print the DIMACS file *)
      let clauses,tbl = parse_to_ast !input |> eval_ast |> ast_to_cnf |> cnf_to_clauses in
      (* tbl contains the literal-to-name correspondance table. 
         The number of literals is (Hashtbl.length tbl) *)
      Dimacs.print_clauses_to_dimacs !output (Hashtbl.length tbl) clauses;
      Dimacs.print_table !output_table ~prefix:(if !output == !output_table then "c " else "") tbl
        (* table_prefix allows to add the 'c' before each line of the table
           display, when and only when everything is outputed in a single
           file. Example:
              c 98 p(1,2,3)     -> c means 'comment' in any DIMACS file   *)

  else if (!smt_logic <> "") then begin
    let smt = Smt.to_smt2 (String.uppercase !smt_logic) (parse_to_ast !input) in
    Buffer.output_buffer !output smt;
  end;

  close_out !output;
  close_out !output_table;
  close_in !input;
  exit (get_code OK)

