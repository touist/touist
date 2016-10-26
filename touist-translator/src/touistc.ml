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


(* Used to write the "str" string into the "filename" file *)
let write_to_file (filename:string) (str:string) =
  let out = open_out filename in
  try
    Printf.fprintf out "%s" str;
    close_out out
  with x -> close_out out; raise x

(* Used in Arg.parse when a parameter without any preceeding -flag (-f, -x...)
   Here, this kind of parameter is considered as an inputFilePath *)
let argIsInputFilePath (inputFilePath:string) : unit =
  input_file_path := inputFilePath

(* Used by parse_with_error *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%d:%d:" pos.pos_lnum (pos.pos_cnum - pos.pos_bol+1)

(* [evaluate] handles exceptions when calling the evaluation function [Eval.eval].
 * Eval.eval takes an abstract syntaxic tree and check that it is semantically correct,
 * creates the variables and everything.
 *
 * [ast] means it is of type Syntax.prog,
 * i.e. the "root" type in lexer.mll
 *)
let evaluate (ast:Syntax.prog) : Syntax.exp =
  try Eval.eval ast [] with
  | Eval.Error msg ->
    Printf.fprintf stderr "%s\n" msg;
    exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)

let transform_to_cnf (evaluated_ast:Syntax.exp) : Syntax.exp =
  try Cnf.transform_to_cnf evaluated_ast !debug_cnf with
  | Cnf.Error msg ->
    Printf.fprintf stderr "%s\n" msg;
    exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)


(* [lexer] is an intermediate to the [Lexer.token] function (in lexer.mll);
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
        print_position !output lexbuf;
        Printf.fprintf !output " %s\n" msg;
        exit (get_code COMPILE_WITH_LINE_NUMBER_ERROR)


(*  [invoke_parser] is in charge of calling the parser. It uses
    the incremental API, which allows us to do our own error handling.
    WARNING: for now, the `pos_fname` that should contain the filename
    needed by menhirlib (just for error handling) contains
    "foo.touistl"... For now, the name of the input file name is not
    indicated to the user: useless because we only handle a single touistl file *)
let invoke_parser (text:string) (lexer:Lexing.lexbuf -> Parser.token) (buffer) : Syntax.prog =
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "foo.touistl"; pos_lnum = 1};
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p
  and supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier lexer lexbuf
  and succeed ast = ast
  and fail checkpoint =
    Printf.fprintf stderr "%s" (ErrorReporting.report text !buffer checkpoint !debug_syntax);
    exit (get_code COMPILE_WITH_LINE_NUMBER_ERROR)
  in
  Parser.MenhirInterpreter.loop_handle succeed fail supplier checkpoint

(* [print_solve] outputs the result of a Minisat.solve; the result is passed
   using 'inst' (as 'instance'). One result corresponds to one model.
   'show_hidden' indicates that the hidden literals introduced during
   CNF conversion should be shown. *)
let print_solve output (inst:Minisat.t) (table:(string, Minisat.Lit.t) Hashtbl.t) show_hidden =
  let string_of_value inst (lit:Minisat.Lit.t) = match Minisat.value inst lit with
    | V_true -> "1" | V_false -> "0" | V_undef -> "?"
  in let print_value_and_name name lit = if show_hidden || name.[0] != '&'
    then Printf.fprintf output "%s %s\n" (string_of_value inst lit) name
  in Hashtbl.iter print_value_and_name table

(* [string_of_file] takes an opened file and returns a string of its content. *)
let rec string_of_file (input:in_channel) : string =
  let text = ref "" in
  try
    while true do
      text := !text ^ (input_line input) ^ "\n"
    done; ""
  with End_of_file -> !text

(* [ast_of_channel] takes an opened file and return its Abstract Syntaxic Tree.
   NOTE: this AST is already evaluated (the variables have been handled and
   everything). At this point, the AST can be transformed to DIMACS, SMT... *)
let ast_of_channel (input:in_channel) : Syntax.exp =
    let text_input = string_of_file input in
    let buffer = ref ErrorReporting.Zero in
    let ast = invoke_parser text_input (lexer buffer) buffer in
    evaluate ast

(* The main program *)
let () =
  let cmd = (FilePath.basename Sys.argv.(0)) in (* ./touistl exec. name *)
  let argspecs = (* This list enumerates the different flags (-x,-f...)*)
    [ (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
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
    ("--solve", Arg.Set solve_sat,"Solve the problem and print the first model if it exists");
    ("--limit", Arg.Set_int limit,"(with --solve) Instead of one model, return N models if they exist.
                                            With 0, return every possible model.");
    ("--count", Arg.Set only_count,"(with --solve) Instead of displaying models, return the number of models");
    ("--show-hidden", Arg.Set show_hidden_lits,"(with --solve) Show the hidden '&a' literals used when translating to CNF");
    ("--equiv", Arg.Set_string equiv_file_path,"(with --solve) Check that the given INPUT2 has the same models as INPUT (equivalency)");

    ("--debug-formula-expansion", Arg.Set debug_formula_expansion,"Print how the formula is expanded (bigand...)");
  ]
  in
  let usage = "TouistL compiles files from the TouIST Language \
               to SAT-DIMACS/SMT-LIB2 \n\
               Usage: " ^ cmd ^ " -sat [-o OUTPUT] [-table TABLE] (INPUT | -)\n\
                      Usage: " ^ cmd ^ " -smt2 (QF_IDL|QF_RDL|QF_LIA|QF_LRA) [-o OUTPUT] (INPUT | -) \n\
                      Note: in -sat mode, if TABLE and OUTPUT aren't given, both output will be mixed in stdout."
  in

  (* Step 1: we parse the args. If an arg. is "alone", we suppose
   * it is a inputFilePath *)
  Arg.parse argspecs argIsInputFilePath usage; (* parses the arguments *)

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

  (* *)


  (* Step 3: translation *)
  if (!sat_mode) then
    (* A. solve has been asked *)
    if !solve_sat then
      if !equiv_file_path <> "" then begin
        let _,models = Cnf.transform_to_cnf (ast_of_channel !input) !debug_cnf |> Dimacs.find_models
        and _,models2 = Cnf.transform_to_cnf (ast_of_channel !input_equiv) !debug_cnf |> Dimacs.find_models in
        match Dimacs.ModelSet.equal !models !models2 with
        | true -> Printf.fprintf !output "Equivalent\n"; exit 0
        | false -> Printf.fprintf !output "Not equivalent\n"; exit 1
      end
      else
        let table,models = Cnf.transform_to_cnf (ast_of_channel !input) !debug_cnf |> Dimacs.find_models in
        match Dimacs.ModelSet.cardinal !models with
        | i when !only_count -> Printf.fprintf !output "%d\n" i; exit 0
        | 0 -> Printf.fprintf !output "Unsat\n"; exit 1
        | i -> Dimacs.ModelSet.pprint table !models; exit 0
    else
      (* B. solve not asked: print the DIMACS file *)
      let dimacs,table = Cnf.transform_to_cnf (ast_of_channel !input) !debug_cnf |> Dimacs.to_dimacs in
      Printf.fprintf !output "%s" dimacs;
      (* ~prefix:"" is an optionnal argument that allows to add the 'c' before
         each line of the table display, when and only when everything is
         outputed in a single file. Example:
             c 98 p(1,2,3)     -> c means 'comment' in any DIMACS file   *)
      let table_prefix = (if !output == !output_table then "c " else "") in
      let table_string = Dimacs.string_of_table table ~prefix:table_prefix
      in Printf.fprintf !output_table "%s" table_string

  else if (!smt_logic <> "") then
    let smt = Smt.to_smt2 (String.uppercase !smt_logic) (ast_of_channel !input) in
      Buffer.output_buffer !output smt;


  close_out !output;
  close_out !output_table;
  close_in !input;
  exit (get_code OK)

