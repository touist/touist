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


type mode = SMTLIB2 | SAT_DIMACS
type error =
  | OK
  | COMPILE_WITH_LINE_NUMBER_ERROR
  | COMPILE_NO_LINE_NUMBER_ERROR
  | OTHER

(* COMPILE_WITH_LINE_NUMBER_ERROR == `num_row:num_col:message`*)
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

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol+1)

(* Used to write the "str" string into the "filename" file *)
let write_to_file (filename:string) (str:string) =
  let out = open_out filename in
  try
    Printf.fprintf out "%s" str;
    close_out out
  with x -> close_out out; raise x

(* Used when no outputFilePath is given: builds an arbitrary
outputFilePath name using the inputFilePath name *)
let defaultOutput (inputFilePath:string) (m:mode) : string =
  let inputBase = FilePath.basename inputFilePath in
  match m with
  | SAT_DIMACS -> FilePath.replace_extension inputBase "cnf"
  | SMTLIB2 -> FilePath.replace_extension inputBase "smt2"
  (*in FilePath.concat inputDir outputBase*)

(* Used when no outputFilePath is given: builds an arbitrary
outputFilePath name using the inputFilePath name *)
let defaultOutputTable (inputFilePath:string) : string =
  let inputBase = (FilePath.basename inputFilePath) in
  let inputBaseNoExt = (FilePath.chop_extension inputBase) in
  inputBaseNoExt ^ ".table"

(* Used in Arg.parse when a parameter without any preceeding -flag (-f, -x...)
Here, this kind of parameter is considered as an inputFilePath *)
let argIsInputFilePath (inputFilePath:string) : unit =
  input_file_path := inputFilePath

(* Used by parse_with_error *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol+1)


  
(* [evaluate] handles exceptions when calling the evaluation function [Eval.eval].
 * Eval.eval takes an abstract syntaxic tree and check that it is semantically correct,
 * creates the variables and everything.
 *
 * [ast] means it is of type Syntax.prog,
 * i.e. the "root" type in lexer.mll 
 *)
let evaluate (ast:Syntax.prog) : Syntax.clause =
  try Eval.eval ast [] with
  | Eval.NameError msg ->
      Printf.fprintf stderr "name error with '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | Eval.TypeError msg ->
      Printf.fprintf stderr "type error with '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | Eval.ArgumentError msg ->
      Printf.fprintf stderr "argument error: '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
(*  XXX: Mael: I removed this part to avoid "skipping" 
 *  some exceptions we could have forgotten to handle
 *  
 *  | _ ->
      fprintf stderr "unkwown error\n";
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
*)


  (* This is the main entry point to the lexer. *)

let lexer : (Lexing.lexbuf -> Parser.token) =
  fun lexbuf -> Lexer.token lexbuf

let lexer buffer : (Lexing.lexbuf -> Parser.token) =
  fun lexbuf ->
    let lex = lexer lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
        buffer := ErrorReporting.update !buffer (startp, endp);
        lex

(*  [invoke_parser] is in charge of calling the parser. It uses
    the incremental API, which allows us to do our own error handling. 
   
    [invoke_parser] *)

let invoke_parser filename text lexer buffer : Syntax.prog =
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename; pos_lnum = 1};
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p
  and supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier lexer lexbuf
  and succeed ast = ast
  and fail checkpoint =
      Printf.fprintf stderr "%s" (ErrorReporting.report text !buffer checkpoint);
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  in
    Parser.MenhirInterpreter.loop_handle succeed fail supplier checkpoint
    

let string_of_file (filename:string) : string =
  let inchannel = open_in_bin filename in
    let n = in_channel_length inchannel in
    let text = really_input_string inchannel n in
        close_in inchannel;
        (text)


(* Main parsing/lexing function *)
let translateToSATDIMACS (infile:string) (outfile:string) (tablefile:string) =
  let text = string_of_file infile
  and buffer = ref ErrorReporting.Zero in
  let ast = invoke_parser infile text (lexer buffer) buffer in
    let exp = evaluate ast in
      let c,t = Cnf.to_cnf exp |> Dimacs.to_dimacs in
        write_to_file outfile c;
        write_to_file tablefile (Dimacs.string_of_table t)

let translate_to_smt2 logic infile outfile =
  let text = string_of_file infile
  and buffer = ref ErrorReporting.Zero in
  let ast = invoke_parser infile text (lexer buffer) buffer in
    let exp = evaluate ast in
      let buf = Smt.to_smt2 logic exp
      and out = open_out outfile in
      try Buffer.output_buffer out buf
      with x -> close_out out; raise x

(* The main program *)
let () =
  let cmd = (FilePath.basename Sys.argv.(0)) in (* ./touistl exec. name *)
  let argspecs = (* This list enumerates the different flags (-x,-f...)*)
  [ (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
    ("-o", Arg.Set_string (output_file_path), "file The translated file");
    ("-table", Arg.Set_string (output_table_file_path), "file The literals table table (for SAT_DIMACS)");
    ("-sat", Arg.Set sat_mode, "Use the SAT solver");
    ("-smt2", Arg.Set_string (smt_logic), (
        "logic Use the SMT solver with the specified logic
        QF_IDL allows to deal with boolean and integer, E.g, x - y < b
        QF_RDL is the same as QF_IDL but with reals
        QF_LIA (not documented)
        QF_LRA (not documented)
    See http://smtlib.cs.uiowa.edu/logics.shtml for more info."
    ));
    ("--version", Arg.Set version_asked, "Display version number")
  ]
  in
  let usage = "TouistL compiles files from the TouIST Language \
    to SAT-DIMACS/SMT-LIB2 \n\
    Usage: " ^ cmd ^ " -sat [-o translatedFile] [-table tableFile] file \n\
    Usage: " ^ cmd ^ " -smt2 (QF_IDL|QF_RDL|QF_LIA|QF_LRA) [-o translatedFile] file \n\
    Note: if either tableFile or translatedFile is missing, \n\
    an artibrary name will be given."
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
  if ((String.length !input_file_path) == 0)(* NOTE: !var is like *var in C *)
  then (
    print_endline (cmd^": you must give an input file (try --help)");
    exit (get_code OTHER)
  );

  (* If no output file has been given, set to a default one *)
  if (String.length !output_file_path) == 0 && !sat_mode then
    output_file_path := (defaultOutput !input_file_path SAT_DIMACS);
  
  if (String.length !output_file_path) == 0 && (String.length !smt_logic != 0) then
    output_file_path := (defaultOutput !input_file_path SMTLIB2);

  if ((String.length !output_table_file_path) == 0)
  then
    output_table_file_path := (defaultOutputTable !input_file_path);
  
  (* Check that either -smt2 or -sat have been selected *)
  if (!sat_mode && (!smt_logic <> "")) then
    (print_endline (cmd^": cannot use both SAT and SMT solvers (try --help)");
     exit (get_code OTHER));

  if (not !sat_mode) && (!smt_logic = "") then
    (print_endline (cmd^": you must choose a solver to use: -sat or -smt2 (try --help)");
     exit (get_code OTHER));

  (* SMT Mode: check if one of the available QF_? has been given after -smt2 *)
  if (not !sat_mode) && (not (List.exists (fun x->x=(String.uppercase !smt_logic)) smt_logic_avail)) then 
    (print_endline (cmd^": you must specify the logic used (-smt2 logic_name) (try --help)");
     print_endline ("Example: -smt2 QF_IDL");
     exit (get_code OTHER));

  (* Step 3: translation *)
  if (!sat_mode) then
    translateToSATDIMACS !input_file_path !output_file_path !output_table_file_path;
  
  if (!smt_logic <> "") then
    translate_to_smt2 (String.uppercase !smt_logic) !input_file_path !output_file_path;
  
  exit (get_code OK)

(* Quick testing main function *)
(*
let () =
  let input_file = FilePath.basename Sys.argv.(1) in
  let out_file = FilePath.replace_extension input_file "cnf" in
  let table_file = "." ^ (FilePath.chop_extension input_file) ^ "_table" in
  let exp = Eval.eval (Parser.prog Lexer.lexer (Lexing.from_channel (open_in Sys.argv.(1)))) [] in
  let c,t = Cnf.to_cnf exp |> Dimacs.to_dimacs in
  write_to_file out_file c;
  write_to_file table_file (Dimacs.string_of_table t)
*)
