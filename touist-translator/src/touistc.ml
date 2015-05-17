open Lexer
open Lexing
open Arg (* Parses the arguments *)
open FilePath (* Operations on file names *)
open Printf
open Version


(* NOTE for utop or ocaml users: to open the FilePath module
from the fileutils package, do the following:
  #use "topfind";;
  #require "fileutils";;
If it doesn't work, check if the fileutils package is installed:
  #list;; (uses the topfind library from the ocamlfind package)
Then you can open the FilePath module:
  open FilePath;;
*)

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

let sat_mode = ref false
let version_asked = ref false
let smt_logic = ref ""
let input_file_path = ref ""
let output_file_path = ref ""
let output_table_file_path = ref ""
let output_file_basename = ref ""

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
  fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol+1)

(* parse_with_error handles exceptions when calling
the parser and lexer *)
let parse_and_eval_with_error lexbuf =
  try Eval.eval (Parser.prog Lexer.lexer lexbuf) [] with
  | SyntaxError msg -> 
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (get_code COMPILE_WITH_LINE_NUMBER_ERROR) (* Should be "None" *)
  | Parser.Error ->
      fprintf stderr "unkwown syntax error\n";
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | Eval.NameError msg ->
      fprintf stderr "name error with '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | Eval.TypeError msg ->
      fprintf stderr "type error with '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | Eval.ArgumentError msg ->
      fprintf stderr "argument error: '%s'\n" msg;
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)
  | _ ->
      fprintf stderr "unkwown error\n";
      exit (get_code COMPILE_NO_LINE_NUMBER_ERROR)

    

(* Main parsing/lexing function *)
let translateToSATDIMACS infile outfile tablefile =
  let exp =
    parse_and_eval_with_error (Lexing.from_channel (open_in infile))
  in
  let c,t = Cnf.to_cnf exp |> Dimacs.to_dimacs in
  write_to_file outfile c;
  write_to_file tablefile (Dimacs.string_of_table t)

let translate_to_smt2 logic infile outfile =
  let exp = parse_and_eval_with_error (Lexing.from_channel (open_in infile)) in
  let buf = Smt.to_smt2 logic exp in
  let out = open_out outfile in
  try Buffer.output_buffer out buf
  with x -> close_out out; raise x

(* The main program *)
let () =
  let cmd = (FilePath.basename Sys.argv.(0)) in (* ./touistl exec. name *)
  let argspecs = (* This list enumerates the different flags (-x,-f...)*)
  [ (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
    ("-o", Arg.Set_string (output_file_path), "The translated file");
    ("-table", Arg.Set_string (output_table_file_path), "The literals table table (for SAT_DIMACS)");
    ("-sat", Arg.Set sat_mode, "Use the SAT solver");
    ("-smt2", Arg.Set_string (smt_logic), "Use the SMT solver with the specified logic");
    ("--version", Arg.Set version_asked, "display version number")
  ]
  in
  let usage = "TouistL compiles files from the TouIST Language \
    to SAT-DIMACS/SMT-LIB2 \n\
    Usage: " ^ cmd ^ " -sat [-o translatedFile] [-table tableFile] file \n\
    Usage: " ^ cmd ^ " -smt2 logic [-o translatedFile] file \n\
    Note: if either tableFile or translatedFile is missing, \n\
    artibrary names will be given."
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
    print_endline (cmd ^ ": you must give an input file");
    exit (get_code OTHER)
  );

  if (String.length !output_file_path) == 0 && !sat_mode then
    output_file_path := (defaultOutput !input_file_path SAT_DIMACS);
  
  if (String.length !output_file_path) == 0 && (String.length !smt_logic != 0) then
    output_file_path := (defaultOutput !input_file_path SMTLIB2);

  if ((String.length !output_table_file_path) == 0)
  then
    output_table_file_path := (defaultOutputTable !input_file_path);
  
  if (!sat_mode && (!smt_logic <> "")) then
    (print_endline "Cannot use both SAT and SMT solvers";
     exit (get_code OTHER));

  if (not !sat_mode) && (!smt_logic = "") then
    (print_endline "You must choose a solver to use (-sat or -smt2)";
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
