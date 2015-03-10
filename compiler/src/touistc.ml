open Lexer
open Lexing
open Arg (* Parses the arguments *)
open FilePath (* Operations on file names *)

(*
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol+1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)
*)

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
  | COMPILE_ERRORS
  | COMPILE_JUSTWARNINGS
  | ARGUMENTS_ERROR
let get_code (e : error) : int = match e with
  | OK -> 0
  | COMPILE_ERRORS -> 1
  | COMPILE_JUSTWARNINGS -> 2
  | ARGUMENTS_ERROR -> 3

let input_file_path = ref ""
let output_file_path = ref ""
let output_table_file_path = ref ""
let output_file_basename = ref ""

(* Used to write the "str" string into the "filename" file *)
let write_to_file (filename:bytes) (str:string) =
  let out = open_out filename in
  try
    Printf.fprintf out "%s" str;
    close_out out
  with x -> close_out out; raise x

(* Used when no outputFilePath is given: builds an arbitrary
outputFilePath name using the inputFilePath name *)
let defaultOutput (inputFilePath:bytes) (m:mode) : string =
  let inputBase = FilePath.basename inputFilePath
  in let inputDir = FilePath.dirname inputFilePath
  in let outputBase = match m with
  | SAT_DIMACS -> FilePath.replace_extension inputBase "cnf"
  | SMTLIB2 -> FilePath.replace_extension inputBase "smt2"
  in FilePath.concat inputDir outputBase

(* Used when no outputFilePath is given: builds an arbitrary
outputFilePath name using the inputFilePath name *)
let defaultOutputTable (inputFilePath:bytes) : string =
  let inputBase = (FilePath.basename inputFilePath)
  in let inputBaseNoExt = (FilePath.chop_extension inputBase)
  in let inputDir = (FilePath.dirname inputFilePath)
  in let outputTableBase = ("." ^ inputBaseNoExt ^ "_table")
  in (FilePath.concat inputDir outputTableBase)

(* Used in Arg.parse when a parameter without any preceeding -flag (-f, -x...)
Here, this kind of parameter is considered as an inputFilePath *)
let argIsInputFilePath (inputFilePath:bytes) : unit =
  input_file_path := inputFilePath

let translateToSATDIMACS
  (inputFilePath:bytes)
  (outputFilePath:bytes)
  (outputTableFilePath:bytes)
  : unit =
  let exp =
    Eval.eval (Parser.prog Lexer.lexer (Lexing.from_channel
      (open_in inputFilePath))
    )
  in let c,t = Cnf.to_cnf exp |> Dimacs.to_dimacs
  in
    write_to_file outputFilePath c;
    write_to_file outputTableFilePath (Dimacs.string_of_table t)

(* The main program *)
let _ =
  let cmd = (FilePath.basename Sys.argv.(0)) (* ./touistl exec. name *)
  in let argspecs = (* This list enumerates the different flags (-x,-f...)*)
  [ (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
    ("-o", Arg.Set_string (output_file_path), "The translated file");
    ("-table", Arg.Set_string (output_table_file_path), "The literals table table (for SAT_DIMACS)")
  ]
  in let usage = "TouistL compiles files from the TouIST Language "
    ^"to SAT-DIMACS/SMT-LIB2 \n"
    ^"Usage: "^cmd^" [-o translatedFile] [-table tableFile] file \n"
    ^"Note: if either tableFile or translatedFile is missing, \n"
    ^"artibrary names will be given."
  in

  (* Step 1: we parse the args. If an arg. is "alone", we suppose
   * it is a inputFilePath *)
  Arg.parse argspecs argIsInputFilePath usage; (* parses the arguments *)

  (* Step 2: we see if we got every parameter we need *)
  if ((String.length !input_file_path) == 0)(* NOTE: !var is like *var in C *)
  then (
    print_endline (cmd^": you must give an input file path");
    exit (get_code ARGUMENTS_ERROR)
    );

  if ((String.length !output_file_path) == 0)
  then (
    output_file_path := (defaultOutput !input_file_path SAT_DIMACS);
    print_endline
      (cmd ^ ": no output path given ; will be stored in "^ !output_file_path);
    );

  if ((String.length !output_table_file_path) == 0)
  then (
    output_table_file_path := (defaultOutputTable !input_file_path);
    print_endline
      (cmd ^ ": no output table file path given ; will be stored in "^ !output_table_file_path)
    );

  (* Step 3: translation *)
  translateToSATDIMACS !input_file_path !output_file_path !output_table_file_path;

  exit (get_code OK)
