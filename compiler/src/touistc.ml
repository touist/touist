open Lexer
open Lexing

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


let write_to_file filename str = 
  let out = open_out filename in
  try
    Printf.fprintf out "%s" str;
    close_out out
  with x -> close_out out; raise x

let _ =
  let in_file_path = Sys.argv.(1) in
  let in_file = FilePath.basename in_file_path in
  let dimacs_filepath = FilePath.replace_extension in_file "cnf" in
  let table_filepath  = "." ^ (FilePath.chop_extension in_file) ^ "_table" in
  let exp =
    Eval.eval (Parser.prog Lexer.lexer (Lexing.from_channel (open_in in_file_path)))
  in
  let c,t = Cnf.to_cnf exp |> Dimacs.to_dimacs in
  write_to_file dimacs_filepath c;
  write_to_file table_filepath (Dimacs.string_of_table t)

