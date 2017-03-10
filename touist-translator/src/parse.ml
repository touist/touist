(** Function for parsing a touistl document into an abstract syntaxic tree (AST).

    [parse] is the main function.

    After this step, the AST (its type is [Syntax.ast]) can go through different functions:
    - (1) [Eval.eval] for type-checking and evaluation of the expressions
        (bigor, bigand, variables...)
    - (2) [Cnf.ast_to_cnf] and then [Sat.cnf_to_clauses] to transform the AST
        into a SAT expression
    - (2') [Smt.to_smt2] to transform the AST into a SMT2 expression
    - (3) [Sat.clauses_to_solver] and [Sat.solve_clauses] to solve the SAT problem

    The SMT solver has not (yet) been brought to the [touistc] library; you
    must use touist.jar for now.
*)

open Parser
open Syntax
open Lexing

exception Error of string * loc

(** [string_of_loc] will print the position of the error; the two positions
    correspond to where the error starts and where it ends. 
    Example of call with dummy positions:
        string_of_loc (Lexing.dummy_pos,Lexing.dummy_pos)
    When you have only one Lexing.pos available, repeat it twice:
        string_of_loc (pos,pos)
    Optional 'detailed' will give two extra numbers which are the absolute
    positions in terms of characters from the beginning of the file:
        string_of_loc ~detailed:true loc
    'loc' is the location (with start and end) of a faulty piece of code we
    want to write an error about. 
*)
let string_of_loc ?detailed:(d=false) (loc:loc) : string =
  let s,e = loc in (* start, end *)
  let relative = Printf.sprintf "%d:%d:" s.pos_lnum (s.pos_cnum - s.pos_bol+1) in
  let absolute = Printf.sprintf "%d:%d:" s.pos_cnum e.pos_cnum in
  match d with
  | false -> relative              (* num_line:num_col: *)
  | true  -> relative ^":"^ absolute (* num_line:num_col:token_start:token_end: *)

(** [lexer] is used [parse] in order to get the next token of the input
    stream. It is an intermediate to the [Lexer.token] function (in lexer.mll);
    - Rationale: the parser only accepts Parser.token; but [Lexer.token] returns
      Parser.token list. [lexer] acts as a buffer, returning one by one the list
      of tokens returned by [Lexer.token].
    - Drawback: ALL tokens must be returned as a list, even though most token
      case returns a single token, e.g.,
        ["=>" { IMPLIES }]   must be translated into     [{ [IMPLIES] }]
    - Note: see details in [Lexer.token] (file lexer.mll)
    
    @raise Lexer.Error (message, loc) where 'loc' contains the start/end of the
        faulty item
*)
let lexer buffer : (Lexing.lexbuf -> Parser.token) =
  let tokens = ref [] in (* tokens stored to be processed (see above) *)
  fun lexbuf ->
    match !tokens with
    | x::xs -> tokens := xs; x (* tokens isn't empty, use one of its tokens *)
    | [] -> (* tokens is empty, we can read a new token *)
      let t = Lexer.token lexbuf in
      buffer := Parser_error_report.update !buffer (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      match t with
      | [] -> failwith "One token at least must be returned in 'token rules' "
      | x::xs -> tokens := xs; x

(** [parse] is the main function for parsing touistl. It uses the incremental
    API of menhirLib, which allows us to do our own error handling.

    @param parser is the 'entry point' of the parser that is defined in parser.mly,e.g.,
        %start <Syntax.ast> touist_simple, touist_smt
    @param detailed_err allows to display absolute positions of the faulty text.
    
    Example:   parse Parser.Incremental.touist_simple "let î = 1: p($i)" 

    WARNING: for now, the `pos_fname` that should contain the filename
    needed by menhirlib (just for error handling) contains
    "foo.touistl"... For now, the name of the input file name is not
    indicated to the user: useless because we only handle a single touistl file 
*)
let parse (parser) ?debug:(debug=false) (text:string) : Syntax.ast =
  let buffer = ref Parser_error_report.Zero in
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = "foo.touistl"; pos_lnum = 1};
  let checkpoint = parser lexbuf.lex_curr_p
  and supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier (lexer buffer) lexbuf
  and succeed ast = ast
  and fail checkpoint =
    let msg = (Parser_error_report.report text !buffer checkpoint debug)
    and loc = Parser_error_report.area_pos !buffer (* area_pos returns (start_pos,end_pos) *)
    in raise (Error (msg,loc))
  in
    Parser.MenhirInterpreter.loop_handle succeed fail supplier checkpoint

(** Directly calls [parser] with [Parser.Incremental.touist_simple] *)
let parse_sat ?debug:(d=false) text = parse Parser.Incremental.touist_simple ~debug:d text

(** Same for [Parser.Incremental.touist_simple] *)
let parse_smt ?debug:(d=false) text = parse Parser.Incremental.touist_smt ~debug:d text