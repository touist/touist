open Parse

type error = OK | ERROR
let get_code (e : error) : int = match e with OK -> 0 | ERROR -> 1

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
let solve_sat = ref false
let limit = ref 1
let only_count = ref false
let show_hidden_lits = ref false
let debug_formula_expansion = ref false
let equiv_file_path = ref ""
let input_equiv = ref stdin
let linter = ref false (* for displaying syntax errors (during parse only) *)
let linter_and_expand = ref false (* same but with semantic errors (during eval)*)
let detailed_position = ref false (* display absolute position of error *)
let debug_syntax = ref false
let debug_cnf = ref false
let verbose = ref false

(* [process_arg_alone] is the function called by the command-line argument
   parser when it finds an argument with no preceeding -flag (-f, -x...).
   In our case, an argument not preceeded by a flag is the touistl input file. *)
let process_arg_alone (file_path:string) : unit = input_file_path := file_path


(* [string_of_file] takes an opened file and returns a string of its content. *)
let rec string_of_file (input:in_channel) : string =
  let text = ref "" in
  try
    while true do
      text := !text ^ (input_line input) ^ "\n"
    done; ""
  with End_of_file -> !text

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
    "TouistL compiles files from the TouIST Language to SAT-Sat/SMT-LIB2.\n"^
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
    exit (get_code ERROR)
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
    exit (get_code ERROR) end;
  if (not !sat_mode) && (!smt_logic = "") then begin
    print_endline (cmd^": you must choose a solver to use: -sat or -smt2 (try --help)");
    exit (get_code ERROR) end;

  (* SMT Mode: check if one of the available QF_? has been given after -smt2 *)
  if (not !sat_mode) && (not (List.exists (fun x->x=(String.uppercase !smt_logic)) smt_logic_avail)) then
    (print_endline (cmd^": you must specify the logic used (-smt2 logic_name) (try --help)");
     print_endline ("Example: -smt2 QF_IDL");
     exit (get_code ERROR));

  try
  
  (* linter = only show syntax errors *)
  if !linter then 
    (let _ = Parse.parse_sat (string_of_file !input) in (); exit (get_code OK));
  if !linter_and_expand then (* same but adds the semantic (using [eval_ast]) *)
    (let _ = Parse.parse_sat ~debug:!debug_syntax (string_of_file !input) |> Eval.eval in (); exit (get_code OK));

  (* Step 3: translation *)
  if (!sat_mode) then
    (* A. solve has been asked *)
    if !solve_sat then
      if !equiv_file_path <> "" then begin
        let models = Parse.parse_sat ~debug:!debug_syntax (string_of_file !input) 
                      |> Eval.eval
                      |> Cnf.ast_to_cnf ~debug:!debug_cnf
                      |> Sat.cnf_to_clauses 
                      |> Sat.solve_clauses
        and models2 = Parse.parse_sat ~debug:!debug_syntax (string_of_file !input_equiv)
                      |> Eval.eval 
                      |> Cnf.ast_to_cnf ~debug:!debug_cnf
                      |> Sat.cnf_to_clauses 
                      |> Sat.solve_clauses
        in match Sat.ModelSet.equal !models !models2 with
        | true -> Printf.fprintf !output "Equivalent\n"; exit 0
        | false -> Printf.fprintf !output "Not equivalent\n"; exit 1
      end
      else
        let clauses,table = Parse.parse_sat ~debug:!debug_syntax (string_of_file !input)
                            |> Eval.eval 
                            |> Cnf.ast_to_cnf ~debug:!debug_cnf
                            |> Sat.cnf_to_clauses
        in
        let models =
          (if !only_count then Sat.solve_clauses (clauses,table)
           else 
             let print_model model i = Printf.fprintf !output "==== model %d\n%s" i (Sat.Model.pprint table model)
              in Sat.solve_clauses ~limit:!limit ~print:print_model (clauses,table))
        in
        match Sat.ModelSet.cardinal !models with
        | i when !only_count -> Printf.fprintf !output "%d\n" i; exit 0
        | 0 -> Printf.fprintf stderr "Unsat\n"; exit 1
        | i -> (* case where we already printed models in [solve_clause] *)
          Printf.fprintf !output "==== Found %d models, limit is %d (--limit N for more models)\n" i !limit; exit 0
    else
      (* B. solve not asked: print the Sat file *)
      let clauses,tbl = Parse.parse_sat ~debug:!debug_syntax (string_of_file !input) 
                        |> Eval.eval
                        |> Cnf.ast_to_cnf ~debug:!debug_cnf
                        |> Sat.cnf_to_clauses
      in
      (* tbl contains the literal-to-name correspondance table. 
         The number of literals is (Hashtbl.length tbl) *)
      Sat.print_clauses_to_dimacs !output (Hashtbl.length tbl) clauses;
      Sat.print_table !output_table ~prefix:(if !output == !output_table then "c " else "") tbl
        (* table_prefix allows to add the 'c' before each line of the table
           display, when and only when everything is outputed in a single
           file. Example:
              c 98 p(1,2,3)     -> c means 'comment' in any Sat file   *)

  else if (!smt_logic <> "") then begin
    let smt = Smt.to_smt2 (String.uppercase !smt_logic) (Parse.parse_smt ~debug:!debug_syntax (string_of_file !input)) in
    Buffer.output_buffer !output smt;
  end;

  close_out !output;
  close_out !output_table;
  close_in !input;
  exit (get_code OK)

  with
    | Lexer.Error (msg,loc) -> (Printf.fprintf stderr "%s %s\n" (Parse.string_of_loc ~detailed:!detailed_position loc) msg; exit (get_code ERROR))
    | Parse.Error (msg,loc) -> (Printf.fprintf stderr "%s %s" (Parse.string_of_loc ~detailed:!detailed_position loc) msg; exit (get_code ERROR))
    | Eval.Error (msg,loc) -> (Printf.fprintf stderr "%s %s\n" (Parse.string_of_loc ~detailed:!detailed_position loc) msg; exit (get_code ERROR))
    | Cnf.Error msg -> (Printf.fprintf stderr "%s %s\n" (Parse.string_of_loc ~detailed:!detailed_position (Lexing.dummy_pos,Lexing.dummy_pos)) msg; exit (get_code ERROR))
