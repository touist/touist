open Parse

type error = OK | ERROR
let get_code (e : error) : int = match e with OK -> 0 | ERROR -> 1

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
let equiv_file_path = ref ""
let input_equiv = ref stdin
let linter = ref false (* for displaying syntax errors (during parse and eval) *)
let detailed_position = ref false (* display absolute position of error *)
let debug_syntax = ref false
let debug_cnf = ref false
let latex = ref false
let show = ref false

(* [process_arg_alone] is the function called by the command-line argument
   parser when it finds an argument with no preceeding -flag (-f, -x...).
   In our case, an argument not preceeded by a flag is the touistl input file. *)
let process_arg_alone (file_path:string) : unit = input_file_path := file_path

let exit_with (exit_code:error) = exit (get_code exit_code)

(* In case we have had non-fatal messages (= warnings) during any of the touist commands,
   display them before exiting. *)
let show_msgs_and_exit msgs (exit_code:error) = 
  Msgs.print_msgs ~color:(Unix.isatty Unix.stderr) ~detailed:!detailed_position msgs;
  exit (get_code exit_code)

(* The main program *)
let () =
  let cmd = (FilePath.basename Sys.argv.(0)) in (* ./touistl exec. name *)
  let argspecs = [ (* This list enumerates the different flags (-x,-f...)*)
    (* "-flag", Arg.toSomething (ref var), "Usage for this flag"*)
    ("-o", Arg.Set_string (output_file_path), "OUTPUT is the translated file");
    ("--table", Arg.Set_string (output_table_file_path),
     "TABLE (--sat only) The output file that contains the literals table.
      By default, prints to stdout.");
    ("--sat", Arg.Set sat_mode,
        "Select the SAT solver (enabled by default when --smt not selected)");
    ("--smt", Arg.Set_string (smt_logic), (
        "LOGIC Select the SMT solver with the specified LOGIC from the
        SMT2-LIB specification:
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
    ("--show", Arg.Set show,"Show the expanded AST after evaluation (= expansion)");
    ("--solve", Arg.Set solve_sat,"Solve the problem and print the first model if it exists");
    ("--limit", Arg.Set_int limit,"(with --solve) Instead of one model, return N models if they exist.
                                            With 0, return every possible model.");
    ("--count", Arg.Set only_count,"(with --solve) Instead of displaying models, return the number of models");
    ("--latex", Arg.Set latex,"Transform the touistl laguage into latex");
    ("--show-hidden", Arg.Set show_hidden_lits,"(with --solve) Show the hidden '&a' literals used when translating to CNF");
    ("--equiv", Arg.Set_string equiv_file_path,"INPUT2 (with --solve) Check that INPUT2 has the same models as INPUT (equivalency)");
    ("--linter", Arg.Set linter,"Display syntax and semantic errors and exit");
    ("--detailed-position", Arg.Set detailed_position,"Detailed position with 'num_line:num_col:token_start:token_end: '");
  ]
  in
  let usage =
    "TouIST solves propositional logic problems written in TouIST language\n"^
    "and supports conversion to SAT-DIMACS and SMT-LIB2 solver formats.\n"^
    "Usage: " ^ cmd ^ " [--sat] [-o OUTPUT] [--table TABLE] (INPUT | -)\n"^
    "Usage: " ^ cmd ^ " --smt (QF_IDL|QF_RDL|QF_LIA|QF_LRA) [-o OUTPUT] (INPUT | -)\n"^
    "Note: in --sat mode, if TABLE and OUTPUT aren't given, both output will be mixed in stdout."
  in
  (* Step 1: we parse the args. If an arg. is "alone", we suppose
   * it is the touistl input file (this is handled by [process_arg_alone]) *)
  Arg.parse argspecs process_arg_alone usage; (* parses the arguments *)

  (* Step 1.5: if we are asked the version number
   * NOTE: !version_asked means like in C, *version_asked.
   * It doesn't mean "not version_asked" *)
  if !version_asked then (
    print_endline ("Version: " ^ Version.version);
    if Version.has_git_tag then print_endline ("Git: "^Version.git_tag);
    let built_list = ["minisat"] @ if Version.has_yices2 then ["yices2"] else [] in
    print_endline ("Built with: " ^ List.fold_left (fun acc e -> match acc with "" -> e | _ -> acc^","^e) "" built_list);
    exit_with OK
  );

  (* Step 2: we see if we got every parameter we need *)

  (* Check (file | -) and open input and output *)
  if (!input_file_path = "") && not !use_stdin (* NOTE: !var is like *var in C *)
  then (
    Printf.fprintf stderr "%s: you must give an input file.\nTo read from stdin, add - to the arguments. For more info, try --help.\n" cmd;
    exit_with ERROR
  );
  if !use_stdin then (input := stdin) else (input := open_in !input_file_path);

  (* Check that --smt and --sat have not been both selected *)
  if (!sat_mode && (!smt_logic <> "")) then begin
    Printf.fprintf stderr "%s: cannot use both --sat and --smt solvers (try --help)\n" cmd;
    exit_with ERROR end;

  (* When neither --smt nor --sat has been given, we default to --sat mode *)
  if (not !sat_mode) && (!smt_logic = "") then sat_mode := true;

#ifdef yices2
  (* SMT Mode: check if one of the available QF_? has been given after --smt *)
  if (not !sat_mode) && not (Solvesmt.logic_supported !smt_logic) then
    (Printf.fprintf stderr
    "%s: you must give a correct SMT-LIB logic after --smt (try --help)\nExample: --smt QF_IDL\n" cmd;
    exit_with ERROR);
#endif

  if !output_file_path <> "" && (!sat_mode || (!smt_logic <> ""))
  then output := open_out !output_file_path;

  if !output_table_file_path <> "" && !sat_mode
  then output_table := open_out !output_table_file_path;

  if !equiv_file_path <> ""
  then input_equiv := open_in !equiv_file_path;

  try

  (* latex = parse and transform with latex_of_ast *)
  if !latex then
    (let ast,msgs =
      if !sat_mode
      then Parse.parse_sat (string_of_chan !input)
      else Parse.parse_smt (string_of_chan !input)
    in (Printf.fprintf !output "%s\n" (Latex.latex_of_ast ast); show_msgs_and_exit !msgs OK));

  (* linter = only show syntax and semantic errors *)
  if !linter then
    if (!sat_mode) then
      (let _,msgs = Parse.parse_sat ~debug:!debug_syntax (string_of_chan !input) 
        |> Eval.eval ~smt:(not !sat_mode) ~onlychecktypes:true in show_msgs_and_exit !msgs OK)
    else
      (let _,msgs = Parse.parse_smt ~debug:!debug_syntax (string_of_chan !input) 
        |> Eval.eval ~smt:(not !sat_mode) ~onlychecktypes:true in show_msgs_and_exit !msgs OK);
  if !show then
    if (!sat_mode) then
      (let ast,msgs = Parse.parse_sat ~debug:!debug_syntax (string_of_chan !input) 
        |> Eval.eval ~smt:(not !sat_mode) in
        if !show then Printf.fprintf !output "%s\n" (Pprint.string_of_ast ast); 
        show_msgs_and_exit !msgs OK)
    else
      (let ast,msgs = Parse.parse_smt ~debug:!debug_syntax (string_of_chan !input) 
        |> Eval.eval ~smt:(not !sat_mode) in
        if !show then Printf.fprintf !output "%s\n" (Pprint.string_of_ast ast);
        show_msgs_and_exit !msgs OK);

  (* Step 3: translation *)
  if (!sat_mode) then
    (* A. solve has been asked *)
    if !solve_sat then
      if !equiv_file_path <> "" then begin
        let solve input = 
          let ast,msgs = Parse.parse_sat ~debug:!debug_syntax (string_of_chan input) |> Eval.eval
          in let models = Cnf.ast_to_cnf ~debug:!debug_cnf ast |> Sat.cnf_to_clauses |> Sat.solve_clauses
          in models,msgs
        in
        let models,msgs = solve !input
        and models2,msgs2 = solve !input_equiv
        in match Sat.ModelSet.equal !models !models2 with
        | true -> Printf.fprintf !output "Equivalent\n"; show_msgs_and_exit !msgs OK
        | false -> Printf.fprintf !output "Not equivalent\n"; show_msgs_and_exit !msgs ERROR
      end
      else
        let ast,msgs = Parse.parse_sat ~debug:!debug_syntax (string_of_chan !input) |> Eval.eval in
        let clauses,table = Cnf.ast_to_cnf ~debug:!debug_cnf ast |> Sat.cnf_to_clauses
        in
        let models =
          (if !only_count then Sat.solve_clauses (clauses,table)
           else 
             let print_model model i = Printf.fprintf !output "==== model %d\n%s\n" i (Sat.Model.pprint ~sep:"\n" table model)
              in Sat.solve_clauses ~limit:!limit ~print:print_model (clauses,table))
        in
        match Sat.ModelSet.cardinal !models with
        | i when !only_count -> Printf.fprintf !output "%d\n" i; show_msgs_and_exit !msgs OK
        | 0 -> Printf.fprintf stderr "Unsat\n"; show_msgs_and_exit !msgs ERROR
        | i -> (* case where we already printed models in [solve_clause] *)
          Printf.fprintf !output "==== Found %d models, limit is %d (--limit N for more models)\n" i !limit; show_msgs_and_exit !msgs OK
    else
      (* B. solve not asked: print the Sat file *)
      let ast,msgs = Parse.parse_sat ~debug:!debug_syntax (string_of_chan !input) |> Eval.eval in
      let clauses,tbl = Cnf.ast_to_cnf ~debug:!debug_cnf ast |> Sat.cnf_to_clauses
      in
      (* tbl contains the literal-to-name correspondance table. 
         The number of literals is (Hashtbl.length tbl) *)
      Sat.print_clauses_to_dimacs !output (Hashtbl.length tbl) clauses;
      Sat.print_table !output_table ~prefix:(if !output = !output_table then "c " else "") tbl;
      show_msgs_and_exit !msgs OK
        (* table_prefix allows to add the 'c' before each line of the table
           display, when and only when everything is outputed in a single
           file. Example:
              c 98 p(1,2,3)     -> c means 'comment' in any Sat file   *)

  else if (!smt_logic <> "") && not !solve_sat then begin
    let ast,msgs = Parse.parse_smt ~debug:!debug_syntax (string_of_chan !input)
        |> Eval.eval ~smt:(!smt_logic <> "") in
    let smt = Smt.to_smt2 (String.uppercase !smt_logic) ast in
    Buffer.output_buffer !output smt;
    show_msgs_and_exit !msgs OK
  end
  else if (!smt_logic <> "") && !solve_sat then begin
    #ifdef yices2
      let ast,msgs = Parse.parse_smt ~debug:!debug_syntax (string_of_chan !input)
          |> Eval.eval ~smt:(!smt_logic <> "") in
      let str = Solvesmt.ast_to_yices ast |> Solvesmt.model (String.uppercase !smt_logic) in
      if str = ""
      then (Printf.fprintf stderr "Unsat\n"; show_msgs_and_exit !msgs ERROR)
      else Printf.fprintf !output "%s\n" str;
      show_msgs_and_exit !msgs OK
  #else
      Printf.fprintf stderr
        ("This touist binary has not been compiled with yices2 support, cannot solve with --smt. You can still solve with --sat.");
      exit_with ERROR
  #endif
  end;

  close_out !output;
  close_out !output_table;
  close_in !input;
  
  exit_with OK

  with
    | Msgs.Fatal msgs -> (show_msgs_and_exit msgs ERROR)
