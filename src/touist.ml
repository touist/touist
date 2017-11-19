open TouistParse
open TouistErr
open Cmdliner
type error =
  | OK
  | UNKNOWN
  | CMD_USAGE
  | CMD_UNSUPPORTED
  | TOUIST_SYNTAX
  | TOUIST_TIMEOUT
  | TOUIST_MEMORY
  | TOUIST_UNKNOWN
  | SOLVER_UNSAT
  | SOLVER_UNKNOWN
  | SOLVER_TIMEOUT
  | SOLVER_MEMORY

let get_code (e : error) : int = match e with
  | OK              -> 0
  | UNKNOWN         -> 1
  | CMD_USAGE       -> 2
  | CMD_UNSUPPORTED -> 3
  | TOUIST_SYNTAX   -> 4
  | TOUIST_TIMEOUT  -> 5
  | TOUIST_MEMORY   -> 6
  | TOUIST_UNKNOWN  -> 7
  | SOLVER_UNSAT    -> 8
  | SOLVER_UNKNOWN  -> 9
  | SOLVER_TIMEOUT  -> 10
  | SOLVER_MEMORY   -> 11

type language = Sat | Smt | Qbf
let sat_flag = ref false
let qbf_flag = ref false
let smt_flag = ref ""
let mode = ref Sat
let input_file_path = ref "/dev/stdin"
let output_file_path = ref ""
let output_table_file_path = ref ""
let output_file_basename = ref ""
let use_stdin = ref false
let output = ref stdout
let output_table = ref stdout
let input = ref stdin
let solve_flag = ref false
let limit = ref 1
let only_count = ref false
let show_hidden_lits = ref false
let equiv_file_path = ref ""
let input_equiv = ref stdin
let linter = ref false (* for displaying syntax errors (during parse and eval) *)
let error_format = ref "%f: line %l, col %c-%C: %t: %m" (* display absolute position of error *)
let debug = ref false
let debug_syntax = ref false
let debug_cnf = ref false
let latex = ref false
let latex_full = ref false
let show = ref false
let wrap_width = ref 76
let sat_interactive = ref false
let debug_dimacs = ref false
let solver_ext = ref None

(* [process_arg_alone] is the function called by the command-line argument
   parser when it finds an argument with no preceeding -flag (-f, -x...).
   In our case, an argument not preceeded by a flag is the touistl input file. *)
let process_arg_alone (file_path:string) : unit = input_file_path := file_path

let exit_with (exit_code:error) = exit (get_code exit_code)

(* WARNING: lit_of_int should be capable of handling as input
   - non-zero negative integers -> should be transformed into a negative literal
   - non-zero positive integers -> transformed into a positive literal
   - zero will never be passed. *)
let solve_ext lit_tbl lit_abs lit_sign lit_of_int (print_dimacs:out_channel -> unit) cmd =
  let prog, opts = match cmd |> Re_str.split (Re_str.regexp " +") with
    | v::next -> v, Array.of_list next
    | _ -> failwith "error with --solver 'command'" in
  if !debug then Printf.eprintf "== cmd: '%s %s'\n" prog (opts |> Array.fold_left (fun acc v-> if acc = "" then v else v^" "^acc) "");
  if !debug then (Printf.eprintf "== stdin given to '%s':\n" cmd; print_dimacs stderr);
  let proc_stdout, proc_stdin, proc_stderr = Unix.open_process_full cmd [||] in
  print_dimacs proc_stdin; close_out proc_stdin;
  (* Try to parse a space-separated list of integers; returns empty list if
     something is not an integer. *)
  let ints_of_string s =
    try begin
      s |> Re_str.split (Re_str.regexp " +") |> List.fold_left
        (fun acc s -> match int_of_string s with
           | v when v!=0 -> lit_of_int v :: acc
           | _ -> acc (* in DIMACS, '0' are line endings; we skip '0' *)) []
    end with Failure err -> []
  in
  let rec process in_chan =
    try let line = input_line in_chan in
      if !debug then Printf.eprintf "%s\n" line;
      let line_lst =
        match String.get line 0 with
        | 'V' | 'v' -> String.sub line 2 (String.length line -2) |> ints_of_string
        | _ -> ints_of_string line
        | exception Invalid_argument _ (* index out of bounds, if line empty *) -> []
      in line_lst @ (process in_chan)
    with End_of_file -> []
  in
  if !debug then Printf.eprintf "== stdout from '%s':\n" cmd;
  let valuated_lits = process proc_stdout in
  if !debug then Printf.eprintf "== stderr from '%s':\n%s" cmd (TouistParse.string_of_chan proc_stderr);
  match Unix.close_process_full (proc_stdout, proc_stdin, proc_stderr) with
  | Unix.WEXITED 10 when List.length valuated_lits > 0 ->
    (lit_tbl |> Hashtbl.iter (fun lit lit_name ->
         if lit_name.[0] <> '&' || !show_hidden_lits then
           let valuation = (
             try valuated_lits |> List.find (fun l -> lit = (lit_abs l)) |> fun v ->
                 if lit_sign v then "1" else "0"
             with Not_found -> "?")
           in Printf.fprintf !output "%s %s\n" valuation lit_name);
     exit_with OK)
  | Unix.WEXITED 127 -> (Printf.eprintf "Command '%s' not found (try with --debug)\n"
                           (match !solver_ext with Some s -> s | None -> "???"); exit_with SOLVER_UNKNOWN)
  | Unix.WEXITED 10 -> (Printf.eprintf
                          "Command '%s' returned SAT but did not print a model (try with --debug)\n\
                           A DIMACS model line must be integers optionally beginning with v or V\n\
                           and optionally ending with 0.\n"
                          (match !solver_ext with Some s -> s | None -> "???"); exit_with SOLVER_UNKNOWN)
  | Unix.WEXITED 20 -> (Printf.eprintf
                          "Command '%s' returned UNSAT (try with --debug)\n"
                          (match !solver_ext with Some s -> s | None -> "???"); exit_with SOLVER_UNSAT)
  | Unix.WEXITED c -> Printf.eprintf
                        "Command '%s' returned unknown code %d (try with --debug)\nExpected codes are 10 for SAT and 20 for UNSAT\n"
                        (match !solver_ext with Some s -> s | None -> "???") c; exit_with SOLVER_UNKNOWN;
  | _ -> Printf.eprintf "Error with %s, received signal\n" (cmd)

let main _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = print_endline "ok"

(* Use of Cmdliner has been inspired by:
   compiler/compileArg.ml in https://github.com/ocsigen/js_of_ocaml,
   bin/main.ml in https://github.com/janestreet/jbuilder. *)
let input_t =
  Arg.(value & pos 0 string "" & info [] ~docv:"INPUT" ~doc:
    {|The input TouIST file. Use $(b,-) to read from the standard input
    instead.|})
let output =
  Arg.(value & opt (some file) None & info ["o";"output"] ~docv:"OUTPUT" ~doc:
    {|Select the file $(docv) for printing results. With $(b,--sat), $(b,--smt)
    or $(b,--qbf), results will be respectively the DIMACS, QDIMACS and SMT-LIB
    translations of the TouIST given in $(i,INPUT).|})
let output_table =
  Arg.(value & (opt file "") & info ["table"] ~docv:"TABLE" ~doc:
    {|Select the file $(docv) for printing the mapping table. Only useful
    for $(b,--sat) and $(b,--qbf). With this option, the mapping between
    the variable names and the DIMACS integers will be printed to $(docv).
    Without $(b,--table), the table will be displayed as comments at the
    beginning of the DIMACS output:
        c <prop-name> <n>
    where $(b,<n>) is the positive non-zero integer associated with
    $(b,<prop-name>). |})
let language =
  Arg.(value & vflag Sat [
    Smt, Arg.info ["smt"] ~doc:{|Select SMT TouIST language as input.|};
    Qbf, Arg.info ["qbf"] ~doc:{|Select QBF TouIST language as input.|};
    Sat, Arg.info ["sat"] ~doc:{|Select SAT TouIST language as input
      (enabled by default when $(b,--smt) or $(b,--qbf) not selected)|}])
let smt_logic =
  Arg.(value & opt (some string) None ~vopt:(Some "QF_LIA") & info ["logic"] ~docv:"LOGIC" ~doc:
    {|Select the $(docv) to be used in SMT mode. $(docv) must be a known
    SMT2-LIB logic; note that the TouIST language can only express these four
    logics:
      * QF_IDL allows to deal with boolean and integer, E.g, x - y < b
      * QF_RDL is the same as QF_IDL but with reals
      * QF_LIA (not documented)
      * QF_LRA (not documented)
    See http://smtlib.cs.uiowa.edu/logics.shtml for more info.|})
let debug_flag =
  Arg.(value & flag & info ["debug"] ~doc:{|Print information for debugging
    touist. More specifically:
    * with $(b,--solver), prints the stdin, stdout and stderr of the given
      solver;
    * print the 'loc' (location) when displaying syntax errors;
    * when an exception is raised, print the call stack.|})
let debug_syntax_flag =
  Arg.(value & flag & info ["debug-syntax"] ~doc:{|When a syntax error is
    displayed, also give the automaton number; this is useful when trying
    to fix a wrong error message in `src/lib/parser.messages'.|})
let debug_cnf_flag =
  Arg.(value & flag & info ["debug-cnf"] ~doc:{|Print step by step CNF
    transformation (only with $(b,--sat) and $(b,--qbf)|})
let show_flag =
  Arg.(value & flag & info ["show"] ~doc:{|Show the expanded Abstract
    Syntaxic Tree after evaluation, i.e., after every `bigand', `bigor',
    variables and such are replaced by actual formulas. This option may
    be useful to understand why your TouIST input seems to be wrongly
    interpreted by $(mname).|})
let solve_flag =
  Arg.(value & flag & info ["solve"] ~doc:{|Solve the problem and print the
    first model if it exists. The solver is selected depending on the input
    language chosen:
    * with $(b,--sat), solve using MiniSat,
    * with $(b,--smt), solve using Yices,
    * with $(b,--qbf), solve using Quantor.|})
let limit =
  Arg.(value & opt int 1 & info ["limit"] ~docv:"N" ~doc:{|(with $(b,--solve)) Instead of
    one model, return $(docv) models if they exist. With $(i,0), return every
    possible model. WARNING: the number of model can be extremely high;
    this option is intended for small problems.|})
let count_flag =
  Arg.(value & flag & info ["count"] ~doc:{|(with $(b,--solve)) Instead of
  displaying models, return the number of models.|})
let latex_flag =
  Arg.(value & opt (some string) None ~vopt:(Some "mathjax") & info ["latex"] ~docv:"MODE" ~doc:
    {|Transform the TouIST input to LaTeX. The supported values for $(i,MODE)
    are `mathjax' and `document'.|})
let show_hidden_flag =
  Arg.(value & flag & info ["show-hidden"] ~doc:{|(with $(b,--solve)) Show
    the hidden '&a' literals used when translating to CNF.|})
let equiv_second_input =
  Arg.(value & opt (some file) None & info ["equiv"] ~docv:"INPUT2" ~doc:
    {|(with $(b,--solve)) Check that $(docv) has the same models as $(i,INPUT)
    (equivalency). Note that the equivalency is computed 'naively' by
    retrieiving all models and comparing them. In the future, we will instead
    combine both $(i,INPUT) $(docv) and check that one entails the other (and
    conversely).|})
let linter_flag =
  Arg.(value & flag & info ["linter"] ~doc:{|Display syntax and semantic errors
    and exit. With this option, we only do a minimal valuation so that it
    prints the errors as fast as possible.|})
let error_format =
  Arg.(value & opt string "" & info ["error-format"] ~docv:"FMT" ~doc:
    {|Customize the formatting of error messages.|})
let wrap_width =
  Arg.(value & opt int 76 & info ["wrap-width"] ~docv:"N" ~doc:
    {|Wrapping width for error messages.|})
let interactive_models =
  Arg.(value & flag & info ["interactive"] ~doc:
    {|($(b,--sat) mode) Display next model on key press; $(i,INPUT) must be a
    file.|})
let debug_dimacs_flag =
  Arg.(value & flag & info ["debug-dimacs"] ~doc:{|
    ($(b,--sat) and $(b,--qbf) modes) Display names instead of integers in
    DIMACS/QDIMACS|})
let solver_external =
  Arg.(value & opt (some string) None & info ["solver"] ~docv:"CMD" ~doc:
    {|($(b,--sat) and $(b,--qbf) modes) Use $(docv) for solving. $(docv)
    must expect DIMACS (QDIMACS) on standard input, print a DIMACS model on
    standard output and return 10 on SAT and 20 for UNSAT.|})
let cmd =
  let doc = "translate and solves SAT, QBF and SMT problems written in TouIST"
  and usage1 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(i,INPUT)"
  and usage2 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--solve) $(i,INPUT)"
  and usage3 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--solver)=$(i,CMD) $(i,INPUT)"
  and usage4 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--latex)[=$(i,MODE)] $(i,INPUT)"
  in
  let man = [
    `S Manpage.s_synopsis;
    `P usage1; `Noblank;
    `P usage2; `Noblank;
    `P usage3; `Noblank;
    `P usage4;
    `S Manpage.s_description;
    `P "$(tname) translates and solves problems written in TouIST language,
       which is based on propositional logic (SAT) with extensions to SMT and
       QBF. Output formats for translation include DIMACS, QDIMACS and SMT-LIB.";
    `P "To select the standard input, use $(b,-) as $(i,INPUT).";
    `P ("Embedded solvers available: "^"minisat"
          ^ if TouistSmtSolve.enabled then ", yices2" else ""
          ^ if TouistQbfSolve.enabled then ", qbf" else "");
    `S "LANGUAGE";
    `P "$(mname) accepts three language variants:";
    `I ("$(b,--sat)", "Propositional logic. It is the language
            selected by default, you may omit $(b,--sat).");
    `I ("$(b,--smt)[=$(i,LOGIC)]", "SAT Modulo Theory (SMT). By default,
            $(i,LOGIC) is set to `QF_LIA' (linear integers algebra).");
    `I ("$(b,--qbf)","Quantified boolean formulas (QBF).");
    `S "MODES";
    `P "$(mname) has multiple modes:";
    `I (usage1,
        "translation to solver-compatible formats: DIMACS (with $(b,--sat)),
          QDIMACS (with $(b,--qbf)) and SMT-LIB (with $(b,--smt)[=$(i,LOGIC)]");
    `I (usage2,
        "solving with $(b,--solve) using embedded solvers MiniSat
        (for $(b,--sat)), Quantor (for $(b,--qbf)) and Yices
        (for $(b,--smt)[=$(i,LOGIC)])");
    `I (usage3,
        "solving using an external solver with $(b,--solver=)$(i,CMD)");
    `I (usage4,
        "translation to LaTeX with $(b,--latex)[=$(i,MODE)]");
    `S Manpage.s_bugs; `P "Report bugs to <mael.valais@gmail.com>.";
    `S Manpage.s_see_also; `P "" ]
  in
  Term.(const main $ input_t $ output $ output_table $ language $ smt_logic
                   $ debug_flag $ debug_syntax_flag $ debug_cnf_flag
                   $ show_flag $ solve_flag $ limit $ count_flag $ latex_flag
                   $ show_hidden_flag $ equiv_second_input
                   $ linter_flag $ error_format $ wrap_width $ interactive_models
                   $ debug_dimacs_flag $ solver_external),
  Term.(info "touist" ~version:("%%VERSION%%")
     ~doc ~man ~exits:(List.map (fun (doc,err) -> exit_info ~doc (get_code err)) [
    "on success; with $(b,--solve) and $(b,--solver), also means SAT", OK;
    "unknown error", UNKNOWN;
    "command-line usage error", CMD_USAGE;
    "command-line option is unsupported", CMD_UNSUPPORTED;
    "TouIST syntax error", TOUIST_SYNTAX;
    "translation timeout", TOUIST_TIMEOUT;
    "translation memory overflow", TOUIST_MEMORY;
    "unknown $(tname) error", TOUIST_UNKNOWN;
    "on UNSAT problem", SOLVER_UNSAT;
    "unknown solver error", SOLVER_UNKNOWN;
    "solver timeout", SOLVER_TIMEOUT;
    "solver returned memory overflow", SOLVER_MEMORY;
    ]))

let () = Term.exit @@ Term.eval cmd
(*
  (* Step 1: we parse the args. If an arg. is "alone", we suppose
   * it is the touistl input file (this is handled by [process_arg_alone]) *)
  Arg.parse argspecs process_arg_alone usage; (* parses the arguments *)
  if !debug then Printexc.record_backtrace true;
  TouistErr.wrap_width := !wrap_width;
  TouistErr.format := !error_format;
  TouistErr.color := (Unix.isatty Unix.stderr);

  try

    (* Step 2: we see if we got every parameter we need *)

    (* Check (file | -) and open input and output *)
    if (!input_file_path = "/dev/stdin") && not !use_stdin (* NOTE: !var is like *var in C *)
    then fatal (Error,Usage,"you must give an input file (use - for reading from stdin).\n",None);

    if !use_stdin then input := stdin else input := open_in !input_file_path;

    let count = List.fold_left (fun acc v -> if v then acc+1 else acc) 0
    in
    if (count [!sat_flag; !smt_flag<>""; !qbf_flag]) > 1 then
      fatal (Error,Usage,"only one of --sat, --smt or --qbf must be given.\n",None);

    (* Set the mode *)
    if      !sat_flag     then mode := Sat
    else if !smt_flag<>"" then mode := Smt
    else if !qbf_flag     then mode := Qbf
    else mode := Sat;

      (* SMT Mode: check if one of the available QF_? has been given after --smt *)
    if TouistSmtSolve.enabled && (!mode = Smt) && not (TouistSmtSolve.logic_supported !smt_flag) then
        fatal (Error,Usage,"you must give a correct SMT-LIB \
                            logic after --smt (try --help)\nExample: --smt QF_IDL\n",None);

      if !output_file_path <> ""
      then output := open_out !output_file_path;

    if !output_table_file_path <> ""
    then output_table := open_out !output_table_file_path;

    if !equiv_file_path <> ""
    then input_equiv := open_in !equiv_file_path;

    let input_text = string_of_chan !input in

    (* latex = parse and transform with latex_of_ast *)
    (* linter = only show syntax and semantic errors *)
    if !latex || !latex_full || !linter then begin
      let ast_plain =
        match !mode with
        | Sat -> TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path input_text
        | Smt -> TouistParse.parse_smt ~debug:!debug_syntax ~filename:!input_file_path input_text
        | Qbf -> TouistParse.parse_qbf ~debug:!debug_syntax ~filename:!input_file_path input_text
      in
      (if !linter then let _= ast_plain |> TouistEval.eval ~smt:(!mode = Smt) ~onlychecktypes:true in ());
      (if !latex then Printf.fprintf !output "%s\n" (TouistLatex.latex_of_ast ~full:false ast_plain));
      (if !latex_full then Printf.fprintf !output
           "\\documentclass[fleqn]{article}\n\
            \\usepackage{mathtools}\n\
            \\allowdisplaybreaks\n\
            \\begin{document}\n\
            \\begin{multline*}\n\
            %s\n\
            \\end{multline*}\n\
            \\end{document}\n" (TouistLatex.latex_of_ast ~full:true ast_plain));
      exit_with OK
    end;

    if !show then begin
      let ast = match !mode with
        | Sat -> TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path input_text
                 |> TouistEval.eval ~smt:(!mode = Smt)
        | Smt -> TouistParse.parse_smt ~debug:!debug_syntax ~filename:!input_file_path input_text
                 |> TouistEval.eval ~smt:(!mode = Smt)
        | Qbf -> TouistParse.parse_qbf ~debug:!debug_syntax ~filename:!input_file_path input_text
                 |> TouistEval.eval ~smt:(!mode = Smt)
      in (Printf.fprintf !output "%s\n" (TouistPprint.string_of_ast ~utf8:true ast);
          exit_with OK)
    end;

    (* Step 3: translation *)
    match !mode, !solve_flag, !solver_ext with
    (* A. solve has been asked *)
    | Sat, true, None ->
      if !equiv_file_path <> "" then begin
        let solve input =
          let ast = TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path (string_of_chan input) |> TouistEval.eval
          in let models = TouistCnf.ast_to_cnf ~debug:!debug_cnf ast |> TouistSatSolve.minisat_clauses_of_cnf |> TouistSatSolve.solve_clauses ~verbose:!debug
          in models
        in
        let models = solve !input
        and models2 = solve !input_equiv
        in match TouistSatSolve.ModelSet.equal !models !models2 with
        | true -> Printf.fprintf !output "Equivalent\n"; exit_with OK
        | false -> Printf.fprintf !output "Not equivalent\n"; exit_with SOLVER_UNSAT
      end
      else
        let ast = TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path input_text |> TouistEval.eval in
        let clauses,table = TouistCnf.ast_to_cnf ~debug:!debug_cnf ast |> TouistSatSolve.minisat_clauses_of_cnf
        in
        let models =
          begin
            if !only_count then TouistSatSolve.solve_clauses ~verbose:!debug (clauses,table)
            else
              let print_model model i =
                if !limit != 1
                then Printf.fprintf !output "==== model %d\n%s\n" i (TouistSatSolve.Model.pprint ~sep:"\n" table model)
                else Printf.fprintf !output "%s\n" (TouistSatSolve.Model.pprint ~sep:"\n" table model)
              (* Notes:
                 1) print_endline vs. printf: print_endline is a printf with
                 'flush stdout' at the end. If we don't put 'flush', the user
                 won't see the output before scanf asks for some input.
                 2) scanf: I don't know!!! *)
              (* [continue_asking] is needed by [solve_clauses]; it simply
                 asks after displaying each model if it should continue
                 displaying the next one. *)
              and continue_asking _ i =
                Printf.fprintf !output "=== count: %d; continue? (y/n) " i; flush stdout;
                try Scanf.scanf "%s\n" (fun c -> not (c="q" || c="n"))
                with End_of_file -> Printf.eprintf "error: could not enter \
                                                    interactive mode as stdin unexpectedly closed\n";
                  exit_with UNKNOWN
              (* stdin unexpectedly closed (e.g., piped cmd) *)
              (* [continue_limit] is also needed by [solve_clauses]. It stops
                 the display of models as soon as the number of models displayed
                 reaches [limit]. *)
              and continue_limit _ i = i < !limit
              in
              TouistSatSolve.solve_clauses ~verbose:!debug ~print:print_model
                ~continue:(if !sat_interactive then continue_asking else continue_limit)
                (clauses,table)
          end
        in
        (match TouistSatSolve.ModelSet.cardinal !models with
         | i when !only_count -> Printf.fprintf !output "%d\n" i; exit_with OK
         | 0 -> Printf.fprintf stderr "unsat\n"; exit_with SOLVER_UNSAT
         | 1 -> (* case where we already printed models in [solve_clause] *)
           exit_with OK
         | i -> (* case where we already printed models in [solve_clause] *)
           Printf.fprintf !output "==== found %d models, limit is %d (--limit N for more models)\n" i !limit; exit_with OK)
    | Sat, false, None -> (* B. solve not asked: print the Sat file *)
      let ast = TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path input_text |> TouistEval.eval in
      let clauses,tbl = TouistCnf.ast_to_cnf ~debug:!debug_cnf ast |> TouistSatSolve.minisat_clauses_of_cnf
      in TouistSatSolve.print_dimacs ~debug_dimacs:!debug_dimacs (clauses,tbl)
        ~out_table:!output_table !output;
      exit_with OK
    | Sat, _, Some cmd ->
      let ast = TouistParse.parse_sat ~debug:!debug_syntax ~filename:!input_file_path input_text |> TouistEval.eval in
      let clauses,tbl = TouistCnf.ast_to_cnf ~debug:!debug_cnf ast |> TouistSatSolve.minisat_clauses_of_cnf
      in cmd |> solve_ext tbl (Minisat.Lit.abs) (Minisat.Lit.sign)
           (* because given integers can be negative: *)
           (fun v -> abs v |> Minisat.Lit.make |> fun l -> if v>0 then l else Minisat.Lit.neg l)
           (TouistSatSolve.print_dimacs ~debug_dimacs:!debug_dimacs (clauses,tbl))
    | Smt, false, _ ->
      let ast = TouistParse.parse_smt ~debug:!debug_syntax ~filename:!input_file_path input_text
                |> TouistEval.eval ~smt:(!mode = Smt) in
      let smt = TouistSmt.to_smt2 !smt_flag ast in
      Buffer.output_buffer !output smt;
      exit_with OK
    | Smt, true, _ ->

let ast = TouistParse.parse_smt ~debug:!debug_syntax ~filename:!input_file_path input_text
          |> TouistEval.eval ~smt:(!mode = Smt) in
let yices_form,tbl = TouistSmtSolve.ast_to_yices ast in
(match TouistSmtSolve.solve !smt_flag yices_form with
 | None -> Printf.fprintf stderr "unsat\n"; exit_with SOLVER_UNSAT |> ignore
 | Some m -> Printf.fprintf !output "%s\n" (TouistSmtSolve.string_of_model tbl m); exit_with OK |> ignore);

| Qbf, _, _ -> begin
    let ast = TouistParse.parse_qbf ~debug:!debug_syntax ~filename:!input_file_path input_text
              |> TouistEval.eval ~smt:(!mode = Smt) in
    let prenex = TouistQbf.prenex ast in
    let cnf = TouistQbf.cnf prenex in
    if !debug_cnf then begin
      Printf.fprintf stderr "formula: %s\nprenex: %s\ncnf: %s\n"
        (TouistPprint.string_of_ast ~utf8:true ast)
        (TouistPprint.string_of_ast ~utf8:true prenex)
        (TouistPprint.string_of_ast ~utf8:true cnf)
    end;
    match !solver_ext, !solve_flag with
    | Some cmd, _ -> (* --qbf + --solver CMD: use external solver *)
      let quants,int_clauses,int_tbl = TouistQbf.qbfclauses_of_cnf cnf
      in cmd |> solve_ext int_tbl (abs) (fun v -> v>0) (fun v -> v)
           (TouistQbf.print_qdimacs (quants,int_clauses,int_tbl))
    | None, false -> (* --qbf: we print QDIMACS *)
      let quants,int_clauses,int_tbl = TouistQbf.qbfclauses_of_cnf cnf in
      TouistQbf.print_qdimacs ~debug_dimacs:!debug_dimacs
        (quants,int_clauses,int_tbl) ~out_table:!output_table !output
    | None, true -> (* --qbf + --solve: we solve using Quantor *)
    let qcnf,table = TouistQbfSolve.qcnf_of_cnf cnf in
    match TouistQbfSolve.solve ~hidden:!show_hidden_lits (qcnf,table) with
    | Some str -> Printf.fprintf !output "%s\n" str
    | None -> (Printf.fprintf stderr "unsat\n"; exit_with SOLVER_UNSAT |> ignore);
end;

(* I had to comment these close_out and close_in because it would
   raise 'bad descriptor file' for some reason. Because the program is always
   going to shut at this point, we can rely on the operating system for closing
   opened files.

   close_out !output;
   close_out !output_table;
   close_in !input;
*)

exit_with OK

with
(* Warning: the [msg] already contains an ending '\n'. No need for adding
   another ending newline after it. *)
| TouistFatal msg ->
  if !debug then Printf.eprintf "Stacktrace:\n%s\n" (Printexc.get_backtrace ());
  Printf.eprintf "%s" (TouistErr.string_of_msg msg);
  exit_with (match msg with _,Usage,_,_ -> CMD_USAGE | _ -> TOUIST_SYNTAX)
| Sys_error err ->
  (* [err] is provided by the system; it doesn't end with a newline. *)
  Printf.eprintf "%s\n" (TouistErr.string_of_msg (Error,Usage,err,None));
  exit_with CMD_USAGE
| x -> raise x
*)

