open Touist
open Cmdliner
type error =
  | OK (* also means SAT *)
  | UNSAT
  | TRANSL_ERROR
  | SOLVER_ERROR
  | CLI_ERROR
  | BUG

let get_code (e : error) : int = match e with
  | OK (* SAT *)    -> 0
  | UNSAT           -> 8
  | TRANSL_ERROR    -> 50
  | SOLVER_ERROR    -> 100
  | CLI_ERROR       -> 124
  | BUG             -> 125

let code_msgs = [
  "on success (or SAT with $(b,--solve) and $(b,--solver)).", OK;
  "on UNSAT (with $(b,--solve) and $(b,--solver)).", UNSAT;
  "on TouIST syntax error or translation error.", TRANSL_ERROR;
  "on solver error (with $(b,--solve) and $(b,--solver)).", SOLVER_ERROR;
  "on command line parsing errors.", CLI_ERROR;
  "on unexpected internal errors (bugs).", BUG;
]

type lang =
  | Sat
  | Qbf
  | Smt of string
let string_of_lang =
  function Sat->"--sat" | Qbf->"--qbf" | Smt l->("--smt="^l)

type solve_opts ={
  count: bool;
  limit: int;
  interactive: bool;
  equiv: (string * in_channel) option;
  hidden: bool; (* show hidden lits on model display *)
}
type solve_ext_opt = {
  cmd: string;
  hidden: bool; (* show hidden lits on model display *)
}
type transl_opt = { linter: bool; table: (string * out_channel) option; debug_dimacs: bool}
type latex_mode = Mathjax | Katex | Document
type latex_opt = {
  mode: latex_mode;
  linter: bool;
}
type show =
  | Form
  | Cnf | CnfDuring (* print info during the expansion of the cnf *)
  | Prenex | PrenexDuring
type mode =
  | Solve of solve_opts
  | SolveExt of solve_ext_opt
  | Translate of transl_opt
  | Latex of latex_opt
  | Show of show
type common_opt = {
  error_format: string;
  wrap_width: int;
  verbose: int; (* 0 = off, 1 = a bit verbose, 0 = more verbose *)
}

let exit_with (exit_code:error) = exit (get_code exit_code)

(* WARNING: lit_of_int should be capable of handling as input
   - non-zero negative integers -> should be transformed into a negative literal
   - non-zero positive integers -> transformed into a positive literal
   - zero will never be passed. *)
let solve_ext common_opt show_hidden output lit_tbl lit_abs lit_sign lit_of_int (print_dimacs:?line_begin:string -> out_channel -> unit) cmd =
  let prog, opts = match cmd |> Re.Str.(split (regexp " +")) with
    | v::next -> v, Array.of_list next
    | _ -> failwith ("error with --solver=cmd") in
  if common_opt.verbose>1 then Printf.eprintf "== cmd: '%s %s'\n" prog (opts |> Array.fold_left (fun acc v-> if acc = "" then v else v^" "^acc) "");
  if common_opt.verbose>1 then (print_dimacs ~line_begin:"== stdin: " stderr);
  let start = Unix.gettimeofday () in
  let proc_stdout, proc_stdin, proc_stderr = Unix.open_process_full cmd (Unix.environment ()) in
  if common_opt.verbose>0 then Printf.eprintf "== solve time: %f sec\n" ((Unix.gettimeofday ()) -. start);
  print_dimacs proc_stdin; close_out proc_stdin;
  (* Try to parse a space-separated list of integers; returns empty list if
     something is not an integer. *)
  let ints_of_string s =
    try begin
      s |> Re.Str.(split (regexp " +")) |> List.fold_left
        (fun acc s -> match int_of_string s with
           | v when v!=0 -> lit_of_int v :: acc
           | _ -> acc (* in DIMACS, '0' are line endings; we skip '0' *)) []
    end with Failure _ -> []
  in
  let rec process in_chan =
    try let line = input_line in_chan in
      if common_opt.verbose>1 then Printf.eprintf "== stdout: %s\n" line;
      let line_lst =
        match String.get line 0 with
        | 'V' | 'v' -> String.sub line 2 (String.length line -2) |> ints_of_string
        | _ -> ints_of_string line
        | exception Invalid_argument _ (* index out of bounds, if line empty *) -> []
      in line_lst @ (process in_chan)
    with End_of_file -> []
  in
  let valuated_lits = process proc_stdout in
  let rec string_of_chan chan prefix = match input_line chan with
    | t -> prefix ^ t ^ "\n" ^ string_of_chan chan prefix
    | exception End_of_file -> ""
  in
  if common_opt.verbose>1 then Printf.eprintf "%s" (string_of_chan proc_stderr "== stderr: ");
  match Unix.close_process_full (proc_stdout, proc_stdin, proc_stderr) with
  | Unix.WEXITED 10 when List.length valuated_lits > 0 ->
    (lit_tbl |> Hashtbl.iter (fun lit lit_name ->
         if lit_name.[0] <> '&' || show_hidden then
           let valuation = (
             try valuated_lits |> List.find (fun l -> lit = (lit_abs l)) |> fun v ->
                 if lit_sign v then "1" else "0"
             with Not_found -> "?")
           in Printf.fprintf output "%s %s\n" valuation lit_name);
     exit_with OK)
  | Unix.WEXITED 127 ->
    (Printf.eprintf
       "Command '%s' not found (try with -v2)\n" cmd;
     exit_with CLI_ERROR)
  | Unix.WEXITED 10 ->
    (Printf.eprintf
       "Command '%s' returned SAT but did not print a model (try with -v2)\n\
        A DIMACS model line must be integers optionally beginning with v or V\n\
        and optionally ending with 0.\n" cmd;
     exit_with SOLVER_ERROR)
  | Unix.WEXITED 20 ->
    (Printf.eprintf
       "Command '%s' returned UNSAT (try with -v2)\n" cmd;
     exit_with UNSAT)
  | Unix.WEXITED c ->
    Printf.eprintf
      "Command '%s' returned unknown code %d (try with -v2)\n\
       Expected codes are 10 for SAT and 20 for UNSAT\n"
      cmd c; exit_with SOLVER_ERROR;
  | _ -> Printf.eprintf "Error with %s, received signal\n" (cmd)


(* Step 1: we parse the args. If an arg. is "alone", we suppose
 * it is the touistl input file (this is handled by [process_arg_alone]) *)
let main (lang,mode) (input,_input_f) (_output,output_f: string * out_channel) common_opt =
  (* if common_opt.verbose then Printexc.record_backtrace true; *)
  Err.wrap_width := common_opt.wrap_width;
  Err.format := common_opt.error_format;
  Err.color := (Unix.isatty Unix.stderr);
  try
    let input_f = if input="/dev/stdin" then stdin else open_in input in

    let input_text = Parse.string_of_chan input_f in
    let smt = match lang with Smt _ -> true | _-> false in
    let debug_syntax = common_opt.verbose>0 in

    let ast_plain text =
      match lang with
      | Sat -> Parse.parse_sat ~debug_syntax ~filename:input text
      | Smt _ -> Parse.parse_smt ~debug_syntax ~filename:input text
      | Qbf -> Parse.parse_qbf ~debug_syntax ~filename:input text
    in begin
      (* latex = parse and transform with latex_of_ast *)
      (* linter = only show syntax and semantic errors *)
      match lang,mode with
      | _, Latex {mode;linter} -> let ast_plain = ast_plain input_text in
        (if linter then ast_plain |> Eval.eval ~smt ~onlychecktypes:true |>ignore);
        (if mode=Mathjax then Printf.fprintf output_f "%s\n" (Latex.latex_of_ast ~full:false ast_plain));
        (if mode=Katex then Printf.fprintf output_f "%s\n" (Latex.latex_of_ast ~matrix_instead_of_substack:true ~full:false ast_plain));
        (if mode=Document then Printf.fprintf output_f
             "\\documentclass[fleqn]{article}\n\
              \\usepackage{mathtools}\n\
              \\allowdisplaybreaks\n\
              \\begin{document}\n\
              \\begin{multline*}\n\
              %s\n\
              \\end{multline*}\n\
              \\end{document}\n" (Latex.latex_of_ast ~full:true ast_plain));
      | _, Translate {linter=true; _} ->
        (ast_plain input_text |> Eval.eval ~smt ~onlychecktypes:true |> ignore; exit_with OK)
      | _, Show Form -> let ast = match lang with
          | Sat   -> Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
          | Smt _ -> Parse.parse_smt ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
        in (Printf.fprintf output_f "%s\n" (Pprint.string_of_ast ~utf8:true ast); exit_with OK)
      | _, Show Cnf -> let ast = (match lang with
          | Sat   -> Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval ~smt |> Cnf.ast_to_cnf
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text |> Eval.eval ~smt |> Qbf.cnf
          | Smt _ -> failwith "no --show=form with --smt")
        in (Printf.fprintf output_f "%s\n" (Pprint.string_of_ast ~utf8:true ast); exit_with OK)
      | _, Show CnfDuring -> (match lang with
          | Sat   -> Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval ~smt |> Cnf.ast_to_cnf ~debug_cnf:true
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text |> Eval.eval ~smt |> Qbf.cnf ~debug_cnf:true
          | Smt _ -> failwith "no --show=duringcnf with --smt") |> ignore
      | _, Show Prenex -> let ast = match lang with
          | Sat   -> failwith "no --show=prenex with --smt"
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text
                     |> Eval.eval ~smt |> Qbf.cnf |>Qbf.prenex
          | Smt _ -> failwith "no --show=prenex with --smt"
        in (Printf.fprintf output_f "%s\n" (Pprint.string_of_ast ~utf8:true ast); exit_with OK)
      | _, Show PrenexDuring -> let ast = match lang with
          | Sat   -> failwith "no --show=duringprenex with --smt"
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text
                     |> Eval.eval ~smt |> Qbf.cnf |>Qbf.prenex ~debug:true
          | Smt _ -> failwith "no --show=duringprenex with --smt"
        in (Printf.fprintf output_f "%s\n" (Pprint.string_of_ast ~utf8:true ast); exit_with OK)
        (*
        if common_opt.verbose_cnf then begin
          Pprint.(Printf.fprintf stderr "formula: %s\nprenex: %s\ncnf: %s\n"
                    (string_of_ast ~utf8:true ast) (string_of_ast ~utf8:true prenex)
                    (string_of_ast ~utf8:true cnf))
        end;
        *)
      (* A. solve has been asked *)
      | Sat, Solve {equiv=Some (input2,input2_f); _} ->
        let solve (filename,inputchan) =
          Parse.(parse_sat ~debug_syntax ~filename (string_of_chan inputchan))
          |> Eval.eval |> Cnf.ast_to_cnf |> SatSolve.minisat_clauses_of_cnf
          |> SatSolve.solve_clauses ~verbose:(common_opt.verbose>0)
        in
        let models = solve (input,input_f) and models2 = solve (input2,input2_f) in
        (match SatSolve.ModelSet.equal models models2 with
         | true -> Printf.fprintf output_f "Equivalent\n"; exit_with OK
         | false -> Printf.fprintf output_f "Not equivalent\n"; exit_with UNSAT)
      | Sat, Solve {equiv=None; count=true; _} ->
        SatSolve.(Parse.parse_sat ~debug_syntax ~filename:input input_text
                  |> Eval.eval |> Cnf.ast_to_cnf |> minisat_clauses_of_cnf
                  |> solve_clauses ~verbose:(common_opt.verbose>0) |> ModelSet.cardinal
                  |>  Printf.fprintf output_f "%d\n"; exit_with OK)
      | Sat, Solve {equiv=None; limit; count=false; interactive; _} ->
        let print_model table model i =
          if limit != 1
          then Printf.fprintf output_f "==== model %d\n%s\n" i (SatSolve.Model.pprint ~sep:"\n" table model)
          else Printf.fprintf output_f "%s\n" (SatSolve.Model.pprint ~sep:"\n" table model)
        (* Notes:
           1) print_endline vs. printf: print_endline is a printf with
           'flush stdout' at the end. If we don't put 'flush', the user
           won't see the output before scanf asks for some input.
           2) scanf: I don't know!!! *)
        (* [continue_asking] is needed by [solve_clauses]; it simply
           asks after displaying each model if it should continue
           displaying the next one. *)
        and continue_asking _ i =
          Printf.fprintf output_f "== count: %d; continue? (y/n)\n" i; flush stdout;
          try Scanf.scanf "%s\n" (fun c -> not (c="q" || c="n"))
          with End_of_file -> (* stdin unexpectedly closed (e.g., piped cmd) *)
            (Printf.eprintf "touist: interactive mode stopped as stdin unexpectedly closed\n";
             exit_with BUG)
        (* [continue_limit] is also needed by [solve_clauses]. It stops
           the display of models as soon as the number of models displayed
           reaches [limit]. *)
        and continue_limit _ i = i < limit || limit = 0
        in
        let start = Unix.gettimeofday () in
        let cls,tbl = Parse.parse_sat ~debug_syntax ~filename:input input_text
                      |> Eval.eval |> Cnf.ast_to_cnf |> SatSolve.minisat_clauses_of_cnf
        in
        if common_opt.verbose>0 then
          (Printf.eprintf "== literals: %d\n== clauses: %d\n== translation time: %f sec\n"
             (Hashtbl.length tbl) (List.length cls) (Unix.gettimeofday () -. start));
        let start = Unix.gettimeofday () in
        let models =
          (cls,tbl) |> SatSolve.solve_clauses ~verbose:(common_opt.verbose>0)
            ~print:print_model
            ~continue:(if interactive then continue_asking else continue_limit)
        in
        (if common_opt.verbose>0 then Printf.eprintf "== solve time: %f sec\n" (Unix.gettimeofday () -. start));
        (match SatSolve.ModelSet.cardinal models with
         | 0 -> Printf.fprintf stderr "unsat\n"; exit_with UNSAT
         | 1 -> (* case where we already printed models in [solve_clause] *)
           exit_with OK
         | i -> (* case where we already printed models in [solve_clause] *)
           Printf.fprintf output_f
             "== found %d models, limit is %d (--limit=N for more models)\n" i limit;
             exit_with OK)
      | Sat, Translate {linter=false; table; debug_dimacs} -> (* B. solve not asked: print the Sat file *)
        let start = Unix.gettimeofday () in
        let ast = Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval in
        let clauses,tbl = Cnf.ast_to_cnf ast |> SatSolve.minisat_clauses_of_cnf
        in SatSolve.print_dimacs ~debug_dimacs (clauses,tbl)
          ~out_table:(match table with Some (_,f) -> f | None -> output_f) output_f;
        (if common_opt.verbose>0 then Printf.eprintf "== translation time: %f sec\n" (Unix.gettimeofday () -. start));
        exit_with OK
      | Sat, SolveExt {cmd; hidden} ->
        let start = Unix.gettimeofday () in
        let ast = Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval in
        let clauses,tbl = Cnf.ast_to_cnf ast |> SatSolve.minisat_clauses_of_cnf in
        if common_opt.verbose>0 then (Printf.eprintf "== literals %d\n== clauses: %d\n== translation time: %f sec\n"
                                        (Hashtbl.length tbl) (List.length clauses) (Unix.gettimeofday () -. start));
        cmd |> solve_ext common_opt hidden output_f tbl (Minisat.Lit.abs) (Minisat.Lit.sign)
          (* because given integers can be negative: *)
          (fun v -> abs v |> Minisat.Lit.make |> fun l -> if v>0 then l else Minisat.Lit.neg l)
          (fun ?(line_begin="") out -> SatSolve.print_dimacs ~line_begin (clauses,tbl) out)
      | Smt logic, Translate _ ->
        let ast = Parse.parse_smt ~debug_syntax ~filename:input input_text
                  |> Eval.eval ~smt in
        ast |> Smt.to_smt2 logic  |> Buffer.output_buffer output_f;
        exit_with OK
      | Smt logic, Solve _ ->
        let start = Unix.gettimeofday () in
        let yices_form,tbl =
          Parse.parse_smt ~debug_syntax ~filename:input input_text
          |> Eval.eval ~smt |> Touist_yices2.SmtSolve.ast_to_yices in
        (if common_opt.verbose>0 then Printf.eprintf "== translation time: %f sec\n" (Unix.gettimeofday () -. start));
        let start = Unix.gettimeofday () in
        let res = Touist_yices2.SmtSolve.solve logic yices_form in
        (if common_opt.verbose>0 then Printf.eprintf "== solve time: %f sec\n" (Unix.gettimeofday () -. start));
        (match res with
         | None -> Printf.fprintf stderr "unsat\n"; exit_with UNSAT |> ignore
         | Some m -> Printf.fprintf output_f "%s\n" (Touist_yices2.SmtSolve.string_of_model tbl m); exit_with OK |> ignore);
      | Qbf, SolveExt {cmd; hidden} -> (* --qbf + --solver CMD: use external solver *)
        let start = Unix.gettimeofday () in
        let quants,int_clauses,int_tbl =
          Parse.parse_qbf ~debug_syntax ~filename:input input_text
          |> Eval.eval ~smt |> Qbf.prenex |> Qbf.cnf |> Qbf.qbfclauses_of_cnf
        in
        (if common_opt.verbose>0 then
           (Printf.eprintf "== literals: %d\n== clauses: %d\n== translation time: %f sec\n"
              (Hashtbl.length int_tbl) (List.length int_clauses) (Unix.gettimeofday () -. start));
         cmd |> solve_ext common_opt hidden output_f int_tbl
           (abs) (fun v -> v>0) (fun v -> v)
           (fun ?(line_begin="") out -> Qbf.print_qdimacs ~line_begin (quants,int_clauses,int_tbl) out);
        )
      | Qbf, Translate {table; debug_dimacs; _} -> (* --qbf: we print QDIMACS *)
        let start = Unix.gettimeofday () in
        let quants,int_clauses,int_tbl =
          Parse.parse_qbf ~debug_syntax ~filename:input input_text
          |> Eval.eval ~smt |> Qbf.prenex |> Qbf.cnf |> Qbf.qbfclauses_of_cnf in
        (Qbf.print_qdimacs ~debug_dimacs (quants,int_clauses,int_tbl)
           ~out_table:(match table with Some (_,f)->f | None->output_f) output_f);
        (if common_opt.verbose>0 then Printf.eprintf "== translation time: %f sec\n" (Unix.gettimeofday () -. start));
      | Qbf, Solve {hidden; _} -> (* --qbf + --solve: we solve using Quantor *)
        let start = Unix.gettimeofday () in
        let qcnf,table =
          Parse.parse_qbf ~debug_syntax ~filename:input input_text
          |> Eval.eval ~smt |> Qbf.prenex |> Qbf.cnf
          |> Touist_qbf.QbfSolve.qcnf_of_cnf
        in
        (if common_opt.verbose>0 then Printf.eprintf "== translation time: %f sec\n" (Unix.gettimeofday () -. start));
        let start = Unix.gettimeofday () in
        (match Touist_qbf.QbfSolve.solve ~hidden (qcnf,table) with
         | Some str -> Printf.fprintf output_f "%s\n" str
         | None -> (Printf.fprintf stderr "unsat\n"; exit_with UNSAT |> ignore));
         (if common_opt.verbose>0 then Printf.eprintf "== solve time: %f sec\n" (Unix.gettimeofday () -. start));
      | Smt _, SolveExt _ -> failwith "--solver not compatible with --smt"
    end;

    (* I had to comment these close_out and close_in because it would
        raise 'bad descriptor file' for some reason. Because the program is always
        going to shut at this point, we can rely on the operating system for closing
        opened files.

        close_out output_f;
        close_out output_f_table;
        close_in !input;
    *)

    exit_with OK

  with
  (* Warning: the [msg] already contains an ending '\n'. No need for adding
     another ending newline after it. *)
  | Err.TouistFatal msg ->
    if common_opt.verbose>0 then Printf.eprintf "Stacktrace:\n%s\n" (Printexc.get_backtrace ());
    Printf.eprintf "%s" (Err.string_of_msg msg);
    exit_with (match msg with _,Usage,_,_ -> CLI_ERROR | _ -> TRANSL_ERROR)
  | Sys_error err ->
    (* [err] is provided by the system; it doesn't end with a newline. *)
    Printf.eprintf "%s\n" (Err.string_of_msg (Error,Usage,err,None));
    exit_with CLI_ERROR
  | x -> raise x


(* Use of Cmdliner has been inspired by:
   compiler/compileArg.ml in https://github.com/ocsigen/js_of_ocaml,
   bin/main.ml in https://github.com/janestreet/jbuilder. *)
let input_conv =
  (function
    | "-" -> `Ok ("/dev/stdin",stdin)
    | path when Sys.file_exists path ->
      (try `Ok (path,open_in path)
       with Sys_error err -> `Error ("cannot open '"^path^"': "^err))
    | path -> `Error ("file not found: "^path)),
  (fun fmt (v,_) -> Format.fprintf fmt "%s" v)

let input =
  let input_arg =
    Arg.(value & pos 0 (some input_conv) None & info [] ~docv:"INPUT" ~doc:
           "The input TouIST file. Use $(b,-) to read from the standard
             input instead.")
  in let check_input = function
      | Some (input,input_t) -> `Ok (input,input_t)
      | None -> `Error (false,"you must give an input file (use - for reading from stdin)")
  in Term.(ret (const check_input $ input_arg))

let output_conv =
  (fun path ->
     try `Ok (path,open_out path)
     with Sys_error err -> `Error ("cannot open '"^path^"': "^err)),
  (fun fmt (v,_) -> Format.fprintf fmt "%s" v)

let output =
  Arg.(value & opt output_conv ("/dev/stdout",stdout) & info ["o";"output"] ~docv:"OUTPUT" ~doc:"\
    Select the file $(docv) for printing results. With $(b,--sat),
    $(b,--smt) or $(b,--qbf), results will be respectively the DIMACS,
    QDIMACS and SMT-LIB translations of the TouIST given in $(i,INPUT).")


let language_section = "LANGUAGES"
let language =
  let docs = language_section in
  let sat =
    Arg.(value & flag & info ["sat"] ~docs ~doc:"\
      Propositional logic. It is the language selected by default,
      you may omit $(b,--sat).") in
  let qbf =
    Arg.(value & flag & info ["qbf"] ~docs ~doc:"\
      Quantified boolean formulas (QBF).") in
  let smt =
    let smt_logic_conv =
      (fun logic ->
         if Re.(execp (compile (str "QF_")) logic) then `Ok logic
         else `Error ("logic name '"^logic^"' should begin with 'QF_'")),
      (fun fmt logic -> Format.fprintf fmt "%s" logic) in
    Arg.(value & opt (some smt_logic_conv) None ~vopt:(Some "QF_LRA") & info ["smt"]
           ~docv:"LOGIC" ~docs ~doc:"\
      Select the SAT Modulo Theory (SMT) input. By default, $(docv) is set to
      `QF_LRA' (Linear Real Arithmetic).")
  and one_of sat qbf smt =
    match sat, qbf   , smt   with
    | false  , false , Some l ->
      (* SMT Mode: check if one of the available QF_? has been given after
         --smt is Yices2 is available. *)
      (match Touist_yices2.SmtSolve.(enabled, logic_supported l) with
       | true, true | false, _ -> `Ok (Smt l)
       | true, false -> `Error (false,("given --smt=LOGIC '"^l^"' is not known (e.g., --smt=QF_IDL, see --help)")))
    | false  , true  , None   -> `Ok Qbf
    | _      , false , None   -> `Ok Sat (* default to sat *)
    | _      , _     , _      -> `Error (false,"only one of {--sat,--smt,--qbf} is allowed")
  in Term.(ret (const one_of $ sat $ qbf $ smt))


let external_solv_section = "EXTERNAL SOLVER"
let solvext_flag =
  Arg.(value & opt (some string) None & info ["solver"] ~docs:external_solv_section
         ~docv:"CMD" ~doc:"\
    (with {$(b,--sat),$(b,--qbf)}) Use $(docv) for solving. $(docv) must
    expect DIMACS (QDIMACS) on standard input, print a DIMACS model on
    standard output and exits with 10 on SAT and 20 for UNSAT. You can
    display the stdin, stdout and stderr of $(docv) using $(b,-v2).")

let solve_section = "SOLVE"
let solve_flags =
  let docs = solve_section in
  let solve_flag =
    Arg.(value & flag & info ["solve"] ~docs ~doc:"\
      Solve the problem
      and print the first model if it exists. The solver is selected
      depending on the input language.")
  and limit =
    Arg.(value & opt int 1 & info ["limit"] ~docs ~docv:"N" ~doc:"\
      (with $(b,--solve)) Instead of one model, return $(docv) models if
      they exist. With $(i,0), return every possible model. WARNING: the
      number of model can be extremely high; this option is intended for
      small problems.")
  and count_flag =
    Arg.(value & flag & info ["count"] ~docs ~doc:"\
      (with $(b,--solve)) Try to return the count of models instead of
      returning the models. This option will only work for small problems:
      the number of models explodes when the number of propositions is big.")
  and interactive =
    Arg.(value & flag & info ["interactive"] ~docs ~doc:"\
      ($(b,--sat) mode) Display the models one after
      the other. Press enter or any other key to continue and `q' or `n'
      to stop. $(i,INPUT) must be a file.")
  and equiv =
    Arg.(value & opt (some input_conv) None & info ["equiv"] ~docs ~docv:"INPUT2" ~doc:"\
      (with $(b,--solve)) Check that $(docv) has the same models as
      $(i,INPUT) (equivalency). Note that the equivalency is computed
      'naively' by retrieiving all models and comparing them. In the
      future, we will instead combine both $(i,INPUT) $(docv) and check
      that one entails the other (and conversely).")
  and hidden =
    Arg.(value & flag & info ["show-hidden"] ~docs ~doc:"\
    (with $(b,--solve)) Show the valuation of usually hidden '&a' literals.
    These literals are caused by the Tseitin transformation when translating
    the formula into CNF. Only useful for $(b,--sat) and $(b,--qbf).")
  in let check_solve_opts solv extsolv limit count interac equiv hidden =
       match solv,extsolv,limit,count,interac,equiv,hidden with
       | false   ,_      ,_    ,_    ,_      ,_    ,true  -> `Error (false,"--show-hidden only compatible with --solve or --solver")
       | true    ,Some _ ,_    ,_    ,_      ,_    ,_     -> `Error (false,"--solve and --solver cannot be used simultaneously")
       | false   ,None   ,1    ,false,false  ,None ,false -> `Ok None
       | false   ,Some c ,1    ,false,false  ,None ,hidden-> `Ok (Some (SolveExt {cmd=c;hidden}))
       | false   ,_      ,_    ,_    ,_      ,_    ,_     -> `Error (false,"the args {--limit,--count,--interactive,--equiv} can only be used with --solve")
       | true    ,_      ,limit,count,interac,equiv,hidden-> `Ok (Some (Solve {limit; count; interactive=interac; equiv; hidden}))

  in Term.(ret (const check_solve_opts $ solve_flag $ solvext_flag $ limit $ count_flag $ interactive $ equiv $ hidden))

let latex_section = "LATEX"
let latex_flag =
  Arg.(value & opt (some (enum [("mathjax",Mathjax);("katex",Katex);("document",Document)])) None
         ~vopt:(Some Mathjax) & info ["latex"] ~docv:"TEX" ~docs:latex_section ~doc:"\
    Transform the TouIST input to LaTeX.")


let linter_flag =
  Arg.(value & flag & info ["linter"] ~doc:"\
    Display syntax and semantic errors and exit. With this option, we only
    do a minimal valuation by bypassing the expansive `bigand', `exact',
    `powerset'... constructs so that it prints the errors as fast as
    possible.")
let mode_section = "MODES"
let translation_section = "TRANSLATE"
let table =
  Arg.(value & opt (some output_conv) None & info ["table"]
         ~docs:translation_section ~docv:"TABLE" ~doc:"\
    Select the file $(docv) for printing the mapping table. Only
    useful for $(b,--sat) and $(b,--qbf).")

let debug_dimacs_flag =
  Arg.(value & flag & info ["debug-dimacs"] ~docs:translation_section ~doc:"\
    ($(b,--sat) and $(b,--qbf) modes) Display names instead of integers in
    DIMACS/QDIMACS")

let show_section = "SHOW"
let show_flag =
  Arg.(value & opt (some (enum [
      "form",Form;"cnf",Cnf;"prenex",Prenex;
      "duringcnf",CnfDuring;"duringprenex",PrenexDuring
    ])) None ~vopt:(Some Form) & info ["show"] ~docv:"AST" ~docs:show_section ~doc:"\
    Show a step of the evaluation of the TouIST input. By default,
    show the expanded formula (`form').")

let lang_and_mode =
  let choose_mode lang solv latex show linter table debug_dimacs =
    let is_some = function None->false | _->true in match
      lang  ,solv  ,latex , show with
    | _     ,Some _,Some _,_
    | _     ,Some _,_     ,Some _
    | _     ,_     ,Some _,Some _ -> `Error (false, "{--solve,--solver,--show,--latex} are mutually exclusive")
    | Smt _ ,_     ,_     ,Some _ -> `Error (false, "--show does not support --smt")
    | Smt _ ,_     ,_     ,_ when is_some table -> `Error (false, "--table has no meaning with --smt")
    | _     ,None  ,None  ,None   -> `Ok (lang, Translate {linter; table; debug_dimacs})
    | _     ,_     ,_     ,_ when debug_dimacs -> `Error (false,"--debug-dimacs only available in translation mode (see help)")
    | _     ,_     ,_     ,Some m -> `Ok (lang, Show m)
    | _     ,Some _,_     ,_ when linter -> `Error (false,"cannot use {--solve,--solver} and --linter at the same time")
    | Smt _ ,Some (Solve _),_,_ when not Touist_yices2.SmtSolve.enabled -> `Error (false,"not compiled with yices2 support: --solver cannot be used with --smt")
    | Qbf   ,Some (Solve _),_,_ when not Touist_qbf.QbfSolve.enabled -> `Error (false,"not compiled with qbf support: --solver cannot be used with --qbf.")
    | _     ,Some s, _     , _      -> `Ok (lang, s) (* lint not useful for --solve/--solver *)
    | _     ,None  , Some m, _      -> `Ok (lang, Latex {mode=m;linter})
  in Term.(ret (const choose_mode $ language $ solve_flags $ latex_flag $ show_flag $ linter_flag $ table $ debug_dimacs_flag))

let common_opt =
  let docs = Manpage.s_options in
  let error_format =
    Arg.(value & opt string "%f: line %l, col %c-%C: %t: %m"
         & info ["error-format"] ~docs ~docv:"FMT" ~doc:"\
      Customize the formatting of error messages. The placeholders are
      `%f' (file name), `%l'-`%L' (line number start-end), `%c'-`%C'
      (column number start-end), `%b'-`%B' (buffer offset start-end),
      `%t' (error type: warning, error), `%m' (error message) and
      `\\\\n' (new line).")
  and wrap_width =
    Arg.(value & opt int 76 & info ["wrap-width"] ~docs ~docv:"N" ~doc:"\
      Choose the width of the messages (errors, warnings).
      With $(docv) set to 0, you can disable the wrapping.")
  and verbose_flag =
    Arg.(value & opt int 0 ~vopt:1 & info ["verbose";"v"] ~docv:"LVL" ~docs ~doc:"\
      Print information for debugging touist. $(i,LVL) controls the verbosity.
      With $(i,LVL)>=$(i,1), display stacktrace and display 'loc' (location) and
      automaton number on syntax error (useful when trying to fix a wrong error
      message in `src/lib/parser.messages').
      With $(i,LVL)>=$(i,2), $(b,--solver) prints the stdin, stdout and stderr of
      the given solver.")
  in let common_opt error_format wrap_width verbose =
       {error_format; wrap_width; verbose}
  in Term.(const common_opt $ error_format $ wrap_width $ verbose_flag)

let cmd =
  let doc = "translate and solves SAT, QBF and SMT problems written in TouIST."
  and usage1 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(i,INPUT)"
  and usage2 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--solve) $(i,INPUT)"
  and usage3 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--solver)=$(i,CMD) $(i,INPUT)"
  and usage4 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--latex)[=$(i,TEX)] $(i,INPUT)"
  and usage5 = "$(mname) [$(b,--sat)|$(b,--qbf)|$(b,--smt)[=$(i,LOGIC)]] [$(i,OPTION)] $(b,--show)[=$(i,AST)] $(i,INPUT)"
  in
  let man = [
    `S Manpage.s_synopsis;
    `P usage1; `Noblank;
    `P usage2; `Noblank;
    `P usage3; `Noblank;
    `P usage4; `Noblank;
    `P usage5;
    `S Manpage.s_description;
    `P "$(tname) translates and solves problems written in TouIST language,
       which is based on propositional logic (SAT) with extensions to SMT and
       QBF. Output formats for translation include DIMACS, QDIMACS and SMT-LIB.";
    `P "In some cases, e.g., $(b,--smt) or $(b,--latex) which can take an
        optional argument, you might want to use $(b,--) in order to
        disambiguate, i.e, "; `Noblank;
    `Pre "    $(mname) --smt file.touist         <- wrong"; `Noblank;
    `Pre "    $(mname) --smt -- file.touist      <- ok";
    `P "You can see the time spent on each step (translation and solving)
        using $(b,-v).";
    `P ("Embedded solvers compiled in $(mname): minisat"
        ^ (if Touist_yices2.SmtSolve.enabled then ", yices2" else "")
        ^ (if Touist_qbf.QbfSolve.enabled then ", qbf" else "")^".");
    `S Manpage.s_arguments;
    `S language_section;
    `P "$(mname) accepts three language variants: $(b,--sat),  $(b,--qbf) and 
       $(b,--smt)[=$(i,LOGIC)]. To learn more on the associated TouIST grammars,
       see $(i,https://www.irit.fr/touist/doc/reference-manual.html).";

    `P "By default, $(i,LOGIC) is `QF_LRA' (QF stands for Quantifier Free).
     $(i,LOGIC) can one of:"; `Noblank;
    `I ("`QF_IDL'", "allows you to deal with boolean and integer, e.g, `x - y < b'
        (Integer Difference Logic)"); `Noblank;
    `I ("`QF_RDL'","is the same as `QF_IDL' but with reals (Real Difference Logic)"); `Noblank;
    `I ("`QF_LIA'","(Linear Integer Arithmetic)"); `Noblank;
    `I ("`QF_LRA'","(Linear Real Arithmetic)"); `Noblank;
    `P "Other QF_* exist by cannot be expressed in TouIST. For more information
        on QF_* logics, see $(i,http://smtlib.cs.uiowa.edu/logics.shtml)";
    `P "Detail of the language flags:";

    `S mode_section;
    `P "$(mname) has four modes depending on {$(b,--solve),
        $(b,--solver), $(b,--latex)}. With none of these flags, $(mname)
        will default to the DIMACS translation. To see more on the different
        modes, look at their respective sections:";
    `I ("$(b,"^translation_section^")","by default, translate into DIMACS, QDIMACS or SMT-LIB.");`Noblank;
    `I ("$(b,"^solve_section^")","with $(b,--solve), solve using internal solver.");`Noblank;
    `I ("$(b,"^external_solv_section^")","with $(b,--solver), solve using external solver.");`Noblank;
    `I ("$(b,"^latex_section^")","with $(b,--latex), produce LaTeX output.");`Noblank;
    `I ("$(b,"^show_section^")","with $(b,--show), dump the internal AST.");

    `S translation_section;
    `P "You can translate the TouIST syntax into DIMACS ($(b,--sat)),
    QDIMACS ($(b,--qbf)) and SMT-LIB ($(b,--smt)). The syntax is:";

    `Pre "    $(mname) [$(b,--sat)|$(b,--qbf)] [--debug-dimacs] $(i,INPUT)"; `Noblank;
    `Pre "    $(mname) $(b,--smt)[=$(i,LOGIC)] $(i,INPUT)";

    `P "By default, when translating to DIMACS or QDIMACS, the mapping table
    (i.e., the link between proposition names and (Q)DIMACS integers) is
    displayed as comments before the prelude. For example, with the command";
    `Pre "    echo 'rain => wet_road rain not wet_road' | touist -";
    `P "we get the following DIMACS output:";
    `Pre {|    c wet_road 1      <- mapping between names DIMACS integers
    c rain 2
    p cnf 2 3         <- prelude of the DIMACS file
    1 -2 0
    2 0
    -1 0|};
    `P "With the $(b,--table) option, you can redirect these mapping lines to
        the file $(i,TABLE) (comments `c' are then not displayed).";
    `P "The following options are available in translation mode:";

    `S solve_section;
    `P "The $(b,--solve) option asks $(mname) to solve the problem. Depending
    on the input language ($(b,--sat), $(b,--smt), $(b,--qbf)), the
    corresponding internal solver is picked (MiniSat, Yices, Quantor).
    Syntax is:";

    `Pre "    $(mname) $(b,--solve) [$(b,--sat)|$(b,--qbf)] [--show-hidden|--table] $(i,INPUT)"; `Noblank;
    `Pre "    $(mname) $(b,--solve) $(b,--smt)[=$(i,LOGIC)] $(i,INPUT)";

    `P ("Exit codes are "^string_of_int (get_code OK)^" on SAT and
    "^string_of_int (get_code UNSAT)^" on UNSAT. By default, the first
    model is displayed; you can ask for more models using the
    $(b,--limit)=$(i,N)` option. With $(i,N)>1, the models are separated by
    lines beginning with `====` and for one model, each line contains a
    valuation followed by the corresponding proposition. For example:");
    `Pre "    echo 'a and b' | touist - --solve";
    `P "will return a single model (and in this case, there is only one
    single model):";
    `Pre "    1 a\n    1 b";
    `P "Each line corresponds to a valuation, and each valuation should
    be read `value proposition`. In the example, `a' takes the value
    `true'. With this format, you can easily filter the results. For
    example, the following command will only show the propositions that
    are `true':";
    `Pre "    echo 'a and b' | touist - --solve | grep ^1";
    `P "Using $(b,--limit)=$(i,N) allows to display multiple models. With this
    option, the models are separated by lines beginning with `====` and for
    one model, each line contains a valuation followed by the corresponding
    proposition. For example, with $(i,N)=0' displays an unlimited
    number of models (numbering of models begins at 0):";
    `Pre "    echo 'a or b' | touist - --solve --limit 0";
    `P "will display";
    `Pre {|    ==== model 0
    1 a
    0 b
    ==== model 1
    0 a
    1 b
    ==== model 2
    1 a
    1 b
    ==== found 3 models, limit is 0 (--limit=N for more models)|};

    `P "Using $(b,--solve) with $(b,--smt), the valuations will be integers
    or reals instead of 0 and 1. For example,";
    `Pre "    echo 'x > 3' | touist --solve --smt=QF_IDL -";
    `P "will give you the model:";
    `Pre "    4 x";
    `P "which should be read 'the proposition x takes the value 4'.";

    `P "Using $(b,--solve) with $(b,--qbf), the valuations will be 0, 1 or
    `?'. `?' means that this proposition has an undefined valuation (often
    because the variable is existentially quantified or under an existential
    quantifier. For example:";
    `Pre "    echo 'forall x: x or (exists y: y)' | touist --solve --qbf -";
    `P "which will give you a partial model:";
    `Pre "    ? x"; `Noblank;
    `Pre "    ? y";
    `P "Detail of the options related to solving:";

    `S external_solv_section;
    `P {|$(mname) can use an external solver using $(b,--solver)=$(i,CMD).
         The command $(i,CMD) can take arguments, i.e., `--solver="cmd arg1 arg2"'.
         Syntax is:|};

    `Pre "    $(mname) $(b,--solver)=$(b,CMD) [$(b,--sat)|$(b,--qbf)] [--show-hidden|--verbose] $(i,INPUT)";

    `P "The external solver $(i,CMD) must have the Minisat
    behaviour:";
    `I ("1.","it should accept DIMACS or QDIMACS on standard input"); `Noblank;
    `I ("2.","it should print a model (or a partial model) in DIMACS on
    standard output; the model can span on multiple lines, each line begins
    with `v`, `V` or nothing (for Minisat compatibility), and each line
    is optionally ended with 0.");
    `P "Here is an example of expected output of the command $(i,CMD):";
    `Pre"    v -1 2 -3 4 0"; `Noblank;
    `Pre"    v 5 -6 0";

    `P "The exit status is the same as with $(b,--solve). For debugging
    purposes, you can add $(b,--verbose=2) or $(b,-v2) to see the
    stdin/stdout/stderr of $(i,CMD). You can also use $(b,--show-hidden).";
    `P "Here are some examples of use:";
    `Pre {|    touist --sat sat.touist --solver="minisat /dev/stdin /dev/stdout"|}; `Noblank;
    `Pre {|    touist --sat sat.touist --solver="picosat --partial"|}; `Noblank;
    `Pre {|    touist --sat sat.touist --solver="glucose -model"|}; `Noblank;
    `Pre {|    touist --sat sat.touist --solver="glucose-syrup -model"|}; `Noblank;
    `Pre {|    touist --qbf qbf.touist --solver="./caqe-mac --partial-assignments"|}; `Noblank;
    `Pre {|    touist --qbf qbf.touist --solver="qute --partial-certificate"|}; `Noblank;
    `Pre {|    touist --qbf qbf.touist --solver="depqbf --qdo --no-dynamic-nenofex"|}; `Noblank;
    `Pre {|    touist --qbf qbf.touist --solver="quantor"|}; `Noblank;
    `Pre {|    touist --qbf qbf.touist --solver="rareqs"|};

    `P "Detail on the options for external solving:";

    `S latex_section;
    `P "$(mname) can produce LaTeX from any TouIST file. The syntax is:";
    `Pre "    $(mname) $(b,--latex)[=$(i,TEX)] [$(b,--smt),$(b,--sat)|$(b,--qbf)] $(i,INPUT)";
    `P "$(i,TEX) allows you to select what kind of LaTeX you want:";
    `I ("`mathjax'","for a equation-only LaTeX output compatible with Mathjax."); `Noblank;
    `I ("`katex'","for a equation-only LaTeX output compatible with Katex (replaces
        '\\\\substack' with '\\\\begin{matrix}'."); `Noblank;
    `I ("`document'","for a complete LaTeX file (including `\\\\begin{document})
        that you can directly give to pdfLaTeX. The `mathtools' package is
        necessary for `\\\\begin{pmatrix*}'.");
    `P "Detail of the options related to LaTeX output:";

    `S show_section;
    `P "Sometimes, you want to know what are the different internal
        translations that $(mname) is doing. Using $(b,--show), you can
        dump the AST (Abstract Syntaxic Tree) after or during different steps.
        Syntax is:";
    `Pre "    $(mname) $(b,--show)[=$(i,AST)] [$(b,--sat)|$(b,--qbf)|$(b,--smt)] $(i,INPUT)";
    `P "$(b,AST) can take the following values:"; `Noblank;
    `I ("`form'","dump the AST after evualuation, i.e., when every `bigand',
          `bigor', variables and such are expanded. This option may be useful
          to understand why your TouIST input seems to be wrongly
          interpreted."); `Noblank;
    `I ("`cnf'","dump the AST after the CNF transformation is done."); `Noblank;
    `I ("`prenex'","($(b,--qbf) only) dump the AST after the Prenew transformation."); `Noblank;
    `I ("`duringcnf'","($(b,--{qbf,sat}) only) print the steps during the CNF transformation."); `Noblank;
    `I ("`duringprenex'","($(b,--qbf) only) print the steps during the Prenex transformation.");
    `P "Detail of the options related to showing AST:";

    `S Manpage.s_bugs; `P "Report bugs to <mael.valais@gmail.com>.";
    `S Manpage.s_see_also;
  ]
  in
  Term.(
    ret (const main $ lang_and_mode $ input $ output $ common_opt),
    info "touist" ~version:Version.v
      ~doc ~man ~exits:(code_msgs |> List.map (fun (doc,err) -> exit_info ~doc (get_code err))))

let () = Term.exit ~term_err:(get_code CLI_ERROR) @@ Term.eval cmd
