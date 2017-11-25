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
type transl_opt = { linter: bool; table: (string * out_channel) option }
type latex_mode = Mathjax | Document
type latex_opt = {
  mode: latex_mode;
  linter: bool;
}
type mode =
  | Solve of solve_opts
  | SolveExt of solve_ext_opt
  | Translate of transl_opt
  | Latex of latex_opt
  | Show
type common_opt = {
  error_format: string;
  wrap_width: int;
  debug: bool;
  debug_syntax: bool;
  debug_cnf: bool;
  show: bool;
  debug_dimacs: bool
}

let exit_with (exit_code:error) = exit (get_code exit_code)

(* WARNING: lit_of_int should be capable of handling as input
   - non-zero negative integers -> should be transformed into a negative literal
   - non-zero positive integers -> transformed into a positive literal
   - zero will never be passed. *)
let solve_ext common_opt show_hidden output lit_tbl lit_abs lit_sign lit_of_int (print_dimacs:out_channel -> unit) cmd =
  let prog, opts = match cmd |> Re_str.(split (regexp " +")) with
    | v::next -> v, Array.of_list next
    | _ -> failwith "error with --solver 'command'" in
  if common_opt.debug then Printf.eprintf "== cmd: '%s %s'\n" prog (opts |> Array.fold_left (fun acc v-> if acc = "" then v else v^" "^acc) "");
  if common_opt.debug then (Printf.eprintf "== stdin given to '%s':\n" cmd; print_dimacs stderr);
  let proc_stdout, proc_stdin, proc_stderr = Unix.open_process_full cmd [||] in
  print_dimacs proc_stdin; close_out proc_stdin;
  (* Try to parse a space-separated list of integers; returns empty list if
     something is not an integer. *)
  let ints_of_string s =
    try begin
      s |> Re_str.(split (regexp " +")) |> List.fold_left
        (fun acc s -> match int_of_string s with
           | v when v!=0 -> lit_of_int v :: acc
           | _ -> acc (* in DIMACS, '0' are line endings; we skip '0' *)) []
    end with Failure err -> []
  in
  let rec process in_chan =
    try let line = input_line in_chan in
      if common_opt.debug then Printf.eprintf "%s\n" line;
      let line_lst =
        match String.get line 0 with
        | 'V' | 'v' -> String.sub line 2 (String.length line -2) |> ints_of_string
        | _ -> ints_of_string line
        | exception Invalid_argument _ (* index out of bounds, if line empty *) -> []
      in line_lst @ (process in_chan)
    with End_of_file -> []
  in
  if common_opt.debug then Printf.eprintf "== stdout from '%s':\n" cmd;
  let valuated_lits = process proc_stdout in
  if common_opt.debug then Printf.eprintf "== stderr from '%s':\n%s" cmd (Parse.string_of_chan proc_stderr);
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
       "Command '%s' not found (try with --debug)\n" cmd;
     exit_with CLI_ERROR)
  | Unix.WEXITED 10 ->
    (Printf.eprintf
       "Command '%s' returned SAT but did not print a model (try with --debug)\n\
        A DIMACS model line must be integers optionally beginning with v or V\n\
        and optionally ending with 0.\n" cmd;
     exit_with SOLVER_ERROR)
  | Unix.WEXITED 20 ->
    (Printf.eprintf
       "Command '%s' returned UNSAT (try with --debug)\n" cmd;
     exit_with UNSAT)
  | Unix.WEXITED c ->
    Printf.eprintf
      "Command '%s' returned unknown code %d (try with --debug)\n\
       Expected codes are 10 for SAT and 20 for UNSAT\n"
      cmd c; exit_with SOLVER_ERROR;
  | _ -> Printf.eprintf "Error with %s, received signal\n" (cmd)


(* Step 1: we parse the args. If an arg. is "alone", we suppose
 * it is the touistl input file (this is handled by [process_arg_alone]) *)
let main (input,input_f) (output,output_f: string * out_channel) (lang,mode) common_opt =
  (* if common_opt.debug then Printexc.record_backtrace true; *)
  Err.wrap_width := common_opt.wrap_width;
  Err.format := common_opt.error_format;
  Err.color := (Unix.isatty Unix.stderr);
  try
    let input_f = if input="/dev/stdin" then stdin else open_in input in

    (* SMT Mode: check if one of the available QF_? has been given after --smt *)
    if (match lang with Smt l -> Touist_yices2.SmtSolve.(enabled && not (logic_supported l)) | _->false)
    then Printf.eprintf "you must give a correct SMT-LIB logic after --smt. Example: --smt=QF_IDL";
    let input_text = Parse.string_of_chan input_f in
    let smt = match lang with Smt _ -> true | _-> false in
    let debug_syntax = common_opt.debug_syntax in
    let debug_dimacs = common_opt.debug_dimacs in
    let debug_cnf = common_opt.debug_cnf in

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
        (if mode=Document then Printf.fprintf output_f
             "\\documentclass[fleqn]{article}\n\
              \\usepackage{mathtools}\n\
              \\allowdisplaybreaks\n\
              \\begin{document}\n\
              \\begin{multline*}\n\
              %s\n\
              \\end{multline*}\n\
              \\end{document}\n" (Latex.latex_of_ast ~full:true ast_plain));
      | _, Translate {linter=true} ->
        (ast_plain input_text |> Eval.eval ~smt ~onlychecktypes:true |> ignore; exit_with OK)
      | _,Show -> let ast = match lang with
          | Sat   -> Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
          | Smt _ -> Parse.parse_smt ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
          | Qbf   -> Parse.parse_qbf ~debug_syntax ~filename:input input_text |> Eval.eval ~smt
        in (Printf.fprintf output_f "%s\n" (Pprint.string_of_ast ~utf8:true ast); exit_with OK)
      (* A. solve has been asked *)
      | Sat, Solve {equiv=Some (input2,input2_f)} ->
        let solve inputchan =
          let ast = Parse.parse_sat ~debug_syntax ~filename:input (Parse.string_of_chan inputchan) |> Eval.eval in
          let models = Cnf.ast_to_cnf ~debug_cnf ast |> SatSolve.minisat_clauses_of_cnf |> SatSolve.solve_clauses ~verbose:common_opt.debug
          in models
        in
        let models = solve input_f and models2 = solve input2_f in
        (match SatSolve.ModelSet.equal !models !models2 with
         | true -> Printf.fprintf output_f "Equivalent\n"; exit_with OK
         | false -> Printf.fprintf output_f "Not equivalent\n"; exit_with UNSAT)
      | Sat, Solve {equiv=None; limit; count; interactive} ->
        let ast = Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval in
        let clauses,table = Cnf.ast_to_cnf ~debug_cnf ast |> SatSolve.minisat_clauses_of_cnf
        in
        let models =
          begin
            if count then SatSolve.solve_clauses ~verbose:common_opt.debug (clauses,table)
            else
              let print_model model i =
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
                Printf.fprintf output_f "=== count: %d; continue? (y/n) " i; flush stdout;
                try Scanf.scanf "%s\n" (fun c -> not (c="q" || c="n"))
                with End_of_file ->
                  Printf.eprintf "error: could not enter \
                                  interactive mode as stdin unexpectedly closed\n";
                  exit_with BUG
              (* stdin unexpectedly closed (e.g., piped cmd) *)
              (* [continue_limit] is also needed by [solve_clauses]. It stops
                 the display of models as soon as the number of models displayed
                 reaches [limit]. *)
              and continue_limit _ i = i < limit
              in
              SatSolve.solve_clauses ~verbose:common_opt.debug ~print:print_model
                ~continue:(if interactive then continue_asking else continue_limit)
                (clauses,table)
          end
        in
        (match SatSolve.ModelSet.cardinal !models with
         | i when count -> Printf.fprintf output_f "%d\n" i; exit_with OK
         | 0 -> Printf.fprintf stderr "unsat\n"; exit_with UNSAT
         | 1 -> (* case where we already printed models in [solve_clause] *)
           exit_with OK
         | i -> (* case where we already printed models in [solve_clause] *)
           Printf.fprintf output_f
             "==== found %d models, limit is %d (--limit N for more models)\n"
             i limit; exit_with OK)
      | Sat, Translate {linter=false;table} -> (* B. solve not asked: print the Sat file *)
        let ast = Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval in
        let clauses,tbl = Cnf.ast_to_cnf ~debug_cnf ast |> SatSolve.minisat_clauses_of_cnf
        in SatSolve.print_dimacs ~debug_dimacs (clauses,tbl)
          ~out_table:(match table with Some (_,f) -> f | None -> output_f) output_f;
        exit_with OK
      | Sat, SolveExt {cmd; hidden} ->
        let ast = Parse.parse_sat ~debug_syntax ~filename:input input_text |> Eval.eval in
        let clauses,tbl = Cnf.ast_to_cnf ~debug_cnf ast |> SatSolve.minisat_clauses_of_cnf
        in cmd |> solve_ext common_opt hidden output_f tbl (Minisat.Lit.abs) (Minisat.Lit.sign)
             (* because given integers can be negative: *)
             (fun v -> abs v |> Minisat.Lit.make |> fun l -> if v>0 then l else Minisat.Lit.neg l)
             (SatSolve.print_dimacs ~debug_dimacs (clauses,tbl))
      | Smt logic, Translate _ ->
        let ast = Parse.parse_smt ~debug_syntax ~filename:input input_text
                  |> Eval.eval ~smt in
        ast |> Smt.to_smt2 logic  |> Buffer.output_buffer output_f;
        exit_with OK
      | Smt logic, Solve _ ->

        let ast = Parse.parse_smt ~debug_syntax ~filename:input input_text
                  |> Eval.eval ~smt in
        let yices_form,tbl = Touist_yices2.SmtSolve.ast_to_yices ast in
        (match Touist_yices2.SmtSolve.solve logic yices_form with
         | None -> Printf.fprintf stderr "unsat\n"; exit_with UNSAT |> ignore
         | Some m -> Printf.fprintf output_f "%s\n" (Touist_yices2.SmtSolve.string_of_model tbl m); exit_with OK |> ignore);
      | Qbf, (SolveExt _ | Translate _ | Solve _) -> begin
          let ast = Parse.parse_qbf ~debug_syntax ~filename:input input_text
                    |> Eval.eval ~smt in
          let prenex = Qbf.prenex ast in
          let cnf = Qbf.cnf prenex in
          if common_opt.debug_cnf then begin
            Pprint.(Printf.fprintf stderr "formula: %s\nprenex: %s\ncnf: %s\n"
                      (string_of_ast ~utf8:true ast) (string_of_ast ~utf8:true prenex)
                      (string_of_ast ~utf8:true cnf))
          end;
          begin match mode with
            | SolveExt {cmd; hidden} -> (* --qbf + --solver CMD: use external solver *)
              let quants,int_clauses,int_tbl = Qbf.qbfclauses_of_cnf cnf
              in cmd |> solve_ext common_opt hidden output_f int_tbl (abs) (fun v -> v>0) (fun v -> v)
                   (Qbf.print_qdimacs (quants,int_clauses,int_tbl))
            | Translate {table} -> (* --qbf: we print QDIMACS *)
              let quants,int_clauses,int_tbl = Qbf.qbfclauses_of_cnf cnf in
              (Qbf.print_qdimacs ~debug_dimacs
                 (quants,int_clauses,int_tbl)
                 ~out_table:(match table with Some (_,f)->f | None->output_f) output_f)
            | Solve {hidden} -> (* --qbf + --solve: we solve using Quantor *)
              let qcnf,table = Touist_qbf.QbfSolve.qcnf_of_cnf cnf in
              (match Touist_qbf.QbfSolve.solve ~hidden (qcnf,table) with
               | Some str -> Printf.fprintf output_f "%s\n" str
               | None -> (Printf.fprintf stderr "unsat\n"; exit_with UNSAT |> ignore))
            | Show -> failwith "(bug) --show does not work with --qbf"
            | Latex _ -> failwith "(bug) at this point, --qbf + --latex should not happen"
          end
        end
      | Smt _, SolveExt _ -> failwith "--solver not compatible with --smt"
      (* | lang,mode -> (failwith "cannot handle this set of options" |> ignore) *)
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
    if common_opt.debug then Printf.eprintf "Stacktrace:\n%s\n" (Printexc.get_backtrace ());
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
let input_conv = (function
    | "-" -> `Ok ("/dev/stdin",stdin)
    | path when Sys.file_exists path ->
      (try `Ok (path,open_in path)
       with Sys_error err -> `Error ("cannot open '"^path^"': "^err))
    | path -> `Error ("file not found: "^path)), (fun fmt (v,_) -> Format.fprintf fmt "%s" v)

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
    Select the file $(docv) for printing results. With $(b,--sat), $(b,--smt)
    or $(b,--qbf), results will be respectively the DIMACS, QDIMACS and SMT-LIB
    translations of the TouIST given in $(i,INPUT).")


let language_section = "LANGUAGES"
let language =
  let docs = language_section in
  let sat =
    Arg.(value & flag & info ["sat"] ~docs ~doc:"\
      Propositional logic. It is the language selected by default,
      you may omit $(b,--sat).")
  and qbf =
    Arg.(value & flag & info ["qbf"] ~docs ~doc:"\
      Quantified boolean formulas (QBF).")
  and smt =
    Arg.(value & opt (some string) None ~vopt:(Some "QF_LIA") & info ["smt"]
           ~docv:"LOGIC" ~docs ~doc:"\
      Select the SAT Modulo Theory (SMT) input. By default, $(docv) is set to
      `QF_LIA' (linear integers algebra).")
  and one_of sat qbf smt =
    match sat, qbf   , smt   with
    | false  , false , Some l -> `Ok (Smt l)
    | false  , true  , None   -> `Ok Qbf
    | _      , false , None   -> `Ok Sat (* default to sat *)
    | _      , _     , _      -> `Error (false,"only one of {--sat,--smt,--qbf} is allowed")
  in Term.(ret (const one_of $ sat $ qbf $ smt))

let solve_section = "SOLVE"
let solve_flags =
  let docs = solve_section in
  let solve_flag =
    Arg.(value & flag & info ["solve"] ~docs ~doc:"\
      Solve the problem
      and print the first model if it exists. The solver is selected
      depending on the input language.")
  and solvext_flag =
    Arg.(value & opt (some string) None & info ["solver"] ~docs ~docv:"CMD" ~doc:"\
      (with {$(b,--sat),$(b,--qbf)}) Use $(docv) for solving. $(docv)
      must expect DIMACS (QDIMACS) on standard input, print a DIMACS model on
      standard output and return 10 on SAT and 20 for UNSAT. You can display
      the stdin, stdout and stderr of $(docv) using $(b,--debug).")
  and limit =
    Arg.(value & opt int 1 & info ["limit"] ~docs ~docv:"N" ~doc:"\
      (with $(b,--solve)) Instead of one model, return $(docv) models if they
      exist. With $(i,0), return every possible model. WARNING: the number
      of model can be extremely high; this option is intended for small
      problems.")
  and count_flag =
    Arg.(value & flag & info ["count"] ~docs ~doc:"\
      (with $(b,--solve))
      Instead of displaying models, return the number of models.")
  and interactive =
    Arg.(value & flag & info ["interactive"] ~docs ~doc:"\
      ($(b,--sat) mode) Display next model on key press; $(i,INPUT) must be a
      file.")
  and equiv =
    Arg.(value & opt (some input_conv) None & info ["equiv"] ~docs ~docv:"INPUT2" ~doc:"\
      (with $(b,--solve)) Check that $(docv) has the same models as $(i,INPUT)
      (equivalency). Note that the equivalency is computed 'naively' by
      retrieiving all models and comparing them. In the future, we will instead
      combine both $(i,INPUT) $(docv) and check that one entails the other (and
      conversely).")
  and hidden =
    Arg.(value & flag & info ["show-hidden"] ~docs ~doc:"\
    (with $(b,--solve)) Show the hidden '&a' literals used when translating to
    CNF.")
  in let check_solve_opts solv solvext limit count interac (equiv:(string * in_channel) option) hidden =
       match solv,solvext,limit,count,interac,equiv,hidden with
       | false   ,None   ,_    ,_    ,_      ,_    ,true  -> `Error (false,"--show-hidden only compatible with --solve or --solver")
       | true    ,Some _ ,_    ,_    ,_      ,_    ,_     -> `Error (false,"--solve and --solver cannot be used simultaneously")
       | false   ,_      ,1    ,false,false  ,None ,false -> `Ok None
       | false   ,Some c ,1    ,false,false  ,None ,hidden-> `Ok (Some (SolveExt {cmd=c;hidden}))
       | false   ,_      ,_    ,_    ,_      ,_    ,_     -> `Error (false,"the args {--limit,--count,--interactive,--equiv} can only be used with --solve")
       | true    ,_      ,limit,count,interac,equiv,hidden-> `Ok (Some (Solve {limit; count; interactive=interac; equiv; hidden}))

  in Term.(ret (const check_solve_opts $ solve_flag $ solvext_flag $ limit $ count_flag $ interactive $ equiv $ hidden))

let latex_section = "LATEX"
let latex_flag =
  Arg.(value & opt (some (enum [("mathjax",Mathjax);("document",Document)])) None
         ~vopt:(Some Mathjax) & info ["latex"] ~docs:latex_section ~doc:"\
    Transform the TouIST input to LaTeX. The supported values
    for $(docv) are `mathjax' and `document'.")

let mode_section = "MODES"
let linter_flag =
  Arg.(value & flag & info ["linter"] ~docs:mode_section ~doc:"\
    Display syntax and semantic errors
    and exit. With this option, we only do a minimal valuation so that it
    prints the errors as fast as possible.")

let translation_section = "TRANSLATE"
let table =
  Arg.(value & opt (some output_conv) None & info ["table"]
         ~docs:translation_section ~docv:"TABLE" ~doc:"\
    Select the file $(docv) for printing the mapping table. Only
    useful for $(b,--sat) and $(b,--qbf).")

let lang_and_mode =
  let choose_mode lang solv latex lint table =
    match solv,latex, lint, table with
    | None  , None  , lint, t -> `Ok (lang, Translate {linter=lint;table=t})
    | Some _, _     , _   , _ when lang<>Sat && lang<>Qbf -> `Error (false,"--solver only supports --sat and --qbf")
    | Some _, Some _, _   , _ -> `Error (false,"you cannot use {--solve or --solver} and --latex at the same time")
    | Some _, _     , true, _ -> `Error (false,"you cannot use {--solve or --solver} and --linter at the same time")
    | Some s, _     , _   , _ -> `Ok (lang,s) (* lint not useful for --solve/--solver *)
    | None  , Some m, lint, _ -> `Ok (lang, Latex {mode=m;linter=lint})
  in Term.(ret (const choose_mode $ language $ solve_flags $ latex_flag $ linter_flag $ table))

let common_opt =
  let docs = Manpage.s_options in
  let error_format =
    Arg.(value & opt string "%f: line %l, col %c-%C: %t: %m"
         & info ["error-format"] ~docs ~docv:"FMT" ~doc:"\
        Customize the formatting of error messages.")
  and wrap_width =
    Arg.(value & opt int 76 & info ["wrap-width"] ~docs ~docv:"N" ~doc:"\
      Wrapping width for error messages.")
  and debug_flag =
    Arg.(value & flag & info ["debug"] ~docs ~doc:"\
      Print information for debugging
      touist. More specifically:
      * with $(b,--solver), prints the stdin, stdout and stderr of the given
      solver;
      * print the 'loc' (location) when displaying syntax errors;
      * when an exception is raised, print the call stack.")
  and debug_syntax_flag =
    Arg.(value & flag & info ["debug-syntax"] ~docs ~doc:"\
      When a syntax error is displayed, also give the automaton
      number; this is useful when trying to fix a wrong error message
      in `src/lib/parser.messages'.")
  and debug_cnf_flag =
    Arg.(value & flag & info ["debug-cnf"] ~docs ~doc:"\
      Print step by step CNF
      transformation (only with $(b,--sat) and $(b,--qbf)")
  and show_flag =
    Arg.(value & flag & info ["show"] ~docs ~doc:"\
      Show the expanded Abstract
      Syntaxic Tree after evaluation, i.e., after every `bigand', `bigor',
      variables and such are replaced by actual formulas. This option may
      be useful to understand why your TouIST input seems to be wrongly
      interpreted by $(mname).")
  and debug_dimacs_flag =
    Arg.(value & flag & info ["debug-dimacs"] ~docs ~doc:"\
      ($(b,--sat) and $(b,--qbf) modes) Display names instead of integers in
      DIMACS/QDIMACS")
  in let common_opt error_format wrap_width debug debug_syntax debug_cnf show debug_dimacs =
       {error_format; wrap_width; debug; debug_syntax; debug_cnf; show; debug_dimacs}
  in Term.(const common_opt $ error_format $ wrap_width $ debug_flag
           $ debug_syntax_flag $ debug_cnf_flag $ show_flag $ debug_dimacs_flag)

let cmd =
  let doc = "translate and solves SAT, QBF and SMT problems written in TouIST."
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
    `P ("Embedded solvers available: "^"minisat"
        ^ if Touist_yices2.SmtSolve.enabled then ", yices2" else ""
                                                                 ^ if Touist_qbf.QbfSolve.enabled then ", qbf" else "");
    `S Manpage.s_arguments;
    `S language_section;
    `P "$(mname) accepts three language variants: $(b,--sat),  $(b,--qbf) and  $(b,--smt)[=$(i,LOGIC)].";

    `P "By default, $(i,LOGIC) is `QF_LRA'. $(i,LOGIC) can one of:";`Noblank;
    `P "- `QF_IDL' allows to deal with boolean and integer, e.g, `x - y < b'"; `Noblank;
    `P "- `QF_RDL' is the same as `QF_IDL' but with reals"; `Noblank;
    `P "- `QF_LIA' (not documented)"; `Noblank;
    `P "- `QF_LRA' (not documented)"; `Noblank;
    `P "Other QF_* exist by cannot be expressed in TouIST. For more information
        on QF_* logics, see http://smtlib.cs.uiowa.edu/logics.shtml";

    `S mode_section;
    `P "$(mname) has four modes depending on the flags $(b,--solve),
        $(b,--solver) and $(b,--latex). Without these three flags, $(mname)
        will default to the DIMACS translation.";`Noblank;
    `P ("- by default, translate into DIMACS, QDIMACS or SMT-LIB (see $(b,"^translation_section^"));");`Noblank;
    `P ("- with $(b,--solve), solve using internal solver (see $(b,"^solve_section^"));");`Noblank;
    `P ("- with $(b,--solver), solve using external solver (see $(b,"^solve_section^"));");`Noblank;
    `P ("- with $(b,--latex), produce LaTeX output (see $(b,"^latex_section^")).");

    `S translation_section;
    `P "You can translate the TouIST language into the following formats:"; `Noblank;
    `P "- DIMACS when using $(b,--sat)"; `Noblank;
    `P "- QDIMACS when using $(b,--qbf)"; `Noblank;
    `P "- SMT-LIB when using $(b,--smt)";
    `P "By default, when translating to DIMACS or QDIMACS, the mapping table
        (i.e., the link between proposition names and (Q)DIMACS integers) is
        displayed as comments before the prelude, e.g.,"; `Noblank;
    `Pre "    c rain 1";`Noblank;
    `Pre "    c road 2";`Noblank;
    `Pre "    p cnf 2 2";`Noblank;
    `P "With the $(b,--table) option, you can redirect these mapping lines to
        the file $(i,TABLE) (comments `c' are then not displayed).";
    `S solve_section;
    `P "With $(b,--solve), the internal solver used depends on the input language
        chosen:";`Noblank;
    `P "- with $(b,--sat), solve using MiniSat,";`Noblank;
    `P "- with $(b,--smt), solve using Yices,";`Noblank;
    `P "- with $(b,--qbf), solve using Quantor.";

    `S latex_section;
    `P "$(mname) can produce LaTeX from any TouIST file. $(i,MODE) allows you
        to select what kind of LaTeX you want:";
    `P "- `mathjax' for a equation-only LaTeX output compatible with Mathjax"; `Noblank;
    `P "- `document' for a complete LaTeX file (including `\\\\begin{document})
        that you can directly give to pdfLaTeX.";
    `S Manpage.s_bugs; `P "Report bugs to <mael.valais@gmail.com>.";
    `S Manpage.s_see_also;
  ]
  in
  Term.(
    const main $ input $ output $ lang_and_mode $ common_opt,
    info "touist" ~version:("%%VERSION%%")
      ~doc ~man ~exits:(code_msgs |> List.map (fun (doc,err) -> exit_info ~doc (get_code err))))

let () = Term.exit @@ Term.eval cmd