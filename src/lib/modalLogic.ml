open Types.Ast

module ModalSystem = struct
  type t = S5
  let string_of (modal_system : t) =
    match modal_system with
    | S5 -> "S5"
  let all_modal_systems_as_string () =
    let _exhaustiveness_check (modal_system : t) =
      match modal_system with
      | S5 -> ()
    in
    String.concat ", " [(string_of S5)]
  let get_unsupported_message modal_system = "modal_system:" ^ modal_system ^ "; is not supported. Available modal_systems:"^(all_modal_systems_as_string ())
  let of_string modal_system =
    match modal_system with
    | "S5" -> S5
    | _ -> failwith (get_unsupported_message modal_system)
  let assert_supported modal_system =
    ignore (of_string modal_system)
end

module SolveResult = struct
  type t = SAT | UNSAT
end

module EvaluatedS5Ast = struct
  module IntohyloSerializer = struct
    let prop_forward_map : (string, string) Hashtbl.t = Hashtbl.create 32 (* {"proposition1" -> "p1", "proposition2" -> "p2"} *)
    let prop_backward_map : (string, string) Hashtbl.t = Hashtbl.create 32 (* {"p1" -> "proposition1", "p2" -> "proposition2"} *)
    let prop_counter = ref 1
    let obtain_prop_mapped_name prop =
      match Hashtbl.find_opt prop_forward_map prop with
      | Some mapped -> mapped
      | None ->
          let new_mapped_name = "p" ^ string_of_int !prop_counter in
          Hashtbl.add prop_forward_map prop new_mapped_name;
          Hashtbl.add prop_backward_map new_mapped_name prop;
          incr prop_counter;
          new_mapped_name
    let get_prop_mappings mapping =
      let buf = Buffer.create 1000 in
      let write_to_buf = Buffer.add_string buf in
      let write_many strs = List.iter write_to_buf strs in
      write_to_buf "proposition mapping table (format : touist_proposition intohylo_proposition\\n)\n";
      Hashtbl.iter (fun touist_proposition intohylo_proposition ->
        write_many [touist_proposition; " "; intohylo_proposition; "\n"]) mapping;
      buf
    let buf = Buffer.create 1000
    let write_to_buf = Buffer.add_string buf
    let write_char_to_buf = Buffer.add_char buf
    let rec write_ast_to_buf ast =
      let write_to_buf_with_parens ast =
        match ast with
        | Prop _ -> write_ast_to_buf ast
        | _ ->
            write_char_to_buf '(';
            write_ast_to_buf ast;
            write_char_to_buf ')'
      in
      (match ast with
      | Prop x -> write_to_buf (obtain_prop_mapped_name x)
      | Not x ->
          write_char_to_buf '~';
          write_to_buf_with_parens x;
      | And (x, y) ->
          write_to_buf_with_parens x;
          write_to_buf " & ";
          write_to_buf_with_parens y;
      | Or (x, y) ->
          write_to_buf_with_parens x;
          write_to_buf " | ";
          write_to_buf_with_parens y;
      | Xor (x, y) ->
          let transformed =
            Or (
              And (x, Not y),
              And (Not x, y)
            )
          in
          write_to_buf_with_parens transformed
      | Implies (x, y) ->
          write_to_buf_with_parens x;
          write_to_buf " -> ";
          write_to_buf_with_parens y;
      | Equiv (x, y) ->
          write_to_buf_with_parens x;
          write_to_buf " <-> ";
          write_to_buf_with_parens y;
      | Box x ->
          write_to_buf "[r1]";
          write_to_buf_with_parens x;
      | Diamond x ->
          write_to_buf "<r1>";
          write_to_buf_with_parens x;
      | x -> failwith ("Not supported AST type when writing modal logic AST to buffer (AST Type:" ^ (Pprint.string_of_ast_type ~debug:true x) ^ ")"))
    let serialize ast =
      write_to_buf "begin\n";
      write_ast_to_buf ast;
      write_to_buf "\nend\n";
      (get_prop_mappings prop_forward_map, buf)
  end

  module S5CheetahSolver = struct
    module StringSet = Set.Make(String)
    let solve ast =
      let create_temp_io_files ast =
        let (_, intohylo_buf) = IntohyloSerializer.serialize ast in
        let input_file = Filename.temp_file ~temp_dir:"./.tmp_for_modal_logic_s5" ".tmp_" ".intohylo" in
        let output_file =
          try
            Filename.temp_file ~temp_dir:"./.tmp_for_modal_logic_s5" ".tmp_" ".s5cheetah_model"
          with exn ->
            (try Sys.remove input_file with _ -> ());
            raise exn
        in
        let write_to_file filename content =
          let oc = open_out filename in
          try
            output_string oc content;
            close_out oc
          with exn ->
            close_out_noerr oc;
            raise exn
        in
        write_to_file input_file (Buffer.contents intohylo_buf);
        (input_file, output_file)
      in
      let cleanup_temp_io_files input_file output_file =
        (try Sys.remove input_file with _ -> ());
        (try Sys.remove output_file with _ -> ())
      in
      let parse_model filename =
        let buf = Buffer.create 1000 in
        let ic = open_in filename in
        try
          let first_line = input_line ic in
          match first_line with
          | "SAT" ->
              Buffer.add_string buf "SAT";
              ignore (input_line ic);
              let to_be_discarded_world = "v possible world0: any assignment is ok!" in
              let seen_valuations : (string, unit) Hashtbl.t = Hashtbl.create 16 in (* for having fast lookups *)
              let all_valuations : (string * StringSet.t) list ref = ref [] in (* for having fast iterations *)
              let filter_unique_valuations (line : string) =
                let separator_index = String.index line ':' in
                (* instead of trim, using the following functions for not creating extra string copy*)
                let first_non_space_index_after_separator s =
                  let len = String.length s in
                  let rec aux i =
                    if i >= len then
                      failwith "Modal logic world valuations only contained whitespace"
                    else if ' ' <> s.[i] then
                      i
                    else
                      aux (i + 1)
                  in
                  aux (separator_index+1)
                in
                let last_non_space_index s =
                  let rec aux i =
                    if i < 0 then
                      failwith "Modal logic world valuations only contained whitespace"
                    else if ' ' <> s.[i] then
                      i
                    else
                      aux (i - 1)
                  in
                  aux (String.length s - 1)
                in
                let valuations_start_index = first_non_space_index_after_separator line in
                let valuations = String.sub line valuations_start_index (last_non_space_index line - valuations_start_index + 1) in
                (* Discard worlds with duplicated valuations *)
                if not (Hashtbl.mem seen_valuations valuations) then (
                  Hashtbl.add seen_valuations valuations ();
                  let tokens = String.split_on_char ' ' valuations in
                  let token_set =
                    List.fold_left (fun acc tok -> StringSet.add tok acc) StringSet.empty tokens
                  in
                  all_valuations := (valuations, token_set) :: !all_valuations
                )
              in
              (* Process first world *)
              let first_world = input_line ic in
              if first_world <> to_be_discarded_world then
                filter_unique_valuations first_world;
              (* Process remaining worlds *)
              let rec loop () =
                match input_line ic with
                | line ->
                    filter_unique_valuations line;
                    loop ()
                | exception End_of_file -> ()
              in
              loop ();
              close_in ic;
              let is_strict_subset set1 set2 =
                StringSet.subset set1 set2 && not (StringSet.equal set1 set2)
              in
              let counter = ref 0 in
              (* Discard worlds with duplicated subset valuations *)
              List.iter (fun (unmapped_valuations, tokens1) ->
                if not (List.exists (fun (_, tokens2) -> is_strict_subset tokens1 tokens2) !all_valuations) then (
                  incr counter;
                  let world_label = Printf.sprintf "w%d" !counter in
                  let unmapped_valuations_len = String.length unmapped_valuations in
                  let remap_prop_to_buf token =
                    Buffer.add_string buf (Hashtbl.find IntohyloSerializer.prop_backward_map token)
                  in
                  let process_unmapped_proposition token_start token_end =
                    let token_len = token_end - token_start in
                    if token_len > 0 && '-' = unmapped_valuations.[token_start] then (
                      Buffer.add_char buf '-';
                      let unmapped_prop = String.sub unmapped_valuations (token_start + 1) (token_len - 1) in
                      remap_prop_to_buf unmapped_prop
                    ) else (
                      let unmapped_prop = String.sub unmapped_valuations token_start token_len in
                      remap_prop_to_buf unmapped_prop
                    )
                  in
                  let rec remap_valuations_to_buf token_start token_end =
                    if token_end >= unmapped_valuations_len then ( (* token (proposition) found *)
                      if token_start < unmapped_valuations_len then (
                        process_unmapped_proposition token_start token_end
                      )
                    ) else if ' ' = unmapped_valuations.[token_end] then ( (* token (proposition) found *)
                      if token_start < token_end then (
                        process_unmapped_proposition token_start token_end;
                        Buffer.add_char buf ' ';
                      );
                      remap_valuations_to_buf (token_end + 1) (token_end + 1)
                    ) else ( (* token becomes longer *)
                      remap_valuations_to_buf token_start (token_end + 1)
                    )
                  in
                  Buffer.add_string buf "\n";
                  Buffer.add_string buf world_label;
                  Buffer.add_string buf ": ";
                  remap_valuations_to_buf 0 0;
                )
              ) !all_valuations;
              Buffer.add_string buf ("\nworlds_count: " ^ (string_of_int !counter));
              (SolveResult.SAT, buf)
          | "UNSAT" ->
              Buffer.add_string buf "UNSAT";
              close_in ic;
              (SolveResult.UNSAT, buf)
          | x -> failwith ("Received unexpected line in S5Cheetah_model instead of SAT/UNSAT, line: " ^ x);
        with
        | End_of_file ->
            close_in ic;
            (SolveResult.SAT, buf)
        | exn ->
            close_in_noerr ic;
            raise exn
      in
      let exec input_intohylo_file output_file =
          let executable_path = "./solver_executables/modal_logic/S5Cheetah" in
          if not (Sys.file_exists input_intohylo_file) then
            failwith ("Error: input_intohylo_file: '" ^ input_intohylo_file ^ "' not found.");
          if not (Sys.file_exists executable_path) then
            failwith ("Error: executable '" ^ executable_path ^ "' not found.");
          let args = [| executable_path; "-model"; input_intohylo_file; output_file |] in
          let dev_null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
          try
            let pid = Unix.create_process executable_path args dev_null dev_null dev_null in
            (* Wait for the child process to finish *)
            let _, status = Unix.waitpid [] pid in
            Unix.close dev_null;
            match status with
            | Unix.WEXITED 0 -> parse_model output_file
            | Unix.WEXITED code -> failwith (executable_path ^ " failed with exit code: " ^ string_of_int code)
            | Unix.WSIGNALED n -> failwith (executable_path ^ " killed by signal: " ^ string_of_int n)
            | Unix.WSTOPPED n -> failwith (executable_path ^ " stopped by signal: " ^ string_of_int n)
          with exn ->
            Unix.close dev_null;
            raise exn
      in
      let (input_file, output_file) = create_temp_io_files ast in
      try
        let (solve_result, solve_buf) = exec input_file output_file in
        cleanup_temp_io_files input_file output_file;
        (solve_result, solve_buf)
      with exn ->
        cleanup_temp_io_files input_file output_file;
        raise exn;
  end
  let solve ast = S5CheetahSolver.solve ast;
end

module EvaluatedAst = struct
  let translate (modal_system : ModalSystem.t) ast =
    match modal_system with
    | S5 -> EvaluatedS5Ast.IntohyloSerializer.serialize ast
  let solve (modal_system : ModalSystem.t) ast =
    match modal_system with
    | S5 -> EvaluatedS5Ast.solve ast
end