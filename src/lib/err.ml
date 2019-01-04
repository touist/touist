open Lexing (* for Lexing.position *)

type msg_type = Error | Warning
type during = Usage | Parse | Lex | Eval | Sat | Cnf | Prenex
type loc = Lexing.position * Lexing.position
type msg = msg_type * during * string * loc option

exception TouistFatal of msg

(* Parameters that can be set globally *)
let wrap_width = ref 76
let format = ref "%l:%c: %t: %m"
let loc_format = ref "%l:%c"
let color = ref false (* advice: use Unix.isatty to set it *)
let discard_warnings = ref false

let string_of_type = function
  | Warning -> "warning"
  | Error -> "error"

let string_of_during = function
  | Usage -> "cmd usage"
  | Parse -> "parsing"
  | Lex -> "lexing"
  | Eval -> "evaluation"
  | Sat -> "sat solving"
  | Cnf -> "cnf transform"
  | Prenex -> "prenex transform"

(** [get_loc] translates a 'loc' to an understandable tuple that contains
    (num_line, num_col, token_start, token_end). *)
let get_loc loc : int * int * int * int =
  let s,e = loc in (s.pos_lnum, (s.pos_cnum - s.pos_bol+1), s.pos_cnum, e.pos_cnum)

let loc_placeholders loc chr =
  let s,e = loc in (* start, end *)
  match chr with
  | 'f' -> s.pos_fname (* file name *)
  | 'l' -> (string_of_int s.pos_lnum) (* line (start) *)
  | 'c' -> (string_of_int (s.pos_cnum - s.pos_bol+1)) (* column (start) *)
  | 'L' -> (string_of_int e.pos_lnum) (* line (end) *)
  | 'C' -> (string_of_int (e.pos_cnum - e.pos_bol+1)) (* column (end) *)
  | 'b' -> (string_of_int s.pos_cnum) (* buffer position (start) *)
  | 'B' -> (string_of_int e.pos_cnum) (* buffer position (end) *)
  | c -> "%" ^ Char.escaped c

let all_placeholders loc typ _ msg = function
  | 'm' -> msg
  | 't' -> string_of_type typ
  | c -> match loc with None -> "" | Some loc -> loc_placeholders loc c

(** [replace] replaces all '%c' for any character 'c' using the provided
    function 'placeholder'. Also replaces all two-characters '\n' to actual
    one-character newline.
    %t must always be set before %m.
    If location isn't available, all text/placeholders before the first
    non-location placeholder (as well as any trailing whitespaces) will be
    skipped. *)
let replace (placeholder : char -> string) text =
  let text = Re.Str.global_replace (Re.Str.regexp "\\\\n") "\n" text in
  let text = Re.Str.global_replace (Re.Str.regexp "\\\\t") "\t" text in
  let rec replace cur_pos =
    try
      let next_pos = Re.Str.search_forward (Re.Str.regexp "%[a-zA-Z]") text cur_pos in
      (*Printf.printf "cur=%d next=%d len=%d (%s)\n" cur_pos next_pos (String.length text) text;*)
      String.sub text cur_pos (next_pos-cur_pos)
      ^ (String.get text (next_pos+1) |> placeholder)
      ^ if next_pos+2 <= String.length text-1 then replace (next_pos+2) else ""
    with Not_found -> String.sub text cur_pos (String.length text - cur_pos)
  in replace (if (placeholder 'l')=""
              then Re.Str.search_forward (Re.Str.regexp "%[^flcLCbB]") text 0 else 0)

let string_of_loc ?(fmt=(!loc_format)) (loc:loc) : string =
  replace (loc_placeholders loc) fmt

(** Wraps the text at width. Indendation is kept as long no new line is read.
    If width = 0, do not wrap. *)
let format_width _ width text =
  let rec format prev_indent from_pos =
    let cur_indent = try (Re.Str.search_forward (Re.Str.regexp "[^ ]") text from_pos)-from_pos
      with Not_found -> 0 in
    let wrap_pos =
      let newline_pos =
        try Re.Str.search_forward (Re.Str.regexp "\n") text from_pos
        with Not_found -> String.length text
      in if newline_pos > from_pos+width then from_pos+width else newline_pos
    in
    let rec spaces = function 0 -> "" | x -> " "^ spaces (x-1) in
    match wrap_pos with
    | _ when from_pos >= wrap_pos -> ""
    | _ when wrap_pos = String.length text ->
      (*Printf.fprintf stdout "newline=%d,indent=%d,prev_indent=%d,len=%d, '%s'\n"
        wrap_pos cur_indent prev_indent (String.length text) (String.sub text from_pos (wrap_pos-from_pos)); *)
      spaces prev_indent ^ String.sub text from_pos (wrap_pos-from_pos)
    | _ when String.get text wrap_pos = '\n' -> (* wrap at a \n symbol *)
      spaces prev_indent ^ String.sub text from_pos (wrap_pos-from_pos)
      ^"\n"^ format 0 (wrap_pos+1)
    | _ -> (* wrap at any point because width is too large *)
      let last_space = try Re.Str.search_backward (Re.Str.regexp "\\( \\|: \\|, \\|. \\)") text wrap_pos with Not_found -> wrap_pos in
      let last_space_end = last_space + if (String.get text last_space)=' ' then 0 else 1 in
      spaces prev_indent ^ String.sub text from_pos (last_space_end-from_pos)
      ^ "\n" ^ format (cur_indent+prev_indent) (last_space_end+1)
  in if width != 0 then format 0 0 else text;;

let string_of_msg ?(width=(!wrap_width)) ?(color=(!color)) ?(fmt=(!format)) (message:msg) =
  let color_backquote text = let colorize str = "\x1b[33m" ^ str ^ "\x1b[0m" in
    Re.Str.global_substitute (Re.Str.regexp "`\\([^`]+\\)`") (fun s -> "`"^ colorize (Re.Str.matched_group 1 s) ^"`") text in
  let color_quoted text = let colorize str = "\x1b[33m" ^ str ^ "\x1b[0m" in
    Re.Str.global_substitute (Re.Str.regexp "'\\([^']*\\)'") (fun s ->
        let s = (Re.Str.matched_group 1 s) in
        if (String.length s) = 0 then "''" else "'"^ colorize s ^"'") text in
  let color_code text = let colorize str = "\x1b[37m" ^ str ^ "\x1b[0m" in
    Re.Str.global_substitute (Re.Str.regexp "^\\(    +.*\\)$") (fun s -> colorize (Re.Str.matched_group 1 s)) text in
  let color_type text = let colorize str = match str with
      | "warning" -> (* yellow bold *) "\x1b[33m\x1b[1m" ^str^ "\x1b[0m"
      | "error" ->   (* red bold    *) "\x1b[31m\x1b[1m" ^str^ "\x1b[0m"
      | str -> str
    in Re.Str.substitute_first (Re.Str.regexp "\\(error\\|warning\\)") (fun s -> colorize (Re.Str.matched_group 1 s)) text in
  let color_all text = if color then text |> color_code |> color_backquote |> color_quoted |> color_type else text in
  let typ,_,text,loc = message in
  replace (all_placeholders loc typ color text) fmt |> format_width color width |> color_all

let warn msg = if !discard_warnings then () else Printf.fprintf stderr "%s" (string_of_msg msg)
let fatal msg = raise @@ TouistFatal msg

let _ = Printexc.register_printer (fun ex -> match ex with TouistFatal msg -> Some (string_of_msg msg) | _ -> None)