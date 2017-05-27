(** This is where the compiler errors are managed.

    We call a 'message' an error or a warning.
    Two cases for displaying the errors:
    - either one of the parse/eval/smt/sat function will raise the Fatal exception.
      In this case, after the exception is catched, you can run [print_msgs].
    - or no fatal error has been encoundered; if you want to display the errors,
      you can run [print_msgs].

   Rules for good error or warning messages:
    - a message must always end with a newline
    - use a new line (\n) only when the line break is mandatory for
      understanding. The text will be wrapped automatically.
    - use indentations for pieces of code you want to show: the indentation
      will be kept when wrapping as long as no \n is encountered.
      Indented text is colored.
    - the text in simple quote '...' or backquotes `...` is colored
*)

open Lexing (* for Lexing.position *)

type msg_type = Error | Warning
type loc = Lexing.position * Lexing.position
type during = Parse | Lex | Eval | Sat | Cnf
type msg = msg_type * during * string * loc

module Msg =
struct
  type t = msg
  let compare l1 l2 = Pervasives.compare l1 l2
end

include Set.Make(Msg)

exception Fatal of t

let has_error (msgs : t) : bool =
  exists (fun msg -> match msg with Error,_,_,_ -> true | _ -> false) msgs

let add_msg (msgs:t ref) msg = msgs := add msg !msgs
let add_fatal (msgs:t ref) msg =
  add_msg msgs msg; raise (Fatal !msgs)

let null_loc = (Lexing.dummy_pos,Lexing.dummy_pos)

let replace pattern replacement text =
    let open Str in global_replace (regexp pattern) replacement text

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

let all_placeholders loc typ with_colors msg = function
  | 'm' -> msg
  | 't' -> (match typ with Warning -> "warning" | Error -> "error")
  | c -> loc_placeholders loc c

(** [replace] replaces all '%c' for any character 'c' using the provided
    function 'placeholder'. Also replaces all two-characters '\n' to actual
    one-character newline. *)
let replace (placeholder : char -> string) text =
  let text = Str.global_replace (Str.regexp "\\\\n") "\n" text in
  let text = Str.global_replace (Str.regexp "\\\\t") "\t" text in
  let rec replace cur_pos =
    try
      let next_pos = Str.search_forward (Str.regexp "%[a-zA-Z]") text cur_pos in
      (*Printf.printf "cur=%d next=%d len=%d (%s)\n" cur_pos next_pos (String.length text) text;*)
      String.sub text cur_pos (next_pos-cur_pos)
        ^ (String.get text (next_pos+1) |> placeholder)
        ^ if next_pos+2 <= String.length text-1 then replace (next_pos+2) else ""
    with Not_found -> String.sub text cur_pos (String.length text - cur_pos)
  in replace 0

let string_of_loc ?(fmt="%l:%c") (loc:loc) : string =
  replace (loc_placeholders loc) fmt

(** Wraps the text at width. Indendation is kept as long no new line is read.
    If width = 0, do not wrap. *)
let format_width color width text =
  let rec format prev_indent from_pos =
    let cur_indent = try (Str.search_forward (Str.regexp "[^ ]") text from_pos)-from_pos
                     with Not_found -> 0 in
    let wrap_pos =
      let newline_pos =
        try Str.search_forward (Str.regexp "\n") text from_pos
        with Not_found -> String.length text
      in if newline_pos > from_pos+width then from_pos+width else newline_pos
    in
    let rec spaces = function 0 -> "" | x -> " "^ spaces (x-1) in
    match wrap_pos with
    | _ when from_pos >= wrap_pos -> ""
    | _ when wrap_pos = String.length text ->
      Printf.fprintf stdout "newline=%d,indent=%d,prev_indent=%d,len=%d, '%s'\n"
      wrap_pos cur_indent prev_indent (String.length text) (String.sub text from_pos (wrap_pos-from_pos));
      spaces prev_indent ^ String.sub text from_pos (wrap_pos-from_pos)
    | _ when String.get text wrap_pos = '\n' -> (* wrap at a \n symbol *)
      spaces prev_indent ^ String.sub text from_pos (wrap_pos-from_pos)
        ^"\n"^ format 0 (wrap_pos+1)
    | _ -> (* wrap at any point because width is too large *)
      let last_space = try Str.search_backward (Str.regexp "\\( \\|: \\|, \\|. \\)") text wrap_pos with Not_found -> wrap_pos in
      let last_space_end = last_space + if (String.get text last_space)=' ' then 0 else 1 in
        spaces prev_indent ^ String.sub text from_pos (last_space_end-from_pos)
        ^ "\n" ^ format (cur_indent+prev_indent) (last_space_end+1)
  in if width != 0 then format 0 0 else text;;

let rec string_of_msgs ?(width=78) ?(color=false) ?(fmt="%l:%c: %t: %m") (messages:t) =
  let color_backquote text = let colorize str = "\x1b[33m" ^ str ^ "\x1b[0m" in
    Str.global_substitute (Str.regexp "`\\([^`]\\)`") (fun s -> "`"^ colorize (Str.matched_group 1 s) ^"`") text in
  let color_quoted text = let colorize str = "\x1b[33m" ^ str ^ "\x1b[0m" in
    Str.global_substitute (Str.regexp "'\\([^']+\\)'") (fun s -> "'"^ colorize (Str.matched_group 1 s) ^"'") text in
  let color_code text = let colorize str = "\x1b[37m" ^ str ^ "\x1b[0m" in
    Str.global_substitute (Str.regexp "^\\(    +.*\\)$") (fun s -> colorize (Str.matched_group 1 s)) text in
  let color_type text = let colorize str = match str with
    | "warning" -> (* yellow bold *) "\x1b[33m\x1b[1m" ^str^ "\x1b[0m"
    | "error" ->   (* red bold    *) "\x1b[31m\x1b[1m" ^str^ "\x1b[0m"
    | str -> str
    in Str.substitute_first (Str.regexp "\\(error\\|warning\\)") (fun s -> colorize (Str.matched_group 1 s)) text in
  let color_all text = if color then text |> color_code |> color_backquote |> color_quoted |> color_type else text in
  fold
    (fun (typ,_,msg,loc) acc ->
      (replace (all_placeholders loc typ color msg) fmt
        |> format_width color width |> color_all) ^ acc)
    messages ""
