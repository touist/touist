(** This is where the compiler errors are managed.

    We call a 'message' an error or a warning.
    Two cases for displaying the errors:
    - either one of the parse/eval/smt/sat function will raise the Fatal exception.
      In this case, after the exception is catched, you can run [print_msgs].
    - or no fatal error has been encoundered; if you want to display the errors,
      you can run [print_msgs]. *)

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
  | 't' ->
    let typ_txt = match typ with Warning -> "warning" | Error -> "error" in
    let colorstart,colorend = match with_colors,typ with
      | false, _       -> "",""
      | true , Warning -> "\x1b[33m\x1b[1m" (* yellow bold *), "\x1b[0m" (* reset *)
      | true , Error   -> "\x1b[31m\x1b[1m" (* red bold    *), "\x1b[0m" (* reset *)
    in colorstart ^ typ_txt ^ colorend
  | c -> loc_placeholders loc c

(** [replace] replaces all '%c' for any character 'c' using the provided
    function 'placeholder'. Also replaces all '\n' to actual newlines. *)
let replace (placeholder : char -> string) text =
  let text = Str.global_replace (Str.regexp "\\\\n") "\n" text in
  let text = Str.global_replace (Str.regexp "\\\\t") "\t" text in
  let rec replace placeholder text =
    try
      let next_pos = Str.search_forward (Str.regexp "%[a-zA-Z]") text 0 in
      let expanded = placeholder (String.get text (next_pos+1)) in
      Str.string_before text next_pos
        ^ expanded
        ^ replace placeholder (Str.string_after text (next_pos+2))
    with Not_found -> ""
  in replace placeholder text

let string_of_loc ?(fmt="%l:%c") (loc:loc) : string =
  replace (loc_placeholders loc) fmt

(** Wraps the text at width. Indendation is kept as long no new line is read.
    If width = 0, do not wrap. *)
let format_width width text =
  let rec format prev_indent text =
    let indent = try Str.search_forward (Str.regexp "[^ ]") text 0 with Not_found -> 0 in
    let wrap_pos =
      let newline =
        try Str.search_forward (Str.regexp "\n") text 0
        with Not_found -> String.length text in
      if newline > width then width else newline
    in
    let rec spaces = function 0 -> "" | x -> " "^ spaces (x-1) in
    (*Printf.printf "%s\n-> newline=%d,indent=%d,prev_indent=%d,len=%d\n\n" text wrap_pos indent prev_indent (String.length text);*)
    match wrap_pos with
    | wrap when wrap = String.length text -> spaces prev_indent ^ text
    | wrap when String.get text wrap = '\n' -> (* wrap at a \n symbol *)
      spaces prev_indent ^ Str.string_before text (wrap+1) ^ format 0 (Str.string_after text (wrap+1))
    | wrap -> (* wrap at any point because width is too large *)
      let last_space = try Str.search_backward (Str.regexp "[ :,.]") text wrap with Not_found -> wrap in
        spaces prev_indent
        ^ Str.string_before text (if (String.get text last_space)=' ' then last_space else last_space+1)
        ^ "\n" ^ format (indent+prev_indent) (Str.string_after text (last_space+1))
  in if width != 0 then format 0 text else text;;

let rm_trailing_whitespace text = Str.global_replace (Str.regexp "[\n ]*$") "" text

(* NOTE: all msg should be finished by a trailing newline (\n) *)
let rec string_of_msgs ?(width=78) ?(color=false) ?(fmt="%l:%c: %t: %m") (messages:t) =
  fold
    (fun (typ,_,msg,loc) acc ->
      let msg = rm_trailing_whitespace msg in
      (replace (all_placeholders loc typ color msg) fmt |> format_width width) ^ acc)
    messages ""

(** Rules for good error or warning messages:
    - use \n only when the line break is mandatory for understanding.
      The text will be wrapped automatically.
    - use indentations for pieces of code you want to show: the indentation
      will be kept when wrapping as long as no \n is encountered.
    - you must not end your message with a new line (the \n character).
      This new line will be automatically removed anyway.
*)
