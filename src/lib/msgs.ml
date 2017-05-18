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

(* NOTE: all msg should be finished by a trailing newline (\n) *)
let rec string_of_msgs ?(color=false) ?(fmt="%l:%c: %t: %m\n") (messages:t) =
  fold
    (fun (typ,_,msg,loc) acc ->
      (replace (all_placeholders loc typ color msg) fmt) ^ acc)
    messages ""

(** [print_msgs] will display the messages that have been produced by parse, eval, sat,
    cnf or smt.
    @param detailed enables the 'detailed location' mode (adds the absolute positions)  *)
let rec print_msgs ?(color=false) ?(fmt="%l:%c: %t: %m\n") (msgs:t) =
  Printf.fprintf stderr "%s" (string_of_msgs ~color:color ~fmt:fmt msgs)

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
