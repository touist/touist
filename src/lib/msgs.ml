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


let string_of_loc ?(detailed=false) (loc:loc) : string =
  let s,e = loc in (* start, end *)
  let relative = Printf.sprintf "%d:%d" s.pos_lnum (s.pos_cnum - s.pos_bol+1) in
  let absolute = Printf.sprintf "%d:%d" s.pos_cnum e.pos_cnum in
  match detailed with
  | false -> relative              (* num_line:num_col *)
  | true  -> relative ^":"^ absolute (* num_line:num_col:token_start:token_end *)

let rec string_of_msgs ?(color=false) ?(detailed=false) (messages:t) = 
  fold 
    (fun (typ,_,msg,loc) acc ->
      let colstart,colend = match color,typ with 
        | false,_ -> "",""
        | _,Warning -> "\x1b[33m\x1b[1m" (* yellow bold *), "\x1b[0m" (* reset *)
        | _,Error   -> "\x1b[31m\x1b[1m" (* red bold    *), "\x1b[0m" (* reset *)
      and txt_type = match typ with
        | Warning -> "warning"
        | Error -> "error"
      (* NOTE: all msg should be finished by a trailing newline (\n) *)
      in (Printf.sprintf "%s: %s%s%s: %s" (string_of_loc ~detailed:detailed loc) colstart txt_type colend msg) ^ acc)
      messages ""
(** [print_msgs] will display the messages that have been produced by parse, eval, sat,
    cnf or smt. 
    @param detailed enables the 'detailed location' mode (adds the absolute positions)  *)
let rec print_msgs ?(color=false) ?(detailed=false) (msgs:t) =
  Printf.fprintf stderr "%s" (string_of_msgs ~color:color ~detailed:detailed msgs)

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
