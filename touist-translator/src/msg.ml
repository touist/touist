(** This is where the compiler errors are managed. 

    We call a 'message' an error or a warning.
    Two cases for displaying the errors:
    - either one of the parse/eval/smt/sat function will raise the Fatal exception.
      In this case, after the exception is catched, you can run [print_msgs]. 
    - or no fatal error has been encoundered; if you want to display the errors,
      you can run [print_msgs]. *)

open Lexing (* for Lexing.position *)

type during = Parse | Lex | Eval | Sat | Cnf
type msg_type = Error | Warning
type loc = Lexing.position * Lexing.position
type msg = msg_type * during * string * loc
exception Fatal

let messages = ref []

let clear_messages : unit = messages := []
let rec has_error' (l:msg list) = match l with 
  | [] -> false
  | ((Error,_,_,_))::xs -> true
  | x::xs -> has_error' xs
let has_error : bool =
  has_error' !messages

let add_msg msg = messages := msg::!messages
let add_fatal msg =
    add_msg msg; raise (Fatal)
let null_loc = (Lexing.dummy_pos,Lexing.dummy_pos)

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
let string_of_loc ?(detailed=false) (loc:loc) : string =
let s,e = loc in (* start, end *)
let relative = Printf.sprintf "%d:%d" s.pos_lnum (s.pos_cnum - s.pos_bol+1) in
let absolute = Printf.sprintf "%d:%d" s.pos_cnum e.pos_cnum in
match detailed with
| false -> relative              (* num_line:num_col *)
| true  -> relative ^":"^ absolute (* num_line:num_col:token_start:token_end *)

let rec print_msgs' ?(color=false) ?(detailed=false) (messages:msg list) = match messages with
  | [] -> ()
  | (typ,_,msg,loc)::next ->
      let colstart,colend = match color,typ with 
        | false,_ -> "",""
        | _,Warning -> "\x1b[33m\x1b[1m" (* yellow bold *), "\x1b[0m" (* reset *)
        | _,Error   -> "\x1b[31m\x1b[1m" (* red bold    *), "\x1b[0m" (* reset *)
      and txt_type = match typ with
        | Warning -> "warning"
        | Error -> "error"
      in
      begin
        Printf.fprintf stderr "%s: " (string_of_loc ~detailed:detailed loc);
        Printf.fprintf stderr "%s%s%s: " colstart txt_type colend;
        Printf.fprintf stderr "%s\n" msg;
        print_msgs' ~color:color ~detailed:detailed next
      end
(** [print_msgs] will display the messages that have been produced by parse, eval, sat,
    cnf or smt. 
    @param detailed enables the 'detailed location' mode (adds the absolute positions)  *)
let rec print_msgs ?(color=false) ?(detailed=false) () =
  print_msgs' ~color:color ~detailed:detailed !messages