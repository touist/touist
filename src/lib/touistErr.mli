(** This is where the compiler errors are managed.

    We call a 'message' an error or a warning.
    Two cases for displaying the errors:
    - either one of the parse/eval/smt/sat function will raise the TouistFatal exception.
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

type msg_type = Error | Warning
type during = Usage | Parse | Lex | Eval | Sat | Cnf | Prenex
type loc = Lexing.position * Lexing.position
type msg = msg_type * during * string * loc option

exception TouistFatal of msg

(** {2 Print the errors} *)

val string_of_loc : ?fmt:string -> loc -> string
val string_of_msg : ?width:int -> ?color:bool -> ?fmt:string -> msg -> string
val string_of_type : msg_type -> string
val string_of_during : during -> string

(** [get_loc] translates a 'loc' to an understandable tuple that contains
  (num_line, num_col, token_start, token_end). *)
val get_loc : loc -> int * int * int * int


(** {2 Give errors} *)

val warn : msg -> unit
val fatal : msg -> 'a

(** {2 Change the error formatting} *)

val wrap_width : int ref
val format : string ref
val loc_format : string ref
val color : bool ref
val discard_warnings : bool ref