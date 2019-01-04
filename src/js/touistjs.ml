(** The touist SAT solver is written in C, so I cannot provide the javascript
    solver-part of Touist. Nevertheless, I can give access to anything that
    is parsing-related:
    - linter (check types...)
    - conversion to latex
*)

type loc = {
  line: int;
  col: int;
  start_abs: int;
  end_abs: int;
}
[@@deriving yojson]
type msg = {
  level: string;
  message: string;
  loc: loc;
} [@@deriving yojson]

type messages = {
  messages: msg list;
} [@@deriving yojson]

type linter_ret =
  | Lint_Succ of messages
  | Lint_Error of messages
[@@deriving yojson]

type latex_ret =
  | Latex_Succ of string * messages
  | Latex_Error of messages
[@@deriving yojson]


(* WARNING: because 'warnings' are outputed to stderr directly,
   we only get (for now) the latest error. *)
let to_msg (msg: Touist.Err.msg) : msg =
  let open Touist.Err in begin
    let typ,_,msg,pos = msg in
    let l,c,s,e = (match pos with None -> 0,0,0,0 | Some l -> get_loc l)  in
    let loc = {line=l;col=c;start_abs=s;end_abs=e} in
    {level = string_of_type typ; message=msg; loc=loc}
  end
open Js_of_ocaml

let _ =
  Js.export_all (object%js
    method linter (txt:Js_of_ocaml.Js.js_string Js.t) : Js.js_string Js.t =
      let ret =
      try let _ = Touist.Parse.parse_sat (Js.to_string txt) |> Touist.Eval.eval ~onlychecktypes:true in
        Lint_Succ ({messages=[]})
      with Touist.Err.TouistFatal msgs ->
        Lint_Error ({messages=[to_msg msgs]})
      in linter_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string

    method latex (txt:Js.js_string Js.t) : Js.js_string Js.t =
      let ret =
      try let ast = Touist.Parse.parse_sat (Js.to_string txt) in let latex = Touist.Latex.latex_of_ast ast in
        Latex_Succ (latex ~full:false, {messages=[]})
      with Touist.Err.TouistFatal msgs ->
        Latex_Error ({messages=[to_msg msgs]})
      in latex_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string
  end)