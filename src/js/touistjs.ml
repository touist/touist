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
[@@deriving to_yojson]
type msg = {
  level: string;
  message: string;
  loc: loc;
} [@@deriving to_yojson]

type messages = {
  messages: msg list;
} [@@deriving to_yojson]

type linter_ret =
  | Lint_Succ of messages
  | Lint_Error of messages
[@@deriving to_yojson]

type latex_ret =
  | Latex_Succ of string * messages
  | Latex_Error of messages
[@@deriving to_yojson]

let to_msg_list (msgs : Msgs.t) : msg list =
  let open Msgs in
  fold (fun (typ,_,msg,pos) acc ->
    let l,c,s,e = get_loc pos in
    let loc = {line=l;col=c;start_abs=s;end_abs=e} in
    {level = string_of_type typ; message=msg; loc=loc}::acc)
    msgs []

let _ =
  Js.export_all (object%js
    method linter (txt:Js.js_string Js.t) : Js.js_string Js.t =
      let ret =
      try let _,msgs = Parse.parse_sat (Js.to_string txt) |> Eval.eval ~onlychecktypes:true in
        Lint_Succ ({messages=to_msg_list !msgs})
      with Msgs.Fatal msgs ->
        Lint_Error ({messages=to_msg_list msgs})
      in linter_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string

    method latex (txt:Js.js_string Js.t) : Js.js_string Js.t =
      let ret = 
      try let ast,msgs = Parse.parse_sat (Js.to_string txt) in let latex = Latex.latex_of_ast ast in
        Latex_Succ (latex, {messages=to_msg_list !msgs})
      with Msgs.Fatal msgs ->
        Latex_Error ({messages=to_msg_list msgs})
      in latex_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string
  end)