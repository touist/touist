(** Handles errors in {!Parse.parse} produced by the menhir in
    incremental parser.
    [report] is the main function. *)

(* To understand what is a checkpoint and everything, you can
   check the menhir incremental parser API in
   IncrementalEngine.ml (google it) *)

(* Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/FredMaris/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available
 * under the terms of the GNU Lesser General Public License (LGPL)
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 *
 * A huge part of this code has been inspired by cparser/ErrorReports.ml
 * contained in the project AbsInt/CompCert. Here are the terms:
 *
 * The Compcert verified compiler
 * Jacques-Henri Jourdan, INRIA Paris-Rocquencourt
 * Copyright Institut National de Recherche en Informatique et en
 * Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.  This file is also distributed
 * under the terms of the INRIA Non-Commercial License Agreement. *)

open Lexing
open Parser.MenhirInterpreter
module S = MenhirLib.General (* Streams *)

(* -------------------------------------------------------------------------- *)

(* There are places where we may hit an internal error and we would like to fail
   abruptly because "this cannot happen". Yet, it is safer when shipping to
   silently cover up for our internal error. Thus, we typically use an idiom of
   the form [if debug then assert false else <some default value>]. *)

let debug = ref false (* This variable is set when calling the function [report]*)

(* -------------------------------------------------------------------------- *)

(* The parser keeps track of the last two tokens in a two-place buffer. *)

type 'a buffer =
| Zero
| One of 'a
| Two of 'a * (* most recent: *) 'a

(* [push buffer x] pushes [x] into [buffer], causing the buffer to slide. *)

let update buffer x : _ buffer =
  match buffer, x with
  | Zero, _ ->
      One x
  | One x1, x2
  | Two (_, x1), x2 ->
      Two (x1, x2)

(* [show f buffer] prints the contents of the buffer. The function [f] is
   used to print an element. *)

let show f buffer : string =
  match buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens, we
         cannot have detected a syntax error. *)
      if !debug then assert false else ""
  | One invalid ->
      (* It is unlikely, but possible, that we have read just one token. *)
      Printf.sprintf "before '%s'" (f invalid)
  | Two (valid, invalid) ->
      (* In the most likely case, we have read two tokens. *)
      Printf.sprintf "after '%s' and before '%s'" (f valid) (f invalid)

(* [exact_pos] returns the start position of the invalid token;
   this is useful for giving the user a one-position indication of where
   the error is. *)
let exact_pos buffer : position =
  match buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens, we
         cannot have detected a syntax error. *)
      assert false
  | One invalid
  | Two (_, invalid) -> let start_pos,_ = invalid in start_pos

(* [area_pos] returns the area (= start and end positions) where the error
   was found; this is useful for red-underlying the error in an IDE.
   It will give the beginning of the token preceeding the invalid token
   and the end of the invalid token.
   If there is no preceeding token, returns the invalid token twice.
   NOTE: position = Lexing.position *)

let area_pos (buffer : (position*position) buffer) : position * position =
  match buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens, we
         cannot have detected a syntax error. *)
      assert false
  | One invalid -> let (startpos,endpos) = invalid in (startpos,endpos)
  | Two (previous, invalid) -> let (startpos,_),(_,endpos) = previous,invalid
    in startpos, endpos

(* -------------------------------------------------------------------------- *)

(* [extract text (pos1, pos2)] extracts the sub-string of [text] delimited
   by the positions [pos1] and [pos2]. *)

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try
    String.sub text ofs1 len
  with Invalid_argument _ ->
    (* In principle, this should not happen, but if it does, let's make this
       a non-fatal error. *)
    if !debug then assert false else "???"

(* -------------------------------------------------------------------------- *)

(* [compress text] replaces every run of at least one whitespace character
   with exactly one space character. *)

let compress text =
  Re.Str.global_replace (Re.Str.regexp "[ \t\n\r]+") " " text

(* -------------------------------------------------------------------------- *)

(* [sanitize text] eliminates any special characters from the text [text].
   They are (arbitrarily) replaced with a single dot character. *)

let sanitize text =
  String.map (fun c ->
    if Char.code c < 32 || Char.code c >= 127 then '.' else c
  ) text

(* -------------------------------------------------------------------------- *)

(* [shorten k text] limits the length of [text] to [2k+3] characters. If the
   text is too long, a fragment in the middle is replaced with an ellipsis. *)

let shorten k text =
  let n = String.length text in
  if n <= 2 * k + 3 then
    text
  else
    String.sub text 0 k ^
    "..." ^
    String.sub text (n - k) k

(* -------------------------------------------------------------------------- *)

(* [stack checkpoint] extracts the parser's stack out of a checkpoint. *)

let stack checkpoint =
  match checkpoint with
  | HandlingError env ->
      stack env
  | _ ->
      assert false (* this cannot happen, I promise *)

(* -------------------------------------------------------------------------- *)

(* [state checkpoint] extracts the number of the current state out of a
   parser checkpoint. *)

let state checkpoint : int =
  match Lazy.force (stack checkpoint) with
  | S.Nil ->
      (* Hmm... The parser is in its initial state. Its number is
         usually 0. This is a BIG HACK. TEMPORARY *)
      0
  | S.Cons (Element (s, _, _, _), _) ->
      number s

(* -------------------------------------------------------------------------- *)

(* TEMPORARY move to MenhirLib.General *)

let rec drop n (xs : 'a S.stream) : 'a S.stream =
  match n, xs with
  | 0, _
  | _, lazy (S.Nil) ->
      xs
  | _, lazy (S.Cons (_, xs)) ->
      drop (n - 1) xs

(* -------------------------------------------------------------------------- *)

(* [element checkpoint i] returns the [i]-th cell of the parser stack. The index
   [i] is 0-based. [i] should (ideally) be within bounds; we raise [Not_found]
   if it isn't. *)

let element checkpoint i : element =
  let i' = if i>0 then (i-1) else i in
  match Lazy.force (drop i' (stack checkpoint)) with
  | S.Nil ->
      (* [i] is out of range. This could happen if the handwritten error
         messages are out of sync with the grammar, or if a mistake was
         made. We fail in a non-fatal way. *)
      raise Not_found
  | S.Cons (Element (a, b, p1, p2), _) ->
      match i with
      | 0 -> let p1',p2' = (positions (match checkpoint with HandlingError env -> env | _ -> failwith "Shouldnt happen")) in
        Element (a, b, p1', p2')
      | _ -> Element (a, b, p1, p2)

(* -------------------------------------------------------------------------- *)

(* [range text e] converts the stack element [e] to the fragment of the source
   text that corresponds to this stack element. The fragment is placed within
   single quotes and shortened if it is too long. We also ensure that it does
   not contain any special characters. *)

let width = 30

let range text (e : element) : string =
  (* Extract the start and positions of this stack element. *)
  let Element (_, _, pos1, pos2) = e in
  (* Get the underlying source text fragment. *)
  let fragment = extract text (pos1, pos2) in
  (* Sanitize it and limit its length. Enclose it in single quotes. *)
  "'" ^ shorten width (sanitize (compress fragment)) ^ "'"

(* -------------------------------------------------------------------------- *)

(* We allow an error message to contain the special form $i, where is a 0-based
   index into the stack. We replace it with the fragment of the source text that
   corresponds to this stack entry. *)

let fragment text checkpoint message =
  try
    let i = int_of_string (Re.Str.matched_group 1 message) in
    range text (element checkpoint i)
  with
  | Failure _ ->
      (* In principle, this should not happen, but if it does, let's cover up
         for our internal error. *)
      if !debug then assert false else "???"
  | Not_found ->
      (* In principle, this should not happen, but if it does, let's cover up
         for our internal error. *)
      if !debug then assert false else "???"

let fragments text checkpoint (message : string) : string =
  Re.Str.global_substitute
    (Re.Str.regexp "\\$\\([0-9]+\\)")
    (fragment text checkpoint)
    message

(* -------------------------------------------------------------------------- *)

(* [report text buffer checkpoint] constructs an error message. The C source
   code must be stored in the string [text]. The start and end positions of the
   last two tokens that were read must be stored in [buffer]. The parser state
   (i.e., the automaton's state and stack) must be recorded in the checkpoint
   [checkpoint]. *)

(* The start and end positions of the invalid token are [lexbuf.lex_start_p]
   and [lexbuf.lex_curr_p], since this is the last token that was read. But
   we need not care about that here. *)

let report text buffer checkpoint debug' : string =
  debug := debug'; (* Sets the debug flag for the whole ErrorReporting.ml file *)
  let where = show (extract text) buffer in
  (* Find out in which state the parser failed. *)
  let s : int = state checkpoint in
  (* Choose an error message, based on the state number [s].
     Then, customize it, based on dynamic information. *)
  let message = try
    ParserMsgs.message s |>
    fragments text checkpoint
  with Not_found ->
    (* If the state number cannot be found -- which, in principle,
       should not happen, since our list of erroneous states is
       supposed to be complete! -- produce a generic message. *)
    Printf.sprintf "This is an unknown syntax error (number %d). This error \
                   is missing in parser.messages (see HOWTODEBUG.md).\n" s
  in
  (* Construct the full error message. *)
  let message = Printf.sprintf "syntax error %s. %s"
    where
    message
  ^
  if !debug then
      Printf.sprintf "Debug: Automaton state: %d (see src/parser.messages)\n" s
  else ""
in message
