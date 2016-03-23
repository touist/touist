(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Paris-Rocquencourt                            *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2015 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

open Parser
open Parser.MenhirInterpreter

(* In order to submit artificial tokens to the parser, we need a function that
   converts a terminal symbol to a (dummy) token. Unfortunately, we cannot (in
   general) auto-generate this code, because it requires making up semantic
   values of arbitrary OCaml types. *)

let terminal2token (type a) (symbol : a terminal) : token =
  match symbol with
  | T_EOF -> EOF 
  | T_BEGIN -> BEGIN
  | T_END -> END
  | T_SETS -> SETS
  | T_FORMULA -> FORMULA
  | T_IN -> IN
  | T_SUBSET -> SUBSET
  | T_EMPTY -> EMPTY
  | T_UNION -> UNION
  | T_INTER -> INTER
  | T_DIFF -> DIFF
  | T_EXACT -> EXACT
  | T_ATMOST -> ATMOST
  | T_ATLEAST -> ATLEAST
  | T_BIGAND -> BIGAND
  | T_BIGOR -> BIGOR
  | T_WHEN -> WHEN
  | T_TOP -> TOP
  | T_BOTTOM -> BOTTOM
  | T_CARD -> CARD
  | T_LPAREN -> LPAREN
  | T_RPAREN -> RPAREN
  | T_LBRACK -> LBRACK
  | T_RBRACK -> RBRACK
  | T_RANGE -> RANGE
  | T_COMMA -> COMMA
  | T_EQUAL -> EQUAL
  | T_NOTEQUAL -> NOTEQUAL
  | T_ADD -> ADD
  | T_SUB -> SUB
  | T_MUL -> MUL
  | T_DIV -> DIV
  | T_MOD -> MOD
  | T_SQRT -> SQRT
  | T_LT -> LT
  | T_GT -> GT
  | T_LE -> LE
  | T_GE -> GE
  | T_AFFECT -> AFFECT
  | T_COLON -> COLON
  | T_TOINT -> TOINT
  | T_TOFLOAT -> TOFLOAT
  | T_BOOL -> BOOL true
  | T_AND -> AND
  | T_OR -> OR
  | T_IMPLIES -> IMPLIES
  | T_EQUIV -> EQUIV
  | T_XOR -> XOR
  | T_NOT -> NOT
  | T_IF -> IF
  | T_THEN -> THEN
  | T_ELSE -> ELSE
  | T_VAR -> VAR ""
  | T_TERM -> TERM ""
  | T_INT -> INT 0
  | T_FLOAT -> FLOAT 0.0
  | T_error -> assert false
