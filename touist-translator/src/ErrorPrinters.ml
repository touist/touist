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
(*
 * error_printer.ml
 * Copyright (C) 2016 Maël Valais <mael.valais@gmail.com>
 *
 * Distributed under terms of the MIT license.
 *)

open Parser.MenhirInterpreter
open Pprint
open Syntax

(* In order to print syntax error messages and/or debugging information, we
   need a symbol printer. *)

let print_symbol symbol : string =
  match symbol with
  | X (T T_EOF)     -> "eof"
  | X (T T_BEGIN)   -> "begin" 
  | X (T T_END)     -> "end" 
  | X (T T_SETS)    -> "sets" 
  | X (T T_FORMULA) -> "formula" 
  | X (T T_IN)      -> "in" 
  | X (T T_SUBSET)  -> "subset" 
  | X (T T_EMPTY)   -> "empty" 
  | X (T T_UNION)   -> "union" 
  | X (T T_INTER)   -> "inter" 
  | X (T T_DIFF)    -> "diff" 
  | X (T T_EXACT)   -> "exact" 
  | X (T T_ATMOST)  -> "atmost" 
  | X (T T_ATLEAST) -> "atleast" 
  | X (T T_BIGAND)  -> "bigand" 
  | X (T T_BIGOR)   -> "bigor" 
  | X (T T_WHEN)    -> "when" 
  | X (T T_TOP)     -> "Top" 
  | X (T T_BOTTOM)  -> "Bot" 
  | X (T T_CARD)    -> "card" 
  | X (T T_LPAREN)  -> "("  
  | X (T T_RPAREN)  -> ")"  
  | X (T T_LBRACK)  -> "["  
  | X (T T_RBRACK)  -> "]"  
  | X (T T_RANGE)   -> ".."  
  | X (T T_COMMA)   -> ","  
  | X (T T_EQUAL)   -> "=="  
  | X (T T_NOTEQUAL)-> "!="  
  | X (T T_ADD)     -> "+"  
  | X (T T_SUB)     -> "-"  
  | X (T T_MUL)     -> "*"  
  | X (T T_DIV)     -> "/"  
  | X (T T_MOD)     -> "mod" 
  | X (T T_SQRT)    -> "sqrt" 
  | X (T T_LT)      -> "<"  
  | X (T T_GT)      -> ">"  
  | X (T T_LE)      -> "<="  
  | X (T T_GE)      -> ">="  
  | X (T T_AFFECT)  -> "="  
  | X (T T_COLON)   -> ":"  
  | X (T T_TOINT)   -> "int" 
  | X (T T_TOFLOAT) -> "float" 
  | X (T T_BOOL)    -> "true" 
  (* | X (T T_BOOL)    -> "false"  *)
  | X (T T_AND)     -> "and" 
  | X (T T_OR)      -> "or" 
  | X (T T_IMPLIES) -> "=>"  
  | X (T T_EQUIV)   -> "<=>"  
  | X (T T_XOR)     -> "xor" 
  | X (T T_NOT)     -> "not" 
  | X (T T_IF)      -> "if" 
  | X (T T_THEN)    -> "then" 
  | X (T T_ELSE)    -> "else" 
  | X (T T_VAR)     -> "$var"
  | X (T T_TERM)    -> "term" 
  | X (T T_INT)     -> "int"
  | X (T T_FLOAT)   -> "float"
  | X (N N_clause)  -> "clause"
  | X (N N_exp)     -> "exp"
  | X (N N_var_decl)-> "var_decl"
  | X (N N_term_or_exp)->"term_or_exp"
  | X (N N_set_decl) -> "set_decl"
  | X (T T_error)   -> "error"
  (* | next_line lexbuf; lexer lexbuf } *)
  (* | comments_parse lexbuf          } *)


(* In order to print a view of the stack that includes semantic values,
   we need an element printer. (If we don't need this feature, then
   [print_symbol] above suffices.) *)

let print_element e : string =
  match e with
  | Element (s, v, _, _) ->
      match incoming_symbol s with
      | T T_EOF     -> "eof"      
      | T T_BEGIN   -> "begin" 
      | T T_END     -> "end" 
      | T T_SETS    -> "sets" 
      | T T_FORMULA -> "formula" 
      | T T_IN      -> "in" 
      | T T_SUBSET  -> "subset" 
      | T T_EMPTY   -> "empty" 
      | T T_UNION   -> "union" 
      | T T_INTER   -> "inter" 
      | T T_DIFF    -> "diff" 
      | T T_EXACT   -> "exact" 
      | T T_ATMOST  -> "atmost" 
      | T T_ATLEAST -> "atleast" 
      | T T_BIGAND  -> "bigand" 
      | T T_BIGOR   -> "bigor" 
      | T T_WHEN    -> "when" 
      | T T_TOP     -> "Top" 
      | T T_BOTTOM  -> "Bot" 
      | T T_CARD    -> "card" 
      | T T_LPAREN  -> "("  
      | T T_RPAREN  -> ")"  
      | T T_LBRACK  -> "["  
      | T T_RBRACK  -> "]"  
      | T T_RANGE   -> ".."  
      | T T_COMMA   -> ","  
      | T T_EQUAL   -> "=="  
      | T T_NOTEQUAL-> "!="  
      | T T_ADD     -> "+"  
      | T T_SUB     -> "-"  
      | T T_MUL     -> "*"  
      | T T_DIV     -> "/"  
      | T T_MOD     -> "mod" 
      | T T_SQRT    -> "sqrt" 
      | T T_LT	    -> "<"  
      | T T_GT	    -> ">"  
      | T T_LE	    -> "<="  
      | T T_GE      -> ">="  
      | T T_AFFECT  -> "="  
      | T T_COLON   -> ":"  
      | T T_TOINT   -> "int" 
      | T T_TOFLOAT	-> "float" 
      | T T_BOOL    -> "true" 
      (* | T T_BOOL    -> "false"  *)
      | T T_AND     -> "and" 
      | T T_OR	    -> "or" 
      | T T_IMPLIES	-> "=>"  
      | T T_EQUIV   -> "<=>"
      | T T_XOR     -> "xor" 
      | T T_NOT     -> "not" 
      | T T_IF      -> "if" 
      | T T_THEN    -> "then" 
      | T T_ELSE    -> "else" 
      | T T_VAR     -> "$" ^ v
      | T T_TERM    -> v 
      | T T_INT     -> string_of_int v
      | T T_FLOAT   -> string_of_float v
      | N N_clause  -> string_of_clause v
      | N N_exp     -> string_of_exp v     
      | N N_var_decl-> 
         let (name, exp_list_option)=v in  
         begin 
           match exp_list_option with
           | None -> "$" ^ name ^ "=" 
           | Some exp_list-> "$" ^ name ^ "=" ^ " " ^ (string_of_exp_list "," exp_list)
         end
      | N N_term_or_exp-> 
         begin
           match v with
           | (e:Syntax.exp) -> string_of_exp e
         end
      | N N_set_decl -> string_of_exp_list "," v
         (* begin  *)
         (*   match v with *)
         (*   | None -> name  *)
         (*   | Some s-> "$" ^ name ^ "=" ^ " " ^ (string_of_set "," s) *)
         (* end *)
      | T T_error   -> "error"
      (* | next_line lexbuf; lexer lexbuf } *)
      (* | comments_parse lexbuf          } *)

(* The public functions. *)

let print = prerr_string
let print_symbol s = print (print_symbol s)
let print_element = Some (fun s -> print (print_element s))
  
