(*
 * lexer.mll: definition of the ocamllex lexer.
 *
 * Project TouIST, 2015. Easily formalize and solve real-world sized problems
 * using propositional logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
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
 * Some of the code has been inspired by cparser/Lexer.mll contained in
 * the project AbsInt/CompCert. Here are the terms:
 *
 * The Compcert verified compiler                      
 * Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            
 * Copyright Institut National de Recherche en Informatique et en     
 * Automatique.  All rights reserved.  This file is distributed       
 * under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, either version 2 of the License, or  
 * (at your option) any later version.  This file is also distributed 
 * under the terms of the INRIA Non-Commercial License Agreement. 
 *)

{
  open Lexing
  open Parser
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol  = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1}

  (* I added this hashtable to be 100% sure that no [ident] will be using a 
  reserved keyword. Inspired from
  http://www.seas.upenn.edu/~cis120/16sp/ocaml-4.01-manual/lexyacc.html#sec286 *)
  let reserved_keywords = Hashtbl.create 70
  let _ = List.iter (fun (kwd, tok) -> Hashtbl.add reserved_keywords kwd tok)
   ["begin",         BEGIN;
    "end",           END;
    "sets",          SETS;
    "formula",       FORMULA;
    "in",            IN;
    "subset",        SUBSET;
    "empty",         EMPTY;
    "union",         UNION;
    "inter",         INTER;
    "diff",          DIFF;
    "exact",         EXACT;
    "atmost",        ATMOST;
    "atleast",       ATLEAST;
    "bigand",        BIGAND;
    "bigor",         BIGOR;
    "when",          WHEN;
    "Top",           TOP;
    "Bot",           BOTTOM;
    "card",          CARD;
    "mod",           MOD;
    "sqrt",          SQRT;
    "int",           TOINT;
    "float",         TOFLOAT;
    "and",           AND;
    "or",            OR;
    "xor",           XOR;
    "not",           NOT;
    "if",            IF;
    "then",          THEN;
    "else",          ELSE;
    "mod",           MOD;
    "sqrt",          SQRT;
    "true",          BOOL true;
    "false",         BOOL false ]
}

let digit      = ['0' - '9']
let alpha      = ['a' - 'z' 'A' - 'Z']
let empty      = ['\t' ' ']
let special    = ['_']
let newline    = '\r' | '\n' | "\r\n"
let ident = (special | digit)* alpha (alpha | special | digit)*
let var   = (special | digit)* alpha (alpha | special | digit)*
let integer    = digit+
let double     = digit+ '.' digit+

rule token = parse
  | eof            { EOF          }
  | empty+         { token lexbuf }
  | "("            { LPAREN       }
  | ")"            { RPAREN       }
  | "["            { LBRACK       }
  | "]"            { RBRACK       }
  | ".."           { RANGE        }
  | ","            { COMMA        }
  | "=="           { EQUAL        }
  | "!="           { NOTEQUAL     }
  | "+"            { ADD          }
  | "-"            { SUB          }
  | "*"            { MUL          }
  | "/"            { DIV          }
  | "=>"           { IMPLIES      }
  | "<=>"          { EQUIV        }
  | "<"            { LT           }
  | ">"            { GT           }
  | "<="           { LE           }
  | ">="           { GE           }
  | "="            { AFFECT       }
  | ":"            { COLON        }

  | '$'(var as v)  { VAR ("$" ^ v) }
    (* This rule is going to take care of both identifiers
     * and every keyword in reserved_keywords *)
  | ident as i     { try Hashtbl.find reserved_keywords i with Not_found ->
                     TERM                       (i) }
  | integer as i   { INT          (int_of_string i) }
  | double as f    { FLOAT      (float_of_string f) }
  | newline        { next_line lexbuf; token lexbuf }
  | ";;"           { comments_parse lexbuf          }
  | _ as c         { raise (Error ("Unexpected char: " ^ (String.make 1 c))) }

and comments_parse = parse
  | '\n'           { next_line lexbuf; token lexbuf }
  | _              { comments_parse lexbuf          }
