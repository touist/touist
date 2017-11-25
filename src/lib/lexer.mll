(* Definition of the ocamllex lexer. *)

(* Project TouIST, 2015. Easily formalize and solve real-world sized problems
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
 * under the terms of the INRIA Non-Commercial License Agreement. *)

{
  open Lexing
  open Touist.Parser
  open Touist.Err

  exception Error of string * loc

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
   ["end",           END;
    "in",            IN;
    "subset(",       SUBSET_PR;
    "empty(",        EMPTY;
    "union(",        UNION_PR;
    "inter(",        INTER_PR;
    "diff(",         DIFF_PR;
    "union",         UNION;
    "inter",         INTER;
    "diff",          DIFF;
    "subset",        SUBSET;
    "exact(",        EXACT;
    "atmost(",       ATMOST;
    "atleast(",      ATLEAST;
    "bigand",        BIGAND;
    "bigor",         BIGOR;
    "when",          WHEN;
    "Top",           TOP;
    "Bot",           BOTTOM;
    "card(",          CARD;
    "mod",           MOD;
    "sqrt(",         SQRT;
    "int(",          TOINT;
    "float(",        TOFLOAT;
    "abs(",          ABS;
    "and",           AND;
    "or",            OR;
    "xor",           XOR;
    "not",           NOT;
    "if",            IF;
    "then",          THEN;
    "else",          ELSE;
    "mod",           MOD;
    "true",          BOOL true;
    "false",         BOOL false;
    "let",           LET;
    "data",          DATA;
    "powerset(",     POWERSET;
    "exists",        EXISTS;
    "forall",        FORALL;
    "for",        FOR;
    ]
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

(* [token] is the function called by the parser for getting the next token.
   [token] returns a list of tokens instead of a single token, i.e., is of type
       (Lexing.lexbuf -> Touist.Parser.token list)                       (1)
   - Why: because of the case (ident as i)'(', we need to return two tokens;
     for example, with 'not(', we must return NOT and LPAREN.
   - Consequence: the parser cannot accept [token] directly; it needs a function
       (Lexing.lexbuf -> Touist.Parser.token)                            (2)
     This forces us to write another function ('lexer' in touist.ml) that
     transforms the function (1) to correct function type (2).
*)
rule token = parse (* is a function (Lexing.lexbuf -> Touist.Parser.token list) *)
  | eof               {[ EOF          ]}
  | empty+            {  token lexbuf  }
  | "["               {[ LBRACK       ]}
  | "]"               {[ RBRACK       ]}
  | ".."              {[ RANGE        ]}
  | ","               {[ COMMA        ]}
  | "=="              {[ EQUAL        ]}
  | "!="              {[ NOTEQUAL     ]}
  | "+"               {[ ADD          ]}
  | "-"               {[ SUB          ]}
  | "*"               {[ MUL          ]}
  | "/"               {[ DIV          ]}
  | "=>"              {[ IMPLIES      ]}
  | "<=>"             {[ EQUIV        ]}
  | "<"               {[ LT           ]}
  | ">"               {[ GT           ]}
  | "<="              {[ LE           ]}
  | ">="              {[ GE           ]}
  | "="               {[ AFFECT       ]}
  | ":"               {[ COLON        ]}
  | ")"               {[ RPAREN       ]}
  | "\\\\"               {[ NEWLINE       ]}
  | '$'(var as v)'('  {[ VARTUPLE ("$" ^ v)]}
  | '$'(var as v)     {[ VAR ("$" ^ v)]}
    (* This rule is going to take care of both identifiers
     * and every keyword in reserved_keywords *)
  | (ident as i)'('
  (* Three cases:
     1. i is an identifier that requires an opening parenthesis, e.g., int().
        In this case, we just send the token INT.
     2. i is an identifier that doesn't require an opening parenthesis, e.g.,
        "not". In this case, two tokens must be sent: NOT and LPAREN
     3. i is an identifier that does not belong to known identifiers. This means
         that it is a tuple-term. This is the "Not_found" case which send a
         TUPLE token.
   *)
    { try [ Hashtbl.find reserved_keywords (i ^ "(") ]  (* case 1 *)
      with Not_found ->
        try [ Hashtbl.find reserved_keywords i ; LPAREN ]          (* case 2 *)
        with Not_found -> [TUPLE i]                   (* case 3 *)
    }
  | ident as i     { try [ Hashtbl.find reserved_keywords i ]
                     with Not_found ->  [TERM i] }
  | "("            {[ LPAREN       ]}
  | integer as i   {[ INT          (int_of_string i) ]}
  | double as f    {[ FLOAT      (float_of_string f) ]}
  | newline        { next_line lexbuf; token lexbuf   }
  | ";;"           { comments_parse lexbuf            }
  | _ as c         { let loc = (lexbuf.lex_curr_p,lexbuf.lex_curr_p)
                     in raise (Error ("unexpected char '"^(String.make 1 c)^"'\n", loc)) }

and comments_parse = parse
  | '\n'           { next_line lexbuf; token lexbuf }
  | _              { comments_parse lexbuf          }
