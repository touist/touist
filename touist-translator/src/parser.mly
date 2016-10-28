(*
 * parser.mly: the defition of the menhir incremental parser (using --table --inspection)
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
 *)

%{
  open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR
%token <string> TERM
%token <string> TUPLE
%token <string> VARTUPLE
%token ADD SUB MUL DIV MOD SQRT TOINT TOFLOAT
%token AND OR XOR IMPLIES EQUIV NOT
%token EQUAL NOTEQUAL LE LT GE GT
%token IN WHEN
%token UNION INTER DIFF SUBSET RANGE
%token EMPTY CARD
%token LBRACK RBRACK
%token LPAREN RPAREN
%token COMMA COLON AFFECT
%token IF THEN ELSE END
%token EXACT ATLEAST ATMOST
%token TOP BOTTOM
%token BIGAND BIGOR
%token DATA
%token LET
%token EOF

%start <Syntax.ast> touist_code



(* The following lines define in which order the tokens should
 * be reduced, e.g. it tells the parser to reduce * before +.
 *
 * Note that the precedence rules apply from bottom to top:
 * the top element will be the less prioritized
 *
 * %left: e.g: a PLUS b TIMES -> a PLUS b
 *   The precedence rule applies from left to right,
 *
 * %right:
 *   The precedence rule applies from right to left
 *
 * %noassoc, e.g. NOT(a)
 *   The precedence rule has no direction; this often
 *   applies for unary oparators *)

%right EQUIV IMPLIES (* Lesser priority on precedence *)
%left OR
%left AND
%left XOR
%left NOT
%left IN
%left LE GE LT GT EQUAL NOTEQUAL
%left SUB
%left ADD
%left MUL DIV
%left MOD
%nonassoc high_precedence (* Highest priority on precedence *)

(* This wierd [high_precedence] is not a TERMINAL, not a
 * production rule... It is an arbitrary name that allows
 * to give precedence indications on production rules.
 * Ex:
 *     formula: SUB formula %prec high_precedence
 * will give this production rule a predecence given by
 * where the
 *     %nonassoc high_precedence
 * is written. Here, we want this production rule to be
 * reduced before any other one because it is the "minus" sign,
 * ex:
 *     -3.905
 * and, like
 *     not(a)
 * the minus sign MUST be reduced as fast as possible. *)

%on_error_reduce comma_list(VAR)
%on_error_reduce comma_list(exp)
%on_error_reduce comma_list(TERM)
(* %on_error_reduce is a nice "trick" to display a a more accurate
   context when an error is handled. For example, with this text:

       "begin formula formula a(b,c end formula"

   - b is shifted and then reduced thanks to the lookahead ","
   - c is shifted and then reduced thanks to the lookahead "end"
   - end is now evaluated; the parser is still fullfilling the rule
        separated_nonempty_list(COMMA,term_or_exp)                (1)
        -> term_or_exp . COMMA | term_or_exp . RPAREN
     At this moment, the term_or_exp is the "c"; as END does not match
     RPAREN or COMMA, the rule (1) fails to be reduceable.

   The problem is that the $0 token in parser.messages will be
     $0 = end
     $1 = c
     $2 = ,    etc...
   because we were trying to reduce "b (RPAREN | COMMA)".
   There is no way to display the "a" which was the actuall important
   information because we don't actually know on which $i it is.

   %on_error_reduce will actually tell the parser not to fail immediately
   and let the "caller rule" that was calling (1). Here, (1) was called
   twice recursively. The failing rule will hence be

     TERM LPAREN separated_nonempty_list(COMMA,term_or_exp) . RPAREN (2)

   Hence we are sure that $1 will give b,c and $3 will give "a" !
*)


%% (* Everthing below that mark is expected to be a production rule *)
   (* Note that VAR { $0 } is equivalent to v=VAR { v } *)

comma_list(T): 
  | x=T { x::[] }
  | x=T COMMA l=comma_list(T) { x::l }

(* [touist_code] is the entry point of the parser *)
touist_code:
  | c=formula* DATA a=affect* EOF { Touist_code (c, Some a) }
  | c=formula* EOF { Touist_code (c, None) }

(* Used in tuple expression; see tuple-variable and tuple-term *)
indices:
  | e=exp { e }
  | t=term { t }

(* a tuple-term is of the form abc(1,d,3): the indices can be *)
term:
  | t=TERM { Term (t,None) } (* simple-term *)
  | t=TUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple-term *)
    { Term (t, Some l) }

set:
  | LBRACK RBRACK { Set_decl [] }
  | LBRACK l=comma_list(exp) RBRACK { Set_decl l }
  | LBRACK l=comma_list(term) RBRACK { Set_decl l }

(* a local var is a variable used in 'let', 'bigand', 'bigor'... *)
local_var:
  | v=VAR { Var (v,None) }
  
(* a global variable is a variable used in the 'data' block
  for defining sets and constants; it can be of the form of a 
  tuple-variable, i.e. with prefix+indices: '$i(1,a,d)'.
  The indices can be either expression or term *)
global_var:
  | v=local_var { v } (* simple-variable *)
  | v=VARTUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple-variable *)
    { Var (v,Some l) }

affect:
  | v=global_var AFFECT e=exp { Affect (v,e) }

exp:
  | LPAREN exp RPAREN { $2 }
  | INT   { Int   $1 }
  | FLOAT { Float $1 }
  | BOOL  { Bool  $1 }
  | v=global_var { v }
  | s=set { s }
  | SUB exp { Neg $2 } %prec high_precedence
  | exp ADD exp { Add ($1, $3) }
  | exp SUB exp { Sub ($1, $3) }
  | exp MUL exp { Mul ($1, $3) }
  | exp DIV exp { Div ($1, $3) }
  | exp MOD exp { Mod ($1, $3) }
  | SQRT    (*LPAREN*) x=exp RPAREN { Sqrt     x }
  | TOINT   (*LPAREN*) x=exp RPAREN { To_int   x }
  | TOFLOAT (*LPAREN*) x=exp RPAREN { To_float x }
  | exp AND     exp { And     ($1, $3) }
  | exp OR      exp { Or      ($1, $3) }
  | exp XOR     exp { Xor     ($1, $3) }
  | exp IMPLIES exp { Implies ($1, $3) }
  | exp EQUIV   exp { Equiv   ($1, $3) }
  | NOT exp { Not $2 }
  | exp EQUAL    exp { Equal            ($1, $3) }
  | exp NOTEQUAL exp { Not_equal        ($1, $3) }
  | exp LT       exp { Lesser_than      ($1, $3) }
  | exp LE       exp { Lesser_or_equal  ($1, $3) }
  | exp GT       exp { Greater_than     ($1, $3) }
  | exp GE       exp { Greater_or_equal ($1, $3) }
  | exp IN exp { In ($1, $3) }
  | UNION (*LPAREN*) x=exp COMMA y=exp RPAREN { Union (x, y) }
  | INTER (*LPAREN*) x=exp COMMA y=exp RPAREN { Inter (x, y) }
  | DIFF (*LPAREN*) x=exp COMMA y=exp RPAREN { Diff  (x, y) }
  | CARD  (*LPAREN*) x=exp RPAREN { Card  x }
  | EMPTY (*LPAREN*) x=exp RPAREN { Empty x }
  | SUBSET (*LPAREN*) x=exp COMMA y=exp RPAREN { Subset (x, y) }
  | LBRACK exp RANGE exp RBRACK { Range ($2, $4) }
  | IF exp THEN exp ELSE exp END { If ($2, $4, $6) }

(* Redundancy of formula and exp
   ============================
   Because of the need of being able to express SMT formulas, the formula and exp
   types are (seemingly) redundant. At first sight, we might think that merging
   formula and exp into a single type would simplify the grammar...
   But formula and exp express two completely different things:
   - an exp will be "computed"; at the end of the touistc translation, its
     result will be reduced to a single float, integer, bool or set.
     An expression of the formula `2+3+$i/5` will give a float.
   - a formula won't be computed, in the sense that the formula
         (x+2 > 0) and not (y-3 != 0)
     will stay the same after touistc translation.
 *)

formula:
  | LPAREN formula RPAREN { $2 }
  | INT   { Int   $1 }
  | FLOAT { Float $1 }

  (* SUB formula makes it really "hard" to solve. Just one example;
     On the first line, the actual list of tokens. On the two following
     lines, two reductions conflicting:
               "formula1 SUB formula2 XOR formula3 ..."
      formula -> formula1 SUB formula2        => ((formula1 SUB formula2) XOR formula3)
      formula ->         SUB formula2        => (formula 1)((SUB formula2) XOR formula3)
   *)
  | SUB formula { Neg $2 } %prec high_precedence
  | formula ADD      formula { Add              ($1, $3) }
  | formula SUB      formula { Sub              ($1, $3) }
  | formula MUL      formula { Mul              ($1, $3) }
  | formula DIV      formula { Div              ($1, $3) }
  | formula EQUAL    formula { Equal            ($1, $3) }
  | formula NOTEQUAL formula { Not_equal        ($1, $3) }
  | formula LT       formula { Lesser_than      ($1, $3) }
  | formula LE       formula { Lesser_or_equal  ($1, $3) }
  | formula GT       formula { Greater_than     ($1, $3) }
  | formula GE       formula { Greater_or_equal ($1, $3) }
  | v=global_var { v }
  | TOP    { Top    }
  | BOTTOM { Bottom }
  | t=term { t }
  | NOT formula { Not $2 }
  | formula AND     formula { And     ($1, $3) }
  | formula OR      formula { Or      ($1, $3) }
  | formula XOR     formula { Xor     ($1, $3) }
  | formula IMPLIES formula { Implies ($1, $3) }
  | formula EQUIV   formula { Equiv   ($1, $3) }
  | EXACT (*LPAREN*) x=exp COMMA y=exp RPAREN { Exact   (x, y) }
  | ATLEAST (*LPAREN*) x=exp COMMA y=exp RPAREN { Atleast (x, y) }
  | ATMOST (*LPAREN*) x=exp COMMA y=exp RPAREN { Atmost  (x, y) }
  | BIGAND comma_list(local_var) IN comma_list(exp) COLON formula END
  { Bigand ($2, $4, None, $6) }
  | BIGAND comma_list(local_var) IN comma_list(exp) WHEN exp COLON formula END
  { Bigand ($2, $4, Some $6, $8) }
  | BIGOR comma_list(local_var) IN comma_list(exp) COLON formula END
  { Bigor ($2, $4, None, $6) }
  | BIGOR comma_list(local_var) IN comma_list(exp) WHEN exp COLON formula END
  { Bigor ($2, $4, Some $6, $8) }
  | IF exp THEN formula ELSE formula END { If ($2, $4, $6) }
  | LET v=local_var AFFECT e=exp COLON c=formula { Let (v,e,c) }
  | LET v=local_var AFFECT e=formula COLON c=formula { Let (v,e,c) }
