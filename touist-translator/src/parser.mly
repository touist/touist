(*
 * parser.mly: the defition of the menhir incremental parser (using --table --inspection)
 *
 * Project TouIST, 2015. Easily formalize and solve real_world sized problems
 * using propal logic and linear theory of reals with a nice language and GUI.
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
%token ADD SUB MUL DIV MOD SQRT TOINT TOFLOAT ABS
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

%nonassoc low_precedence (* Lesser priority on precedence *)
%right EQUIV IMPLIES
%left OR
%left AND
%left XOR
(*%left LE GE LT GT EQUAL NOTEQUAL*)
%left NOT
(* neg_prec = preced. of 'SUB x' has a lesser preced. than 'x SUB x' *)
(* sub_prec = predecence of 'x SUB x' *)
%left ADD SUB sub_prec neg_prec
%left MUL DIV
%left MOD (* Highest priority on precedence *)

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

%on_error_reduce comma_list(any_set)
%on_error_reduce comma_list(set_int)
%on_error_reduce comma_list(set_float)
%on_error_reduce comma_list(set_prop)

%on_error_reduce comma_list(int)
%on_error_reduce comma_list(float)
%on_error_reduce comma_list(prop)
%on_error_reduce comma_list(any_atom)

%on_error_reduce comma_list(indices)

%on_error_reduce comma_list(var)

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

(* The two entry points of our parser *)
%start <Syntax.ast> touist_simple, touist_smt

%% (* Everthing below that mark is expected to be a production rule *)
   (* Note that VAR { $0 } is equivalent to v=VAR { v } *)

comma_list(T):
  | x=T { x::[] }
  | x=T COMMA l=comma_list(T) { x::l }

(* [touist_simple] is the entry point of the parser in sat mode *)
touist_simple:
  | a=global_affect+ f=formula_simple+ EOF { Touist_code (f,Some a) }
  | f=formula_simple+ DATA a=global_affect+ EOF { Touist_code (f,Some a) }
  | f=formula_simple+ EOF { Touist_code (f,None) }


(* [touist_smt] is the entry point of the parser in smt mode *)
touist_smt:
  | a=global_affect+ f=formula_smt+ EOF { Touist_code (f, Some a) }
  | f=formula_smt+ DATA a=global_affect+ EOF { Touist_code (f,Some a) }
  | f=formula_smt+ EOF { Touist_code (f,None) }

(* Used in tuple expression; see tuple_variable and tuple_term *)
indices: i=int | i=float | i=prop { i }

(* a tuple_term is of the form abc(1,d,3): the indices can be *)
prop:
  | v=var { v }
  | t=TERM { Prop t } (* simple_term *)
  | t=TUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple_term *)
    { UnexpProp (t, Some l) }

(* For now, we don't check the type of the variables during the parsing.
   This means that all variables are untyped during parsing.
   The start and end positions of the current rule are $startpos and $endpos.
   These two placeholders can only be used in a semantic action, not in the
   %{ %} header. *)
var:
  | v=VAR { let loc = ($startpos,$endpos) in Var (v,None,loc) }
  | v=VARTUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple_variable *)
    { let loc = ($startpos,$endpos) in Var (v,Some l,loc) }

(* a global variable is a variable used in the 'data' block
  for defining sets and constants; it can be of the form of a
  tuple_variable, i.e. with prefix+indices: '$i(1,a,d)'.
  The indices can be either expression or term *)
global_affect:
  | v=var AFFECT e=global_affect_types { Affect (v,e) }

(* WARNING: any_set and any_atom should only be used in specific places,e.g.,
       formula_smt: equality(any_atom)
   because SMT can handle things like '(x + 2) > 3.1', meaning that the
   types are mixed. *)
global_affect_types: x=any_set | x=any_atom | x=bool { x }

any_atom: x=prop | x=num { x }

if_statement(T): IF cond=bool THEN v1=T ELSE v2=T END { If (cond,v1,v2) }

in_parenthesis(T): LPAREN x=T RPAREN { x }

num: x=float | x=int {x}

num_operations_standard(T):
  | x=T    ADD     y=T  { Add (x,y) }
  | x=T    SUB     y=T  { Sub (x,y) } %prec sub_prec
  |        SUB     x=T  { Neg x     } %prec neg_prec
  | x=T    MUL     y=T  { Mul (x,y) }
  | x=T    DIV     y=T  { Div (x,y) }

num_operations_others(T):
  | x=T    MOD     y=T  { Mod (x,y) }
  | ABS (*LPAREN*) x=T RPAREN { Abs x     }

int:
  | x=INT { Int x }
  | x=var
  | x=in_parenthesis(int)
  | x=num_operations_standard(int)
  | x=num_operations_others(int)
  | x=if_statement(int)
  | TOINT (*LPAREN*) x=num RPAREN { To_int x }
  | CARD  (*LPAREN*) s=any_set RPAREN { Card s }

float:
  | x=FLOAT { Float x }
  | x=var
  | x=in_parenthesis(float)
  | x=num_operations_standard(float)
  | x=num_operations_others(float)
  | x=if_statement(float) { x }
  | SQRT    (*LPAREN*) x=float RPAREN { Sqrt x }
  | TOFLOAT (*LPAREN*) x=num RPAREN { To_float x }
  
(* bool is a boolean computed before solving;
   typically in conditions like 'when' and 'if' *)
bool:
  | b=BOOL { Bool b }
  | b=in_parenthesis(bool)
  | b=connectors(bool)
  | b=equality(int) | b=equality(float) | b=equality(prop)
  | b=order(int) | b=order(float)
  | b=if_statement(bool)
  | b=subset_statement
  | b=in_operator { b }
  | EMPTY  (*LPAREN*) s=any_set RPAREN { Empty s }
  | b=var { b }

subset_statement:
  | SUBSET (*LPAREN*) s1=set_int   COMMA s2=set_int   RPAREN { Subset (s1,s2) }
  | SUBSET (*LPAREN*) s1=set_float COMMA s2=set_float RPAREN { Subset (s1,s2) }
  | SUBSET (*LPAREN*) s1=set_prop  COMMA s2=set_prop  RPAREN { Subset (s1,s2) }

in_operator:
  | x=int   IN s=set_int
  | x=float IN s=set_float
  | x=prop  IN s=set_prop  { In (x,s) }


equality(T):
  | x=T  EQUAL    y=T   { Equal (x,y) }
  | x=T  NOTEQUAL y=T   { Not_equal (x,y) }

order(T):
  | x=T   LT      y=T   { Lesser_than (x,y) }
  | x=T   LE      y=T   { Lesser_or_equal (x,y) }
  | x=T   GT      y=T   { Greater_than (x,y) }
  | x=T   GE      y=T   { Greater_or_equal (x,y) }

connectors(T):
  | NOT           x=T   { Not x}
  | x=T  AND      y=T   { And (x,y) }
  | x=T  OR       y=T   { Or (x,y) }
  | x=T  XOR      y=T   { Xor (x,y) }
  | x=T  IMPLIES  y=T   { Implies (x,y) }
  | x=T  EQUIV    y=T   { Equiv (x,y) }

set_decl_range(T): LBRACK s1=T RANGE s2=T RBRACK { Range (s1,s2) }
set_decl_explicit(T): LBRACK l=comma_list(T) RBRACK { Set_decl l }
set_empty: LBRACK RBRACK { Set_decl [] }
  
set_operation(T):
  | s=if_statement(T) { s }
  | UNION (*LPAREN*) s1=T COMMA s2=T RPAREN { Union (s1,s2) }
  | INTER (*LPAREN*) s1=T COMMA s2=T RPAREN { Inter (s1,s2) }
  | DIFF  (*LPAREN*) s1=T COMMA s2=T RPAREN { Diff (s1,s2) }

set_float:
  | x=set_decl_range(float)
  | x=set_decl_explicit(float)
  | x=set_operation(set_float)
  | x=var { x }

set_int:
  | x=set_decl_range(int)
  | x=set_decl_explicit(int)
  | x=set_operation(set_int)
  | x=var { x }

set_prop:
  | x=set_decl_explicit(prop)
  | x=set_operation(set_prop)
  | x=var { x }

any_set:
  | s=set_float | s=set_int | s=set_prop | s=set_empty { s }

formula(F):
  | f=in_parenthesis(F)
  | f=if_statement(F)
  | f=connectors(F)
  | f=generalized_connectors(F) (* are only on formulas! No need for parametrization *)
  | f=let_affect(int,F) | f=let_affect(float,F) | f=let_affect(prop,F)
  | f=prop { f }
  | TOP { Top }
  | BOTTOM { Bottom }

let_affect(T,F): LET var=var AFFECT content=T COLON form=F { Let (var,content,form) } %prec low_precedence

formula_simple:
  | f=formula(formula_simple) { f }

formula_smt:
  | f=formula(formula_smt)
  | f=equality(any_atom) (* can be '1 != prop' for example *)
  | f=order(any_atom) { f }

generalized_connectors(F):
  | BIGAND v=comma_list(var) IN s=comma_list(any_set) c=when_cond? COLON f=F END { Bigand (v,s,c,f) }
  | BIGOR  v=comma_list(var) IN s=comma_list(any_set) c=when_cond? COLON f=F END { Bigor  (v,s,c,f) }
  | EXACT (*LPAREN*)   x=int COMMA s=set_prop RPAREN { Exact   (x,s) }
  | ATLEAST (*LPAREN*) x=int COMMA s=set_prop RPAREN { Atleast (x,s) }
  | ATMOST (*LPAREN*)  x=int COMMA s=set_prop RPAREN { Atmost  (x,s) }

when_cond: WHEN x=bool { x }


