(** Defition of the menhir incremental parser (using --table --inspection) *)

(* Project TouIST, 2015. Easily formalize and solve real_world sized problems
 * using propal logic and linear theory of reals with a nice language and GUI.
 *
 * https://github.com/touist/touist
 *
 * Copyright Institut de Recherche en Informatique de Toulouse, France
 * This program and the accompanying materials are made available
 * under the terms of the GNU Lesser General Public License (LGPL)
 * version 2.1 which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl-2.1.html *)

%{
  open Types.Ast
  open Err
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
%token RANGE POWERSET
%token UNION_PR INTER_PR DIFF_PR SUBSET_PR (* PR = prefixed version *)
%token UNION INTER DIFF SUBSET (* infix versions *)
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
%token FORALL EXISTS
%token FOR NEWLINE
%token QUOTE

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
%left NEWLINE
%left newlineBefore
%nonassoc affect_before_exprsmt
%nonassoc low_precedence (* Lesser priority on precedence *)
%left INTER
%left UNION DIFF
%left SUBSET
%right EQUIV IMPLIES
%left OR
%left AND
%left XOR
(*%left LE GE LT GT EQUAL NOTEQUAL*)
%left NOT
%nonassoc EQUAL NOTEQUAL LT LE GT GE IN
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

%on_error_reduce comma_list(expr)

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
%start <Types.Ast.t> touist_simple, touist_smt, touist_qbf

%% (* Everthing below that mark is expected to be a production rule *)
   (* Note that VAR { $0 } is equivalent to v=VAR { v } *)

comma_list(T):
  | x=T { x::[] }
  | x=T COMMA l=comma_list(T) { x::l }

(* A touistl code is a blank-separated list of either formulas or 
   global variable affectations. Global affectations can only occur
   in this 'top' list ('top' because it is at the top of the ast tree). *)
affect_or(T):
  | a=global_affect {a}  %prec affect_before_exprsmt
  | f=T option(DATA) {f} (* DATA is now useless but stays for compatibilty *)

(* [touist_simple] is the entry point of the parser in sat mode *)
touist_simple:
  | f=affect_or(formula_simple)+ EOF { Layout (Loc ($startpos,$endpos), Touist_code (f)) }


(* [touist_smt] is the entry point of the parser in smt mode *)
touist_smt:
  | f=affect_or(formula_smt)+ EOF { Layout (Loc ($startpos,$endpos), Touist_code (f)) }

touist_qbf: f=affect_or(formula_qbf)+ EOF { Layout (Loc ($startpos,$endpos), Touist_code (f)) }

(* Used in tuple expression; see tuple_variable and tuple_term *)
%inline indices: i=expr { i }

(* a tuple_term is of the form abc(1,d,3): the indices can be *)
prop:
  | t=TERM { Layout (Loc ($startpos,$endpos), Prop t)} (* simple_term *)
  | t=TUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple_term *)
    { Layout (Loc ($startpos,$endpos), UnexpProp (t, Some l)) }

(* For now, we don't check the type of the variables during the parsing.
   This means that all variables are untyped during parsing.
   The start and end positions of the current rule are $startpos and $endpos.
   These two placeholders can only be used in a semantic action, not in the
   %{ %} header. *)
var:
  | v=VAR { Layout (Loc ($startpos,$endpos), Var (v,None)) }
  | v=VARTUPLE (*LPAREN*) l=comma_list(indices) RPAREN (* tuple_variable *)
    { Layout (Loc ($startpos,$endpos), Var (v,Some l)) }

(* a global variable is a variable used in the 'data' block
  for defining sets and constants; it can be of the form of a
  tuple_variable, i.e. with prefix+indices: '$i(1,a,d)'.
  The indices can be either expression or term *)
%inline global_affect: v=var AFFECT e=expr { Layout (Loc ($startpos,$endpos), Affect (v,e)) }

%inline if_statement(T): IF cond=expr THEN v1=T ELSE v2=T END { Layout (Loc ($startpos,$endpos), If (cond,v1,v2)) }

%inline in_parenthesis(T): LPAREN x=T RPAREN { Layout (Loc ($startpos,$endpos), Layout (Paren, x)) }

%inline arith_binop:
  | ADD { Types.Add }
  | MUL { Types.Mul }
  | DIV { Types.Div }
%inline num_operations_standard(T):
  | x=T  b=arith_binop   y=T  { Layout (Loc ($startpos,$endpos), ArithBinop (x,b,y)) }
  | x=T    SUB     y=T  { Layout (Loc ($startpos,$endpos), ArithBinop (x,Sub,y))} %prec sub_prec
  |        SUB     x=T  { Layout (Loc ($startpos,$endpos), ArithUnop (Neg, x))  } %prec neg_prec

%inline num_operations_others(T):
  | x=T    MOD     y=T  { Layout (Loc ($startpos,$endpos), ArithBinop (x,Mod,y)) }
  | ABS (*LPAREN*) x=T RPAREN { Layout (Loc ($startpos,$endpos), ArithUnop (Abs, x)) }

%inline int: x=INT { Layout (Loc ($startpos,$endpos), Int x) }
%inline float: x=FLOAT { Layout (Loc ($startpos,$endpos), Float x) }
%inline bool: x=BOOL { Layout (Loc ($startpos,$endpos), Bool x) }

expr:
  | b=var {b}
  | x=int {x}
  | TOINT (*LPAREN*) x=expr RPAREN { Layout (Loc ($startpos,$endpos), ArithUnop (To_int, x)) }
  | CARD  (*LPAREN*) s=expr RPAREN { Layout (Loc ($startpos,$endpos), Card s) }
  | x=float {x}
  | x=in_parenthesis(expr)
  | x=num_operations_standard(expr)
  | x=num_operations_others(expr)
  | x=if_statement(expr) { x }
  | SQRT    (*LPAREN*) x=expr RPAREN { Layout (Loc ($startpos,$endpos), ArithUnop (Sqrt, x)) }
  | TOFLOAT (*LPAREN*) x=expr RPAREN { Layout (Loc ($startpos,$endpos), ArithUnop (To_float, x)) }
  | b=bool {b}
  | b=connectors(expr)
  | b=binrel(expr) {b}
  | EMPTY  (*LPAREN*) s=expr RPAREN { Layout (Loc ($startpos,$endpos), IsEmpty s) }
  | s1=expr SUBSET s2=expr { Layout (Loc ($startpos,$endpos), Subset (s1,s2)) }
  | SUBSET_PR (*LPAREN*) s1=expr COMMA s2=expr RPAREN {
      let loc = ($startpos,$endpos) in
      warn (Warning,Parse,"'subset(A,B)' is deprecated, please use \
        'A subset B' instead.\n",Some loc);
      Layout (Loc loc, Subset (s1,s2)) }
  | p=prop {p}
  | x=expr   IN s=expr { Layout (Loc ($startpos,$endpos), In (x,s)) }
  | x=set_decl_range(expr)
  | x=set_empty
  | x=set_decl_explicit(expr)
  | x=set_builder(expr)
  | x=set_operation(expr) {x}
  | QUOTE f=formula_simple QUOTE { Layout (Loc ($startpos,$endpos), Formula f) }

%inline arith_binrel:
  | EQUAL { Types.Equal }
  | NOTEQUAL { Types.Not_equal }
  | LT { Types.Lesser_than }
  | LE { Types.Lesser_or_equal }
  | GT { Types.Greater_than }
  | GE { Types.Greater_or_equal }
%inline binrel(T):
  | x=T   b=arith_binrel      y=T   { Layout (Loc ($startpos,$endpos), ArithBinrel (x,b,y)) }

%inline logic_binop:
  | AND       { Types.And }
  | OR        { Types.Or }
  | XOR       { Types.Xor }
  | IMPLIES   { Types.Implies }
  | EQUIV     { Types.Equiv }
%inline connectors(T):
  | NOT           x=T   { Layout (Loc ($startpos,$endpos), Not x) }
  | x=T  b=logic_binop  y=T   { Layout (Loc ($startpos,$endpos), LogicBinop (x,b,y)) }

%inline set_decl_range(T): LBRACK s1=T RANGE s2=T RBRACK { Layout (Loc ($startpos,$endpos), Range (s1,s2)) }
%inline set_decl_explicit(T): LBRACK l=comma_list(T) RBRACK { Layout (Loc ($startpos,$endpos), Set_decl l) }
%inline set_empty: LBRACK RBRACK { Layout (Loc ($startpos,$endpos), Set_decl []) }
%inline set_builder(T): LBRACK f=T FOR vars=comma_list(var) IN sets=comma_list(expr) c=when_cond? RBRACK
  {try List.fold_left2 (fun _ _ _ -> ()) () vars sets;
      Layout (Loc ($startpos,$endpos), SetBuilder (f,vars,sets,c))
   with Invalid_argument _ ->
      fatal (Error,Parse, ("list comprehension must have the same number of variables and sets.\n"),
        Some ($startpos,$endpos))}

%inline set_binop:
  | UNION      { Types.Union }
  | INTER      { Types.Inter }
  | DIFF       { Types.Diff }
%inline set_operation(T):
  | s1=T b=set_binop s2=T { Layout (Loc ($startpos,$endpos), SetBinop (s1,b,s2)) }
  | UNION_PR (*LPAREN*) s1=T COMMA s2=T RPAREN {
      let loc = ($startpos,$endpos) in
      warn (Warning,Parse,"'union(A,B)' is deprecated, please use \
        'A union B' instead.\n",Some loc);
      Layout (Loc loc, SetBinop (s1,Union,s2)) }
  | INTER_PR (*LPAREN*) s1=T COMMA s2=T RPAREN {
      let loc = ($startpos,$endpos) in
      warn (Warning,Parse,"'inter(A,B)' is deprecated, please use \
        'A inter B' instead.\n",Some loc);
      Layout (Loc loc, SetBinop (s1,Inter,s2))}
  | DIFF_PR  (*LPAREN*) s1=T COMMA s2=T RPAREN {
      let loc = ($startpos,$endpos) in
      warn (Warning,Parse,"'diff(A,B)' is deprecated, please use \
        'A diff B' instead.\n",Some loc);
      Layout (Loc loc, SetBinop (s1,Diff,s2)) }
  | POWERSET (*LPAREN*) s=T RPAREN { Layout (Loc ($startpos,$endpos), Powerset s) }

%inline formula(F):
  | f=in_parenthesis(F)
  | f=if_statement(F)
  | f=connectors(F)
  | f=generalized_connectors(F) (* are only on formulas! No need for parametrization *)
  | f=let_affect(expr,F) {f}
  | NEWLINE f=F { Layout (NewlineBefore, f) } %prec newlineBefore
  | f=F NEWLINE { Layout (NewlineAfter, f) }

let_affect(T,F): LET vars=comma_list(var) AFFECT contents=comma_list(T) COLON form=F
    {try List.fold_right2 (fun var content acc ->
      Layout (Loc ($startpos,$endpos), Let (var,content,acc))) vars contents form
    with Invalid_argument _ ->
      fatal (Error,Parse,
        ("'let' statement does not have the same number of variables and values.\n"),
        Some ($startpos,$endpos))
    } %prec low_precedence

formula_simple:
  | f=var {f}
  | f=formula(formula_simple)
  | f=prop {f}
  | TOP { Top }
  | BOTTOM { Bottom }

formula_qbf:
  | f=var {f}
  | f=formula(formula_qbf)
  | f=prop {f}
  | TOP { Top }
  | BOTTOM { Bottom }
  | f=quant(exists,formula_qbf)
  | f=quant(forall,formula_qbf) {f}

formula_smt:
  | f=formula(formula_smt)
  | f=expr_smt { f }

(* WARNING: SMT can handle things like '(x + 2) > 3.1', meaning that the
   types are mixed. *)
expr_smt:
  | f=prop {f}
  | TOP { Top }
  | BOTTOM { Bottom }
  | x=var {x}
  | x=int
  | x=float
  | x=num_operations_standard(expr_smt)
  | x=binrel(expr_smt) {x}
  | x=in_parenthesis(expr_smt) {x}

%inline generalized_connectors(F):
  | BIGAND v=comma_list(var) IN s=comma_list(expr) c=when_cond? COLON f=F END 
    { Layout (Loc ($startpos,$endpos), Bigand (v,s,c,f)) }
  | BIGOR  v=comma_list(var) IN s=comma_list(expr) c=when_cond? COLON f=F END 
    { Layout (Loc ($startpos,$endpos), Bigor (v,s,c,f)) }
  | EXACT (*LPAREN*)   x=expr COMMA s=expr RPAREN { Layout (Loc ($startpos,$endpos), Cardinality (Exact, x,s)) }
  | ATLEAST (*LPAREN*) x=expr COMMA s=expr RPAREN { Layout (Loc ($startpos,$endpos), Cardinality (Atleast, x,s)) }
  | ATMOST (*LPAREN*)  x=expr COMMA s=expr RPAREN { Layout (Loc ($startpos,$endpos), Cardinality (Atmost, x,s)) }

%inline when_cond: WHEN x=expr { x }

%inline prop_or_var: p=prop | p=var {p}

%inline exists:
  | EXISTS { Types.Exists }
%inline forall:
  | FORALL { Types.Forall }
%inline quant(Q,F): q=Q v=comma_list(prop_or_var) for_opt=for_statement? COLON form=F
  { let res = form |> List.fold_right (fun v acc -> Layout (Loc ($startpos,$endpos), Quantifier (q,v,acc))) v in
    match for_opt with
    | None -> res
    | Some (var,content) -> Layout (Loc ($startpos,$endpos), For (var,content,res))
  }

%inline for_statement: FOR v=var IN content=expr { (v,content) }
