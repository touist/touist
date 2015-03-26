%{
  open Ast
  
  (* Avoid the use of the Set.of_list function which requires version 4.02.0 *)
  let intset_of_list =
    List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty
  
  let floatset_of_list =
    List.fold_left (fun acc x -> FloatSet.add x acc) FloatSet.empty

  let stringset_of_list =
    List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty
%}

%token BAND BOR
%token AND OR XOR IMPLIES EQUIV NOT
%token TOP BOTTOM
%token <string> TERM
%token <bool> BOOL
%token EQUAL NOTEQUAL LT GT LE GE
%token EMPTY IN SUBSET

%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token ADD SUB MUL DIV MOD SQRT
%token TOINT TOFLOAT

%token UNION INTER DIFF
%token RANGE DOT CARD

%token AFFECT IF THEN ELSE BIGAND BIGOR WHEN

%token BEGIN SETS FORMULA END EOF
%token LPAREN RPAREN LBRACK RBRACK COMMA COLON

%start <Ast.prog> prog

%left BOR
%left BAND
%right EQUIV IMPLIES
%left OR
%left AND
%left XOR
%left NOT
%left ADD SUB
%nonassoc neg
%left MUL DIV
%left MOD
%left LT GT LE GE
%left EQUAL NOTEQUAL

%%

prog:
  | BEGIN SETS s = nonempty_list(affect) END SETS BEGIN FORMULA e = nonempty_list(exp) END FORMULA EOF { Begin (Some s, e) }
  | BEGIN FORMULA e = nonempty_list(exp) END FORMULA EOF { Begin (None, e) }

affect:
  | v = VAR AFFECT e = exp { Affect (v, e) }

exp:
  | v = VAR        { Var       v }
  | s = set_exp    { SetExp    s }
  | i = int_exp    { IntExp    i }
  | f = float_exp  { FloatExp  f }
  | b = bool_exp   { BoolExp   b }
  | c = clause_exp { ClauseExp c }
  | IF b = bool_exp THEN e1 = exp ELSE e2 = exp END { If (b, e1, e2) }
  | s = set_exp DOT LPAREN e = exp RPAREN { Dot (s, e) }

int_exp:
  | LPAREN e = int_exp RPAREN { e }
  | v  = VAR { IVar v }
  | i  = INT { Int  i }
  | i1 = int_exp ADD i2 = int_exp { Add (i1, i2) }
  | i1 = int_exp SUB i2 = int_exp { Sub (i1, i2) }
  | i1 = int_exp MUL i2 = int_exp { Mul (i1, i2) }
  | i1 = int_exp DIV i2 = int_exp { Div (i1, i2) }
  | i1 = int_exp MOD i2 = int_exp { Mod (i1, i2) }
  | SUB i = int_exp { Neg i } %prec neg
  | TOINT LPAREN f = float_exp RPAREN { To_int f }
  | CARD  LPAREN s = set_exp   RPAREN { Card   s }

float_exp:
  | LPAREN e = float_exp RPAREN { e }
  | v  = VAR   { FVar  v }
  | f  = FLOAT { Float f }
  | f1 = float_exp ADD  f2 = float_exp { FAdd  (f1, f2) }
  | f1 = float_exp SUB  f2 = float_exp { FSub  (f1, f2) }
  | f1 = float_exp MUL  f2 = float_exp { FMul  (f1, f2) }
  | f1 = float_exp DIV  f2 = float_exp { FDiv  (f1, f2) }
  | SUB f = float_exp { FNeg f } %prec neg
  | SQRT    LPAREN f = float_exp RPAREN { Sqrt     f }
  | TOFLOAT LPAREN i = int_exp   RPAREN { To_float i }

bool_exp:
  | LPAREN e = bool_exp RPAREN { e }
  | v = VAR  { BVar v }
  | b = BOOL { Bool b }
  | NOT b = bool_exp { BNot b }
  | b1 = bool_exp BAND      b2 = bool_exp  { BAnd              (b1, b2) }
  | b1 = bool_exp BOR       b2 = bool_exp  { BOr               (b1, b2) }
  | b1 = bool_exp XOR       b2 = bool_exp  { BXor              (b1, b2) }
  | b1 = bool_exp IMPLIES   b2 = bool_exp  { BImplies          (b1, b2) }
  | b1 = bool_exp EQUIV     b2 = bool_exp  { BEquiv            (b1, b2) }
  | i1 = int_exp EQUAL      i2 = int_exp   { Equal             (i1, i2) }
  | i1 = int_exp NOTEQUAL   i2 = int_exp   { Not_equal         (i1, i2) }
  | i1 = int_exp LT         i2 = int_exp   { Lesser_than       (i1, i2) }
  | i1 = int_exp LE         i2 = int_exp   { Lesser_or_equal   (i1, i2) }
  | i1 = int_exp GT         i2 = int_exp   { Greater_than      (i1, i2) }
  | i1 = int_exp GE         i2 = int_exp   { Greater_or_equal  (i1, i2) }
  | f1 = float_exp EQUAL    f2 = float_exp { FEqual            (f1, f2) }
  | f1 = float_exp NOTEQUAL f2 = float_exp { FNot_equal        (f1, f2) }
  | f1 = float_exp LT       f2 = float_exp { FLesser_than      (f1, f2) }
  | f1 = float_exp LE       f2 = float_exp { FLesser_or_equal  (f1, f2) }
  | f1 = float_exp GT       f2 = float_exp { FGreater_than     (f1, f2) }
  | f1 = float_exp GE       f2 = float_exp { FGreater_or_equal (f1, f2) }
  | s1 = set_exp EQUAL  s2 = set_exp { SEqual (s1, s2) }
  | i  = int_exp   IN s = set_exp { In (IntExp i, s)   }
  | f  = float_exp IN s = set_exp { In (FloatExp f, s) }
  | t  = TERM      IN s = set_exp { In (ClauseExp (Term (t, None)), s) }
  | SUBSET LPAREN s1 = set_exp COMMA s2 = set_exp RPAREN { Subset (s1, s2) }
  | EMPTY LPAREN s = set_exp RPAREN { Empty s }

set_exp:
  | LPAREN e = set_exp RPAREN { e }
  | v  = VAR      { SVar v }
  | s  = set_decl { Set  s }
  | UNION LPAREN s1 = set_exp COMMA s2 = set_exp RPAREN { Union (s1, s2) }
  | INTER LPAREN s1 = set_exp COMMA s2 = set_exp RPAREN { Inter (s1, s2) }
  | DIFF  LPAREN s1 = set_exp COMMA s2 = set_exp RPAREN { Diff  (s1, s2) }
  | LBRACK i1 = int_exp RANGE i2 = int_exp RBRACK { Range (i1, i2) }

set_decl:
  | LBRACK RBRACK { GenSet.Empty }
  | LBRACK i = separated_nonempty_list(COMMA, int_exp)   RBRACK { GenSet.IS (intset_of_list (List.map Eval.eval_int i)) }
  | LBRACK f = separated_nonempty_list(COMMA, float_exp) RBRACK { GenSet.FS (floatset_of_list (List.map Eval.eval_float f)) }
  | LBRACK s = separated_nonempty_list(COMMA, TERM)  RBRACK { GenSet.SS (stringset_of_list s) }

clause_exp:
  | LPAREN e = clause_exp RPAREN { e }
  | TOP    { Top    }
  | BOTTOM { Bottom }
  | t = TERM { Term (t, None) }
  | t = TERM LPAREN opt = term_option RPAREN { Term (t, Some opt) }
  | NOT c = clause_exp { Not c }
  | c1 = clause_exp AND     c2 = clause_exp { And     (c1, c2) }
  | c1 = clause_exp OR      c2 = clause_exp { Or      (c1, c2) }
  | c1 = clause_exp XOR     c2 = clause_exp { Xor     (c1, c2) }
  | c1 = clause_exp IMPLIES c2 = clause_exp { Implies (c1, c2) }
  | c1 = clause_exp EQUIV   c2 = clause_exp { Equiv   (c1, c2) }
  | BIGAND v = separated_nonempty_list(COMMA, VAR) IN s = separated_nonempty_list(COMMA, set_exp) COLON e = exp END { Bigand (v, s, None, e) }
  | BIGAND v = separated_nonempty_list(COMMA, VAR) IN s = separated_nonempty_list(COMMA, set_exp) WHEN b = bool_exp COLON e = exp END { Bigand (v, s, Some b, e) }
  | BIGOR  v = separated_nonempty_list(COMMA, VAR) IN s = separated_nonempty_list(COMMA, set_exp) COLON e = exp END { Bigor (v, s, None, e) }
  | BIGOR  v = separated_nonempty_list(COMMA, VAR) IN s = separated_nonempty_list(COMMA, set_exp) WHEN b = bool_exp COLON e = exp END { Bigor  (v, s, Some b, e) }

term_option:
  | e = exp  { Exp e }
  | s = TERM { Str s }

