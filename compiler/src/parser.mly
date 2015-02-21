%{
  (* Ocaml code here *)
%}

%token TRUE FALSE AND OR XOR IMPLY NOT
%token TRUECLAUSE FALSECLAUSE
%token <string> TERM
%token EQUAL NOTEQUAL LT GT LE GE
%token EMPTY IN SUBSET
%token EXACT ATLEAST ATMOST

%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token ADD SUB MUL DIV MOD SQRT
%token TOINT TOFLOAT

%token UNION INTER DIFF UPPERSET
%token RANGE DOT CARD

%token AFFECT IF THEN ELSE BIGAND BIGOR WITH

%token BEGIN SETS FORMULA END EOF
%token LPAREN RPAREN LBRACK RBRACK COMMA

%start <Ast.prog> prog

%%

prog:
  | BEGIN SETS s = list(command) END SETS BEGIN FORMULA c = list(command) END FORMULA EOF { Ast.Begin (Some s, c) }
  | BEGIN FORMULA c = list(command) END FORMULA EOF { Ast.Begin (None, c) }

command:
  | v = VAR AFFECT s = sexp { Ast.Affect (v, Ast.Set s) }
  | v = VAR AFFECT a = aexp { Ast.Affect (v, a)         }
  | LPAREN IF b = bexp THEN c1 = command ELSE c2 = command RPAREN { Ast.If (b, c1, c2) }
  | LPAREN BIGAND b = bigbody RPAREN { Ast.Bigand b }
  | LPAREN BIGOR  b = bigbody RPAREN { Ast.Bigor  b }
  | c = bexp { Ast.Clause c }

bigbody:
  | v = VAR IN s = sexp c = command { (v, s, None, c) }
  | v = VAR IN s = sexp WITH b = bexp c = command { (v, s, Some b, c) }

bexp:
  | TRUE        { Ast.True        }
  | FALSE       { Ast.False       }
  | TRUECLAUSE  { Ast.Trueclause  }
  | FALSECLAUSE { Ast.Falseclause }
  | t = TERM    { Ast.Term t      }
  | LPAREN e1 = bexp AND   e2 = bexp RPAREN { Ast.And   (e1, e2) }
  | LPAREN e1 = bexp OR    e2 = bexp RPAREN { Ast.Or    (e1, e2) }
  | LPAREN e1 = bexp XOR   e2 = bexp RPAREN { Ast.Xor   (e1, e2) }
  | LPAREN e1 = bexp IMPLY e2 = bexp RPAREN { Ast.Imply (e1, e2) }
  | NOT    e  = bexp                        { Ast.Not   e        }
  | LPAREN a1 = aexp EQUAL    a2 = aexp RPAREN { Ast.Equal            (a1, a2) }
  | LPAREN a1 = aexp NOTEQUAL a2 = aexp RPAREN { Ast.Not_equal        (a1, a2) }
  | LPAREN a1 = aexp LT       a2 = aexp RPAREN { Ast.Lesser_than      (a1, a2) }
  | LPAREN a1 = aexp LE       a2 = aexp RPAREN { Ast.Lesser_or_equal  (a1, a2) }
  | LPAREN a1 = aexp GT       a2 = aexp RPAREN { Ast.Greater_than     (a1, a2) }
  | LPAREN a1 = aexp GE       a2 = aexp RPAREN { Ast.Greater_or_equal (a1, a2) }
  | EMPTY  s  = sexp                                  { Ast.Empty   s        }
  | LPAREN v  = set_body IN      s  = sexp     RPAREN { Ast.In      (v, s)   }
  | LPAREN s1 = sexp     SUBSET  s2 = sexp     RPAREN { Ast.Subset  (s1, s2) }
  | LPAREN i  = aexp     EXACT   b  = bigbody  RPAREN { Ast.Exact   (i, b)   }
  | LPAREN i  = aexp     ATLEAST b  = bigbody  RPAREN { Ast.Atleast (i, b)   }
  | LPAREN i  = aexp     ATMOST  b  = bigbody  RPAREN { Ast.Atmost  (i, b)   }

aexp:
  | v = VAR   { Ast.Var   v }
  | i = INT   { Ast.Int   i }
  | f = FLOAT { Ast.Float f }
  | LPAREN  a1 = aexp ADD a2 = aexp RPAREN { Ast.Add (a1, a2) }
  | LPAREN  a1 = aexp SUB a2 = aexp RPAREN { Ast.Sub (a1, a2) }
  | LPAREN  a1 = aexp MUL a2 = aexp RPAREN { Ast.Mul (a1, a2) }
  | LPAREN  a1 = aexp DIV a2 = aexp RPAREN { Ast.Div (a1, a2) }
  | LPAREN  a1 = aexp MOD a2 = aexp RPAREN { Ast.Mod (a1, a2) }
  | SQRT    LPAREN a = aexp RPAREN { Ast.Sqrt     a }
  | TOINT   LPAREN a = aexp RPAREN { Ast.To_int   a }
  | TOFLOAT LPAREN a = aexp RPAREN { Ast.To_float a }

sexp:
  | LBRACK s = separated_list(COMMA, set_body) RBRACK { Ast.Set_body s }
  | LPAREN s1 = sexp UNION s2 = sexp RPAREN { Ast.Union (s1, s2) }
  | LPAREN s1 = sexp INTER s2 = sexp RPAREN { Ast.Inter (s1, s2) }
  | LPAREN s1 = sexp DIFF  s2 = sexp LPAREN { Ast.Diff  (s1, s2) }
  | UPPERSET LPAREN v = VAR COMMA s = sexp RPAREN { Ast.Upperset (Ast.Var v, s) }
  | LPAREN a1 = aexp RANGE a2 = aexp RPAREN { Ast.Range (a1, a2) }
  | s = sexp DOT LPAREN i = aexp RPAREN { Ast.Dot (s, i) }
  | CARD LPAREN s = sexp RPAREN { Ast.Card s }

set_body:
  | a = aexp { Ast.Num  a }
  | b = bexp { Ast.Prop b }
