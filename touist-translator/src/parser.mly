%{
  open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VAR
%token <string> TERM
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
%token TOP BOTTOM
%token BIGAND BIGOR
%token BEGIN SETS FORMULA
%token PIPE
%token EOF

%start <Syntax.prog> prog

%left IN
%left ADD SUB
%left MUL DIV
%left MOD
%right EQUIV IMPLIES
%left OR
%left AND
%left XOR
%left NOT

%%

prog:
  | BEGIN SETS affect+ END SETS BEGIN FORMULA clause+ END FORMULA EOF
  { Prog (Some $3, $8) }
  | BEGIN FORMULA clause+ END FORMULA EOF
  { Prog (None, $3) }

affect:
  | VAR AFFECT exp { Affect ($1, $3) }

exp:
  | LPAREN exp RPAREN { $2 }
  | INT   { Int   $1 }
  | FLOAT { Float $1 }
  | BOOL  { Bool  $1 }
  | VAR   { Var   $1 }
  | set_decl { Set_decl $1 }
  | exp ADD exp { Add ($1, $3) }
  | exp SUB exp { Sub ($1, $3) }
  | exp MUL exp { Mul ($1, $3) }
  | exp DIV exp { Div ($1, $3) }
  | exp MOD exp { Mod ($1, $3) }
  | SQRT    LPAREN exp RPAREN { Sqrt     $3 }
  | TOINT   LPAREN exp RPAREN { To_int   $3 }
  | TOFLOAT LPAREN exp RPAREN { To_float $3 }
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
  | UNION LPAREN exp COMMA exp RPAREN { Union ($3, $5) }
  | INTER LPAREN exp COMMA exp RPAREN { Inter ($3, $5) }
  | DIFF  LPAREN exp COMMA exp RPAREN { Diff  ($3, $5) }
  | CARD  LPAREN exp RPAREN { Card  $3 }
  | EMPTY LPAREN exp RPAREN { Empty $3 }
  | SUBSET LPAREN exp COMMA exp RPAREN { Subset ($3, $5) }
  | LBRACK exp RANGE exp RBRACK { Range ($2, $4) }
  | IF exp THEN exp ELSE exp END { If ($2, $4, $6) }

clause:
  | LPAREN clause RPAREN { $2 }
  | TOP    { Top    }
  | BOTTOM { Bottom }
  | TERM { Term ($1, None) }
  | TERM PIPE exp PIPE { Term ($1, Some $3) }
  | NOT clause { CNot $2 }
  | clause AND     clause { CAnd     ($1, $3) }
  | clause OR      clause { COr      ($1, $3) }
  | clause XOR     clause { CXor     ($1, $3) }
  | clause IMPLIES clause { CImplies ($1, $3) }
  | clause EQUIV   clause { CEquiv   ($1, $3) }
  | BIGAND separated_nonempty_list(COMMA,VAR) IN separated_nonempty_list(COMMA,exp) COLON clause END
  { Bigand ($2, $4, None, $6) }
  | BIGAND separated_nonempty_list(COMMA,VAR) IN separated_nonempty_list(COMMA,exp) WHEN exp COLON clause END
  { Bigand ($2, $4, Some $6, $8) }
  | BIGOR separated_nonempty_list(COMMA,VAR) IN separated_nonempty_list(COMMA,exp) COLON clause END
  { Bigor ($2, $4, None, $6) }
  | BIGOR separated_nonempty_list(COMMA,VAR) IN separated_nonempty_list(COMMA,exp) WHEN exp COLON clause END
  { Bigor ($2, $4, Some $6, $8) }
  | IF exp THEN clause ELSE clause END { CIf ($2, $4, $6) }

set_decl:
  | LBRACK separated_nonempty_list(COMMA, exp) RBRACK { $2 }
