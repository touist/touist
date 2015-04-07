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
%token RANGE CARD

%token AFFECT IF THEN ELSE BIGAND BIGOR WHEN

%token BEGIN SETS FORMULA END EOF
%token LPAREN RPAREN LBRACK RBRACK COMMA COLON PIPE

%start <Ast.prog> prog

%right EQUIV IMPLIES
%left OR
%left AND
%left XOR
%left NOT
%left ADD SUB
%nonassoc neg
%left MUL DIV
%left MOD

%%

prog:
  | BEGIN SETS nonempty_list(affect) END SETS BEGIN FORMULA nonempty_list(clause_exp) END FORMULA EOF
  { Begin (Some $3, $8) }
  | BEGIN FORMULA nonempty_list(clause_exp) END FORMULA EOF
  { Begin (None, $3) }

affect:
  | VAR AFFECT int_exp    { Affect ($1, IntExp $3)    }
  | VAR AFFECT float_exp  { Affect ($1, FloatExp $3)  }
  | VAR AFFECT bool_exp   { Affect ($1, BoolExp $3)   }
  | VAR AFFECT set_exp    { Affect ($1, SetExp $3)    }
  (*| VAR AFFECT var_or_exp { Affect ($1, $3) }*)

var_or_exp:
  (*| VAR { Var $1 }*)
  | int_exp { IntExp $1 }
  | float_exp { FloatExp $1 }
  | bool_exp { BoolExp $1 }
  | set_exp { SetExp $1 }

int_exp:
  | LPAREN int_exp RPAREN { $2 }
  | VAR { IVar $1 }
  | INT { Int  $1 }
  | int_exp ADD int_exp { Add ($1, $3) }
  | int_exp SUB int_exp { Sub ($1, $3) }
  | int_exp MUL int_exp { Mul ($1, $3) }
  | int_exp DIV int_exp { Div ($1, $3) }
  | int_exp MOD int_exp { Mod ($1, $3) }
  | SUB int_exp { Neg $2 } %prec neg
  | TOINT LPAREN float_exp RPAREN { To_int $3 }
  | CARD  LPAREN set_exp   RPAREN { Card   $3 }

float_exp:
  | LPAREN float_exp RPAREN { $2 }
  | VAR   { FVar  $1 }
  | FLOAT { Float $1 }
  | float_exp ADD  float_exp { FAdd  ($1, $3) }
  | float_exp SUB  float_exp { FSub  ($1, $3) }
  | float_exp MUL  float_exp { FMul  ($1, $3) }
  | float_exp DIV  float_exp { FDiv  ($1, $3) }
  | SUB float_exp { FNeg $2 } %prec neg
  | SQRT    LPAREN float_exp RPAREN { Sqrt     $3 }
  | TOFLOAT LPAREN int_exp   RPAREN { To_float $3 }

bool_exp:
  | LPAREN bool_exp RPAREN { $2 }
  | VAR  { BVar $1 }
  | BOOL { Bool $1 }
  | NOT bool_exp { BNot $2 }
  | bool_exp  AND      bool_exp  { BAnd              ($1, $3) }
  | bool_exp  OR       bool_exp  { BOr               ($1, $3) }
  | bool_exp  XOR      bool_exp  { BXor              ($1, $3) }
  | bool_exp  IMPLIES  bool_exp  { BImplies          ($1, $3) }
  | bool_exp  EQUIV    bool_exp  { BEquiv            ($1, $3) }
  | int_exp   EQUAL    int_exp   { Equal             ($1, $3) }
  | int_exp   NOTEQUAL int_exp   { Not_equal         ($1, $3) }
  | int_exp   LT       int_exp   { Lesser_than       ($1, $3) }
  | int_exp   LE       int_exp   { Lesser_or_equal   ($1, $3) }
  | int_exp   GT       int_exp   { Greater_than      ($1, $3) }
  | int_exp   GE       int_exp   { Greater_or_equal  ($1, $3) }
  | float_exp EQUAL    float_exp { FEqual            ($1, $3) }
  | float_exp NOTEQUAL float_exp { FNot_equal        ($1, $3) }
  | float_exp LT       float_exp { FLesser_than      ($1, $3) }
  | float_exp LE       float_exp { FLesser_or_equal  ($1, $3) }
  | float_exp GT       float_exp { FGreater_than     ($1, $3) }
  | float_exp GE       float_exp { FGreater_or_equal ($1, $3) }
  | set_exp EQUAL set_exp { SEqual ($1, $3) }
  | int_exp   IN set_exp { In (IntExp $1, $3)   }
  | float_exp IN set_exp { In (FloatExp $1, $3) }
  | TERM      IN set_exp { In (ClauseExp (Term ($1, None)), $3) }
  | SUBSET LPAREN set_exp COMMA set_exp RPAREN { Subset ($3, $5) }
  | EMPTY LPAREN set_exp RPAREN { Empty $3 }

set_exp:
  | LPAREN set_exp RPAREN { $2 }
  | VAR      { SVar $1 }
  | set_decl { Set  $1 }
  | UNION LPAREN set_exp COMMA set_exp RPAREN { Union ($3, $5) }
  | INTER LPAREN set_exp COMMA set_exp RPAREN { Inter ($3, $5) }
  | DIFF  LPAREN set_exp COMMA set_exp RPAREN { Diff  ($3, $5) }
  | LBRACK int_exp RANGE int_exp RBRACK { Range ($2, $4) }

set_decl:
  | LBRACK RBRACK { GenSet.Empty }
  | LBRACK separated_nonempty_list(COMMA, int_exp) RBRACK
  { GenSet.IS (intset_of_list (List.map Eval.eval_int $2)) }
  | LBRACK separated_nonempty_list(COMMA, float_exp) RBRACK
  { GenSet.FS (floatset_of_list (List.map Eval.eval_float $2)) }
  | LBRACK separated_nonempty_list(COMMA, TERM) RBRACK
  { GenSet.SS (stringset_of_list $2) }

clause_exp:
  | LPAREN clause_exp RPAREN { $2 }
  | TOP    { Top    }
  | BOTTOM { Bottom }
  | TERM { Term ($1, None) }
  | TERM PIPE term_option PIPE { Term ($1, Some $3) }
  | NOT clause_exp { Not $2 }
  | clause_exp AND     clause_exp { And     ($1, $3) }
  | clause_exp OR      clause_exp { Or      ($1, $3) }
  | clause_exp XOR     clause_exp { Xor     ($1, $3) }
  | clause_exp IMPLIES clause_exp { Implies ($1, $3) }
  | clause_exp EQUIV   clause_exp { Equiv   ($1, $3) }
  | BIGAND separated_nonempty_list(COMMA, VAR) IN separated_nonempty_list(COMMA, set_exp) COLON clause_exp END
  { Bigand ($2, $4, None, $6) }
  | BIGAND separated_nonempty_list(COMMA, VAR) IN separated_nonempty_list(COMMA, set_exp) WHEN bool_exp COLON clause_exp END
  { Bigand ($2, $4, Some $6, $8) }
  | BIGOR  separated_nonempty_list(COMMA, VAR) IN separated_nonempty_list(COMMA, set_exp) COLON clause_exp END
  { Bigor ($2, $4, None, $6) }
  | BIGOR  separated_nonempty_list(COMMA, VAR) IN separated_nonempty_list(COMMA, set_exp) WHEN bool_exp COLON clause_exp END
  { Bigor  ($2, $4, Some $6, $8) }
  | IF bool_exp THEN clause_exp ELSE clause_exp END
  { CIf ($2, $4, $6) }

term_option:
  | var_or_exp { Exp $1 }
  | TERM { Str $1 }

