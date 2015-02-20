%{
  (* OCaml code here *)
%}

%token TRUE FALSE AND OR XOR EQUAL NOTEQUAL LT GT LE GE EMPTY IN SUBSET
%token ADD SUBSTRACT MULTIPLY DIVIDE MOD CARD SQRT
%token INT FLOAT
%token <int> INTEGER
%token <float> RATIONAL
%token SET UNION INTER MINUS UPPERSET
%token EXACT ATLEAST ATMOST
%token IF THEN ELSE
%token TRUECLAUSE FALSECLAUSE
%token NOT IMPLY
%token BIGOR BIGAND
%token BEGIN END
%token FORMULA SETS
%token CONSTANT
%token LP RP LB RB
%token WITH OF
%token RANGE DOT COMMA NEWLINE
%token AFFECT
%token <string> IDENT
%token <string> VAR
%token <string> CONST
%token EOF

%start <Ast.prog> start

%left OR AND EQUAL NOTEQUAL LT GT LE GE SUBSET
%nonassoc NOT
%left IMPLY

%%

start:
  | BEGIN SETS s = sets END SETS BEGIN FORMULA f = formula END FORMULA EOF { `Begin (Some(s), f) }
  | BEGIN FORMULA f = formula END FORMULA EOF { `Begin (None, f) }

sets:
  | def = separated_list(NEWLINE, define_set) { def }
(*  | def = define_set          { [def]  }
  | def = define_set s = sets { def::s }*)

define_set:
  | VAR a = atom AFFECT e = int_expr   { `Affect (a, `Int_expr   e) }
  | VAR a = atom AFFECT e = float_expr { `Affect (a, `Float_expr e) }
  | SET a = atom AFFECT e = set_expr   { `Affect (a, `Set_expr   e) }

formula:
  | f = separated_list(NEWLINE, clause_expr) { f }
(*  | c = clause_expr             { Ast.Clause [c]    }
  | c = clause_expr f = formula { Ast.Clause (c::f) }*)

bool_expr:
  | TRUE                                          { `Bool true                 }
  | FALSE                                         { `False                     }
  | LP    i  = IDENT    IN       e  = set_expr RP { `In               (i, e)   }
  | LP    e1 = set_expr SUBSET   e2 = set_expr RP { `Subset           (e1, e2) }
  | LP    a1 = atom     EQUAL    a2 = atom RP     { `Equal            (a1, a2) }
  | LP    a1 = atom     NOTEQUAL a2 = atom RP     { `Not_equal        (a1, a2) }
  | LP    a1 = atom     LT       a2 = atom RP     { `Lesser_than      (a1, a2) }
  | LP    a1 = atom     LE       a2 = atom RP     { `Lesser_or_equal  (a1, a2) }
  | LP    a1 = atom     GT       a2 = atom RP     { `Greater_than     (a1, a2) }
  | LP    a1 = atom     GE       a2 = atom RP     { `Greater_or_equal (a1, a2) }
  | EMPTY e  = set_expr                           { `Empty            e        }
  | LP    e1 = bool_expr AND   e2 = bool_expr RP  { `And              (e1, e2) }
  | LP    e1 = bool_expr OR    e2 = bool_expr RP  { `Or               (e1, e2) }
  | LP    e1 = bool_expr IMPLY e2 = bool_expr RP  { `Imply            (e1, e2) }
  | LP    e1 = bool_expr XOR   e2 = bool_expr RP  { `Xor              (e1, e2) }
  | NOT   e  = bool_expr                          { `Not              e        }

int_expr:
  | i = INTEGER                                    { `Integer   i        }
  | LP    e1 = int_expr ADD       e2 = int_expr RP { `Add       (e1, e2) }
  | LP    e1 = int_expr MULTIPLY  e2 = int_expr RP { `Multiply  (e1, e2) }
  | LP    e1 = int_expr SUBSTRACT e2 = int_expr RP { `Substract (e1, e2) }
  | LP    e1 = int_expr DIVIDE    e2 = int_expr RP { `Divide    (e1, e2) }
  | LP    e1 = int_expr MOD       e2 = int_expr RP { `Modulo    (e1, e2) }
  | CARD  e  = set_expr                            { `Card      e        }
  | INT   e  = float_expr                          { `Int       e        }

float_expr:
  | f = RATIONAL                                       { `Rational  f        }
  | LP    e1 = float_expr ADD       e2 = float_expr RP { `Add       (e1, e2) }
  | LP    e1 = float_expr MULTIPLY  e2 = float_expr RP { `Multiply  (e1, e2) }
  | LP    e1 = float_expr SUBSTRACT e2 = float_expr RP { `Substract (e1, e2) }
  | LP    e1 = float_expr DIVIDE    e2 = float_expr RP { `Divide    (e1, e2) }
  | FLOAT e  = int_expr                                { `Float     e        }
  | SQRT  e  = float_expr                              { `Sqrt      e        }

set_expr:
  | LP tl = term_list RP { `Set tl }
  | LP e1 = set_expr UNION e2 = set_expr RP { `Union        (e1, e2) }
  | LP e1 = set_expr INTER e2 = set_expr RP { `Intersection (e1, e2) }
  | LP e1 = set_expr MINUS e2 = set_expr RP { `Difference   (e1, e2) }
  | LP e1 = int_expr RANGE e2 = int_expr RP { `Range        (e1, e2) }
  |    e  = set_expr DOT   i  = int_expr    { `Dot          (e, i)   }
  | UPPERSET LP i = IDENT IN e = set_expr RP { `Upperset (i, e) }
  | LP IF b = bool_expr THEN e1 = set_expr ELSE e2 = set_expr RP { `If (b, e1, e2) }

clause_expr:
  | TRUECLAUSE  { `Trueclause  }
  | FALSECLAUSE { `Falseclause }
  | NOT e = clause_expr { `Not e }
  | LP e1 = clause_expr AND   e2 = clause_expr RP { `And   (e1, e2) }
  | LP e1 = clause_expr OR    e2 = clause_expr RP { `Or    (e1, e2) }
  | LP e1 = clause_expr IMPLY e2 = clause_expr RP { `Imply (e1, e2) }
  | LP e1 = clause_expr XOR   e2 = clause_expr RP { `Xor   (e1, e2) }
  | LP BIGAND  b = bigbody RP { `Bigand b }
  | LP BIGOR   b = bigbody RP { `Bigor  b }
  | LP EXACT   i = INTEGER OF b = bigbody RP { `Exact   (i, b) }
  | LP ATLEAST i = INTEGER OF b = bigbody RP { `Atleast (i, b) }
  | LP ATMOST  i = INTEGER OF b = bigbody RP { `Atmost  (i, b) }
  | LP IF b = bool_expr THEN e1 = clause_expr ELSE e2 = clause_expr RP { `If (b, e1, e2) }

bigbody:
  | id = IDENT IN s = set_expr      c = clause_expr               { (id, s, None, c)   }
  | id = IDENT IN s = set_expr WITH b = bool_expr c = clause_expr { (id, s, Some b, c) }

atom:
  | id = IDENT { (id, None) }
  | id = IDENT LP tl = term_list RP { (id, Some tl) }

term_list:
  | tl = separated_list(COMMA, term) { tl }

term:
  | id = IDENT    { `Ident    id }
  | i  = INTEGER  { `Integer  i  }
  | f  = RATIONAL { `Rational f  }
