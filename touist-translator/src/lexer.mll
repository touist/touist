{
  open Lexing
  open Parser
  exception SyntaxError of string
  
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol  = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1}
}

let digits     = ['0' - '9']
let alpha      = ['a' - 'z' 'A' - 'Z']
let empty      = ['\t' ' ']
let special    = [ '_' '(' ')']
let newline    = '\r' | '\n' | "\r\n"
let identifier = (special | digits)* alpha (alpha | special | digits)*
let integer    = digits+
let double     = digits+ '.' digits+

rule lexer = parse
  | eof            { EOF          }
  | empty+         { lexer lexbuf }
  | "begin"        { BEGIN        }
  | "end"          { END          }
  | "sets"         { SETS         }
  | "formula"      { FORMULA      }
  | "in"           { IN           }
  | "subset"       { SUBSET       }
  | "empty"        { EMPTY        }
  | "union"        { UNION        }
  | "inter"        { INTER        }
  | "diff"         { DIFF         }
  | "bigand"       { BIGAND       }
  | "bigor"        { BIGOR        }
  | "when"         { WHEN         }
  | "Top"          { TOP          }
  | "Bot"          { BOTTOM       }
  | "card"         { CARD         }
  | "("            { LPAREN       }
  | "["            { LBRACK       }
  | "]"            { RBRACK       }
  | ")"            { RPAREN       }
  | "."            { DOT          }
  | ".."           { RANGE        }
  | ","            { COMMA        }
  | "=="           { EQUAL        }
  | "!="           { NOTEQUAL     }
  | "&&"           { BAND         }
  | "||"           { BOR          }
  | "+"            { ADD          }
  | "-"            { SUB          }
  | "*"            { MUL          }
  | "/"            { DIV          }
  | "mod"          { MOD          }
  | "sqrt"         { SQRT         }
  | "<"            { LT           }
  | ">"            { GT           }
  | "<="           { LE           }
  | ">="           { GE           }
  | "="            { AFFECT       }
  | ":"            { COLON        }
  | "int"          { TOINT        }
  | "float"        { TOFLOAT      }
  | "true"         { BOOL true    }
  | "false"        { BOOL false   }
  | "and"          { AND          }
  | "or"           { OR           }
  | "=>"           { IMPLIES      }
  | "xor"          { XOR          }
  | "not"          { NOT          }
  | "if"           { IF           }
  | "then"         { THEN         }
  | "else"         { ELSE         }
  | '$' identifier { VAR    (lexeme lexbuf) }
  | identifier     { TERM   (lexeme lexbuf) }
  | integer        { INT    (int_of_string   (lexeme lexbuf)) }
  | double         { FLOAT  (float_of_string (lexeme lexbuf)) }
  | newline        { next_line lexbuf; lexer lexbuf }
  | ";;"           { comments_parse lexbuf          }
  | _              { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
and comments_parse = parse
  | '\n'           { next_line lexbuf; lexer lexbuf }
  | _              { comments_parse lexbuf          }
