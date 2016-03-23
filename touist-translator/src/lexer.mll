{
  open Parser
  open Lexing (* Just for the `next_line` function *)
      
      
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol  = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1}
}

let digit      = ['0' - '9']
let alpha      = ['a' - 'z' 'A' - 'Z']
let empty      = ['\t' ' ']
let special    = ['_']
let newline    = '\r' | '\n' | "\r\n"
let identifier = (special | digit)* alpha (alpha | special | digit)*
let variable   = (special | digit)* alpha (alpha | special | digit)*
let integer    = digit+
let double     = digit+ '.' digit+

rule token = parse
  | eof            { EOF          }
  | empty+         { token lexbuf }
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
  | "exact"        { EXACT        }
  | "atmost"       { ATMOST       }
  | "atleast"      { ATLEAST      }
  | "bigand"       { BIGAND       }
  | "bigor"        { BIGOR        }
  | "when"         { WHEN         }
  | "Top"          { TOP          }
  | "Bot"          { BOTTOM       }
  | "card"         { CARD         }
  | "("            { LPAREN       }
  | ")"            { RPAREN       }
  | "["            { LBRACK       }
  | "]"            { RBRACK       }
  | ".."           { RANGE        }
  | ","            { COMMA        }
  | "=="           { EQUAL        }
  | "!="           { NOTEQUAL     }
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
  | '$' variable   { VAR    (Lexing.lexeme lexbuf) }
  | identifier     { TERM   (Lexing.lexeme lexbuf) }
  | integer        { INT    (int_of_string   (Lexing.lexeme lexbuf)) }
  | double         { FLOAT  (float_of_string (Lexing.lexeme lexbuf)) }
  | newline        { next_line lexbuf; token lexbuf }
  | ";;"           { comments_parse lexbuf          }
  | _              { raise (Error ("Unexpected char: " ^ (Lexing.lexeme lexbuf))) }
and comments_parse = parse
  | '\n'           { next_line lexbuf; token lexbuf }
  | _              { comments_parse lexbuf          }
