{
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
let special    = [ '_' '-']
let newline    = '\r' | '\n' | "\r\n"
let identifier = (special | digits)* alpha (alpha | special | digits)*
let integer    = '-'? digits+
let double     = '-'? digits+ '.' digits+

rule lexer = parse
  | eof            { EOF                          }
  | empty+         { lexer lexbuf                 }
  | "begin"        { BEGIN                        }
  | "end"          { END                          }
  | "sets"         { SETS                         }
  | "formula"      { FORMULA                      }
  | "constant"     { CONSTANT                     }
  | "set"          { SET                          }
  | "in"           { IN                           }
  | "subset"       { SUBSET                       }
  | "empty"        { EMPTY                        }
  | "union"        { UNION                        }
  | "inter"        { INTER                        }
  | "minus"        { MINUS                        }
  | "upperset"     { UPPERSET                     }
  | "bigand"       { BIGAND                       }
  | "bigor"        { BIGOR                        }
  | "exact"        { EXACT                        }
  | "atleast"      { ATLEAST                      }
  | "almost"       { ALMOST                       }
  | "of"           { OF                           }
  | "with"         { WITH                         }
  | "true-clause"  { TRUECLAUSE                   }
  | "false-clause" { FALSECLAUSE                  }
  | "#"            { CARD                         }
  | "("            { LP                           }
  | ")"            { RP                           }
  | "."            { DOT                          }
  | ".."           { RANGE                        }
  | ","            { COMMA                        }
  | "=="           { EQUAL                        }
  | "<>"           { NOTEQUAL                     }
  | "+"            { ADD                          }
  | "-"            { SUBSTRACT                    }
  | "*"            { MULTIPLY                     }
  | "/"            { DIVIDE                       }
  | "mod"          { MOD                          }
  | "sqrt"         { SQRT                         }
  | "["            { LB                           }
  | "]"            { RB                           }
  | "<"            { LT                           }
  | ">"            { GT                           }
  | "<="           { LE                           }
  | ">="           { GE                           }
  | "="            { AFFECT                       }
  | "int"          { INT                          }
  | "real"         { REAL                         }
  | "true"         { TRUE                         }
  | "false"        { FALSE                        }
  | "and"          { AND                          }
  | "or"           { OR                           }
  | "=>"           { IMPLY                        }
  | "xor"          { XOR                          }
  | "~"            { NOT                          }
  | "if"           { IF                           }
  | "then"         { THEN                         }
  | "else"         { ELSE                         }
  | '?' identifier { VAR v                        }
  | '!' identifier { CONST c                      }
  | identifier     { IDENT i                      }
  | integer        { INTEGER  (int_of_string n)   }
  | double         { RATIONAL (float_of_string f) }
  | newline        { next_line lexbuf; NEWLINE    }
  | ";;"           { comments_parse lexbuf        }
  | _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and comments_parse = parse
  | '\n'           { incr line; parse lexbuf      }
  | _              { comments_parse lexbuf        }
