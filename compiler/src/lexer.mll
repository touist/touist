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
  | eof            { EOF                       }
  | empty+         { lexer lexbuf              }
  | "begin"        { BEGIN                     }
  | "end"          { END                       }
  | "sets"         { SETS                      }
  | "formula"      { FORMULA                   }
  | "in"           { IN                        }
  | "subset"       { SUBSET                    }
  | "empty"        { EMPTY                     }
  | "union"        { UNION                     }
  | "inter"        { INTER                     }
  | "diff"         { DIFF                      }
  | "upperset"     { UPPERSET                  }
  | "bigand"       { BIGAND                    }
  | "bigor"        { BIGOR                     }
  | "exact"        { EXACT                     }
  | "atleast"      { ATLEAST                   }
  | "almost"       { ALMOST                    }
  | "of"           { OF                        }
  | "with"         { WITH                      }
  | "true-clause"  { TRUECLAUSE                }
  | "false-clause" { FALSECLAUSE               }
  | "#"            { CARD                      }
  | "("            { LPAREN                    }
  | ")"            { RPAREN                    }
  | "."            { DOT                       }
  | ".."           { RANGE                     }
  | ","            { COMMA                     }
  | "=="           { EQUAL                     }
  | "!="           { NOTEQUAL                  }
  | "+"            { ADD                       }
  | "-"            { SUB                       }
  | "*"            { MUL                       }
  | "/"            { DIV                       }
  | "mod"          { MOD                       }
  | "sqrt"         { SQRT                      }
  | "["            { LBRACK                    }
  | "]"            { RBRACK                    }
  | "<"            { LT                        }
  | ">"            { GT                        }
  | "<="           { LE                        }
  | ">="           { GE                        }
  | "="            { AFFECT                    }
  | "int"          { TOINT                     }
  | "real"         { TOFLOAT                   }
  | "true"         { TRUE                      }
  | "false"        { FALSE                     }
  | "and"          { AND                       }
  | "or"           { OR                        }
  | "=>"           { IMPLY                     }
  | "xor"          { XOR                       }
  | "~"            { NOT                       }
  | "if"           { IF                        }
  | "then"         { THEN                      }
  | "else"         { ELSE                      }
  | '$' identifier { VAR v                     }
  | identifier     { TERM t                    }
  | integer        { INT  (int_of_string n)    }
  | double         { FLOAT (float_of_string f) }
  | newline        { next_line lexbuf; NEWLINE }
  | ";;"           { comments_parse lexbuf     }
  | _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and comments_parse = parse
  | '\n'           { incr line; parse lexbuf }
  | _              { comments_parse lexbuf   }
