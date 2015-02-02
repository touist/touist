{
  open Tokens
  exception LexingError
}

let digits = ['0' - '9']
let alpha  = ['a' - 'z' 'A' - 'Z']
let empty  = ['\n' '\t' ' ']

rule lexer = parse
  | eof        { Eof          }
  | empty+     { lexer lexbuf }
  | "True"     { True         }
  | "False"    { False        }
  | "&&"       { And          }
  | "||"       { Or           }
  | "=="       { Equal        }
  | "<>"       { NotEqual     }
  | "<"        { LT           }
  | ">"        { GT           }
  | "<="       { LE           }
  | ">="       { GE           }
  | "empty"    { Empty        }
  | "in"       { In           }
  | "subset"   { Subset       }
  | "+"        { Plus         }
  | "-"        { Minus        }
  | "*"        { Times        }
  | "/"        { Divide       }
  | "mod"      { Modulo       }
  | "card"     { Cardinal     }
  | "int"      { IntPart      }
  | "sqrt"     { Sqrt         }
  | "union"    { Union        }
  | "inter"    { Intersect    }
  | "minus"    { Diff         }
  | "if"       { If           }
  | "then"     { Then         }
  | "else"     { Else         }
  | "upperset" { Upperset     }
  | ".."       { Range        }
  | "."        { Dot          }
  | "#T"       { Top          }
  | "#F"       { Bottom       }
  | "~"        { Negation     }
  | "and"      { LogicalAnd   }
  | "or"       { LogicalOr    }
  | "=>"       { Arrow        }
  | "bigand"   { BigAnd       }
  | "bigor"    { BigOr        }
  | "begin"    { Begin        }
  | "end"      { End          }
  | "sets"     { Sets         }
  | "formula"  { Formula      }
  | "with"     { With         }
  | "("        { LeftPar      }
  | ")"        { RightPar     }
  | "="        { Affect       }
  | digits+                     as n { Int   (int_of_string n)   }
  | digits+ '.' digits*         as f { Float (float_of_string f) }
  | '?' alpha (alpha | digits)* as v { Var   v                   }
  | '!' alpha (alpha | digits)* as c { Const c                   }
