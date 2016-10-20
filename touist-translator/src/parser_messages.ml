
(* This file was auto-generated based on "src/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 1 | 0 ->
        "You must specify (at least) a \"formula\" bloc,\nor a \"sets\" bloc followed by a \"formula\" bloc.\nExample:\n    begin sets    ... end sets\n    begin formula ... end formula\n"
    | 2 ->
        "An affectation of the form \"$var = ...\" is expected.\n"
    | 113 ->
        "An affectation sign '=' is expected here.\n"
    | 114 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 115 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 117 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 118 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 119 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 120 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 228 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 229 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 237 ->
        "At least one clause is expected.\n$0 is not the beginning of a clause.\n"
    | 179 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 180 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 232 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 233 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 201 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 202 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 189 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 190 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 183 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 191 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 192 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 193 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 194 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 205 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 206 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 195 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 196 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 197 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 198 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 207 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 208 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 199 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 200 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 239 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 240 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 185 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 203 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 204 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 187 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 188 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 231 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 223 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 131 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 132 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 225 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 181 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 182 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 133 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 140 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 29 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 30 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 141 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 142 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 218 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 219 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 220 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 31 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 32 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 56 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 57 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 41 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 42 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 33 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 34 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 35 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 43 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 44 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 45 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 46 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 47 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 48 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 60 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 61 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 49 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 50 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 51 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 52 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 62 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 63 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 53 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 54 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 37 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 38 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 58 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 59 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 39 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 40 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 5 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 6 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 7 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 8 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 9 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 10 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 11 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 93 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 13 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 84 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 85 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 86 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 80 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 82 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 18 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 20 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 70 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 71 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 72 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 73 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 74 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 22 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 23 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 24 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 144 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 150 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 151 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 155 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 157 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 158 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 159 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 213 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 156 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 215 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 216 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 160 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 162 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 164 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 165 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 166 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 178 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 163 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 210 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 211 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 167 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 172 ->
        "Ill-formed 'atleast' statement.\nAt this point, '(' was expected.\nInstead, $0 was read.\n"
    | 12 ->
        "Ill-formed '$2' statement.\nAt this point, an expression or variable was expected.\nInstead, $0 was read.\n"
    | 91 ->
        "Ill-formed use of $3 statement.\n"
    | 152 ->
        "Ill-formed use of 'exact' statement.\nAn expression was expected after $1.\n"
    | _ ->
        raise Not_found
