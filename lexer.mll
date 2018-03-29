{
  open Parser;;
  exception Eof;;
}

rule token = parse
  | [' ' '\t' '\n']          { token lexbuf }
  | ";;"                     { EOL }
  | "+"                      { ADD }
  | "-"                      { SUB }
  | "*"                      { MUL }
  | "<"                      { LT }
  | "="                      { EQ }
  | "<="                     { LE }
  | ">"                      { GT }
  | ">="                     { GE }
  | "<>"                     { NEQ }
  | "("                      { LPAREN }
  | ")"                      { RPAREN }
  | "let"                    { LET }
  | "rec"                    { REC }
  | "in"                     { IN }
  | "if"                     { IF }
  | "then"                   { THEN }
  | "else"                   { ELSE }
  | "try"                    { TRY }
  | "with"                   { WITH }
  | "raise"                  { RAISE }
  | "E"                      { EX }
  | "fun"                    { FUN }
  | "->"                     { ARROW }
  | (['0'-'9']+ as i)        { VAL (int_of_string i) }
  | (['a'-'z''A'-'Z']+ as i) { IDE i }
  | eof                      { EOF }
