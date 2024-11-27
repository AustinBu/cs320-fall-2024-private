{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "()" { UNIT }
  | "->" { ARROW }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "+" { ADD }
  | "-" { SUB }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | "assert" { ASSERT }
  | "int" { INTTY }
  | "bool" { BOOLTY }
  | "unit" { UNITTY }
  | "rec" { REC }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
 