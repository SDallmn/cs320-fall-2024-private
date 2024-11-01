{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "if"  {IF}
  | "then"  {THEN}
  | "else"  {ELSE}
  | "=" { EQ }
  | "in"  {IN}
  | "fun"  {FUN}
  | "{" {LCURLY}
  | "}" {RCURLY}
  | "()" {UNIT}
  | "true"  {TRUE}
  | "false"  {FALSE}
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  
  (* | "let" {LET}
  | "in"  {IN}
  | "fun"  {FUN} *)
  
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
  


(* {
  open Par
  exception Lexing_error of string
}

(* let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let num = '-'? digit+
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = (lower | '_') (lower | upper | digit | '_' | '\'')* *)
let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']

rule read = parse
  | whitespace { read lexbuf }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | ident {
      match Lexing.lexeme lexbuf with
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | "let" -> LET
      | "in" -> IN
      | "fun" -> FUN
      | "true" -> TRUE
      | "false" -> FALSE
      | "()" -> UNIT
      | _ -> VAR (Lexing.lexeme lexbuf)
    }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | eof { EOF }
  | _ { raise (Lexing_error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }

{
  (* Entry point for the lexer *)
  let token lexbuf = read lexbuf
} *)