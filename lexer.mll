{
  open Parser

  exception Error of string

  let position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    pos.pos_lnum, pos.pos_cnum - pos.pos_bol

  let lexing_error lexbuf =
    let input = Lexing.lexeme lexbuf in
    let line, col = position lexbuf in
    let msg = Printf.sprintf "%d:%d: unexpected '%s'" line col input in
    raise (Error msg)
}

let whitespace = [' ' '\t']+
let newline = ['\n']
let alpha = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let sign = ['+' '-']
let integer = sign? digit+
let boolean = "true" | "false"
let identifier = ('_' | alpha) ('_' | alpha | digit)*

rule read = parse
  | whitespace      { read lexbuf }
  | newline         { Lexing.new_line lexbuf; read lexbuf }
  | "if"            { IF }
  | "else"          { ELSE }
  | "while"         { WHILE }
  | "skip"          { SKIP }
  | ":="            { GETS }
  | "+"             { PLUS }
  | "<"             { LESS }
  | "!"             { NOT }
  | "&&"            { AND }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | ";"             { SEMICOLON }
  | integer as i    { INT (int_of_string i) }
  | boolean as b    { BOOL (bool_of_string b) }
  | identifier as n { NAME n }
  | eof             { EOF }
  | _               { lexing_error lexbuf }
