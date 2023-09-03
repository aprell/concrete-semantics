let parse lines =
  lines |> Lexing.from_string |> Parser.program Lexer.read

let unparse = Ast.pp_command
