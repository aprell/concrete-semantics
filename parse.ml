let parse lines =
  lines |> Lexing.from_string |> Parser.program Lexer.read
