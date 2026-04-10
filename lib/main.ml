let compile_source (source : string) : string =
  Lexing.from_string source
  |> Parser.programme Lexer.read
  |> Asm.lower |> Emit.emit

(* could add more functions later to show the AST, etc. *)
