open Cccc
open Js_of_ocaml

let compile_source (source : string) : string =
  let lexbuf = Lexing.from_string source in
  let ast = Parser.programme Lexer.read lexbuf in
  let ir = Ir_gen.ir_of_ast ast in
  let asm = Asm_gen.asm_of_ir ir in
  Emit.string_of_asm asm

let () = Js.export "compile" compile_source
