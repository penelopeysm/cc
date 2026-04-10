open Cccc
open Js_of_ocaml

let () = Js.export "compile" Cccc.Main.compile_source
