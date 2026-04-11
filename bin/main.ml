open Climate
open Cccc

let main ~(input_fname : string) ~(output_fname : string option)
    ~(dump_ast : bool) ~(dump_ir : bool) : unit =
  In_channel.with_open_text input_fname (fun inx ->
      let lexbuf = Lexing.from_channel inx in
      let ast = Parser.programme Lexer.read lexbuf in
      if dump_ast then Ast.pp_t ast;
      let ir = Ir_gen.ir_of_ast ast in
      if dump_ir then Ir.pp_t ir;
      let asm = Asm_gen.asm_of_ir ir in
      let asm_text = Emit.string_of_asm asm in
      match output_fname with
      | Some fname ->
          Out_channel.with_open_text fname (fun out_channel ->
              Out_channel.output_string out_channel asm_text)
      | None ->
          print_string asm_text;
          exit 0)

let () =
  let command =
    Command.singleton ~doc:"cccc: c compiler compiling c"
    @@
    let open Arg_parser in
    let+ input_fname = pos_req 0 string ~doc:"Path to the input .c or .i file"
    and+ output_fname =
      named_opt [ "o" ] string
        ~doc:
          "Path to the output file. If not provided, output will be printed to \
           stdout"
    and+ dump_ast =
      flag [ "a"; "dump-ast" ] ~doc:"Additionally dump the AST to stderr"
    and+ dump_ir =
      flag [ "i"; "dump-ir" ] ~doc:"Additionally dump the IR to stderr"
    in
    main ~input_fname ~output_fname ~dump_ast ~dump_ir
  in
  Command.run command
