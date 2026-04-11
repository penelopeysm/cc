(* Platform.system is defined via a Dune build rule *)
type platform_type = MacOS | Linux | Other

let platform =
  match Platform.system with "macosx" -> MacOS | "linux" -> Linux | _ -> Other

let emit_register (buf : Buffer.t) (reg : Asm.register) : unit =
  match reg with
  (* todo: these only refer to the lower 32 bits *)
  | Asm.AX -> Buffer.add_string buf "%eax"
  | Asm.R10 -> Buffer.add_string buf "%r10d"

let emit_operand (buf : Buffer.t) (op : Asm.operand) : unit =
  match op with
  | Asm.Register r -> emit_register buf r
  | Asm.Immediate i ->
      Buffer.add_char buf '$';
      Buffer.add_string buf (string_of_int i)
  | Asm.PseudoRegister s -> failwith ("pseudoregister %" ^ s ^ " not fixed up")
  | Asm.Stack stackpos ->
      Buffer.add_string buf (string_of_int stackpos);
      Buffer.add_string buf "(%rbp)"

let emit_instruction (buf : Buffer.t) (inst : Asm.instruction) : unit =
  match inst with
  | Asm.Mov { src; dst } ->
      Buffer.add_string buf "  movl ";
      emit_operand buf src;
      Buffer.add_string buf ", ";
      emit_operand buf dst;
      Buffer.add_char buf '\n'
  | Asm.Ret ->
      (* restore caller's stack pointer *)
      Buffer.add_string buf "  movq %rbp, %rsp\n";
      Buffer.add_string buf "  popq %rbp\n";
      (* the retval will already have been moved to AX *)
      Buffer.add_string buf "  ret\n"
  | Asm.Unary { op; target } ->
      Buffer.add_string buf
        (match op with Asm.Neg -> "  negl " | Asm.Not -> "  notl ");
      emit_operand buf target;
      Buffer.add_char buf '\n'
  | Asm.AllocateStack { size } ->
      Buffer.add_string buf "  subq $";
      Buffer.add_string buf (string_of_int size);
      Buffer.add_string buf ", %rsp\n"

let emit_func (buf : Buffer.t) (func : Asm.func) : unit =
  match func with
  | Asm.Function { name; insts } ->
      (* On macOS, x86 symbol names are prefixed with underscores -- not necessary
       on Linux *)
      let mangled_name = if platform = MacOS then "_" ^ name else name in
      let global_decl = "  .globl " ^ mangled_name in
      Buffer.add_string buf global_decl;
      Buffer.add_char buf '\n';
      let func_decl = mangled_name ^ ":" in
      Buffer.add_string buf func_decl;
      Buffer.add_char buf '\n';
      (* enter new stack frame *)
      Buffer.add_string buf "  pushq %rbp\n";
      Buffer.add_string buf "  movq %rsp, %rbp\n";
      List.iter (emit_instruction buf) insts

let string_of_asm = function
  | Asm.Programme { entry } ->
      let buf = Buffer.create 1024 in
      emit_func buf entry;
      (* Disable executable stack on Linux *)
      if platform = Linux then
        Buffer.add_string buf "  .section .note.GNU-stack,\"\",@progbits\n";
      Buffer.contents buf
