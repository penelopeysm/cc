let asm_of_var = function
  | Ir.Var { identifier } -> Asm.PseudoRegister identifier

let asm_of_value = function
  | Ir.Constant i -> Asm.Immediate i
  | Ir.Variable v -> asm_of_var v

let asm_of_unary_op = function Ir.Minus -> Asm.Neg | Ir.Complement -> Asm.Not

let asm_of_instruction = function
  | Ir.Return { src } ->
      [ Asm.Mov { src = asm_of_value src; dst = Asm.Register Asm.AX }; Asm.Ret ]
  | Ir.UnaryOp { op; src; dst } ->
      [
        Asm.Mov { src = asm_of_value src; dst = asm_of_var dst };
        Asm.Unary { op = asm_of_unary_op op; target = asm_of_var dst };
      ]

let asm_of_func = function
  | Ir.Func { name; insts } ->
      let insts = List.concat_map asm_of_instruction insts in
      let insts, stack_size = Fix_asm.replace_pseudoregisters insts in
      let insts =
        if stack_size > 0 then Asm.AllocateStack { size = stack_size } :: insts
        else insts
      in
      let insts = Fix_asm.fix_movl insts in
      Asm.Function { name; insts }

let asm_of_ir = function
  | Ir.Programme func -> Asm.Programme { entry = asm_of_func func }
