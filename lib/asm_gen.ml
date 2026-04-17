let asm_of_var = function
  | Ir.Var { identifier } -> Asm.PseudoRegister identifier

let asm_of_value = function
  | Ir.Constant i -> Asm.Immediate i
  | Ir.Variable v -> asm_of_var v

let asm_of_unary_op = function Ir.Minus -> Asm.Neg | Ir.Complement -> Asm.Not

let asm_of_instruction = function
  | Ir.Return { src } ->
      [
        Asm.Movl { src = asm_of_value src; dst = Asm.Register Asm.AX }; Asm.Ret;
      ]
  | Ir.UnaryOp { op; src; dst } ->
      [
        Asm.Movl { src = asm_of_value src; dst = asm_of_var dst };
        Asm.Unary { op = asm_of_unary_op op; target = asm_of_var dst };
      ]
  | Ir.BinaryOp { op; left; right; dst } -> (
      match op with
      (* for addl, subl and imull we need to move the left operand into the
         destination register. The right operand can be an immediate value so we
         can use it directly in the binary instruction. *)
      | Ir.Add ->
          [
            Asm.Movl { src = asm_of_value left; dst = asm_of_var dst };
            Asm.Binary
              { op = Asm.Add; left = asm_of_value right; dst = asm_of_var dst };
          ]
      | Ir.Subtract ->
          [
            Asm.Movl { src = asm_of_value left; dst = asm_of_var dst };
            Asm.Binary
              { op = Asm.Sub; left = asm_of_value right; dst = asm_of_var dst };
          ]
      | Ir.Multiply ->
          [
            Asm.Movl { src = asm_of_value left; dst = asm_of_var dst };
            Asm.Binary
              { op = Asm.Mul; left = asm_of_value right; dst = asm_of_var dst };
          ]
      | Ir.Divide | Ir.Modulo -> failwith "TODO")

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
