module FixPseudo = struct
  module StringMap = Map.Make (struct
    type t = string

    let compare = String.compare
  end)

  type t = {
    (* NOTE: this maps pseudoregister names to POSITIVE values *)
    mutable pseudos_to_stackpos : int StringMap.t;
    mutable required_stack_space : int;
  }

  let create () : t =
    { pseudos_to_stackpos = StringMap.empty; required_stack_space = 0 }

  let fix_operand (a : t) (op : Asm.operand) =
    match op with
    | Asm.PseudoRegister nm -> (
        match StringMap.find_opt nm a.pseudos_to_stackpos with
        | None ->
            let sz = 4 in
            (* increment before adding the memory location *)
            a.required_stack_space <- a.required_stack_space + sz;
            let positive_stackval = a.required_stack_space in
            a.pseudos_to_stackpos <-
              StringMap.add nm positive_stackval a.pseudos_to_stackpos;
            Asm.Stack (-positive_stackval)
        | Some positive_stackval -> Asm.Stack (-positive_stackval))
    | _ -> op
end

let fix_inst (a : FixPseudo.t) (inst : Asm.instruction) : Asm.instruction =
  let open FixPseudo in
  match inst with
  (* no need to do anything *)
  | Asm.Ret -> inst
  | Asm.AllocateStack _ -> inst
  (* patch operands *)
  | Asm.Movl { src; dst } ->
      Asm.Movl { src = fix_operand a src; dst = fix_operand a dst }
  | Asm.Unary { op; target } -> Asm.Unary { op; target = fix_operand a target }
  | Asm.Binary _ -> inst

(* Fixes up the instruction list to replace pseudoregisters with stack
   locations. Additionally returns the amount of stack space that is required to
   hold all the values that were previously stored in pseudoregisters. *)
let replace_pseudoregisters (insts : Asm.instruction list) :
    Asm.instruction list * int =
  let acc = FixPseudo.create () in
  (* NB: can't return this:
     (List.map (fun i -> fix_inst acc i) insts, acc.required_stack_space)
     because that evaluates the required stack space before the map and makes it
     always 0! *)
  let fixed_insts = List.map (fun i -> fix_inst acc i) insts in
  (fixed_insts, acc.required_stack_space)

(** replace movl from memory to memory *)

let fix_movl_inst (inst : Asm.instruction) : Asm.instruction list =
  match inst with
  | Asm.Movl { src = Asm.Stack s1; dst = Asm.Stack s2 } ->
      [
        Asm.Movl { src = Asm.Stack s1; dst = Asm.Register R10 };
        Asm.Movl { src = Asm.Register R10; dst = Asm.Stack s2 };
      ]
  | _ -> [ inst ]

let fix_movl (insts : Asm.instruction list) : Asm.instruction list =
  List.concat_map fix_movl_inst insts
