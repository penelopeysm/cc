type register = AX | R10

type operand =
  | Register of register
  | Immediate of int
  | PseudoRegister of string
  (* convention here is that Stack holds negative numbers *)
  | Stack of int

type unary_operator = Neg | Not

type instruction =
  | Mov of { src : operand; dst : operand }
  | Ret
  | Unary of { op : unary_operator; target : operand }
  | AllocateStack of { size : int }

type func = Function of { name : string; insts : instruction list }
type t = Programme of { entry : func }
