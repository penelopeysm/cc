let indent_size = 2

let prerr_indent (indent_level : int) : unit =
  let indent_str = String.make (indent_level * indent_size) ' ' in
  Stdlib.prerr_string indent_str

type var = Var of { identifier : string }

let pp_var = function
  | Var { identifier } -> prerr_string @@ "Var(%" ^ identifier ^ ")"

type value = Constant of int | Variable of var

let pp_value = function
  | Constant i ->
      prerr_string "Constant(";
      prerr_string (string_of_int i);
      prerr_string ")"
  | Variable v -> pp_var v

type unary_operator = Minus | Complement

let pp_unary_operator = function
  | Minus -> prerr_string "Minus"
  | Complement -> prerr_string "Complement"

type instruction =
  | Return of { src : value }
  | UnaryOp of { op : unary_operator; src : value; dst : var }

let pp_instruction (indent_level : int) (inst : instruction) : unit =
  prerr_indent indent_level;
  match inst with
  | Return { src } ->
      prerr_string "return ";
      pp_value src;
      prerr_newline ()
  | UnaryOp { op; src; dst } ->
      prerr_string "Unary(op=";
      pp_unary_operator op;
      prerr_string ", src=";
      pp_value src;
      prerr_string ", dst=";
      pp_var dst;
      prerr_string ")";
      prerr_newline ()

type func = Func of { name : string; insts : instruction list }

let pp_func (indent_level : int) = function
  | Func { name; insts } ->
      prerr_indent indent_level;
      prerr_endline ("func " ^ name);
      List.iter (fun inst -> pp_instruction (indent_level + 1) inst) insts

type t = Programme of func

let pp_t = function
  | Programme f ->
      prerr_endline "programme";
      pp_func 1 f
