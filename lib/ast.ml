let indent_size = 2

let prerr_indent (indent_level : int) : unit =
  let indent_str = String.make (indent_level * indent_size) ' ' in
  Stdlib.prerr_string indent_str

type identifier = Identifier of { name : string }

let get_identifier_name (Identifier { name }) : string = name

let pp_identifier (Identifier { name }) (indent_level : int) : unit =
  prerr_indent indent_level;
  ANSITerminal.prerr_string [ Bold ] "Identifier";
  Stdlib.prerr_string " ";
  Stdlib.prerr_string name

type unary_operator = Decrement | Minus | Complement

let pp_unary_op (op : unary_operator) (indent_level : int) : unit =
  prerr_indent indent_level;
  ANSITerminal.prerr_string [ Bold ] "UnaryOperator";
  Stdlib.prerr_string " ";
  let op_str =
    match op with Decrement -> "--" | Minus -> "-" | Complement -> "~"
  in
  Stdlib.prerr_string op_str

type exp =
  | IntLiteral of { value : int }
  | UnaryOp of { op : unary_operator; operand : exp }

let rec pp_exp (e : exp) (indent_level : int) : unit =
  match e with
  | IntLiteral { value } ->
      prerr_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "IntLiteral";
      Stdlib.prerr_string " ";
      Stdlib.prerr_string (string_of_int value)
  | UnaryOp { op; operand } ->
      prerr_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "UnaryOp\n";
      pp_unary_op op (indent_level + 1);
      pp_exp operand (indent_level + 1)

type statement = Return of { return_value : exp }

let pp_statement (s : statement) (indent_level : int) : unit =
  match s with
  | Return { return_value } ->
      prerr_indent indent_level;
      ANSITerminal.prerr_string [ Bold ] "Return\n";
      pp_exp return_value (indent_level + 1)

type func = Function of { name : identifier; body : statement }

let pp_func (Function { name; body }) (indent_level : int) : unit =
  prerr_indent indent_level;
  ANSITerminal.prerr_string [ Bold ] "Function ";
  Stdlib.prerr_string @@ get_identifier_name name;
  Stdlib.prerr_string "\n";
  pp_statement body (indent_level + 1)

type t = Programme of { entry : func }

let pp_t (Programme { entry }) : unit =
  pp_func entry 0;
  prerr_endline ""
