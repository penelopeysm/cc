%{
open Ast
%}

%token <int> INTLIT
%token <string> IDENTIFIER
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token DECREMENT
%token MINUS
%token COMPLEMENT
%token EOF

(* Keywords *)
%token INT
%token VOID
%token RETURN

%start <Ast.t> programme
%%

programme:
  | f = func; EOF { Programme { entry = f } }

func:
  | INT; func_name = identifier; LEFT_PAREN; VOID; RIGHT_PAREN; LEFT_BRACE; stmt = statement; RIGHT_BRACE { Function{ name = func_name; body = stmt} }

statement:
  | RETURN; expr = expression; SEMICOLON { Return {return_value = expr} }

unary_operator:
  | MINUS { Minus }
  | COMPLEMENT { Complement }

expression:
  | i = INTLIT { IntLiteral { value = i}}
  | op = unary_operator; operand = expression { UnaryOp{ op; operand } }
  | LEFT_PAREN; expr = expression; RIGHT_PAREN { expr }

identifier:
  | id = IDENTIFIER { Identifier { name = id} }
