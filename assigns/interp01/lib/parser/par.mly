%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF THEN ELSE LET IN FUN ARROW
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token LPAREN RPAREN
%token TRUE FALSE UNIT

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.expr> prog

%%

prog:
  | e=expr; EOF { e }

expr:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr { If (e1, e2, e3) }
  | LET; x=VAR; EQ; e1=expr; IN; e2=expr { Let (x, e1, e2) }
  | FUN; x=VAR; ARROW; e=expr { Fun (x, e) }
  | e=expr2 { e }

expr2:
  | e1=expr2; op=bop e2=expr2 { Bop (op, e1, e2) }
  | e = expr3; es = expr3* { mk_app e es }

expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | n=NUM { Num n }
  | x=VAR { Var x }
  | LPAREN; e=expr; RPAREN { e }

%inline bop:
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | ADD { Add }
  | SUB { Sub }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
