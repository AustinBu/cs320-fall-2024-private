%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token IF THEN ELSE LET IN FUN ARROW ASSERT
%token ADD SUB MUL DIV MOD LT LTE GT GTE EQ NEQ AND OR
%token LPAREN RPAREN COLON
%token TRUE FALSE UNIT
%token INTTY BOOLTY UNITTY
%token REC

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | t=toplets EOF { t }

toplets:
  | { [] }
  | t=toplet; ts=toplets { t :: ts }

toplet:
  | LET; v=VAR; x=args_opt; COLON; t=ty; EQ; e=sfexpr
    { { is_rec=false; name=v; args=x; ty=t; value=e } }
  | LET; REC; v=VAR; x=args; COLON; t=ty; EQ; e=sfexpr
    { { is_rec=true; name=v; args=x; ty=t; value=e } }

args_opt:
  | { [] }
  | x=args { x }

args:
  | LPAREN; v=VAR; COLON; t=ty; RPAREN { [(v, t)] }
  | LPAREN; v=VAR; COLON; t=ty; RPAREN; y=args { (v, t) :: y}


ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | t1=ty ARROW t2=ty { FunTy (t1, t2) }
  | LPAREN t=ty RPAREN { t }

sfexpr:
  | LET; v=VAR; x=args_opt; COLON t=ty; EQ; e1=sfexpr; IN; e2=sfexpr { 
    SLet { is_rec=false; name=v; args=x; ty=t; value=e1; body=e2 } 
    }
  | LET; REC; v=VAR; x=args; COLON t=ty; EQ; e1=sfexpr; IN; e2=sfexpr { 
    SLet { is_rec=true; name=v; args=x; ty=t; value=e1; body=e2 } 
   }
  | IF; e1=sfexpr; THEN; e2=sfexpr; ELSE; e3=sfexpr { SIf (e1, e2, e3) }
  | FUN; x=args; ARROW; e=sfexpr { 
    match x with
    | (v, t) :: rest -> SFun { arg=(v, t); args=rest; body=e }
    | [] -> failwith "err"
  }
  | e=sfexpr2 { e }

sfexpr2:
  | e1=sfexpr2; op=bop e2=sfexpr2 { SBop (op, e1, e2) }
  | ASSERT e=sfexpr3 { SAssert (e) }
  | e = sfexpr3; es = sfexpr3* { mk_app e es }

sfexpr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n=NUM { SNum n }
  | x=VAR { SVar x }
  | LPAREN; e=sfexpr; RPAREN { e }

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
