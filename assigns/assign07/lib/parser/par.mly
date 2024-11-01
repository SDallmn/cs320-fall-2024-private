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
%token IF
%token THEN
%token ELSE
%token IN
%token FUN
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token OR
%token ARROW
%token LET
%token EQUALS
%token RPAREN
%token LPAREN
%token TRUE
%token FALSE
%token LCURLY
%token RCURLY
%token UNIT

%left ADD SUB MUL DIV MOD LE LTE GE GTE NEQ EQ
%right AND OR

%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr 
    {If (e1, e2, e3)}
  | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr
    {Let (x, e1, e2)}
  | FUN; x = VAR; ARROW; e1 = expr 
    {Fun (x , e1)}
  | e = expr2 
    { e } 

expr2:
  | e1 = expr2; op = bop; e2 = expr2 
    {Bop (op, e1, e2)}
  | e = expr3; es = expr3* { mk_app e es }

expr3:
  | TRUE {True}
  | FALSE {False}
  | () {Unit}
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN {e}

  // match e with
  //   | TRUE -> True 
  //   | FALSE -> False 
  //   | NUM -> Num 
  //   | VAR -> Var

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt}
  | GTE {Gte}
  | EQ {Eq}
  | NEQ {Neq}
  | AND {And}
  | OR {Or}













// %{
// open Utils
// %}

// %token <int> NUM
// %token <string> VAR
// %token IF THEN ELSE LET IN FUN ARROW LPAREN RPAREN EQ LT LTE GT GTE NEQ AND OR PLUS MINUS TIMES DIV MOD TRUE FALSE UNIT EOF

// %left OR
// %left AND
// %left EQ NEQ LT LTE GT GTE
// %left PLUS MINUS
// %left TIMES DIV MOD
// %right ARROW

// %start prog
// %type <prog> prog

// %%

// prog:
//   | expr { $1 }

// expr:
//   | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
//   | LET VAR EQ expr IN expr { Let ($2, $4, $6) }
//   | FUN VAR ARROW expr { Fun ($2, $4) }
//   | expr2 { $1 }

// expr2:
//   | expr2 bop expr2 { Bop ($2, $1, $3) }
//   | expr3 { $1 }
//   | expr2 expr3 { App ($1, $2) }  // Handling application

// expr3:
//   | LPAREN expr RPAREN { $2 }
//   | NUM { Num $1 }
//   | VAR { Var $1 }
//   | TRUE { True }
//   | FALSE { False }
//   | UNIT { Unit }

// bop:
//   | PLUS { Add }
//   | MINUS { Sub }
//   | TIMES { Mul }
//   | DIV { Div }
//   | MOD { Mod }
//   | LT { Lt }
//   | LTE { Lte }
//   | GT { Gt }
//   | GTE { Gte }
//   | EQ { Eq }
//   | NEQ { Neq }
//   | AND { And }
//   | OR { Or }
