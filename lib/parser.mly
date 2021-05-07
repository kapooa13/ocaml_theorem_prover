 /* File parser.mly */
%token <char> VARNAME
%token <string> CONNAME
%token COMPOSE
%token COMMA
%token LPAREN RPAREN
%token EOL
%token EOF
%start main             /* the entry point */
%type <Expr.expr> main
%%
main:
    expr EOL                { $1 }
  | expr EOF                { $1 }
;
expr:
    VARNAME                    { Expr.Var $1 }
  | expr COMPOSE expr          { Expr.compose [$1; $3] }
  | CONNAME LPAREN list_expr RPAREN { Expr.Const ($1, $3) }
  | CONNAME                    { Expr.Const ($1, [])}
;
list_expr:
    expr                       { [$1] }
  | expr COMMA list_expr       { $1 :: $3 }
;
