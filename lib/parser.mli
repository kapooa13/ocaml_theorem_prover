type token =
  | VARNAME of (char)
  | CONNAME of (string)
  | COMPOSE
  | COMMA
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
