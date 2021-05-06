type token =
  | VARNAME of (char)
  | CONNAME of (string)
  | COMPOSE
  | COMMA
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  259 (* COMPOSE *);
  260 (* COMMA *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VARNAME *);
  258 (* CONNAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\004\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\008\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\004\000\007\000"

let yydgoto = "\002\000\
\005\000\010\000\011\000"

let yysindex = "\002\000\
\012\255\000\000\000\000\010\255\000\000\003\255\012\255\012\255\
\000\000\013\255\002\255\015\255\012\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\254\254\000\000\000\000\000\000\000\000\
\000\000\014\255\000\000\005\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\006\000"

let yytablesize = 20
let yytable = "\006\000\
\005\000\005\000\001\000\005\000\005\000\008\000\012\000\014\000\
\003\000\009\000\003\000\003\000\003\000\004\000\007\000\008\000\
\013\000\008\000\015\000\006\000"

let yycheck = "\001\000\
\003\001\004\001\001\000\006\001\007\001\003\001\008\000\006\001\
\004\001\007\001\006\001\007\001\001\001\002\001\005\001\003\001\
\004\001\003\001\013\000\006\001"

let yynames_const = "\
  COMPOSE\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  VARNAME\000\
  CONNAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 12 "parser.mly"
                            ( _1 )
# 81 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 15 "parser.mly"
                               ( Expr.Var _1 )
# 88 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 16 "parser.mly"
                               ( Expr.compose [_1; _3] )
# 96 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'list_expr) in
    Obj.repr(
# 17 "parser.mly"
                                    ( Expr.Const (_1, _3) )
# 104 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 18 "parser.mly"
                               ( Expr.Const (_1, []))
# 111 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                               ( [_1] )
# 118 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 22 "parser.mly"
                               ( _1 :: _3 )
# 126 "parser.ml"
               : 'list_expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
