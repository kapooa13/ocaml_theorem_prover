(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t']          { token lexbuf }     (* skip blanks *)
  | ['\n' ]             { EOL }
  | ['a'-'z']           as vname { VARNAME(vname) }
  | ['a'-'z']['a'-'z']+ as cname { CONNAME(cname) }
  | '.'                 { COMPOSE }
  | ','                 { COMMA }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | eof                 { raise Eof }
