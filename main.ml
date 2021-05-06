open Lib

let () = print_endline "Hello, World!"

(* Exprs *)

let a = Expr.Var 'a'
let b = Expr.Var 'b'
let c = Expr.get_exprs (Expr.Compose [a])
let () = print_endline (String.concat " " (List.map Expr.string_of_expr c))

let x = Expr.Var 'x'
let y = Expr.Var 'y'
let () = print_endline (Bool.to_string (Expr.compose [x] = x))
let () = print_endline (Bool.to_string (Expr.compose [Expr.Const ("zip", [x; y])] = Expr.Const ("zip", [x; y])))
let () = print_endline (Bool.to_string (Expr.compose [a; b] = Expr.Compose [a; b]))

let () = print_endline "End"
let a = Expr.Var 'a'
let b = Expr.Var 'b'
let c = Expr.get_exprs (Expr.Compose [a])
let () = print_endline (String.concat " " (List.map Expr.string_of_expr c))

(* Laws *)


