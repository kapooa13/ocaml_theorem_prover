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

let e = Expr.Compose [Expr.Var 'f'; Expr.Var 'g'; Expr.Const ("zip", [Expr.Compose [Expr.Var 'm'; Expr.Var 'n']; Expr.Compose [Expr.Var 'p'; Expr.Var 'q']])]
let () = print_endline (Expr.string_of_expr e)
(* let () = List.iter (fun x -> print_endline (Rewrite.string_of_subexpr x)) (Rewrite.subExprs (Expr.compose [a; b])) *)
let () = List.iter (fun x -> print_endline (Rewrite.string_of_subexpr x)) (Rewrite.subExprs e)
(* 
let n = 3
let l = List.map (fun x -> x + 2) (Rewrite.take (n - 2) (List.init (n - 1) Fun.id))
let s = List.init (n - 1) Fun.id

let prow i = List.map (fun x -> (i, x)) s
let prod = List.concat (List.map (fun x -> prow x) l)

let () = print_endline (String.concat " " (List.map string_of_int l))
let () = print_endline (String.concat " " (List.map string_of_int s))

let print_tuples l = List.iter (fun (a, b) -> Printf.printf "(%d, %d) " a b) l

let () = print_tuples prod
let () = print_endline ("")
 *)