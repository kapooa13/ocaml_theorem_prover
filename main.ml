open Lib

(* let () = print_endline "Hello, World!" *)

(* Exprs *)

(* let a = Expr.Var 'a'
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
 *)
(* Laws *)

(* let e = Expr.Compose [Expr.Var 'f'; Expr.Var 'g'; Expr.Const ("zip", [Expr.Compose [Expr.Var 'm'; Expr.Var 'n']; Expr.Compose [Expr.Var 'p'; Expr.Var 'q']])]
let () = print_endline (Expr.string_of_expr e)
let () = List.iter (fun x -> print_endline (Rewrite.string_of_subexpr x)) (Rewrite.get_subexprs e)
 *)(* 
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

let pair_expr = Expr.Const ("pair", [Var 'f'; Var 'g'])
let fst_expr = Expr.Const ("fst", [])
let snd_expr = Expr.Const ("snd", [])
let fst_law = Law.Law ("definition fst", Expr.compose [fst_expr; pair_expr], Expr.Var 'f')
let snd_law = Law.Law ("definition snd", Expr.compose [snd_expr; pair_expr], Expr.Var 'g')
let laws = [fst_law; snd_law]
let ppair_expr = Expr.Const ("pair", [Var 'a'; Var 'a'])
let pair_proof = Rewrite.prove laws (Expr.compose [fst_expr; ppair_expr], Expr.compose [snd_expr; ppair_expr])
(* let () = print_endline pair_proof *)

exception ParseError of string

let parse_expr (s : string) =
	Parser.main Lexer.token (Lexing.from_string s)

let parse_eqn (s : string) =
	let eqn_list = String.split_on_char '=' s in
	if List.length eqn_list = 2 then (
		let lhs_expr = parse_expr (List.nth eqn_list 0) in
		let rhs_expr = parse_expr (List.nth eqn_list 1) in
		(lhs_expr, rhs_expr)
	) else (
		raise (ParseError "Equation not of expected form 'lhs = rhs'")
	)

let parse_law (s : string) =
	let law_list = String.split_on_char ':' s in
	if List.length law_list = 2 then (
		let law_name = List.nth law_list 0 in
		let (lhs, rhs) = parse_eqn (List.nth law_list 1) in
		Law.Law (law_name, lhs, rhs)
	) else (
		raise (ParseError "Law not of expected form 'law_name : lhs = rhs'")
	)


let print_expr expr = 
	print_endline (Expr.string_of_expr (expr))
let print_eqn (lhs, rhs) = 
	print_endline ("lhs: " ^ (Expr.string_of_expr lhs) ^ "\nrhs: " ^ (Expr.string_of_expr rhs))
let print_law law = 
	print_endline (Law.string_of_law law)


let () = print_endline (Expr.string_of_expr (parse_expr "a . b . c"))
let () = print_endline (Expr.string_of_expr (parse_expr "zip(x)"))
let () = print_endline (Expr.string_of_expr (parse_expr "zip(x, f, g)"))
let () = print_endline (Expr.string_of_expr (parse_expr "f . g . h . zip (x , y)"))
let () = print_eqn (parse_eqn "fst . pair(a, a) = snd . pair (a, a)")
let () = print_law (parse_law "definition fst : fst . pair(f, g) = f")



