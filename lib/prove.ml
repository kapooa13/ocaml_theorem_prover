
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

let rec prove laws eqn =
	let (lhs, rhs) = parse_eqn eqn in (
		"TPT: " ^ Expr.string_of_expr lhs ^ " == " ^ Expr.string_of_expr rhs ^ "\n" ^
		"LHS:"  ^ Law.string_of_calc (prove_eqn laws (lhs, rhs)) ^ "RHS:"
	)

and prove_eqn laws (lhs, rhs) =
	let (basic, others) = List.partition Law.basic_law laws in
	Law.paste (Subexpr.calculate (basic, others) lhs) (Subexpr.calculate (basic, others) rhs)

