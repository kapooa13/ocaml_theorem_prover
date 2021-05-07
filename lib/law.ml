
(* Lawname *)
type lawname = string

(* Law *)
type law =
    Law of lawname * Expr.expr * Expr.expr

let string_of_law law =
	match law with
	| Law (name, lhs, rhs) ->  name ^ ": " ^ Expr.string_of_expr lhs ^ " = " ^ Expr.string_of_expr rhs

(* Step *)
type step =
	Step of lawname * Expr.expr

let get_step_expr step =
	match step with
	| Step (_, expr) -> expr

let get_step_name step =
	match step with
	| Step (lname, _) -> lname

(* Calculation *)
type calculation =
	Calc of Expr.expr * step list

let get_calc_expr calc =
	match calc with
	| Calc (expr, _) -> expr

let get_calc_steps calc =
	match calc with
	| Calc (_, steps) -> steps 

(* Other functions *)
let basic_law law =
	match law with
	| Law (_, lhs, rhs) -> Expr.complexity lhs > Expr.complexity rhs

let rec conclusion calc =
	match calc with
	| Calc (expr, []) -> expr
	| Calc (_, step :: rest) ->
		(match step with
		| Step (_, expr) -> conclusion (Calc (expr, rest)))

let link expr1 expr2 =
	if expr1 = expr2 then [] else [Step ("... ??? ...", expr2)]

let init xs =
	List.rev (List.tl (List.rev xs))

let last xs =
	List.hd (List.rev xs)

let reverseCalc calc = 
	match calc with
	| Calc (_, []) -> calc
	| Calc (expr, steps) -> (
		let exprs = List.map get_step_expr steps in
		let laws = List.map get_step_name steps in
	    let rev_steps = List.rev (List.map (fun (x, y) -> Step (x, y)) (List.combine laws (expr :: init exprs))) in
	    Calc (last exprs, rev_steps)
	)

let paste lcalc rcalc =
	let x = conclusion lcalc in
	let y = conclusion rcalc in
	Calc (get_calc_expr lcalc, get_calc_steps lcalc @ link x y @ get_calc_steps (reverseCalc rcalc))

let string_of_step step =
	match step with
	| Step (lname, expr) -> "=  { " ^  lname ^ " }\n  " ^ Expr.string_of_expr expr ^ "\n"

let string_of_calc calc =
	match calc with
	| Calc (expr, steps) -> "\n  " ^ Expr.string_of_expr expr ^ "\n" ^ String.concat "" (List.map string_of_step steps)

(* Law tests *)

let%test _ =
  let nil = Expr.Const ("nil", []) in 
  basic_law (Law ("nil Expr.conststant", (Expr.Compose [nil; Expr.Var 'f']), nil))

let%test _ =
  let ide = Expr.Const ("id", []) in
  let mp_ide = Expr.Const ("map", [ide]) in 
  basic_law (Law ("map id, reverse", ide, mp_ide)) = false

let%test _ =
  let nil = Expr.Const ("nil", []) in
  let mapf = Expr.Const ("map", [Expr.Var 'f']) in 
  basic_law (Law ("nil natural", (Expr.Compose [mapf; nil]), nil))

(* Conclusion tests *)

let c0 = Calc (Expr.Var 'f', [])
let c1 = Calc (Expr.Var 'f', [Step ("reason1", Expr.Var 'g'); Step ("reason2", Expr.Var 'h')])
let e5 = Expr.Compose ([
			Expr.Const ("fst", []); 
			Expr.Const ("pair", [
					Expr.Compose [
							Expr.Var 'f'; 
							Expr.Const ("fst", [])
						];
					Expr.Compose [
							Expr.Var 'g';
							Expr.Const ("snd", [])
					]
				]);
			Expr.Const ("zip", [])
		])

let%test _ = conclusion c0 = Expr.Var 'f'
let%test _ = conclusion c1 = Expr.Var 'h'
let%test _ = conclusion (Calc (e5, [Step ("refl", e5)])) = e5

(* Reverse Calc tests *)

let x = Expr.Var 'x'
let a = Expr.Var 'a'
let b = Expr.Var 'b'

let%test _ = reverseCalc (Calc (x, [])) = Calc (x, [])
let%test _ = reverseCalc (Calc (x, [Step ("foo", a); Step ("bar", b)])) = (Calc(b, [Step ("bar", a); Step ("foo", x)]))


