
type subst = (Expr.varname * Expr.expr) list

let rec binding subs vname =
	match subs with
	| [] -> Expr.Var vname
	| (u, e) :: s -> if u == vname then e else binding s vname

let rec apply_subst subs expr =
	match expr with
	| Expr.Var v -> binding subs v
	| Expr.Const (f, xs) -> Expr.Const (f, List.map (apply_subst subs) xs)
	| Expr.Compose xs -> (
		let ys = Expr.compose xs in
		match ys with
		| Expr.Compose ss -> Expr.compose (List.map (apply_subst subs) ss)
		| _ -> ys
	)

let rec split n xs =
	match xs with
	| []       -> []
	| y :: ys  -> (
		match n with
		| 0 -> [] 
		| 1 -> [xs]
		| _ -> [[y]] @ split (n - 1) ys
	)

let rec parts n xs =
	match n with
	| 0 -> if xs = [] then [[]] else []
	| _ -> (
		match xs with
		| [] -> []
		| y :: ys -> (
			let case_1 = List.map (fun x -> [[y]] @ x) (parts (n - 1) ys) in
			let case_2 = List.map (fun x -> ([y] @ List.hd x) :: List.tl x) (parts n ys) in
 			case_1 @ case_2
		)
	)


let rec xmatch subs e1 e2 =
	match e1 with
	| Expr.Var v -> (
		if binding subs v = e1 then [(v, e2) :: subs]
		else if binding subs v = e2 then [subs]
		else []
	) 
	| Expr.Const (f, patterns) -> (
		match e2 with
		| Expr.Const (g, exprs) -> if f = g then xmatchlist subs (List.combine patterns exprs) else []
		| _ -> []
	)
	| Expr.Compose patterns -> (
		match e2 with
		| Expr.Compose exprs -> (
			let zss = parts (List.length patterns) exprs in
			let alignCompose patterns = List.map (fun zs -> List.combine patterns (List.map Expr.compose zs)) zss in
			List.concat (List.map (fun x -> xmatchlist subs x) (alignCompose patterns))
		)
		| _ -> []
	)

and xmatchlist subs list_expr_pairs =
	match list_expr_pairs with
	| [] -> [subs] 
	| (pattern, expr) :: rest -> (
		let nextSubs = xmatch subs pattern expr in
		List.concat (List.map (fun nextSub -> xmatchlist nextSub rest) nextSubs)
	)
