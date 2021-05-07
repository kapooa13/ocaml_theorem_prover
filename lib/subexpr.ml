
type location =
	| All
	| Seg of int * int
	| Arg of int * location

let rec string_of_location loc =
	match loc with
	| All -> "All"
	| Seg (idx, len) -> "Seg " ^ string_of_int idx ^ " " ^ string_of_int len
	| Arg (idx, aloc) -> "Arg " ^ string_of_int idx ^ " (" ^ string_of_location aloc ^ ")"

type subexpr = 
	Subexpr of Expr.expr * location

let string_of_subexpr sube =
	match sube with
	| Subexpr (expr, loc) -> "subexpression " ^ Expr.string_of_expr expr ^ " located at " ^ string_of_location loc

let get_expr_subexpr sube =
	match sube with
	| Subexpr (expr, _) -> expr

let get_loc_subexpr sube =
	match sube with
	| Subexpr (_, loc) -> loc

let rec take n xs =
	match xs with
	| [] -> []
	| y :: ys -> if n <= 0 then [] else y :: take (n - 1) ys

let rec drop n xs =
	match xs with
	| [] -> []
	| _ :: ys -> if n <= 0 then xs else drop (n - 1) ys

let rec args es =
	let sexprs = List.map get_subexprs es in
	let f idx sube = Subexpr ((get_expr_subexpr sube), (Arg (idx, get_loc_subexpr sube))) in
	List.concat (List.mapi (fun idx sube -> List.map (f idx) sube) sexprs)

and get_subexprs expr =
	match expr with
	| Expr.Var _         -> [Subexpr (expr, All)]
	| Expr.Const (_, es) -> Subexpr (expr, All) :: args es
	| Expr.Compose es    -> Subexpr (expr, All) :: segments es @ args es

and segments es =
	let n = List.length es in
	let l = List.map (fun x -> x + 2) (take (n - 2) (List.init (n - 1) Fun.id)) in
	let s = List.init (n - 1) Fun.id in
	let prow i = List.map (fun x -> (i, x)) s in
	let prod = List.concat (List.map prow l) in
	(List.map (fun (l, s) -> Subexpr (Expr.compose (take l (drop s es)), Seg (s, l))) prod)

let rec replace expr loc replacement =
	match loc with
	 | All -> replacement
	 | Arg (idx, aloc) -> (
	 	match expr with
	 	| Expr.Const (f, xs) -> (
	 		let replaced_expr = replace (List.nth xs idx) aloc replacement in
	 		Expr.Const (f, replace_at idx replaced_expr xs)
	 	)
	 	| Expr.Compose xs -> (
	 		let replaced_expr = replace (List.nth xs idx) aloc replacement in
	 		Expr.compose (replace_at idx replaced_expr xs)
	 	)
	 	| _ -> expr
	 )
	 | Seg (idx, len) -> (
	 	match expr with
	 	| Expr.Compose xs -> Expr.compose (replace_seg idx len replacement xs)
	 	| _ -> expr
	 )

and replace_at pos value xs =
	List.mapi (fun idx x -> if pos == idx then value else x) xs

and replace_seg pos len seg xs =
	(take pos xs) @ (seg :: (drop (pos + len) xs))

let rec rewrite (llaws, rlaws) expr =
	let laws = llaws @ rlaws in
	let subexprs = get_subexprs expr in
	let prow i = List.map (fun x -> (i, x)) subexprs in
	let prod = List.concat (List.map prow laws) in
	List.concat (List.map (fun (law, sube) -> applyLaw law sube expr) prod)

and applyLaw law sube expr = 
	match law with
	| Law.Law (lname, lhs, rhs) -> (
		match sube with
		| Subexpr (isub, iloc) -> (
			let matchings = Subst.mmatch lhs isub in
			let replaced = replace expr iloc rhs in
			List.map (fun sub -> (lname, Subst.apply_subst sub replaced)) matchings
		)
	)

let rec repeatedly rw current =
	match (rw current) with
	| (name, next) :: _ -> (name, next) :: repeatedly rw next
	| [] -> []

let calculate pls expr =
	Law.Calc (expr, List.map (fun (lname, expr) -> Law.Step (lname, expr)) (repeatedly (rewrite pls) expr))
