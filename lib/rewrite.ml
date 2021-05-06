
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
	let sexprs = List.map subExprs es in
	let f idx sube = Subexpr ((get_expr_subexpr sube), (Arg (idx, get_loc_subexpr sube))) in
	List.concat (List.mapi (fun idx sube -> List.map (f idx) sube) sexprs)

and subExprs expr =
	match expr with
	| Expr.Var _         -> [Subexpr (expr, All)]
	| Expr.Const (_, es) -> Subexpr (expr, All) :: args es
	| Expr.Compose es    -> Subexpr (expr, All) :: segments es @ args es

and segments es =
	let n = List.length es in
	let l = List.map (fun x -> x + 2) (take (n - 2) (List.init (n - 1) Fun.id)) in
	let s = List.init (n - 1) Fun.id in
	let prow i = List.map (fun x -> (i, x)) s in
	let prod = List.concat (List.map (fun x -> prow x) l) in
	(List.map (fun (l, s) -> Subexpr (Expr.compose (take l (drop s es)), Seg (s, l))) prod)
