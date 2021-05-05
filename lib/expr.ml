
type expr = 
	| Var of char 
	| Const of string * expr list
	| Compose of expr list

let rec string_of_expr expr =
	match expr with
	| Var c -> "Var " ^ String.make 1 c
	| Const (f, xs) ->
	  (match xs with
	  	  | []  -> f
	  	  | x :: [] -> f ^ " " ^ string_of_expr x
	  	  | y :: ys -> f ^ "(" ^ (List.fold_left (fun x y -> x ^ ", " ^ y) (string_of_expr y) (List.map string_of_expr ys)) ^ ")")
	| Compose xs -> List.fold_left (fun x y -> x ^ " . " ^ y) (string_of_expr (List.hd xs)) (List.map string_of_expr (List.tl xs))

let rec getExprs expr =
	match expr with
	| Compose xs -> List.fold_left (@) [] (List.map getExprs xs)
	| _          -> [expr]

exception ComposeError of string

(*
If compose returns `Compose xs`, then it should satisfy the following property:
   * length xs >= 2
   * xs itself has no element in the form `Compose ys`
*)
let compose xs =
	match getExprs (Compose xs) with
	| []      -> raise (ComposeError "Invalid argument to smart constructor")
	| x :: [] -> x
	| _       -> Compose (getExprs (Compose xs))

let complexity expr =
	match expr with
	| Var _ -> 1
	| Const _ -> 1
	| Compose exprs -> List.length exprs

let simpleton expr =
	match expr with
	| Var _ -> true
	| Const (_, exprs) -> List.length exprs = 0
	| _           -> false

let singleton xs =
	match xs with
	| _ :: [] -> true
	| _  -> false
