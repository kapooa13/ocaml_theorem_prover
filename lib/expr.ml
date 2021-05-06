
type varname = char

type expr = 
	| Var of varname
	| Const of string * expr list
	| Compose of expr list

let simpleton expr =
	match expr with
	| Var _ -> true
	| Const (_, exprs) -> List.length exprs = 0
	| _           -> false

let rec string_of_expr expr =
	match expr with
	| Var c -> String.make 1 c
	| Const (f, xs) ->(
		match xs with
	  	  | []  -> f
	  	  | x :: [] -> (
	  	  	if simpleton x 
	  	  	then f ^ " " ^ string_of_expr x
	  	  	else f ^ "(" ^ string_of_list_expr " , " xs ^ ")"
	  	  )
	  	  | _ -> f ^ "(" ^ string_of_list_expr " , " xs ^ ")"
	  	)
	| Compose xs -> string_of_list_expr " . " xs

and string_of_list_expr sep xs =
	List.fold_left (fun x y -> x ^ sep ^ y) (string_of_expr (List.hd xs)) (List.map string_of_expr (List.tl xs))

let rec get_exprs expr =
	match expr with
	| Compose xs -> List.fold_left (@) [] (List.map get_exprs xs)
	| _          -> [expr]

exception ComposeError of string

(*
If compose returns `Compose xs`, then it should satisfy the following property:
   * length xs >= 2
   * xs itself has no element in the form `Compose ys`
*)
let compose xs =
	match get_exprs (Compose xs) with
	| []      -> raise (ComposeError "Invalid argument to smart constructor")
	| x :: [] -> x
	| _       -> Compose (get_exprs (Compose xs))

let complexity expr =
	match expr with
	| Var _ -> 1
	| Const _ -> 1
	| Compose exprs -> List.length exprs


let singleton xs =
	match xs with
	| _ :: [] -> true
	| _  -> false
