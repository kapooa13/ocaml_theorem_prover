let () = print_endline "Hello, World!"

type expr = 
	| Var of char 
	| Const of string * expr list
	| Compose of expr list

let rec string_of_expr expr =
	match expr with
	| Var c -> "Var " ^ String.make 1 c
	| Const (name, xs) ->
	  (match xs with
	  	  | [] -> name
	  	  | _  -> name)
	| Compose xs -> "Compose"

(*
If compose returns `Compose xs`, then it should satisfy the following property:
   * length xs >= 2
   * xs itself has no element in the form `Compose ys`
*)

let rec getExprs expr =
	match expr with
	| Compose xs -> List.fold_left (@) [] (List.map getExprs xs)
	| _          -> [expr]

let () = print_endline "End"
let a = Var 'a'
let b = Var 'b'
let c = getExprs (Compose [a])
let () = print_endline (String.concat " " (List.map string_of_expr c))

exception ComposeError of string

let compose xs =
	match getExprs (Compose xs) with
	| []      -> raise (ComposeError "Invalid argument to smart constructor")
	| x :: [] -> x
	| _       -> Compose (getExprs (Compose xs))

let x = Var 'x'
let y = Var 'y'
let () = print_endline (Bool.to_string (compose [x] = x))
let () = print_endline (Bool.to_string (compose [Const ("zip", [x; y])] = Const ("zip", [x; y])))
let () = print_endline (Bool.to_string (compose [a; b] = Compose [a; b]))
(* let () =  *)

