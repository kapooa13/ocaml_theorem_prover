
exception ParseError of string

let newline = "\n"

let parse_expr (s : string) =
	Parser.main Lexer.token (Lexing.from_string s)

let parse_eqn (s : string) =
	let eqn_list = String.split_on_char '=' s in
	if List.length eqn_list = 2 then (
		let lhs_expr = List.nth eqn_list 0 |> String.trim |> parse_expr in
		let rhs_expr = List.nth eqn_list 1 |> String.trim |> parse_expr in
		(lhs_expr, rhs_expr)
	) else (
		raise (ParseError ("Equation `" ^ s ^ "` not of expected form `lhs = rhs`"))
	)

let parse_law (s : string) =
	let law_list = String.split_on_char ':' s in
	if List.length law_list = 2 then (
		let law_name = List.nth law_list 0 |> String.trim in
		let (lhs, rhs) = (List.nth law_list 1) |> String.trim |> parse_eqn in
		Law.Law (law_name, lhs, rhs)
	) else (
		raise (ParseError ("Law `" ^ s ^ "` not of expected form `law_name: lhs = rhs`"))
	)

let print_expr expr = 
	print_endline (Expr.string_of_expr (expr))
let print_eqn (lhs, rhs) = 
	print_endline ("lhs: " ^ (Expr.string_of_expr lhs) ^ newline ^ "rhs: " ^ (Expr.string_of_expr rhs))
let print_law law = 
	print_endline (Law.string_of_law law)

let rec prove laws eqn =
	try
		let (lhs, rhs) = parse_eqn eqn in (
			"TPT: " ^ Expr.string_of_expr lhs ^ " == " ^ Expr.string_of_expr rhs ^ newline ^
			"LHS:"  ^ Law.string_of_calc (prove_eqn laws (lhs, rhs)) ^ "RHS:"
		)
	with e ->
        let msg = Printexc.to_string e
        and stack = Printexc.get_backtrace () in
        	if eqn = "" then "Error: No equation to prove" ^ newline ^ msg ^ stack ^ newline
      		else "Calling `prove` on equation: " ^ eqn ^ newline ^ msg ^ stack ^ newline

and prove_eqn laws (lhs, rhs) =
	let (basic, others) = List.partition Law.basic_law laws in
	Law.paste (Subexpr.calculate (basic, others) lhs) (Subexpr.calculate (basic, others) rhs)

(* Pre defined laws *)

let filters =
  List.map
    parse_law
    [ "definition filter: filter p = concat.map(guard p)";
      "definition guard:  guard p  = if(p, wrap, nil)"
    ]
let ifs =
  List.map
    parse_law
    [ "if over composition: if(p, f, g) . h = if(p.h, f.h, g.h)";
      "composition over if: h . if(p, f, g) = if(p,   h.f, h.g)"
    ]
let pairs =
  List.map
    parse_law
    [ "definition fst:     fst . pair(f, g) = f";
      "definition snd:     snd . pair(f, g) = g";
      "definition cross:   cross(f, g)      = pair(f . fst, g . snd)";
      "pair fusion:        pair(f,g) . h    = pair(f . h, g . h)"
    ]
let others =
  List.map
    parse_law
    [ "nil constant:   nil . f        = nil";
      "nil natural:    map f . nil    = nil";
      "wrap natural:   map f . wrap   = wrap . f";
      "concat natural: map f . concat = concat . map (map f)";
      "map functor:    map f . map g  = map (f . g)"
    ]

let defined_laws = filters @ ifs @ pairs @ others

(* Parse tests *)

let f = Expr.Var 'f'
let g = Expr.Var 'g'
let fst = Expr.Const ("fst", [])
let nilexpr = Expr.Const ("nil", [])
let mapexpr expr = Expr.Const ("map", [expr])

let%test _ = parse_expr "concat.map(guard p)" = Expr.compose [Expr.Const ("concat", []); Expr.Const ("map", [Expr.Const ("guard", [Expr.Var 'p'])])]
let%test _ = parse_expr "map f . map g" = Expr.compose [Expr.Const ("map", [f]); Expr.Const ("map", [Expr.Var 'g'])]

let%test _ = parse_eqn "nil . f = nil" = (Expr.compose [nilexpr; f], nilexpr)
let%test _ = parse_eqn "fst . pair(f, g) = f" = (Expr.compose [Expr.Const ("fst", []); Expr.Const ("pair", [f; g])], f)

let%test _ = parse_law "map functor: map f . map g = map (f . g)" = Law.Law ("map functor", Expr.compose [mapexpr f; mapexpr g], mapexpr (Expr.compose [f; g]))
