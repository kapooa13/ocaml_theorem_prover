open Lib

let print_expr expr = 
	print_endline (Expr.string_of_expr (expr))
let print_eqn (lhs, rhs) = 
	print_endline ("lhs: " ^ (Expr.string_of_expr lhs) ^ "\nrhs: " ^ (Expr.string_of_expr rhs))
let print_law law = 
	print_endline (Law.string_of_law law)

let filters =
  List.map
    Rewrite.parse_law
    [ "definition filter: filter p = concat.map(guard p)";
      "definition guard:  guard p  = if(p, wrap, nil)"
    ]
let ifs =
  List.map
    Rewrite.parse_law
    [ "if over composition: if(p, f, g) . h = if(p.h, f.h, g.h)";
      "composition over if: h . if(p, f, g) = if(p,   h.f, h.g)"
    ]
let pairs =
  List.map
    Rewrite.parse_law
    [ "definition fst:     fst . pair(f, g) = f";
      "definition snd:     snd . pair(f, g) = g";
      "definition cross:   cross(f, g)      = pair(f . fst, g . snd)";
      "pair fusion:        pair(f,g) . h    = pair(f . h, g . h)"
    ]
let others =
  List.map
    Rewrite.parse_law
    [ "nil constant:   nil . f        = nil";
      "nil natural:    map f . nil    = nil";
      "wrap natural:   map f . wrap   = wrap . f";
      "concat natural: map f . concat = concat . map (map f)";
      "map functor:    map f . map g  = map (f . g)"
    ]

let laws = filters @ ifs @ pairs @ others

let () = print_endline (Rewrite.prove laws "filter p . map f = map f . filter (p . f)")

