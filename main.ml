open Lib

let filters =
  List.map
    Prove.parse_law
    [ "definition filter: filter p = concat.map(guard p)";
      "definition guard:  guard p  = if(p, wrap, nil)"
    ]
let ifs =
  List.map
    Prove.parse_law
    [ "if over composition: if(p, f, g) . h = if(p.h, f.h, g.h)";
      "composition over if: h . if(p, f, g) = if(p,   h.f, h.g)"
    ]
let pairs =
  List.map
    Prove.parse_law
    [ "definition fst:     fst . pair(f, g) = f";
      "definition snd:     snd . pair(f, g) = g";
      "definition cross:   cross(f, g)      = pair(f . fst, g . snd)";
      "pair fusion:        pair(f,g) . h    = pair(f . h, g . h)"
    ]
let others =
  List.map
    Prove.parse_law
    [ "nil constant:   nil . f        = nil";
      "nil natural:    map f . nil    = nil";
      "wrap natural:   map f . wrap   = wrap . f";
      "concat natural: map f . concat = concat . map (map f)";
      "map functor:    map f . map g  = map (f . g)"
    ]

let laws = filters @ ifs @ pairs @ others

let () = print_endline (Prove.prove laws "fst . pair(a, a)  = snd . pair (a, a)")
let () = print_endline (Prove.prove laws "fst . pair(a, b) = b")

let () = print_endline (Prove.prove laws "filter p . map f  = map f . filter (p . f)")
let () = print_endline (Prove.prove laws "map (f . g) . nil = nil . f")
let () = print_endline (Prove.prove laws "fst . pair(a, b) . snd . pair(a, b) = a . b")
