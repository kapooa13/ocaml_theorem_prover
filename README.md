# ocaml-theorem-prover

Automatic theorem prover written in OCaml.

## Building

To build the project, clone the repo, and type `make`. This builds the lexer, parser, the `main.exe` executable, and then executes it.

## Example

We give our `prove` function a list of laws along with a string for the expression we would like to prove. It then proceeds to parse the expression and attempt to prove that the LHS can be reduced to RHS using the given laws. The output is shown below.

```
TPT: fst . pair(a , a) == snd . pair(a , a)
LHS:
  fst . pair(a , a)
=  { definition fst }
  a
=  { definition snd }
  snd . pair(a , a)
RHS:
```

Of course, if we give it something nonsensical, it says it doesn't know how to prove it by showing a mystery step that magically solves our proof.

```
TPT: fst . pair(a , b) == b
LHS:
  fst . pair(a , b)
=  { definition fst }
  a
=  { ... ??? ... }
  b
RHS:
```

## Lexing and Parsing

The project uses `ocamllex` and `ocamlyacc` for generating the lexer and parser respectively. Once we define a grammar for the expression type, building off of that to create a parser for laws and equations is fairly easy. Some of the issues with regards to precedence were solved later on. See `lexer.mll` and `parser.mly` for implementation details.

## Testing

We use `ppx_inline_test` to write out tests for every type, their respective methods, along with tests for the parsing methods. The tests can be run by executing `make test`.

## More Examples

Below outlined are some more examples. They can all be found in `main.ml`.

### Interchangeability of map and filter

```
TPT: filter p . map f == map f . filter(p . f)
LHS:
  filter p . map f
=  { definition filter }
  concat . map(guard p) . map f
=  { map functor }
  concat . map(guard p . f)
=  { definition guard }
  concat . map(if(p , wrap , nil) . f)
=  { if over composition }
  concat . map(if(p . f , wrap . f , nil . f))
=  { nil constant }
  concat . map(if(p . f , wrap . f , nil))
=  { wrap natural }
  concat . map(if(p . f , map f . wrap , nil))
=  { nil natural }
  concat . map(if(p . f , map f . wrap , map f . nil))
=  { composition over if }
  concat . map(map f . if(p . f , wrap , nil))
=  { map functor }
  concat . map(map f) . map(if(p . f , wrap , nil))
=  { concat natural }
  map f . concat . map(if(p . f , wrap , nil))
=  { definition guard }
  map f . concat . map(guard(p . f))
=  { definition filter }
  map f . filter(p . f)
RHS:
```

### Nil composition

```
TPT: map(f . g) . nil == nil . f
LHS:
  map(f . g) . nil
=  { nil natural }
  nil
=  { nil constant }
  nil . f
RHS:
```

### Composition of pairs

```
TPT: fst . pair(a , b) . snd . pair(a , b) == a . b
LHS:
  fst . pair(a , b) . snd . pair(a , b)
=  { definition fst }
  a . snd . pair(a , b)
=  { definition snd }
  a . b
RHS:
```