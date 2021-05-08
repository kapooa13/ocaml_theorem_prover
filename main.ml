open! Lib
open! Js_of_ocaml

let prove_statement_with_laws (laws : Law.law list) (stmt : string) =
  Prove.prove laws stmt

(* uses pre defined laws *)
let prove_statement (stmt : string) =
  prove_statement_with_laws Prove.defined_laws stmt

let parse_js_laws laws_js_str =
  let laws_str = laws_js_str |> Js.to_string in
  let laws_list = String.split_on_char ';' laws_str in
  List.map (fun x -> x |> String.trim |> Prove.parse_law) laws_list

let () =
  Js.export_all
  (object%js
    method prove laws str = 
      let parsed_laws = parse_js_laws laws in
        str
        |> Js.to_string
        |> prove_statement_with_laws parsed_laws
        |> Js.string
  end)
