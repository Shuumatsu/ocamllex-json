open Base
open Stdio
open Lib

let parsed_json =
  let parse = Parser.prog Lexer.lexing in
  let lexbuf = Lexing.from_channel stdin in
  parse lexbuf

;;
match parsed_json with
| None ->
  ()
| Some v ->
  Json.show_value v |> Stdio.printf "%s\n";
  Json.serialize v |> Stdio.printf "%s\n" ;
