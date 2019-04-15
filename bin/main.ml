open Base
open Stdio
open Lib

let lexing_stdin () =
  let parse = Parser.prog Lexer.lexing in
  let lexbuf = Lexing.from_channel stdin in
  match parse lexbuf with None -> "" | Some v -> Json.serialize v

;;
lexing_stdin () |> Stdio.printf "%s\n"
