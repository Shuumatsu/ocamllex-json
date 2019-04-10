{
type token =
  | Null
  | True
  | False
  | String of string
  | Int of int
  | Float of float
  | Left_bracket
  | Right_bracket
  | Left_brace
  | Right_brace
  | Comma
  | Colon
  | Eof

exception SyntaxError of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let white_space = [' ' '\t']
let new_line = ['\n']

rule token = parse
  | white_space+ { token lexbuf }
  | new_line { Lexing.new_line lexbuf ; token lexbuf }
  | digit+  { Int(int_of_string @@ Lexing.lexeme lexbuf) }
  | digit+'.'digit+  { Float(float_of_string @@ Lexing.lexeme lexbuf) }
  | '"' { String (string (Buffer.create 17) lexbuf) }
  | '[' { Left_bracket }
  | ']' { Right_bracket }
  | '{' { Left_brace }
  | '}' { Right_brace }
  | ',' { Comma }
  | ':' { Colon }
  | eof { Eof }
  | _ { raise @@ SyntaxError ("unknown input: " ^ Lexing.lexeme lexbuf) }
 
and string buf = parse
  | [^'"' '\n'] { Buffer.add_string buf @@ Lexing.lexeme lexbuf
                ; string buf lexbuf
                }
  | '\n' { Buffer.add_string buf @@ Lexing.lexeme lexbuf
         ; Lexing.new_line lexbuf
         ; string buf lexbuf
         }
  | '"' { Buffer.contents buf }

{
let to_string = function
  | Null ->
      "Null"
  | True ->
      "True"
  | False ->
      "False"
  | String str ->
      "String(" ^ str ^ ")"
  | Int i ->
      "Int(" ^ string_of_int i ^ ")"
  | Float f ->
      "Float(" ^ string_of_float f ^ ")"
  | Left_bracket ->
      "Left_bracket([)"
  | Right_bracket ->
      "Right_bracket(])"
  | Left_brace ->
      "Left_brace({)"
  | Right_brace ->
      "Right_bracket(})"
  | Comma ->
      "Comma"
  | Colon ->
      "Colon"
  | Eof ->
      "Eof"

let lexing_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  let rec loop accu = function
    | Eof ->
        to_string Eof :: accu |> List.rev
    | x ->
        loop (to_string x :: accu) (token lexbuf)
  in
  loop [] (token lexbuf) |> String.concat " " |> print_endline

let () = lexing_stdin ()
}