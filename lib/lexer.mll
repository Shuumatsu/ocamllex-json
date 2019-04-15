{
open Parser

exception SyntaxError of string

let show_position lexbuf =
  let pos = Lexing.lexeme_end_p lexbuf in
  "line " ^ string_of_int pos.pos_lnum ^ ", characters "
  ^ string_of_int pos.pos_bol ^ "-" ^ string_of_int pos.pos_cnum ^ "\n"
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let white_space = [' ' '\t']
let new_line = ['\n']

rule lexing = parse
  | white_space+ { lexing lexbuf }
  | new_line { Lexing.new_line lexbuf ; lexing lexbuf }
  | "true" { True }
  | "false" { False }
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
  | _ { raise
@@ SyntaxError
     ( "unknown input: " ^ Lexing.lexeme lexbuf ^ "\n at position "
     ^ show_position lexbuf ) }

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

}