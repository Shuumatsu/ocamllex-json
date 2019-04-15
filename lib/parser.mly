%token <int> Int
%token <float> Float 
%token <string> String
%token True 
%token False
%token Null 
%token Left_bracket
%token Right_bracket
%token Left_brace
%token Right_brace
%token Comma
%token Colon
%token Eof
%start <Json.value option> prog
%%

prog:
  | Eof       { None }
  | v = value { Some v }

value:
  | i = Int { `Int i }
  | str = String { `String str }
  | f = Float { `Float f }
  | True { `Bool true }
  | False { `Bool false }
  | Null { `Null }
  | Left_bracket; ls = list_fields; Right_bracket { `List ls }
  | Left_brace; ls = obj_fields; Right_brace { `Assoc ls }

obj_fields:
  ls = separated_list(Comma, obj_field) { ls } 

obj_field:
  k = String; Colon; v = value  { (k, v) } 

list_fields:
  ls = separated_list(Comma, value) { ls }  