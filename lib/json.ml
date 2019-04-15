type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]
[@@deriving show]

open Base

let print_string str = "\"" ^ str ^ "\""

let rec serialize = function
  | `Assoc ls ->
      "{" ^ serialize_object_fields ls ^ "}"
  | `List ls ->
      "[" ^ serialize_list_fields ls ^ "]"
  | `Int i ->
      Int.to_string i
  | `Float f ->
      Float.to_string f
  | `Bool b ->
      Bool.to_string b
  | `Null ->
      "null"
  | `String str ->
      print_string str

and serialize_object_fields ls =
  List.map ~f:(fun (k, v) -> print_string k ^ ": " ^ serialize v) ls
  |> String.concat ~sep:", "

and serialize_list_fields ls =
  List.map ~f:(fun v -> serialize v) ls |> String.concat ~sep:", "
