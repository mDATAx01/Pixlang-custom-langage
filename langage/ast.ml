open Util

type type_expression =
  | Type_int
  | Type_real
  | Type_bool
  | Type_coord
  | Type_color
  | Type_pixel
  | Type_list of type_expression
  | Type_generic

module Annotation = struct
  type t = { position : Position.t; mutable type_expr : type_expression option }

  let create pos = { position = pos; type_expr = None }
  let get_pos annotation = annotation.position
  let get_type annotation = annotation.type_expr
  let set_type annotation type_expr = annotation.type_expr <- Some type_expr
end

type binary_operator =
  | Plus
  | Minus
  | Times
  | Div
  | Rem
  | And
  | Or
  | Equal
  | Diff
  | Lt
  | Gt
  | Leq
  | Geq

type unary_operator =
  | Opposite
  | Not
  | Head
  | Tail
  | Floor
  | Real_of_int
  | Cos
  | Sin

type field_accessor =
  | Color_field
  | Coord_field
  | X_field
  | Y_field
  | Blue_field
  | Red_field
  | Green_field

type expression =
  | Const_int of int * Annotation.t
  | Const_real of float * Annotation.t
  | Const_bool of bool * Annotation.t
  | Coord of expression * expression * Annotation.t
  | Color of expression * expression * expression * Annotation.t
  | Pixel of expression * expression * Annotation.t
  | Variable of string * Annotation.t
  | Binary_operator of binary_operator * expression * expression * Annotation.t
  | Unary_operator of unary_operator * expression * Annotation.t
  | Field_accessor of field_accessor * expression * Annotation.t
  | List of expression list * Annotation.t
  | Append of expression * expression * Annotation.t

type statement =
  | Affectation of expression * expression * Annotation.t
  | Declaration of string * type_expression * Annotation.t
  | Block of statement list * Annotation.t
  | IfThenElse of expression * statement * statement * Annotation.t
  | For of
      string * expression * expression * expression * statement * Annotation.t
  | While of expression * statement * Annotation.t
  | Foreach of string * expression * statement * Annotation.t
  | Draw_pixel of expression * Annotation.t
  | Nop
  | Print of expression * Annotation.t

type argument = Argument of string * type_expression * Annotation.t
type program = Program of argument list * statement

let rec string_of_type_expression = function
  | Type_int -> "Int"
  | Type_real -> "Real"
  | Type_bool -> "Bool"
  | Type_coord -> "Coord"
  | Type_color -> "Color"
  | Type_pixel -> "Pixel"
  | Type_list type_expression ->
      "List(" ^ string_of_type_expression type_expression ^ ")"
  | Type_generic -> "'a"

let string_of_binary_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Rem -> "%"
  | And -> " And "
  | Or -> " Or "
  | Equal -> " = "
  | Diff -> " <> "
  | Lt -> " < "
  | Gt -> " > "
  | Leq -> " <= "
  | Geq -> " >= "

let string_of_unary_operator = function
  | Opposite -> "-"
  | Not -> " Not"
  | Head -> "Head"
  | Tail -> "Tail"
  | Floor -> "Floor"
  | Real_of_int -> "Real_of_int"
  | Cos -> "Cos"
  | Sin -> "Sin"

let string_of_field_accessor = function
  | Color_field -> "Color"
  | Coord_field -> "Coord"
  | X_field -> "X"
  | Y_field -> "Y"
  | Blue_field -> "Blue"
  | Red_field -> "Red"
  | Green_field -> "Green"

let rec string_of_expression = function
  | Const_int (value, _) -> string_of_int value
  | Const_real (value, _) -> string_of_float value
  | Const_bool (value, _) -> string_of_bool value
  | Coord (x, y, _) ->
      "Coord(" ^ string_of_expression x ^ ", " ^ string_of_expression y ^ ")"
  | Color (red, green, blue, _) ->
      "Color(" ^ string_of_expression red ^ ", " ^ string_of_expression green
      ^ ", " ^ string_of_expression blue ^ ")"
  | Pixel (pos, color, _) ->
      "Pixel(" ^ string_of_expression pos ^ ", " ^ string_of_expression color
      ^ ")"
  | Binary_operator (op, e1, e2, _) ->
      "(" ^ string_of_expression e1
      ^ string_of_binary_operator op
      ^ string_of_expression e2 ^ ")"
  | Unary_operator (op, e, _) ->
      string_of_unary_operator op ^ "(" ^ string_of_expression e ^ ")"
  | Field_accessor (acc, e, _) ->
      string_of_expression e ^ "." ^ string_of_field_accessor acc
  | Variable (string, _) -> string
  | List (l, _) -> "[" ^ string_of_list l ^ "]"
  | Append (elt, value, _) ->
      string_of_expression elt ^ "::" ^ string_of_expression value

and string_of_pos = function
  | x, y ->
      "Coord(" ^ string_of_expression x ^ ", " ^ string_of_expression y ^ ")"

and string_of_color = function
  | red, green, blue ->
      "Color(" ^ string_of_expression red ^ ", " ^ string_of_expression green
      ^ ", " ^ string_of_expression blue ^ ")"

and string_of_list = function
  | [] -> ""
  | head :: tail -> (
      match tail with
      | [] -> string_of_expression head
      | _ -> string_of_expression head ^ ", " ^ string_of_list tail)

let rec pp_statement fmt = function
  | Affectation (mutable_expression, expression, _) ->
      Format.fprintf fmt "Set(%s, %s)"
        (string_of_expression mutable_expression)
        (string_of_expression expression)
  | Declaration (name, type_expr, _) ->
      Format.fprintf fmt "%s : %s" (string_of_type_expression type_expr) name
  | Block (list_statements, _) ->
      Format.fprintf fmt "$<@[<v 2>@,%a@]@,>$" pp_list_statements
        list_statements
  | IfThenElse (test, statement_then, Nop, _) ->
      Format.fprintf fmt "@[<v 2>If (%s) @,%a@]"
        (string_of_expression test)
        pp_statement statement_then
  | IfThenElse (test, statement_then, statement_else, _) ->
      Format.fprintf fmt "@[<v 2>If (%s)@,%a@]@,@[<v 1>else@,%a@]"
        (string_of_expression test)
        pp_statement statement_then pp_statement statement_else
  | For (name, from_expression, to_expression, step_expression, statement, _) ->
      Format.fprintf fmt "@[<v 2>For %s From %s To %s Step %s@,%a@]" name
        (string_of_expression from_expression)
        (string_of_expression to_expression)
        (string_of_expression step_expression)
        pp_statement statement
  | While (test, statement, _) ->
      Format.fprintf fmt "@[<v 2>While (%s)@,%a@]" (string_of_expression test)
        pp_statement statement
  | Foreach (name, expression, statement, _) ->
      Format.fprintf fmt "@[<v 2>Foreach %s In %s@,%a@]" name
        (string_of_expression expression)
        pp_statement statement
  | Draw_pixel (expression, _) ->
      Format.fprintf fmt "Draw_pixel(%s)" (string_of_expression expression)
  | Nop -> ()
  | Print (expression, _) ->
      Format.fprintf fmt "Print %s" (string_of_expression expression)

and pp_list_statements fmt = function
  | [] -> ()
  | head :: tail -> (
      match tail with
      | [] -> Format.fprintf fmt "%a" pp_statement head
      | _ ->
          Format.fprintf fmt "%a;@,%a" pp_statement head pp_list_statements tail
      )

let pp_argument fmt (Argument (name, typ, _)) =
  Format.fprintf fmt "%s : %s" (string_of_type_expression typ) name

let pp_program fmt (Program (args, body)) =
  Format.fprintf fmt "@[<v 0>@[<v 2>Arguments <@,";
  List.iteri
    (fun i arg ->
      Format.fprintf fmt "%a%s@," pp_argument arg
        (if i = List.length args - 1 then "" else ";"))
    args;
  Format.fprintf fmt ">@]@,%a@,@]" pp_statement body
