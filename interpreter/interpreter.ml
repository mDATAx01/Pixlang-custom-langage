open Ast
open Util

type coord = { mutable x : int; mutable y : int }
type color = { mutable red : int; mutable green : int; mutable blue : int }
type pixel = { mutable coord : coord; mutable color : color }

type value =
  | Int of int
  | Real of float
  | Bool of bool
  | Coord of coord
  | Color of color
  | Pixel of pixel
  | List of value ref list

let rec pp_value fmt = function
  | Int a -> Format.fprintf fmt "%d" a
  | Real a -> Format.fprintf fmt "%f" a
  | Bool b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Coord p -> Format.fprintf fmt "Coord(%d,%d)" p.x p.y
  | Color c -> Format.fprintf fmt "Color(%d,%d,%d)" c.red c.green c.blue
  | Pixel p ->
      Format.fprintf fmt "Pixel(Coord(%d,%d),Color(%d,%d,%d))" p.coord.x
        p.coord.y p.color.red p.color.green p.color.blue
  | List l ->
      Format.fprintf fmt "[%s]"
        (List.fold_left
           (fun s v ->
             Format.asprintf "%s%a" (if s = "" then "" else s ^ ",") pp_value !v)
           "" l)

let int_function_of_binop op v1 v2 =
  match op with
  | Plus -> Int (v1 + v2)
  | Minus -> Int (v1 - v2)
  | Times -> Int (v1 * v2)
  | Div -> Int (v1 / v2)
  | Rem -> Int (v1 mod v2)
  | Equal -> Bool (v1 = v2)
  | Diff -> Bool (v1 <> v2)
  | Lt -> Bool (v1 < v2)
  | Gt -> Bool (v1 > v2)
  | Leq -> Bool (v1 <= v2)
  | Geq -> Bool (v1 >= v2)
  | And | Or -> failwith "Operation undefined on integers"

let float_function_of_binop op v1 v2 =
  match op with
  | Plus -> Real (v1 +. v2)
  | Minus -> Real (v1 -. v2)
  | Times -> Real (v1 *. v2)
  | Div -> Real (v1 /. v2)
  | Rem -> Real (mod_float v1 v2)
  | Equal -> Bool (v1 = v2)
  | Diff -> Bool (v1 <> v2)
  | Lt -> Bool (v1 < v2)
  | Gt -> Bool (v1 > v2)
  | Leq -> Bool (v1 <= v2)
  | Geq -> Bool (v1 >= v2)
  | And | Or -> failwith "Operation undefined on floats"

let bool_function_of_binop op v1 v2 =
  match op with
  | Plus | Minus | Times | Div | Rem ->
      failwith "Operation undefined on booleans"
  | Equal -> Bool (v1 = v2)
  | Diff -> Bool (v1 <> v2)
  | Lt -> Bool (v1 < v2)
  | Gt -> Bool (v1 > v2)
  | Leq -> Bool (v1 <= v2)
  | Geq -> Bool (v1 >= v2)
  | And -> Bool (v1 && v2)
  | Or -> Bool (v1 || v2)

let get_int = function Int i -> i | _ -> failwith "error not producing an int"

let get_pos = function
  | Coord i -> i
  | _ -> failwith "error not producing a position"

let get_color = function
  | Color i -> i
  | _ -> failwith "error not producing a color"

let pos_function_of_binop op v1 v2 =
  match op with
  | Plus | Minus | Times | Div | Rem ->
      Coord
        {
          x = get_int (int_function_of_binop op v1.x v2.x);
          y = get_int (int_function_of_binop op v1.y v2.y);
        }
  | Equal -> Bool (v1.x = v2.x && v1.y = v2.y)
  | Diff -> Bool (v1.x <> v2.x || v1.y <> v2.y)
  | Gt -> Bool (v1.x > v2.x && v1.y > v2.y)
  | Lt -> Bool (v1.x < v2.x && v1.y < v2.y)
  | Leq -> Bool (v1.x <= v2.x && v1.y <= v2.y)
  | Geq -> Bool (v1.x >= v2.x && v1.y >= v2.y)
  | And | Or -> failwith "operation undefined on position"

let color_function_of_binop op v1 v2 =
  match op with
  | Plus | Minus | Times | Div | Rem ->
      Color
        {
          red = get_int (int_function_of_binop op v1.red v2.red);
          green = get_int (int_function_of_binop op v1.green v2.green);
          blue = get_int (int_function_of_binop op v1.blue v2.blue);
        }
  | Equal -> Bool (v1.red = v2.red && v1.blue = v2.blue && v1.green = v2.green)
  | Diff -> Bool (v1.red <> v2.red || v1.blue <> v2.blue || v1.green <> v2.green)
  | Lt -> Bool (v1.red < v2.red && v1.blue < v2.blue && v1.green < v2.green)
  | Gt -> Bool (v1.red > v2.red && v1.blue > v2.blue && v1.green > v2.green)
  | Leq -> Bool (v1.red <= v2.red && v1.blue <= v2.blue && v1.green <= v2.green)
  | Geq -> Bool (v1.red >= v2.red && v1.blue >= v2.blue && v1.green >= v2.green)
  | And | Or -> failwith "operation undefined on color"

let point_function_of_binop op v1 v2 =
  match op with
  | Plus | Minus | Times | Div | Rem ->
      Pixel
        {
          coord = get_pos (pos_function_of_binop op v1.coord v2.coord);
          color = get_color (color_function_of_binop op v1.color v2.color);
        }
  | Equal -> Bool (v1.coord = v2.coord)
  | Diff -> Bool (v1.coord <> v2.coord)
  | Gt -> Bool (v1.coord > v2.coord)
  | Lt -> Bool (v1.coord < v2.coord)
  | Leq -> Bool (v1.coord <= v2.coord)
  | Geq -> Bool (v1.coord >= v2.coord)
  | And | Or -> failwith "operation undefined on position"

let list_function_of_binop op l1 l2 =
  match op with
  | Plus -> List (l1 @ l2)
  | _ -> failwith "operation undefined on list"

let rec interpret_expression environment = function
  | Const_int (i, _) -> Int i
  | Const_real (f, _) -> Real f
  | Const_bool (b, _) -> Bool b
  | Coord (e1, e2, _) -> (
      let v1 = interpret_expression environment e1 in
      let v2 = interpret_expression environment e2 in
      match (v1, v2) with
      | Int i, Int j -> Coord { x = i; y = j }
      | _ -> failwith "Position of non-integers")
  | Color (e1, e2, e3, _) -> (
      let v1 = interpret_expression environment e1 in
      let v2 = interpret_expression environment e2 in
      let v3 = interpret_expression environment e3 in
      match (v1, v2, v3) with
      | Int i1, Int i2, Int i3 -> Color { red = i1; green = i2; blue = i3 }
      | _ -> failwith "Color of non-integers")
  | Pixel (e1, e2, _) -> (
      let v1 = interpret_expression environment e1 in
      let v2 = interpret_expression environment e2 in
      match (v1, v2) with
      | Coord p, Color c -> Pixel { coord = p; color = c }
      | _ -> failwith "Ill formed point")
  | Variable (s, _) -> (
      match Environment.get environment s with
      | Some v -> v
      | _ -> failwith ("undefined variable " ^ s))
  | Binary_operator (op, e1, e2, _) -> (
      let v1 = interpret_expression environment e1 in
      let v2 = interpret_expression environment e2 in
      match (v1, v2) with
      | Int i1, Int i2 -> int_function_of_binop op i1 i2
      | Real f1, Real f2 -> float_function_of_binop op f1 f2
      | Bool b1, Bool b2 -> bool_function_of_binop op b1 b2
      | Coord p1, Coord p2 -> pos_function_of_binop op p1 p2
      | Color c1, Color c2 -> color_function_of_binop op c1 c2
      | Pixel p1, Pixel p2 -> point_function_of_binop op p1 p2
      | List l1, List l2 -> list_function_of_binop op l1 l2
      | Int i, Coord p -> pos_function_of_binop op { x = i; y = i } p
      | Coord p, Int i -> pos_function_of_binop op p { x = i; y = i }
      | Int i, Color c ->
          color_function_of_binop op { blue = i; red = i; green = i } c
      | Color c, Int i ->
          color_function_of_binop op c { blue = i; red = i; green = i }
      | _ -> failwith "inconsistent operands")
  | Unary_operator (op, e, _) -> (
      let v1 = interpret_expression environment e in
      match (op, v1) with
      | Opposite, Int i -> Int (-i)
      | Opposite, Real i -> Real (-.i)
      | Not, Bool b -> Bool (not b)
      | Head, List l -> !(List.hd l)
      | Tail, List l -> List (List.tl l)
      | Floor, Real i -> Int (int_of_float i)
      | Real_of_int, Int i -> Real (float_of_int i)
      | Cos, Real f -> Real (Float.cos f)
      | Sin, Real f -> Real (Float.sin f)
      | _ -> failwith "unary operator not applicable to operand")
  | Field_accessor (field, e, _) -> (
      let v1 = interpret_expression environment e in
      match (field, v1) with
      | Color_field, Pixel p -> Color p.color
      | Coord_field, Pixel p -> Coord p.coord
      | X_field, Coord p -> Int p.x
      | Y_field, Coord p -> Int p.y
      | Blue_field, Color c -> Int c.blue
      | Red_field, Color c -> Int c.red
      | Green_field, Color c -> Int c.green
      | _ -> failwith "field not existing on value")
  | List (e_l, _) ->
      List (List.map (fun e -> ref (interpret_expression environment e)) e_l)
  | Append (e1, e2, _) -> (
      let v1 = interpret_expression environment e1 in
      let v2 = interpret_expression environment e2 in
      match v2 with
      | List l -> List (ref v1 :: l)
      | _ -> failwith "appending to something else than a list")

let rec interpret_statement environment = function
  | Affectation (e1, e2, _) -> (
      let v2 = interpret_expression environment e2 in
      match (e1, v2) with
      | Variable (x, _), _ -> Environment.modify environment x v2
      | Field_accessor (Color_field, e, _), Color c -> (
          match interpret_expression environment e with
          | Pixel p -> p.color <- c
          | _ -> failwith "getting a color outside of a point")
      | Field_accessor (Coord_field, e, _), Coord p -> (
          match interpret_expression environment e with
          | Pixel point -> point.coord <- p
          | _ ->
              failwith "assigning position field to something else than a point"
          )
      | Field_accessor (X_field, e, _), Int x -> (
          match interpret_expression environment e with
          | Coord p -> p.x <- x
          | _ ->
              failwith "Assigning a x field to something else than a position")
      | Field_accessor (Y_field, e, _), Int y -> (
          match interpret_expression environment e with
          | Coord p -> p.y <- y
          | _ ->
              failwith "Assigning a y field to something else than a position")
      | Field_accessor (Blue_field, e, _), Int b -> (
          match interpret_expression environment e with
          | Color c -> c.blue <- b
          | _ ->
              failwith "Assigning a blue field to something else than a color")
      | Field_accessor (Red_field, e, _), Int r -> (
          match interpret_expression environment e with
          | Color c -> c.red <- r
          | _ -> failwith "Assigning a red field to something else than a color"
          )
      | Field_accessor (Green_field, e, _), Int g -> (
          match interpret_expression environment e with
          | Color c -> c.green <- g
          | _ ->
              failwith "Assigning a green field to something else than a color")
      | _ -> failwith "Assigning a wrong type to a field")
  | Declaration (name, t, _) ->
      Environment.add environment name
        (match t with
        | Type_int -> Int 0
        | Type_real -> Real 0.
        | Type_bool -> Bool false
        | Type_coord -> Coord { x = 0; y = 0 }
        | Type_color -> Color { red = 0; green = 0; blue = 0 }
        | Type_pixel ->
            Pixel
              {
                coord = { x = 0; y = 0 };
                color = { red = 0; green = 0; blue = 0 };
              }
        | Type_list _ -> List []
        | Type_generic -> failwith "generic type isn't a real type")
  | Block (list, _) ->
      (* Environment.add_layer environment; *)
      List.iter (interpret_statement environment) list
  (* Environment.remove_layer environment *)
  | IfThenElse (test, i_then, i_else, _) -> (
      match interpret_expression environment test with
      | Bool true -> interpret_statement environment i_then
      | Bool false -> interpret_statement environment i_else
      | _ -> failwith "testing a non-boolean value")
  | For (name, init, target, increment, body, ann) ->
      (* Environment.add_layer environment; *)
      interpret_statement environment
        (Affectation (Variable (name, ann), init, ann));
      while
        interpret_expression environment
          (Binary_operator (Leq, Variable (name, ann), target, ann))
        = Bool true
      do
        interpret_statement environment body;
        interpret_statement environment
          (Affectation
             ( Variable (name, ann),
               Binary_operator (Plus, Variable (name, ann), increment, ann),
               ann ))
      done
      (* Environment.remove_layer environment *)
  | While (test, statement, ann) ->(
    match interpret_expression environment test with
    | Bool false -> ()
    | Bool true -> let _ =  interpret_statement environment statement in interpret_statement environment (While (test, statement, ann))
    | _ -> failwith "testing a non-boolean value")
  | Foreach (name, list, body, _) -> (
      match interpret_expression environment list with
      | List l ->
          (* Environment.add_layer environment; *)
          List.iter
            (fun element ->
              Environment.add_ref environment name element;
              interpret_statement environment body)
            l
      (* Environment.remove_layer environment *)
      | _ -> failwith "Iterating on something else than a list")
  | Draw_pixel (expression, _) -> (
      match interpret_expression environment expression with
      | Pixel p ->
          Graphics.set_color
            (Graphics.rgb p.color.red p.color.green p.color.blue);
          Graphics.plot p.coord.x p.coord.y
      | _ -> failwith "Drawing something else than a point")
  | Nop -> ()
  | Print (expression, _) ->
      Format.printf "%a@,%!" pp_value
        (interpret_expression environment expression)

let rec parse_arg str = function
  | Type_int -> Int (int_of_string str)
  | Type_real -> Real (float_of_string str)
  | Type_bool -> if str = "True" then Bool true else Bool false
  | Type_coord -> Scanf.sscanf str "(%d,%d)" (fun x y -> Coord { x; y })
  | Type_color ->
      Scanf.sscanf str "(%d,%d,%d)" (fun r g b ->
          Color { red = r; green = g; blue = b })
  | Type_pixel ->
      Scanf.sscanf str "((%d,%d),(%d,%d,%d))" (fun x y r g b ->
          Pixel { coord = { x; y }; color = { red = r; green = g; blue = b } })
  | Type_list typ ->
      let trimmed = String.sub str 1 (String.length str - 2) in
      List
        (List.map
           (fun s -> ref (parse_arg s typ))
           (String.split_on_char ',' trimmed))
  | Type_generic -> failwith "generic type isn't a real type"

let interpret_prg (Program (args, body)) provided_args =
  Format.printf "@[<v 0>Exécution:@,";
  let environment = Environment.new_environment () in
  if List.length args > List.length provided_args then
    failwith "Not enough arguments provided";
  List.iteri
    (fun i (Argument (name, typ, _)) ->
      let str = List.nth provided_args i in
      Environment.add environment name (parse_arg str typ))
    args;
  Graphics.open_graph "";
  interpret_statement environment body;
  let _ = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  Format.printf "Fin de l’exécution@,@]";
  Graphics.close_graph ()
