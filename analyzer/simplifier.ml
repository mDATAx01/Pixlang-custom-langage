(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points (entre autres) qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (pixels, coordonnées et couleurs) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)
open Ast

let get_annotation_type (expr : Ast.expression) =
  match expr with
  | Const_int (_, a)
  | Const_real (_, a)
  | Const_bool (_, a)
  | Coord (_, _, a)
  | Color (_, _, _, a)
  | Pixel (_, _, a)
  | Variable (_, a)
  | Binary_operator (_, _, _, a)
  | Unary_operator (_, _, a)
  | Field_accessor (_, _, a)
  | List (_, a)
  | Append (_, _, a) ->
      Annotation.get_type a

let rec simplify_expression (expr : Ast.expression) =
  match expr with
  | Coord (x, y, annotation) ->
      Coord (simplify_expression x, simplify_expression y, annotation)
  | Color (r, g, b, annotation) ->
      Color
        ( simplify_expression r,
          simplify_expression g,
          simplify_expression b,
          annotation )
  | Pixel (coord, color, annotation) ->
      Pixel (simplify_expression coord, simplify_expression color, annotation)
  | Unary_operator (op, e, annotation) -> (
      match e with
      | Unary_operator (Real_of_int, Const_int (n, _), _) when op = Floor ->
          Const_int (n, annotation)
      | _ -> (
          let simplified_e = simplify_expression e in
          match simplified_e with
          | Const_int (i, _) -> (
              match op with
              | Opposite -> Const_int (-i, annotation)
              | Real_of_int -> Const_real (float_of_int i, annotation)
              | _ -> Unary_operator (op, simplified_e, annotation))
          | Const_real (r, _) -> (
              match op with
              | Opposite -> Const_real (-.r, annotation)
              | Floor -> Const_int (int_of_float r, annotation)
              | Cos -> Const_real (Float.cos r, annotation)
              | Sin -> Const_real (Float.sin r, annotation)
              | _ -> Unary_operator (op, simplified_e, annotation))
          | Const_bool (b, _) -> (
              match op with
              | Not -> Const_bool (not b, annotation)
              | _ -> Unary_operator (op, simplified_e, annotation))
          | List (l, _) ->
              List (List.map (fun e -> simplify_expression e) l, annotation)
          | _ -> Unary_operator (op, simplified_e, annotation)))
  | Binary_operator (op, e1, e2, annotation) -> (
      let simplified_e1 = simplify_expression e1 in
      match simplified_e1 with
      | Const_int (i1, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Const_int (i2, _) -> (
              match op with
              | Plus -> Const_int (i1 + i2, annotation)
              | Minus -> Const_int (i1 - i2, annotation)
              | Times -> Const_int (i1 * i2, annotation)
              | Div -> Const_int (i1 / i2, annotation)
              | Rem -> Const_int (i1 mod i2, annotation)
              | Equal -> Const_bool (i1 = i2, annotation)
              | Diff -> Const_bool (i1 <> i2, annotation)
              | Lt -> Const_bool (i1 < i2, annotation)
              | Gt -> Const_bool (i1 > i2, annotation)
              | Leq -> Const_bool (i1 <= i2, annotation)
              | Geq -> Const_bool (i1 >= i2, annotation)
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          | Coord (x, y, _) ->
              Coord
                ( simplify_expression
                    (Binary_operator
                       (op, Const_int (i1, annotation), x, annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, Const_int (i1, annotation), y, annotation)),
                  annotation )
          | Color (r, g, b, _) ->
              Color
                ( simplify_expression
                    (Binary_operator
                       (op, Const_int (i1, annotation), r, annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, Const_int (i1, annotation), g, annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, Const_int (i1, annotation), b, annotation)),
                  annotation )
          (*Cast implicite*)
          | Const_real (_, _) ->
              let a =
                Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos)
                (*On ne savait pas comment mettre une position vide*)
              in
              let _ = Annotation.set_type a Type_real in
              Binary_operator
                ( op,
                  Unary_operator (Real_of_int, simplified_e1, a),
                  simplified_e2,
                  annotation )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | Const_real (r1, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Const_real (r2, _) -> (
              match op with
              | Plus -> Const_real (r1 +. r2, annotation)
              | Minus -> Const_real (r1 -. r2, annotation)
              | Times -> Const_real (r1 *. r2, annotation)
              | Div -> Const_real (r1 /. r2, annotation)
              | Rem -> Const_real (mod_float r1 r2, annotation)
              | Equal -> Const_bool (r1 = r2, annotation)
              | Diff -> Const_bool (r1 <> r2, annotation)
              | Lt -> Const_bool (r1 < r2, annotation)
              | Gt -> Const_bool (r1 > r2, annotation)
              | Leq -> Const_bool (r1 <= r2, annotation)
              | Geq -> Const_bool (r1 >= r2, annotation)
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          (*Cast implicite*)
          | Const_int (_, _) ->
              let a =
                Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos)
                (*On ne savait pas comment mettre une position vide*)
              in
              let _ = Annotation.set_type a Type_real in
              Binary_operator
                ( op,
                  simplified_e1,
                  Unary_operator (Real_of_int, simplified_e2, a),
                  annotation )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | Const_bool (b1, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Const_bool (b2, _) -> (
              match op with
              | Equal -> Const_bool (b1 = b2, annotation)
              | Diff -> Const_bool (b1 <> b2, annotation)
              | Lt -> Const_bool (b1 < b2, annotation)
              | Gt -> Const_bool (b1 > b2, annotation)
              | Leq -> Const_bool (b1 <= b2, annotation)
              | Geq -> Const_bool (b1 >= b2, annotation)
              | And -> Const_bool (b1 && b2, annotation)
              | Or -> Const_bool (b1 || b2, annotation)
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | Coord (x, y, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Const_int (i, _) ->
              Coord
                ( simplify_expression
                    (Binary_operator
                       (op, x, Const_int (i, annotation), annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, y, Const_int (i, annotation), annotation)),
                  annotation )
          | Coord (x2, y2, _) ->
              Coord
                ( simplify_expression (Binary_operator (op, x, x2, annotation)),
                  simplify_expression (Binary_operator (op, y, y2, annotation)),
                  annotation )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | Color (r, g, b, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Const_int (i, _) ->
              Color
                ( simplify_expression
                    (Binary_operator
                       (op, r, Const_int (i, annotation), annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, g, Const_int (i, annotation), annotation)),
                  simplify_expression
                    (Binary_operator
                       (op, b, Const_int (i, annotation), annotation)),
                  annotation )
          | Color (r2, g2, b2, _) ->
              Color
                ( simplify_expression (Binary_operator (op, r, r2, annotation)),
                  simplify_expression (Binary_operator (op, g, g2, annotation)),
                  simplify_expression (Binary_operator (op, b, b2, annotation)),
                  annotation )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | Pixel (coord1, color1, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | Pixel (coord2, color2, _) ->
              Pixel
                ( simplify_expression
                    (Binary_operator (op, coord1, coord2, annotation)),
                  simplify_expression
                    (Binary_operator (op, color1, color2, annotation)),
                  annotation )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      | List (l, _) -> (
          let simplified_e2 = simplify_expression e2 in
          match simplified_e2 with
          | List (l2, _) -> (
              match op with
              | Plus -> List (l @ l2, annotation)
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation)
          (*Cast implicite*))
      | _ -> (
          let simplified_e2 = simplify_expression e2 in
          match get_annotation_type e1 with
          | Some Type_int -> (
              match get_annotation_type e2 with
              | Some Type_real ->
                  let a =
                    Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos)
                    (*On ne savait pas comment mettre une position vide*)
                  in
                  let _ = Annotation.set_type a Type_real in
                  Binary_operator
                    ( op,
                      Unary_operator (Real_of_int, simplified_e1, a),
                      simplified_e2,
                      annotation )
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          | Some Type_real -> (
              match get_annotation_type e2 with
              | Some Type_int ->
                  let a =
                    Annotation.create (Lexing.dummy_pos, Lexing.dummy_pos)
                    (*On ne savait pas comment mettre une position vide*)
                  in
                  let _ = Annotation.set_type a Type_real in
                  Binary_operator
                    ( op,
                      simplified_e1,
                      Unary_operator (Real_of_int, simplified_e2, a),
                      annotation )
              | _ ->
                  Binary_operator (op, simplified_e1, simplified_e2, annotation)
              )
          | _ -> Binary_operator (op, simplified_e1, simplified_e2, annotation))
      )
  | Field_accessor (field, e, annotation) -> (
      let simplified_e = simplify_expression e in
      match simplified_e with
      | Pixel (coord, color, _) -> (
          match field with
          | Color_field -> simplify_expression color
          | Coord_field -> simplify_expression coord
          | _ -> Field_accessor (field, simplified_e, annotation))
      | Coord (x, y, _) -> (
          match field with
          | X_field -> simplify_expression x
          | Y_field -> simplify_expression y
          | _ -> Field_accessor (field, simplified_e, annotation))
      | Color (r, g, b, _) -> (
          match field with
          | Red_field -> simplify_expression r
          | Green_field -> simplify_expression g
          | Blue_field -> simplify_expression b
          | _ -> Field_accessor (field, simplified_e, annotation))
      | _ -> Field_accessor (field, simplified_e, annotation))
  | Append (e, l, annotation) -> (
      let simplified_e = simplify_expression e in
      let simplified_l = simplify_expression l in
      match simplified_l with
      | List (list, _) -> List (simplified_e :: list, annotation)
      | _ -> Append (simplified_e, simplified_l, annotation))
  | _ -> expr

let rec simplify_statement (statement : Ast.statement) =
  match statement with
  | Affectation (var, expr, annotation) ->
      Affectation (var, simplify_expression expr, annotation)
  | Block (l, annotation) ->
      Block (List.map (fun s -> simplify_statement s) l, annotation)
  | IfThenElse (test, i_then, i_else, annotation) -> (
      let simplified_test = simplify_expression test in
      match simplified_test with
      | Const_bool (true, _) -> simplify_statement i_then
      | Const_bool (false, _) -> simplify_statement i_else
      | _ -> IfThenElse (simplified_test, i_then, i_else, annotation))
  | For (name, init, target, increment, body, annotation) -> (
      let simplified_init = simplify_expression init in
      let simplified_target = simplify_expression target in
      let simplified_increment = simplify_expression increment in
      let else_ =
        For
          ( name,
            simplified_init,
            simplified_target,
            simplified_increment,
            simplify_statement body,
            annotation )
      in
      match (simplified_init, simplified_target) with
      | Const_int (start, _), Const_int (end_, _) ->
          if start > end_ then Block ([], annotation) else else_
      | Const_real (start, _), Const_real (end_, _) ->
          if start > end_ then Block ([], annotation) else else_
      | _ -> else_)
  | While (test, body, annotation) -> (
      let simplified_test = simplify_expression test in
      match simplified_test with
      | Const_bool (false, _) -> Block ([], annotation)
      | _ -> While (simplified_test, simplify_statement body, annotation))
  | Foreach (name, list, body, annotation) -> (
      let simplified_list = simplify_expression list in
      match simplified_list with
      | List (l, _) -> (
          match l with
          | [] -> Block ([], annotation)
          | _ -> Foreach (name, simplified_list, body, annotation))
      | _ -> Foreach (name, simplified_list, body, annotation))
  | Draw_pixel (expr, annotation) ->
      Draw_pixel (simplify_expression expr, annotation)
  | Print (expr, annotation) -> Print (simplify_expression expr, annotation)
  | _ -> statement

let simplifier (program : Ast.program) =
  match program with
  | Program (arg_list, statement) ->
      Program (arg_list, simplify_statement statement)
