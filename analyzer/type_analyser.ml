open Util
open Ast

let type_binop report pos t1 t2 binop =
  match binop with
  | Plus | Minus | Times | Div | Rem -> (
      match (t1, t2) with
      | Type_int, Type_int
      | Type_real, Type_real
      | Type_coord, Type_coord
      | Type_color, Type_color
      | Type_pixel, Type_pixel ->
          t1
      | Type_list _, Type_list _ ->
          if binop <> Plus then (
            Error_report.add_error report
              ( Format.sprintf "Operation incompatible with %s and %s"
                  (string_of_type_expression t1)
                  (string_of_type_expression t2),
                pos );
            Type_generic)
          else t1
      | Type_int, Type_real | Type_real, Type_int ->
          Error_report.add_warning report
            (Format.sprintf "Implicite cast from Int to Real", pos);
          Type_real
      | Type_int, Type_color | Type_color, Type_int -> Type_color
      | Type_int, Type_coord | Type_coord, Type_int -> Type_coord
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "Operation incompatible with %s and %s"
                (string_of_type_expression t1)
                (string_of_type_expression t2),
              pos );
          Type_generic)
  | And | Or ->
      if t1 <> Type_bool && t2 <> Type_bool then (
        Error_report.add_error report
          ( Format.sprintf "Operation incompatible with %s and %s"
              (string_of_type_expression t1)
              (string_of_type_expression t2),
            pos );
        Type_generic)
      else Type_bool
  | Equal | Diff ->
      if t1 <> t2 then (
        Error_report.add_error report
          ( Format.sprintf "Operation incompatible with %s and %s"
              (string_of_type_expression t1)
              (string_of_type_expression t2),
            pos );
        Type_generic)
      else Type_bool
  | Lt | Gt | Leq | Geq ->
      if t1 <> t2 || (t1 <> Type_int && t1 <> Type_bool && t1 <> Type_real) then (
        Error_report.add_error report
          ( Format.sprintf "Operation incompatible with %s and %s"
              (string_of_type_expression t1)
              (string_of_type_expression t2),
            pos );
        Type_generic)
      else Type_bool

let type_unop report pos t unop =
  match unop with
  | Opposite -> (
      match t with
      | Type_int | Type_real | Type_generic -> t
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_generic)
  | Not -> (
      match t with
      | Type_bool | Type_generic -> t
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_generic)
  | Head -> (
      match t with
      | Type_list l -> l
      | Type_generic -> t
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_generic)
  | Tail -> (
      match t with
      | Type_list _ -> t
      | Type_generic -> t
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_generic)
  | Floor -> (
      match t with
      | Type_real | Type_generic -> Type_int
      | Type_int ->
          Error_report.add_warning report
            (Format.sprintf "Implicit cast from int to real", pos);
          Type_real
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_int)
  | Real_of_int -> (
      match t with
      | Type_int | Type_generic -> Type_real
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_real)
  | Cos | Sin -> (
      match t with
      | Type_real | Type_generic -> Type_real
      | Type_int ->
          Error_report.add_warning report
            (Format.sprintf "Implicit cast from int to real", pos);
          Type_real
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "operation not compatible with %s"
                (string_of_type_expression t),
              pos );
          Type_generic)

let type_field report pos t field =
  match field with
  | Color_field -> (
      match t with
      | Type_pixel | Type_generic -> Type_color
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "Field incompatible with %s"
                (string_of_type_expression t),
              pos );
          Type_color)
  | Coord_field -> (
      match t with
      | Type_pixel | Type_generic -> Type_coord
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "Field incompatible with %s"
                (string_of_type_expression t),
              pos );
          Type_coord)
  | X_field | Y_field -> (
      match t with
      | Type_coord | Type_generic -> Type_int
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "Field incompatible with %s"
                (string_of_type_expression t),
              pos );
          Type_int)
  | Red_field | Green_field | Blue_field -> (
      match t with
      | Type_color | Type_generic -> Type_int
      | _ ->
          Error_report.add_error report
            ( Format.sprintf "Field incompatible with %s"
                (string_of_type_expression t),
              pos );
          Type_int)

let rec type_expression type_environement report expr =
  match expr with
  | Const_int (_, ann) ->
      Annotation.set_type ann Type_int;
      Type_int
  | Const_real (_, ann) ->
      Annotation.set_type ann Type_real;
      Type_real
  | Const_bool (_, ann) ->
      Annotation.set_type ann Type_bool;
      Type_bool
  | Coord (e1, e2, ann) ->
      let t1 = type_expression type_environement report e1 in
      let t2 = type_expression type_environement report e2 in
      if t1 <> Type_int then
        Error_report.add_error report
          ( Format.sprintf
              "Left element of a coordinate of type %s instead of type Int"
              (string_of_type_expression t1),
            Annotation.get_pos ann );
      if t2 <> Type_int then
        Error_report.add_error report
          ( Format.sprintf
              "Right element of a coordinate of type %s instead of type Int"
              (string_of_type_expression t2),
            Annotation.get_pos ann );
      Annotation.set_type ann Type_coord;
      Type_coord
  | Color (e1, e2, e3, ann) ->
      let t1 = type_expression type_environement report e1 in
      let t2 = type_expression type_environement report e2 in
      let t3 = type_expression type_environement report e3 in
      if t1 <> Type_int then
        Error_report.add_error report
          ( Format.sprintf
              "Left element of a color of type %s instead of type Int"
              (string_of_type_expression t1),
            Annotation.get_pos ann );
      if t2 <> Type_int then
        Error_report.add_error report
          ( Format.sprintf
              "Middle element of a color of type %s instead of type Int"
              (string_of_type_expression t2),
            Annotation.get_pos ann );
      if t3 <> Type_int then
        Error_report.add_error report
          ( Format.sprintf
              "Right element of a color of type %s instead of type Int"
              (string_of_type_expression t3),
            Annotation.get_pos ann );
      Annotation.set_type ann Type_color;
      Type_color
  | Pixel (e1, e2, ann) ->
      let t1 = type_expression type_environement report e1 in
      let t2 = type_expression type_environement report e2 in
      if t1 <> Type_coord then
        Error_report.add_error report
          ( Format.sprintf
              "Left element of a pixel of type %s instead of type Coord"
              (string_of_type_expression t1),
            Annotation.get_pos ann );
      if t2 <> Type_color then
        Error_report.add_error report
          ( Format.sprintf
              "Right element of a pixel of type %s instead of type Color"
              (string_of_type_expression t2),
            Annotation.get_pos ann );
      Annotation.set_type ann Type_pixel;
      Type_pixel
  | Variable (name, ann) -> (
      match Environment.get type_environement name with
      | None ->
          Error_report.add_error report
            (Format.sprintf "Undeclared variable", Annotation.get_pos ann);
          Type_generic
      | Some t ->
          Annotation.set_type ann t;
          t)
  | Binary_operator (op, e1, e2, ann) ->
      let t1 = type_expression type_environement report e1 in
      let t2 = type_expression type_environement report e2 in
      let t_op = type_binop report (Annotation.get_pos ann) t1 t2 op in
      Annotation.set_type ann t_op;
      t_op
  | Unary_operator (op, e, ann) ->
      let t = type_expression type_environement report e in
      let t_op = type_unop report (Annotation.get_pos ann) t op in
      Annotation.set_type ann t_op;
      t_op
  | Field_accessor (field, e, ann) ->
      let t = type_expression type_environement report e in
      let t_f = type_field report (Annotation.get_pos ann) t field in
      Annotation.set_type ann t_f;
      t_f
  | List (l, ann) ->
      let t_list =
        List.sort_uniq compare
          (List.map (type_expression type_environement report) l)
      in
      if List.length t_list > 1 then
        Error_report.add_error report
          ( Format.sprintf "List containing several types",
            Annotation.get_pos ann );
      if List.length t_list = 1 then (
        Annotation.set_type ann (Type_list (List.hd t_list));
        Type_list (List.hd t_list))
      else (
        Annotation.set_type ann (Type_list Type_generic);
        Type_list Type_generic)
  | Append (hd, tl, ann) ->
      let t_hd = type_expression type_environement report hd in
      let t_tl = type_expression type_environement report tl in
      if t_tl <> Type_list Type_generic && t_tl <> Type_list t_hd then (
        Error_report.add_error report
          ( Format.sprintf "Appending a %s to a %s"
              (string_of_type_expression t_hd)
              (string_of_type_expression t_tl),
            Annotation.get_pos ann );
        Annotation.set_type ann Type_generic;
        Type_generic)
      else (
        Annotation.set_type ann (Type_list t_hd);
        Type_list t_hd)

let rec is_affectable expression =
  match expression with
  | Variable _ -> true
  | Field_accessor (_, e, _) -> is_affectable e
  | _ -> false

let rec type_statement type_environment report stmt =
  match stmt with
  | Affectation (target, source, ann) ->
      let t_tgt = type_expression type_environment report target in
      let t_src = type_expression type_environment report source in
      (match (t_tgt, t_src) with
      | Type_list _, Type_list Type_generic | _, Type_generic | Type_generic, _
        ->
          ()
      | _ ->
          if t_tgt <> t_src then
            Error_report.add_error report
              ( Format.sprintf "Left-handside expects a %s but is affected a %s"
                  (string_of_type_expression t_tgt)
                  (string_of_type_expression t_src),
                Annotation.get_pos ann ));
      if not (is_affectable target) then
        Error_report.add_error report
          ( Format.sprintf "Left-handside is not a modifiable expression\n",
            Annotation.get_pos ann )
  | Declaration (name, t, _) -> Environment.add type_environment name t
  | Block (l, _) ->
      let env = Environment.copy type_environment in
      List.iter (type_statement env report) l
  | IfThenElse (test, th, el, ann) ->
      let t = type_expression type_environment report test in
      if t <> Type_bool then
        Error_report.add_error report
          ( Format.sprintf "test should be a Bool, but is a %s"
              (string_of_type_expression t),
            Annotation.get_pos ann );
      type_statement (Environment.copy type_environment) report th;
      type_statement (Environment.copy type_environment) report el
  | For (name, start, last, step, body, ann) ->
      let t_start = type_expression type_environment report start in
      let t_last = type_expression type_environment report last in
      let t_step = type_expression type_environment report step in
      if t_start <> t_last || t_start <> t_step || t_last <> t_step then
        Error_report.add_error report
          ( Format.sprintf "For loop with inconsistent types %s, %s and %s"
              (string_of_type_expression t_start)
              (string_of_type_expression t_last)
              (string_of_type_expression t_step),
            Annotation.get_pos ann );
      if t_start <> Type_int && t_start <> Type_real then
        Error_report.add_error report
          ( Format.sprintf "For loop on %s instead of Int or Real"
              (string_of_type_expression t_start),
            Annotation.get_pos ann );
      let env = Environment.copy type_environment in
      Environment.add env name t_start;
      type_statement env report body
  |While(test, body, ann) ->
      let t_test = type_expression type_environment report test in
      if t_test <> Type_bool then
        Error_report.add_error report
          ( Format.sprintf "While test is a %s but should be a Bool"
              (string_of_type_expression t_test),
            Annotation.get_pos ann ); 
            let env = Environment.copy type_environment in
            type_statement env report body
  | Foreach (name, e, body, ann) ->
      let t = type_expression type_environment report e in
      let t_var =
        match t with
        | Type_list l -> l
        | _ ->
            Error_report.add_error report
              ( Format.sprintf "Foreach iterating on %s instead of a List"
                  (string_of_type_expression t),
                Annotation.get_pos ann );
            Type_generic
      in
      let env = Environment.copy type_environment in
      Environment.add env name t_var;
      type_statement env report body
  | Draw_pixel (e, ann) ->
      let t = type_expression type_environment report e in
      if t <> Type_pixel then
        Error_report.add_error report
          ( Format.sprintf "Drawing a %s instead of a Pixel"
              (string_of_type_expression t),
            Annotation.get_pos ann )
  | Nop -> ()
  | Print (e, _) -> ignore (type_expression type_environment report e)

let type_argument type_environment argument =
  match argument with
  | Argument (name, typ, ann) ->
      Annotation.set_type ann typ;
      Environment.add type_environment name typ

let type_analyser report program =
  let type_environment = Environment.new_environment () in
  match program with
  | Program (arguments, body) ->
      List.iter (type_argument type_environment) arguments;
      type_statement type_environment report body
