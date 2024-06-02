open Util
open Ast

(* Codez ici la passe de renommage de termes.

   Cette passe permet de simplifier grandement la gestion des passes suivantes, et de l’interprétation, en assurant qu’un nom de variable n’est jamais réutilisé dans la même portée de déclaration.

   Pour cela, on va modifier le programme en, pour chaque nom, gardant un nombre correspondant son nombre de redéfinition, et en renommant l’occurence de chaque nom par un identifiant unique (son nom, suivi de son nombre d’occurence, avec un séparateur interdit dans le langage (pour empêcher les redéfinitions)).

   Comme seule la portée des variables est importante, dans deux blocs disjoints, il est possible de réutiliser un même nom.

   Pour obtenir ce résultat, il sera nécessaire de copier les environnement avant d’évaluer des sous-blocs (puisqu’en sortant d’un bloc, il est possible de continuer à utiliser un nom défini plus haut).

   Attention, l’interpréteur ne fonctionnera pas correctement en cas de redéfinition si vous n’effectuez pas correctement cette passe.*)

let rename_arg arg env =
  match arg with
  | Argument (name, typ, annotation) -> (
      match Environment.get env name with
      | None ->
          Environment.add env name 0;
          Argument (name, typ, annotation)
      | Some i ->
          Environment.modify env name (i + 1);
          Argument (name ^ "#" ^ string_of_int (i + 1), typ, annotation))

let rec rename_list_args args env =
  match args with
  | [] -> []
  | arg :: q ->
      let arg = rename_arg arg env in
      arg :: rename_list_args q env

let rec rename_expr expr env =
  match expr with
  | Const_int (i, annotation) -> Const_int (i, annotation)
  | Const_real (f, annotation) -> Const_real (f, annotation)
  | Const_bool (b, annotation) -> Const_bool (b, annotation)
  | Coord (expr1, expr2, annotion) ->
      Coord (rename_expr expr1 env, rename_expr expr2 env, annotion)
  | Color (expr1, expr2, expr3, annotation) ->
      Color
        ( rename_expr expr1 env,
          rename_expr expr2 env,
          rename_expr expr3 env,
          annotation )
  | Pixel (expr1, expr2, annotation) ->
      Pixel (rename_expr expr1 env, rename_expr expr2 env, annotation)
  | Variable (id, annotation) -> (
      match Environment.get env id with
      | None ->
          Environment.add env id 0;
          Variable (id, annotation)
      | Some i ->
          if i = 0 then Variable (id, annotation)
          else Variable (id ^ "#" ^ string_of_int i, annotation))
  | Binary_operator (op, expr1, expr2, annotation) ->
      Binary_operator
        (op, rename_expr expr1 env, rename_expr expr2 env, annotation)
  | Unary_operator (op, expr, annotation) ->
      Unary_operator (op, rename_expr expr env, annotation)
  | Field_accessor (field_accessor, expr, annotation) ->
      Field_accessor (field_accessor, rename_expr expr env, annotation)
  | List (expr_list, annotation) ->
      List (List.map (fun x -> rename_expr x env) expr_list, annotation)
  | Append (expr1, expr2, annotation) ->
      Append (rename_expr expr1 env, rename_expr expr2 env, annotation)

let rec rename_statement statement env =
  match statement with
  | Affectation (expr1, expr2, annotation) ->
      Affectation (rename_expr expr1 env, rename_expr expr2 env, annotation)
  | Declaration (name, typ_expr, annotation) -> (
      match Environment.get env name with
      | None ->
          Environment.add env name 0;
          Declaration (name, typ_expr, annotation)
      | Some i ->
          Environment.modify env name (i + 1);
          Declaration (name ^ "#" ^ string_of_int (i + 1), typ_expr, annotation)
      )
  | Block (statement_list, annotation) ->
      let env_copy = Environment.copy env in
      Block
        ( List.map (fun x -> rename_statement x env_copy) statement_list,
          annotation )
  | IfThenElse (test, th, el, annotation) ->
      let th_copy = Environment.copy env in
      let el_copy = Environment.copy env in
      IfThenElse
        ( rename_expr test env,
          rename_statement th th_copy,
          rename_statement el el_copy,
          annotation )
  | For (id, expr1, expr2, expr3, statement, annotation) -> (
      let env_copy = Environment.copy env in
      match Environment.get env id with
      | None ->
          Environment.add env_copy id 0;
          For
            ( id,
              rename_expr expr1 env,
              rename_expr expr2 env,
              rename_expr expr3 env,
              rename_statement statement env_copy,
              annotation )
      | Some i ->
          Environment.modify env_copy id (i + 1);
          For
            ( id ^ "#" ^ string_of_int (i + 1),
              rename_expr expr1 env,
              rename_expr expr2 env,
              rename_expr expr3 env,
              rename_statement statement env_copy,
              annotation ))
  |While(test, body, annotation) -> (
      let env_copy = Environment.copy env in
      While(rename_expr test env, rename_statement body env_copy, annotation))
  | Foreach (id, test, body, annotation) -> (
      let env_copy = Environment.copy env in
      match Environment.get env id with
      | None ->
          Environment.add env_copy id 0;
          Foreach
            ( id,
              rename_expr test env,
              rename_statement body env_copy,
              annotation )
      | Some i ->
          Environment.modify env_copy id (i + 1);
          Foreach
            ( id ^ "#" ^ string_of_int (i + 1),
              rename_expr test env,
              rename_statement body env_copy,
              annotation ))
  | Draw_pixel (expr, annotation) ->
      Draw_pixel (rename_expr expr env, annotation)
  | Nop -> Nop
  | Print (expr, annotation) -> Print (rename_expr expr env, annotation)

let renaming (program : program) =
  let name_counter = Environment.new_environment () in
  match program with
  | Program (args_list, statement) ->
      let args_list = rename_list_args args_list name_counter in
      let statement = rename_statement statement name_counter in
      Program (args_list, statement)
