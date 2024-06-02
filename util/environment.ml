type 'a t = (string, 'a ref) Hashtbl.t

let new_environment : unit -> 'a t = fun () -> Hashtbl.create 10

let copy (env : 'a t) : 'a t =
  let copy = Hashtbl.copy env in
  Hashtbl.iter (fun name value -> Hashtbl.replace copy name (ref !value)) env;
  copy

let add (env : 'a t) var typ =
  match Hashtbl.find_opt env var with
  | Some r -> r := typ
  | None -> Hashtbl.replace env var (ref typ)

let add_ref (env : 'a t) var reference = Hashtbl.replace env var reference
let get_ref (env : 'a t) var = Hashtbl.find_opt env var
let get (env : 'a t) var = Option.map (fun a -> !a) (get_ref env var)

let modify (env : 'a t) var typ =
  match get_ref env var with Some r -> r := typ | None -> add env var typ

let pp_environment printer fmt (env : 'a t) =
  Format.fprintf fmt "@[<v 2><@,";
  Hashtbl.iter
    (fun name value -> Format.fprintf fmt "%s : %a@," name printer !value)
    env;
  Format.fprintf fmt ">@]"