(** Module that represent environments that are useful for both interpreter and analyser.
    An environment is a dictionary from names (string) to some value.*)

type 'a t
(** The environment type*)

val new_environment : unit -> 'a t
(** Creates an empty environment.*)

val copy : 'a t -> 'a t
(** Creates a copy of the environment. Every cell of the copied environment shares its value with the original one, but isn't linked to it, so modifications to one won't be visible in the other.*)

val add : 'a t -> string -> 'a -> unit
(** [add environement name value] associates [value] to [name].*)

val add_ref : 'a t -> string -> 'a ref -> unit
(** [add_ref environment name ref] associates the reference [ref] to [name] in the topmost layer, even if name is already associated below. 
        Useful for making several names (even in different environment) point to a common object, that is modified by all of them (i.e., aliasing, or reference passing arguments).*)

val get : 'a t -> string -> 'a option
(** [get environment name] gets [Some v] if [v] is the value associated to [name], and returns [None] if it is not defined.*)

val get_ref : 'a t -> string -> 'a ref option
(** [get_ref environment name] get [Some r] if [r] is the reference associated to [name], and returns [None] if it is not defined. Modifications made to that reference will be visible in the environment. Useful for creating aliasing (with {!add_ref}).*)

val modify : 'a t -> string -> 'a -> unit
(** [modify environment name value] replaces the value associated to [name] by [value] if it is defined, and otherwise makes that association.*)

val pp_environment :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
