(** Module that represent programs in the Pixlang language. A program is composed of a list of arguments, followed by a statement that is the body of the program. Statements contain expressions. Arguments, statements and expressions contain annotations (their position and types).
    
This modules contains also a set of printer functions to display programs in a normalised form.*)

(** Types of the language*)
type type_expression =
  | Type_int
  | Type_real
  | Type_bool
  | Type_coord
  | Type_color
  | Type_pixel
  | Type_list of type_expression
  | Type_generic

module Annotation : sig
  (** Module representing annotations of elements of the program. It contains the {!Util.Position.t} of the parsed text that represent the element, and its {!type-type_expression}. The type is only useful for expressions. *)

  type t
  (** The Annotation type.*)

  val create : Util.Position.t -> t
  (** [create pos] creates an {!t} with the position set as [pos] and no type set.
      To be used within the parser (when creating the Ast), and if new expressions/statement are created during the analysis phase (notably if implementing the implicit cast).*)

  val get_pos : t -> Util.Position.t
  (** [get_pos annotation] return the position within [annotation].*)

  val get_type : t -> type_expression option
  (** [get_type annotation] returns [Some t] if [t] is the {!type-type_expression} within the annotation, and [None] if none is set.*)

  val set_type : t -> type_expression -> unit
  (** [set_type annotation t] sets the type within [annotation] to be [t]*)
end

(** Binary operators over {!expression}. Operations over {!Type_coord}, {!Type_color} and {!Type_pixel} are applied member by member (i.e., (a,b) + (c,d) * (a+c,b+d)).*)
type binary_operator =
  | Plus
      (** Plusition over {!Type_int}, {!Type_real}, {!Type_coord}, {!Type_color} and {!Type_pixel}, and concatenation over {!Type_list}. Also defined if one of the operands is a {!Type_int} and the other a {!Type_coord} or {!Type_color}, in which case, the integer is replaced by a pair of a triple (depending of the type) with its value. *)
  | Minus
      (** Substraction over {!Type_int}, {!Type_real}, {!Type_coord}, {!Type_color} and {!Type_pixel}. Also defined if one of the operands is a {!Type_int} and the other a {!Type_coord} or {!Type_color}, in which case, the integer is replaced by a pair of a triple (depending of the type) with its value. *)
  | Times
      (** Multiplication over {!Type_int}, {!Type_real}, {!Type_coord}, {!Type_color} and {!Type_pixel}. Also defined if one of the operands is a {!Type_int} and the other a {!Type_coord} or {!Type_color}, in which case, the integer is replaced by a pair of a triple (depending of the type) with its value. *)
  | Div
      (** Division over {!Type_int}, {!Type_real}, {!Type_coord}, {!Type_color} and {!Type_pixel}. Also defined if one of the operands is a {!Type_int} and the other a {!Type_coord} or {!Type_color}, in which case, the integer is replaced by a pair of a triple (depending of the type) with its value. *)
  | Rem
      (** Modulus over {!Type_int}, {!Type_real}, {!Type_coord}, {!Type_color} and {!Type_pixel}. Also defined if one of the operands is a {!Type_int} and the other a {!Type_coord} or {!Type_color}, in which case, the integer is replaced by a pair of a triple (depending of the type) with its value. *)
  | And  (** Boolean and*)
  | Or  (** Boolean or*)
  | Equal  (** Equality (polymorphic)*)
  | Diff  (** Inequality (polymorphic)*)
  | Lt  (** < (over {!Type_int}, {!Type_real} and {!Type_bool})*)
  | Gt  (** > (over {!Type_int}, {!Type_real} and {!Type_bool})*)
  | Leq  (** <= (over {!Type_int}, {!Type_real} and {!Type_bool})*)
  | Geq  (** >= (over {!Type_int}, {!Type_real} and {!Type_bool})*)

(** Unary operator over the language*)
type unary_operator =
  | Opposite  (** Unary minus (over {!Type_int} and {!Type_real})*)
  | Not  (** Boolean negation*)
  | Head  (** Head of a list*)
  | Tail  (** Tail of a list*)
  | Floor  (** The biggest {!Type_int} smaller than the {!Type_real} given *)
  | Real_of_int  (** The {!Type_real} corresponding to the {!Type_int} given *)
  | Cos  (** Cosinus function over {!Type_real}*)
  | Sin  (** Sinus function over {!Type_real}*)

(** Accessors to field of {!Type_pixel}, {!Type_coord} and {!Type_color}*)
type field_accessor =
  | Color_field
  | Coord_field
  | X_field
  | Y_field
  | Blue_field
  | Red_field
  | Green_field

(** The expressions of the language. Only non-obvious cases are commented below. Every expression contains an {!Annotation.t}*)
type expression =
  | Const_int of int * Annotation.t
  | Const_real of float * Annotation.t
  | Const_bool of bool * Annotation.t
  | Coord of expression * expression * Annotation.t
      (** Creates a {!Type_coord} from the two expressions in argument.*)
  | Color of expression * expression * expression * Annotation.t
      (** Creates a {!Type_color} from the three expressions in argument.*)
  | Pixel of expression * expression * Annotation.t
      (** Creates a {!Type_pixel} from the two expressions in argument.*)
  | Variable of string * Annotation.t
  | Binary_operator of binary_operator * expression * expression * Annotation.t
  | Unary_operator of unary_operator * expression * Annotation.t
  | Field_accessor of field_accessor * expression * Annotation.t
  | List of expression list * Annotation.t
  | Append of expression * expression * Annotation.t

(** The statements of the language. Only non-obvious cases are commented below. Every statement contains an {!Annotation.t}, except {!Nop}*)
type statement =
  | Affectation of expression * expression * Annotation.t
  | Declaration of string * type_expression * Annotation.t
  | Block of statement list * Annotation.t
      (** Block of consecutive statements. Declarations in this block are local to it*)
  | IfThenElse of expression * statement * statement * Annotation.t
  | For of
      string * expression * expression * expression * statement * Annotation.t
      (** For loop. [For(str,init,target,step,body,annot)] starts by initialising variable [str] (which must be declared) to the value of [init], and executes [body] and then increments [str] by the value of [step] as long as the value in [str] is smaller than that of [target]. [target] is reevaluated at each step. Can be used for {!Type_int} or {!Type_real} variables*)
  | While of expression * statement * Annotation.t
        (** While loop. [While(cond,body,annot)] executes [body] as long as [cond] is true. [cond] is reevaluated at each step*)
  | Foreach of string * expression * statement * Annotation.t
      (** [Foreach(str,list,body,annotation)] requires that [list] is a {!Type_list}. It applies [body] to every element of the list (from left to right). In [body], the current element of the loop is stored in variable [str] (which is not required to be declared)*)
  | Draw_pixel of expression * Annotation.t
      (** [Draw_pixel(pixel,annot)] draws [pixel] on the window of the program. [pixel] must be of type {!Type_pixel}*)
  | Nop
  | Print of expression * Annotation.t
      (** [Print(expr,annot)] displays the value of [expr] in the terminal (for debugging purposes)*)

(** Argument of the program*)
type argument = Argument of string * type_expression * Annotation.t

(** Type that represent a Stippled program. It contains a (possibly empty) list of declaration of arguments that will be read on the arguments of the interpreter, and a statement that is the body of the program. Every type can be read of argument, except of list of non-base type (i.e. list of int can be read, but not list of list of int).*)
type program = Program of argument list * statement

val string_of_type_expression : type_expression -> string
val string_of_binary_operator : binary_operator -> string
val string_of_unary_operator : unary_operator -> string
val string_of_field_accessor : field_accessor -> string
val string_of_expression : expression -> string
val string_of_pos : expression * expression -> string
val string_of_color : expression * expression * expression -> string
val string_of_list : expression list -> string
val pp_statement : Format.formatter -> statement -> unit
val pp_argument : Format.formatter -> argument -> unit
val pp_program : Format.formatter -> program -> unit
