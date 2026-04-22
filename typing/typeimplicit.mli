open Types

type argument = {
  arg_flag: apply_flag;
  mutable arg_expression: Typedtree.expression option;
}

type pending_implicit = {
  implicit_id: Ident.t;
  implicit_env: Env.t;
  implicit_loc: Location.t;
  implicit_type: Path.t * Longident.t list * type_expr list;
  mutable implicit_constraints: (type_expr * type_expr) list;
  implicit_argument: argument;
}

val type_implicit_instance :
  (Env.t -> Typedtree.module_expr -> Path.t -> Longident.t list ->
   type_expr list -> Typedtree.module_expr * type_expr list) ref

val pack_implicit_ref :
  (pending_implicit -> Path.t -> Typedtree.expression) ref

val reunify_constraints : unit -> unit
val reset_pending_implicits : unit -> unit
val reset : unit -> unit

val pending_implicits : pending_implicit list list ref

val has_implicit : type_expr -> bool
val instantiate_one_implicit :
  Location.t -> Env.t -> Ident.t -> type_expr ->
  type_expr list -> pending_implicit
val add_pending_implicits : pending_implicit list -> unit
val generalize_implicits : unit -> unit
val generalize_implicits_ref : (unit -> unit) ref

module Link : sig
  val to_path : pending_implicit -> Path.t -> unit
  val to_expr : pending_implicit -> Typedtree.expression -> unit
end
