open Ext_bindings

module Type : sig
  val get_body : Type.t -> Type.t
  val get_var  : Type.t -> Type.t
  val reset : unit -> unit
  type accu := (Type.t * Type.t) list
  val purify : (Type.t -> bool) -> Type.t -> accu -> Type.t * accu
end

module Term : sig
  val get_body : Term.t -> Term.t
  val get_var  : Term.t -> Term.t
  val reset : unit -> unit
  type accu := (Term.t * Term.t) list
  val purify : (Term.t -> bool) -> Term.t -> accu -> Term.t * accu
end

(* resets both Type and Term *)
val reset : unit -> unit
