open Yices2.Ext
open Types_ext

(***********************)
(* Arrays with lengths *)
(***********************)

module AddLength : sig

  include StandardExt

  module ExtraType : sig
    val lfun :
      ?admissible:Term.t -> length_type:Type.t -> dom:Type.t list -> codom:Type.t -> unit -> Type.t
    val is_lfun : Type.t -> bool
    val length_type : Type.t -> Type.t
    val dom         : Type.t -> Type.t list
    val codom       : Type.t -> Type.t
    val length      : Type.t -> Term.t
    val as_fun      : Type.t -> Term.t
    val update      : Type.t -> Term.t
    val application : Type.t -> Term.t
    val admissible  : Type.t -> Term.t
  end

  module ExtraTerm : sig
    val is_lfun     : Term.t -> bool
    val length_type : Term.t -> Type.t
    val dom         : Term.t -> Type.t list
    val codom       : Term.t -> Type.t
    val length      : Term.t -> Term.t
    val as_fun      : Term.t -> Term.t
    val admissible  : lfun:Term.t -> Term.t -> Term.t list -> Term.t
    val update      : Term.t -> Term.t list -> Term.t -> Term.t
    val application : Term.t -> Term.t list -> Term.t
    type reveal =
      Update of Term.t * Term.t list * Term.t
    | Application of Term.t * Term.t list
    | Length of Term.t
    | AsFun of Term.t
    val reveal : 'a Types.termstruct -> reveal option
  end

end

include StandardYicesContext
