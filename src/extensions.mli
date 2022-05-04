open Containers
open Ext_bindings

module type YicesContext = sig

  type term
  type t
  type config
  type model

  val malloc : ?config:config -> unit -> t
  val free : t -> unit
  val status : t -> Types.smt_status
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val check : ?param:Param.t -> t -> Types.smt_status
  val get_model : ?keep_subst:bool -> t -> model

  val pp_log : t Format.printer
end

module type StandardYicesContext =
  YicesContext with type term   = Term.t
                and type config = Config.t
                and type model  = Model.t

module Context : StandardYicesContext with type t = Context.t

type ('model, 'interpolant) answer =
  | Sat of 'model
  | Unsat of 'interpolant

module type Ext = sig

  type old_term
  type old_config
  type old_model

  type term
  type config
  type model
  type t (* mutable state *)

  val malloc : ?config:config -> unit -> old_config option * t
  val free : t -> unit
  val push   : t -> unit
  val pop    : t -> unit

  val assert_formula : (old_term -> unit) -> t -> term -> unit
  val check : old_model -> (model, old_term) answer

end

module type StandardExt =
  Ext with type old_term   := Term.t
       and type old_config := Config.t
       and type old_model  := Model.t
       and type term   := Term.t
       and type config := Config.t
       and type model  := Model.t

module Make
         (Context : YicesContext)
         (C : Ext with type old_term   := Context.term
                   and type old_config := Context.config
                   and type old_model  := Context.model) :
YicesContext with type term = C.term
              and type config = C.config
              and type model  = C.model

module Trivial : sig
  type t = unit
  val malloc : ?config:'a -> t -> 'a option * t
  val free : 'a -> t
  val push : 'a -> t
  val pop : 'a -> t
end

module AddDiff : sig

  include StandardExt

  module ExtraType : sig
    val diff : Types.uninterpreted -> Term.t list
  end

  module ExtraTerm : sig
    val diff : Term.t -> Term.t -> Term.t list
    val reveal : 'a Types.termstruct -> (Term.t * Term.t) option
  end

end

module Diff : StandardYicesContext

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

module DiffLength : StandardYicesContext
