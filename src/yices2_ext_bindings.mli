open Containers
open Sexplib

open Ctypes

open Yices2_high
open Types

module List : module type of List
  
val pp_sexp : Sexp.t Format.printer

include API with type 'a eh := 'a

module Types := Types
module Types : sig
  include module type of Types
  val pp_error_report : error_report Format.printer
end

module Type := Type
module Type : sig
  include module type of Type

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 20 *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t
end

module Term := Term
module Term : sig
  include module type of Term

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 20 *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t
end

module Model := Model
module Model : sig
  include module type of Model

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 1000 *)
  val pp : t Format.printer
end

module Action : sig

  type t =
    | DeclareType of string
    | DeclareFun of string * Type.t
    | Status
    | Reset
    | Push
    | Pop
    | EnableOption of string
    | DisableOption of string
    | AssertFormula of Term.t
    | AssertFormulas of Term.t list
    | Check of Param.t option
    | CheckWithAssumptions of Param.t option * Term.t list
    | Stop
    | GetModel
    | GetUnsatCore

  (** Appends the action sexp(s) on top of input list *)
  val to_sexp : Sexp.t list -> t -> Sexp.t list

end

module Context : sig

  val assert_blocking_clause : Context.t -> unit

  type assertions = Term.t list list
  val pp_assertions : assertions Format.printer

  type options
  val pp_options : options Format.printer

  type t = {
    config     : Config.t option;
    context    : Context.t; (* Raw context as in yices2_high *)
    assertions : assertions ref;
    options    : options;
    log        : Action.t list ref;
  }

  val pp : t Format.printer

  (** Appends the action log of the context on top of input list.
      The first executed action ends up as the head of the list *)
  val to_sexp : Sexplib.Type.t list -> t -> Sexplib.Type.t list

  val malloc : ?config:Config.t -> unit -> t

  (** Free does not free the config field (which could be shared with other contexts) *)
  val free : t -> unit
  val status : t -> Types.smt_status
  val reset  : t -> unit
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> Term.t -> unit
  val assert_formulas : t -> Term.t list -> unit
  val check                  : ?param:Param.t -> t -> Types.smt_status
  val check_with_assumptions : ?param:Param.t -> t -> Term.t list -> Types.smt_status
  val stop : t -> unit
  val get_model : t -> keep_subst:bool -> Model.t
  val get_unsat_core : t -> Term.t list
  val declare_type : t -> string -> unit    
  val declare_fun  : t -> string -> Type.t -> unit
end

module Param := Param
module Param : sig
  include module type of Param
  val default : Context.t -> t -> unit
end

