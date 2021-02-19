open Containers
open Sexplib

open Yices2_high

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

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t
end

module Term := Term
module Term : sig
  include module type of Term

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp_termstruct : _ Types.termstruct -> Sexp.t
  val to_sexp : t -> Sexp.t

  val fv_termstruct : Term.t -> _ Types.termstruct -> bool
  val fv : Term.t -> Term.t -> bool

  (** For bitvector terms *)
  val width : Term.t -> int
end

module BoolStruct : sig
  type 'a t = Leaf of 'a | And of 'a t list | Or of 'a t list | Not of 'a t
  [@@deriving eq, show, ord]
  val map : ('a -> 'b) -> 'a t -> 'b t
  val nnf : bool -> 'a t -> 'a t (* Negation Normal Form *)
end

module Slice : sig
  type t = private {
    extractee : Term.t;
    indices   : (int * int) option;
  }
  val build   : ?indices:(int*int) -> Term.t -> t
  val to_term : t -> Term.t
  val to_sexp : t -> Sexp.t
  val pp      : t Format.printer
  val width   : t -> int
  val fv      : Term.t -> t -> bool
end

module ExtTerm : sig
  type 'a bs    = private BS
  type 'a ts    = private TS

  type (_,_) base =
    | TermStruct : 'a Types.termstruct -> ('a ts, [`tstruct]) base
    | T      : Term.t                  -> (_  ts, [`closed] ) base
    | Bits   : Term.t list             -> ([`bits]  bs, _) base 
    | Slice  : Slice.t BoolStruct.t    -> ([`slice] bs, _) base
    | Concat : [`block] closed list    -> ([`concat],   _) base
    | Block  : { block : _ bs closed;
                 sign_ext  : int; (* Length of sign extension *)
                 zero_ext  : int; (* Length of zero extension *) } -> ([`block], _) base

  and 'a closed      = ('a, [`closed])  base
  type 'a termstruct = ('a, [`tstruct]) base

  type 'a bits   = ([`bits]  bs, 'a) base
  type 'a slice  = ([`slice] bs, 'a) base
  type 'a concat = ([`concat], 'a) base
  type 'a block  = ([`block], 'a) base

  type bit_select = Term.t * int [@@deriving eq, ord]
  type bit_struct = bit_select BoolStruct.t [@@deriving eq, ord]

  val return_block : _ bs closed -> _ block
  val to_sexp      : _ base -> Sexp.t
  val pp           : _ base Format.printer
  val to_term      : _ base -> Term.t
  val build        : 'a termstruct -> 'a closed
  val width        : _ base -> int
  val fv           : Term.t -> _ base -> bool
  val typeof       : _ base -> Type.t
  val bvarray      : Term.t list -> _ block list
  module MTerm(M : Monad) : sig
    type update = { apply : 'a. 'a closed -> 'a closed M.t }
    val map       : update ->   'a termstruct -> 'a termstruct M.t
  end

  type t  = ExtTerm  : _ closed -> t
  type yt = YExtTerm : _ termstruct -> yt
  val of_yterm : Types.yterm -> yt
  val of_term  : Term.t -> yt

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
    | CheckWithModel of Param.t option * Model.t * Term.t list
    | GetModelInterpolant

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

  (** Turns the log into list of S-expressions.
      The first executed action ends up as the head of the list. *)
  val to_sexp : t -> Sexplib.Type.t list

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

  val check_with_model : ?param:Param.t -> t -> Model.t -> Term.t list -> Types.smt_status
  val get_model_interpolant : t -> Term.t
end

module Param := Param
module Param : sig
  include module type of Param
  val default : Context.t -> t -> unit
end

