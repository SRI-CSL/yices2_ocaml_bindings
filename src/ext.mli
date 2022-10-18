open Containers
open Sexplib

open Common

include High.API with type 'a eh := 'a
open Ext_types

val sexp : string -> Sexp.t list -> Sexp.t
val pp_sexp : Sexp.t Format.printer

module Config : Config
module Types  : Types
module Model  : Model with type t     = Model.t
                       and type typ   := Type.t
                       and type term  := Term.t

(* Supported models *)
module SModel : SModel with type term  := Term.t
                        and type model := Model.t

module Assertions : sig
  type t =
    private {
        list  : Term.t list option list; (* List of assertions at each level. None at some level
                                            if a blocking clause has been asserted there *)
        level : int (* Always equal to (List.length list - 1), but gives O(1) access *)
      }
  val init : t

  exception BlockingClauseUsage
  val assertions : t -> Term.t list (* flattens all levels into a list of assertions
                                       raises BlockingClauseUsage
                                       if None is found at any level *)
  val to_sexp : t -> Sexp.t
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
    | AssertBlockingClause
    | Check of Param.t option
    | CheckWithAssumptions of { param : Param.t option; assumptions : Term.t list }
    | Stop
    | GetModel of { keep_subst : bool }
    | GetUnsatCore
    | CheckWithModel of { param : Param.t option; smodel : SModel.t }
    | CheckWithInterpolation of { param : Param.t option;
                                  build_model : bool;
                                  is_first    : bool;
                                  other_assertions : Assertions.t;
                                  other_log : t list }
    | GetModelInterpolant
    | GarbageCollect of Sexp.t list

  (** Appends the action sexp(s) on top of input list *)
  val to_sexp : Sexp.t list -> t -> Sexp.t list

end

module Context : Context with type typ    := Type.t
                          and type term   := Term.t
                          and type assertions := Assertions.t
                          and type action := Action.t
                          and type config := Config.t
                          and type param  := Param.t
                          and type smodel  := SModel.t

module Param : High_types.Param with type 'a eh   := 'a
                                 and type t = Param.t
                                 and type context := Context.t

(** Default is true: *)
val use_type_names : bool ref
val use_term_names : bool ref
val use_type_notations : bool ref
val use_term_notations : bool ref

module Type : Type with type t        = Type.t
                    and type context := Context.t

module TermSet : module type of Set.Make(Term)

module Term : Term with type t    = Term.t
                    and type typ := Type.t
                    and type context := Context.t
                    and type termset := TermSet.t

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
  val is_free : var:Term.t -> t -> bool
  val fv      : t -> TermSet.t
end

module ExtTerm : sig
  type 'a bs = private BS
  type 'a ts = private TS

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
  val is_free      : var:Term.t -> _ base -> bool
  val fv           : _ base -> TermSet.t
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

module GC : sig
  include module type of GC
                       
  (** Like regular GC, with two additional arguments.
   record_log (default is false):
   true means that the logs (referencing terms and types that may disappear)
   are converted to S-expressions, at a computational cost; otherwise logs are erased.
   contexts (default is []):
   which contexts are we looking at for editing their logs (global log is always edited) *)
  val garbage_collect :
    ?record_log:bool ->
    ?contexts:Context.t list ->
    Term.t list -> Type.t list -> bool -> unit
end
