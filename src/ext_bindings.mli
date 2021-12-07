open Containers
open Sexplib

open High

module List : module type of List
  
val pp_sexp : Sexp.t Format.printer

include API with type 'a eh := 'a

module Types := Types
module Types : sig
  include module type of Types
  val pp_error_report : error_report Format.printer
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

  type assertions =
    private {
        list  : Term.t list list; (* List of assertions at each level *)
        level : int (* Always equal to (List.length list - 1), but gives O(1) access *)
      }
  val pp_assertions : assertions Format.printer

  type options
  val pp_options : options Format.printer

  type t =
    private {
        config     : Config.t option; (* In case a config was used at creation *)
        context    : Context.t;       (* Raw context as in yices2_high *)
        assertions : assertions ref;  (* Structure of assertions and level *)
        options    : options;         (* Set options *)
        log        : Action.t list ref; (* Everything that happened to that context *)
        is_alive   : bool ref         (* If the raw context wasn't freed *)
      }

  val pp : t Format.printer

  (** Turns the log into list of S-expressions.
      The first executed action ends up as the head of the list. *)
  val to_sexp : t -> Sexplib.Type.t list

  val malloc : ?config:Config.t -> unit -> t

  (** All contextx ever created *)
  val all  : unit -> t list

  (** Free does not free the config field (which could be shared with other contexts) *)
  val free : t -> unit
  val status : t -> Types.smt_status
  val reset  : t -> unit
  val push   : t -> unit        (* Open new level and go there *)
  val pop    : t -> unit        (* Go back 1 level *)
  val goto   : t -> int -> unit (* Go to level *)
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> ?level:int -> Term.t -> unit
  val assert_formulas : t -> ?level:int -> Term.t list -> unit
  val check                  : ?param:Param.t -> t -> Types.smt_status
  val check_with_assumptions : ?param:Param.t -> t -> Term.t list -> Types.smt_status
  val stop             : t -> unit
  val get_model        : t -> keep_subst:bool -> Model.t
  val get_unsat_core   : t -> Term.t list
  val check_with_model : ?param:Param.t -> t -> Model.t -> Term.t list -> Types.smt_status
  val get_model_interpolant : t -> Term.t

  (* The next two functions are just adding declarations to the log,
     they do not actually introduce new Yices types and terms *)
  val declare_type   : t -> string -> unit    
  val declare_fun    : t -> string -> Type.t -> unit

end

module Type := Type
module Type : sig
  include module type of Type

  val new_uninterpreted  : ?contexts:Context.t list -> ?name:string -> unit -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t

  (** All uninterpreted types *)
  val all_uninterpreted  : unit -> t list

end

module TermSet : module type of Set.Make(Term)

module Term := Term
module Term : sig
  include module type of Term

  val new_uninterpreted  : ?contexts:Context.t list -> ?name:string -> Type.t -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp_termstruct : _ Types.termstruct -> Sexp.t
  val to_sexp            : t -> Sexp.t

  val is_free_termstruct : var:t -> _ Types.termstruct -> bool
  val is_free            : var:t -> t -> bool
  val fv_termstruct      : _ Types.termstruct -> TermSet.t
  val fv                 : t -> TermSet.t

  (** For bitvector terms *)
  val width_of_term : t -> int

  (** All uninterpreted terms *)
  val all_uninterpreted  : unit -> t list

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

module Model := Model
module Model : sig
  include module type of Model

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 1000 *)
  val pp : t Format.printer
end

module Param := Param
module Param : sig
  include module type of Param
  val default : Context.t -> t -> unit
end

module Global : module type of Global
