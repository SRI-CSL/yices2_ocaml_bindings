open Containers
open Sexplib

open Common
open High
   
module type Config = sig
  type t
  val malloc : unit -> t
  val free : t -> unit
  val set : t -> name:string -> value:string -> unit
  val default : ?logic:string -> t -> unit
  val get     : t -> String.t -> String.t
  val options : t -> (String.t * String.t) list
end

module type Types = sig
  include module type of Types
  val pp_error_report : error_report Format.printer
end

type config_options = String.t HStrings.t

type config = {
    config : Low.Types.ctx_config_t Ctypes.ptr;
    options: config_options;
    mcsat  : bool ref
  }


module type Context = sig

  type typ
  type term
  type assertions
  type action
  type config
  type param
  type model
  type smodel

  val pp_options        : unit HStrings.t Format.printer
  val pp_config_options : string HStrings.t Format.printer

  type t

  val assertions     : t -> assertions (* Structure of assertions and level *)
  val options        : t -> unit HStrings.t   (* Set options (hashtbl copy) *)
  val config_options : t -> string HStrings.t (* Set config options (hashtbl copy) *)
  val log            : t -> action list     (* Everything that happened to that context *)
  val is_alive       : t -> bool            (* Whether the raw context wasn't freed *)
  val is_mcsat       : t -> bool            (* Whether the context uses mcsat *)

  (** Turns the log into list of S-expressions.
      The first executed action ends up as the head of the list. *)
  val to_sexp : t -> Sexplib.Type.t list

  (** Prints the assertions. *)
  val pp : t Format.printer

  (** Prints the log. *)
  val pp_log : t Format.printer

  val malloc : ?config:config -> unit -> t
  val malloc_mcsat : unit -> t

  (** All contexts ever created *)
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
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val assert_blocking_clause : t -> unit

  val check_with_model : ?param:param -> t -> model -> term list -> Types.smt_status
  val check : ?param:param -> ?assumptions:term list -> ?smodel:smodel -> t
              -> Types.smt_status
  val stop             : t -> unit
  val get_model        : ?keep_subst:bool -> ?support:term list -> t -> smodel
  val get_unsat_core   : t -> term list
  val get_model_interpolant : t -> term
    
  val check_with_interpolation  :
    ?build_model:bool -> ?param:param -> t -> t
    -> (term, ?support:term list -> unit -> smodel) Types.smt_status_with_answers

  (* The next two functions are just adding declarations to the log,
     they do not actually introduce new Yices types and terms *)
  val declare_type   : t -> string -> unit    
  val declare_fun    : t -> string -> typ -> unit

end

module type Type = sig

  include High_types.Type with type 'a eh := 'a

  type context

  (** Introduce a notation for pretty-printing a type (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  val new_uninterpreted  : ?contexts:context list -> ?name:string -> unit -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t

  (** All uninterpreted types; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

end

module type Term = sig
  include High_types.Term with type 'a eh := 'a

  type context
  type termset

  val new_uninterpreted  : ?contexts:context list -> ?name:string -> typ -> t

  (** Introduce a notation for pretty-printing a term (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp_termstruct : _ Types.termstruct -> Sexp.t
  val to_sexp            : t -> Sexp.t

  val is_free_termstruct : var:t -> _ Types.termstruct -> bool
  val is_free            : var:t -> t -> bool
  val fv_termstruct      : _ Types.termstruct -> termset
  val fv                 : t -> termset

  (** For bitvector terms *)
  val width_of_term : t -> int

  (** All uninterpreted terms; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

end

module type Model = sig
  include High_types.Model with type 'a eh := 'a

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 1000 *)
  val pp : t Format.printer
end

(* Supported models *)
module type SModel = sig

  type term
  type model
     
  type t = { support : term list;
             model   : model }
  val make : ?support:term list -> model -> t
  val pp :
    ?pp_start:unit Format.printer ->
    ?pp_stop:unit Format.printer ->
    ?pp_sep:unit Format.printer -> unit -> t Format.printer

  val from_map  : (term * term) list -> t

  val as_map  : t -> (term * term) list

  (* turns every assignment (x |-> v) into (x === v);
     unless x is Boolean in which case it produces x or (not x) depending on v;
     crashes if v cannot be expressed as a term, like algebraic numbers or functional values *)
  val as_assumptions : t -> term list

  (* converts assumptions into supported model,
     turning every assumption f into a Boolean assignment (x |-> true),
     for a Boolean uninterpreted term x purifying f, i.e. with constraint (x <==> f).
     It returns a triple (smodel, pures, constraints) where
     smodel is the constructed supported model,
     pures are the list of purification variables
     constraints are their definition constraints *)
  val from_assumptions : mcsat:bool -> ?smodel:t -> term list -> t * term list * term list 
end

module type BaseAPI = sig
  include High_types.BaseAPI
  module Types : Types
end


module type API = sig

  include High.API with type 'a eh := 'a

  module Config : Config
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
                            and type model  := Model.t
                            and type smodel  := SModel.t

  module Param : High_types.Param with type 'a eh   := 'a
                                   and type t = Param.t
                                   and type context := Context.t

  module Type : Type with type t        = Type.t
                      and type context := Context.t

  module TermSet : module type of Set.Make(Term)

  module Term : Term with type t    = Term.t
                      and type typ := Type.t
                      and type context := Context.t
                      and type termset := TermSet.t

  module BoolStruct : sig
    type 'a t =
      | Leaf of 'a
      | And of 'a t list
      | Or  of 'a t list
      | Not of 'a t
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

  module Purification : Purification_types.API with type typ  := Type.t
                                                and type term := Term.t
end
