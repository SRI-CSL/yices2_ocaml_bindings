open Containers
open Sexplib

open Common
open High
open Types

module Types = struct
  
  type config_options = String.t HStrings.t

  type config = Config of {
      config : config_ptr;
      options: config_options;
      mcsat  : bool ref
    }

  type 'a bool_struct =
    | Leaf of 'a
    | And of 'a bool_struct list
    | Or  of 'a bool_struct list
    | Not of 'a bool_struct
               [@@deriving eq, show]

  type smodel = SModel of { support : term_t list;
                            model   : model_ptr }

  type assertions = Assertions of {
        list  : term_t list option list; (* List of assertions at each level. None at some level
                                            if a blocking clause has been asserted there *)
        level : int (* Always equal to (List.length list - 1), but gives O(1) access *)
      }

  type action =
    | DeclareType of type_t * int option
    | DeclareFun of term_t * type_t
    | DefineType of string * type_t
    | DefineFun of string * term_t * type_t
    | Status
    | Reset
    | Push
    | Pop
    | EnableOption of string
    | DisableOption of string
    | AssertFormula of term_t
    | AssertFormulas of term_t list
    | AssertBlockingClause
    | Check of param_ptr option
    | CheckWithAssumptions of { param : param_ptr option; assumptions : term_t list }
    | Stop
    | GetModel of { keep_subst : bool }
    | GetUnsatCore
    | CheckWithModel of { param : param_ptr option; smodel : smodel }
    | CheckWithInterpolation of { param : param_ptr option;
                                  build_model : bool;
                                  is_first    : bool;
                                  other_assertions : assertions;
                                  other_log : action list }
    | GetModelInterpolant
    | GarbageCollect of Sexp.t list

  type slice = Slice of {
      extractee : term_t;
      indices   : (int * int) option;
    }

  type 'a bs = private BS
  type 'a ts = private TS

  type (_,_) base =
    | TermStruct : 'a termstruct -> ('a ts, [`tstruct]) base
    | T      : term_t                  -> (_  ts, [`closed] ) base
    | Bits   : term_t list             -> ([`bits]  bs, _) base 
    | SliceStruct : slice bool_struct       -> ([`slice] bs, _) base
    | Concat : [`block] closed list    -> ([`concat],   _) base
    | Block  : { block : _ bs closed;
                 sign_ext  : int; (* Length of sign extension *)
                 zero_ext  : int; (* Length of zero extension *) } -> ([`block], _) base

  and 'a closed     = ('a, [`closed])  base

end

module type Config = sig
  type t
  val malloc : unit -> t
  val free : t -> unit
  val set : t -> name:string -> value:string -> unit
  val default : ?logic:string -> t -> unit
  val get     : t -> String.t -> String.t
  val options : t -> (String.t * String.t) list
end

module type Context = sig

  type typ
  type term
  type assertions
  type action
  type config
  type param
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
  (* When representing functions in S-expressions,
     do we want to use SMT2 arrays?
     if not, function updates will not be compliant with SMT2 standard;
     if yes, do we want multi-argument functions to be Curryified or expressed as tuples?
     (tuples are not in the SMT2 standard but are accepted by, e.g., CVC5)
     in the case of terms, which terms
   *)
  val to_sexp : ?smt2arrays: [`Tuple | `Curry ]*(term -> bool) -> t -> Sexplib.Type.t list

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

  val status : t -> smt_status
  val reset  : t -> unit
  val push   : t -> unit        (* Open new level and go there *)
  val pop    : t -> unit        (* Go back 1 level *)
  val goto   : t -> int -> unit (* Go to level *)
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val assert_blocking_clause : t -> unit

  val check : ?param:param -> ?assumptions:term list -> ?smodel:smodel -> ?hints:term list -> t -> smt_status
  val set_var_order : t -> term list -> smt_status

  val stop             : t -> unit
  val get_model        : ?keep_subst:bool -> ?support:term list -> t -> smodel
  val get_unsat_core   : t -> term list
  val get_model_interpolant : t -> term
    
  val check_with_interpolation  :
    ?build_model:bool -> ?param:param -> t -> t
    -> (term, ?support:term list -> unit -> smodel) smt_status_with_answers

  (* The next two functions are just adding declarations to the log,
     they do not actually introduce new Yices types and terms *)
  val declare_type   : t -> ?card:int -> typ -> unit
  val declare_fun    : t -> term -> typ -> unit

end

module type Names = sig
  type t
  type context
  val set      : ?contexts:context list -> t -> string -> unit
  val remove   : string -> unit
  val clear    : t -> unit
  val is_name  : string -> bool
  val of_name  : string -> t
  val has_name : t -> bool
  val to_name  : t -> string
end  

module type Type = sig

  include High_types.Type with type 'a eh := 'a

  type context
  
  (** Introduce a notation for pretty-printing a type (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  val new_uninterpreted  : ?contexts:context list -> ?name:string -> ?card:int -> unit -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp : ?smt2arrays:[`Tuple | `Curry ] -> t -> Sexplib.Type.t

  (** All uninterpreted types; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

  module Names : Names with type context := context
                        and type t := t
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

  val to_sexp            : ?smt2arrays:[`Tuple | `Curry ]*(t -> bool) -> t -> Sexp.t

  val is_free_termstruct : var:t -> _ termstruct -> bool
  val is_free            : var:t -> t -> bool
  val fv_termstruct      : _ termstruct -> termset
  val fv                 : t -> termset

  (** For bitvector terms *)
  val width_of_term : t -> int

  (** All uninterpreted terms; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

  module Names : Names with type context := context
                        and type t := t
end

module type Value = sig

  type model
  val pp     : model -> yval Format.printer
  val pp_val : model -> yval_t Ctypes.ptr Format.printer

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
     
  type t

  val make : ?support:term list -> model -> t
  val free : t -> unit

  val pp :
    ?pp_start:unit Format.printer ->
    ?pp_stop:unit Format.printer ->
    ?pp_sep:unit Format.printer -> unit -> t Format.printer

  val from_map  : ?support:term list -> (term * term) list -> t

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

  module HighTypes := High.Types
  module ExtTypes  := Types

  include High_types.BaseAPI

  module Types : sig
    include module type of HighTypes
    include module type of ExtTypes
    type context
    val pp_error_report : error_report Format.printer
  end

end

open Types

module type API = sig

  include High.API with type 'a eh := 'a

  module Config : Config with type t = config
  module Model  : Model with type t     = Model.t
                         and type typ   := Type.t
                         and type term  := Term.t

  module Value : Value with type model := Model.t

  (* Supported models *)
  module SModel : SModel with type term  := Term.t
                          and type model := Model.t
                          and type t = smodel

  module Assertions : sig

    type t = assertions

    val init : t

    exception BlockingClauseUsage
    val assertions : t -> Term.t list (* flattens all levels into a list of assertions
                                         raises BlockingClauseUsage
                                         if None is found at any level *)
    val to_sexp : ?smt2arrays:[`Tuple | `Curry ]*(Term.t -> bool) -> t -> Sexp.t
    val pp : t Format.printer
  end
  
  module Action : sig

    type t = action

    (** Appends the action sexp(s) on top of input list *)
    val to_sexp : ?smt2arrays:[`Tuple | `Curry ]*(Term.t -> bool) -> Sexp.t list -> t -> Sexp.t list

  end

  module Context : Context with type typ    := Type.t
                            and type term   := Term.t
                            and type assertions := Assertions.t
                            and type action := Action.t
                            and type config := Config.t
                            and type param  := Param.t
                            and type smodel := SModel.t

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
    type 'a t = 'a bool_struct [@@deriving eq, show, ord]
    val map : ('a -> 'b) -> 'a t -> 'b t
    val nnf : bool -> 'a t -> 'a t (* Negation Normal Form *)
  end

  module Slice : sig
    type t = slice
    val build   : ?indices:(int*int) -> Term.t -> t
    val to_term : t -> Term.t
    val pp      : t Format.printer
    val width   : t -> int
    val is_free : var:Term.t -> t -> bool
    val fv      : t -> TermSet.t
  end

  module ExtTerm : sig

    type nonrec ('a,'b) base = ('a,'b) base

    type 'a closed     = ('a, [`closed])  base
    type 'a termstruct = ('a, [`tstruct]) base

    type 'a bits   = ([`bits]  bs, 'a) base
    type 'a slice  = ([`slice] bs, 'a) base
    type 'a concat = ([`concat], 'a) base
    type 'a block  = ([`block], 'a) base

    type bit_select = Term.t * int [@@deriving eq, ord]
    type bit_struct = bit_select BoolStruct.t [@@deriving eq, ord]

    val return_block : _ bs closed -> _ block
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
    val of_yterm : yterm  -> yt
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
