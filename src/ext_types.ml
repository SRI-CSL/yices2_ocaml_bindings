(** Types and signatures for the Ext layer and its derived utilities. *)
open Containers
open Sexplib

open Common
open High
open Types

module Types = struct

  type smt_status = High.Types.smt_status
  type error_code = High.Types.error_code
  type type_t = High.Types.type_t
  module HTypes = High.Types.HTypes
  module HTerms = High.Types.HTerms
  
  (** Configuration options as a string map. *)
  type config_options = String.t HStrings.t

  (** Configuration wrapper with options and logic hints. *)
  type config = Config of {
      config : config_ptr;
      options: config_options;
      logic  : string option ref;
      mcsat  : bool ref
    }

  (** Boolean structure for composite conditions. *)
  type 'a bool_struct =
    | Leaf of 'a
    | And of 'a bool_struct list
    | Or  of 'a bool_struct list
    | Not of 'a bool_struct
               [@@deriving eq, show]

  (** Supported model with optional support list. *)
  type smodel = SModel of { support : term_t list;
                            model   : model_ptr }

  (** Assertions organized by context level. *)
  type assertions = Assertions of {
        list  : term_t list option list; (* List of assertions at each level. None at some level
                                            if a blocking clause has been asserted there *)
        level : int (* Always equal to (List.length list - 1), but gives O(1) access *)
      }

  (** Actions that can be recorded against a context. *)
  type context_action =
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
    | GetModelInterpolant

  (** Global actions recorded in the log. *)
  type action =
    | DeclareType of type_t * int option
    | DeclareFun of term_t * type_t
    | DefineType of string * type_t
    | DefineFun of string * term_t * type_t
    | CheckWithInterpolation of { param : param_ptr option;
                                  build_model : bool;
                                  context1 : int;
                                  context2 : int }
    | GarbageCollect of Sexp.t list
    | NewContext of { logic : string option }
    | ContextAction of { context_id : int ; context_action : context_action }

  (** Slice descriptor for bitvector extraction. *)
  type slice = Slice of {
      extractee : term_t;
      indices   : (int * int) option;
    }

  (** Private markers for extended term encodings. *)
  type 'a bs = private BS
  type 'a ts = private TS

  (** Base representation for extended terms and bit-structures. *)
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

module type Global = sig
  type term
  include High_types.Global with type 'a eh := 'a

  (**
   {v
      log ()
      v}
      Return the global action log. *)
  val log : unit -> Types.action list

  (**
   {v
      to_sexp ?smt2arrays ()
      v}
      Convert the global log to S-expressions.

      smt2arrays = (mode, as_function) controls how functions are encoded:
      - mode = `Curry or `Tuple for multi-argument array encoding
      - as_function is applied to function-typed terms to decide whether their
        applications stay as function applications or are encoded as array reads. *)
  val to_sexp : ?smt2arrays:[ `Curry | `Tuple ] * (term -> bool) -> unit -> Sexp.t list

  (**
   {v
      pp_log fmt ()
      v}
      Pretty-print the global log. *)
  val pp_log : Format.formatter -> unit
end

module type Config = sig
  type t

  (**
   {v
      malloc ()
      v}
      Allocate a configuration. *)
  val malloc : unit -> t

  (**
   {v
      free cfg
      v}
      Release a configuration. *)
  val free : t -> unit

  (**
   {v
      set cfg ~name ~value
      v}
      Set a configuration option. *)
  val set : t -> name:string -> value:string -> unit

  (**
   {v
      default ?logic cfg
      v}
      Load defaults (optionally for a logic). *)
  val default : ?logic:string -> t -> unit

  (**
   {v
      get cfg key
      v}
      Read a configuration option. *)
  val get     : t -> String.t -> String.t

  (**
   {v
      options cfg
      v}
      Return configuration options as an association list. *)
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

  (** Pretty-printer for a unit option set. *)
  val pp_options        : unit HStrings.t Format.printer

  (** Pretty-printer for config options. *)
  val pp_config_options : string HStrings.t Format.printer

  type t

  (**
   {v
      assertions ctx
      v}
      Return the assertion structure and current level. *)
  val assertions     : t -> assertions

  (**
   {v
      options ctx
      v}
      Return a copy of the context options. *)
  val options        : t -> unit HStrings.t

  (**
   {v
      config_options ctx
      v}
      Return a copy of config options. *)
  val config_options : t -> string HStrings.t

  (**
   {v
      log ctx
      v}
      Return the action log for this context. *)
  val log            : t -> action list

  (**
   {v
      is_alive ctx
      v}
      Whether the raw context has not been freed. *)
  val is_alive       : t -> bool

  (**
   {v
      is_mcsat ctx
      v}
      Whether the context uses MCSAT. *)
  val is_mcsat       : t -> bool

  (**
   {v
      id ctx
      v}
      Return the context id. *)
  val id             : t -> int

  (**
   {v
      of_id i
      v}
      Find a context by id. *)
  val of_id          : int -> t option

  (**
   {v
      all ()
      v}
      Return all live contexts. *)
  val all            : unit -> t Seq.t
  
  (** Turns the log into list of S-expressions.
      The first executed action ends up as the head of the list. *)
  (**
   {v
      to_sexp ?smt2arrays ctx
      v}
      Convert the context log to S-expressions.

      smt2arrays = (mode, as_function) controls how functions are encoded:
      - mode = `Curry or `Tuple for multi-argument array encoding
      - as_function is applied to function-typed terms to decide whether their
        applications stay as function applications or are encoded as array reads. *)
  val to_sexp : ?smt2arrays: [`Tuple | `Curry ]*(term -> bool) -> t -> Sexplib.Type.t list

  (** Prints the assertions. *)
  val pp : t Format.printer

  (** Prints the log. *)
  val pp_log : t Format.printer

  (**
   {v
      malloc ?config ()
      v}
      Allocate a new context. *)
  val malloc : ?config:config -> unit -> t

  (** Default is to allow interpolation. Call malloc_mcsat ~interpol:false () if you want it disabled *)
  val malloc_mcsat : ?interpol:bool -> unit -> t

  (**
   {v
      malloc_logic logic
      v}
      Allocate a context for a specific logic. *)
  val malloc_logic : string -> t

  (** Free does not free the config field (which could be shared with other contexts) *)
  val free : t -> unit

  (**
   {v
      status ctx
      v}
      Return the current status. *)
  val status : t -> smt_status

  (**
   {v
      reset ctx
      v}
      Reset the context to its initial state. *)
  val reset  : t -> unit

  (**
   {v
      push ctx
      v}
      Open a new level. *)
  val push   : t -> unit

  (**
   {v
      pop ctx
      v}
      Go back one level. *)
  val pop    : t -> unit

  (**
   {v
      goto ctx level
      v}
      Go to a specific level. *)
  val goto   : t -> int -> unit

  (**
   {v
      enable_option ctx ~option
      v}
      Enable a context option. *)
  val enable_option   : t -> option:string -> unit

  (**
   {v
      disable_option ctx ~option
      v}
      Disable a context option. *)
  val disable_option  : t -> option:string -> unit

  (**
   {v
      assert_formula ctx t
      v}
      Assert a formula into the context. *)
  val assert_formula  : t -> term -> unit

  (**
   {v
      assert_formulas ctx ts
      v}
      Assert multiple formulas. *)
  val assert_formulas : t -> term list -> unit

  (**
   {v
      assert_blocking_clause ctx
      v}
      Assert a blocking clause at the current level. *)
  val assert_blocking_clause : t -> unit

  (**
   {v
      check ?param ?assumptions ?smodel ?as_inequalities ?hints ctx
      v}
      Check satisfiability with optional parameters. *)
  val check : ?param:param -> ?assumptions:term list -> ?smodel:smodel -> ?as_inequalities:bool -> ?hints:term list -> t -> smt_status

  (**
   {v
      set_fixed_var_order ctx ts
      v}
      Fix the variable order. *)
  val set_fixed_var_order : t -> term list -> smt_status

  (**
   {v
      set_initial_var_order ctx ts
      v}
      Set an initial variable order. *)
  val set_initial_var_order : t -> term list -> smt_status

  (**
   {v
      stop ctx
      v}
      Interrupt the solver. *)
  val stop             : t -> unit

  (**
   {v
      get_model ?keep_subst ?support ctx
      v}
      Return a supported model. *)
  val get_model        : ?keep_subst:bool -> ?support:term list -> t -> smodel

  (**
   {v
      get_unsat_core ctx
      v}
      Return the unsat core as a list of terms. *)
  val get_unsat_core   : t -> term list

  (**
   {v
      get_model_interpolant ctx
      v}
      Return a model interpolant term. *)
  val get_model_interpolant : t -> term
    
  val check_with_interpolation  :
    ?build_model:bool -> ?param:param -> t -> t
    -> (term, ?support:term list -> unit -> smodel) smt_status_with_answers

end

module type Names = sig
  type t
  type context

  (**
   {v
      set x name
      v}
      Assign a name to a term or type. *)
  val set      : t -> string -> unit

  (**
   {v
      remove name
      v}
      Remove the current mapping for name. *)
  val remove   : string -> unit

  (**
   {v
      clear x
      v}
      Clear the base name of a term or type. *)
  val clear    : t -> unit

  (**
   {v
      is_name name
      v}
      Check whether name is bound. *)
  val is_name  : string -> bool

  (**
   {v
      of_name name
      v}
      Resolve a name to a term or type. *)
  val of_name  : string -> t

  (**
   {v
      has_name x
      v}
      Check whether a term or type has a base name. *)
  val has_name : t -> bool

  (**
   {v
      to_name x
      v}
      Return the base name of a term or type. *)
  val to_name  : t -> string
end  

module type Type = sig

  include High_types.Type with type 'a eh := 'a

  type context
  
  (** Introduce a notation for pretty-printing a type (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  (**
   {v
      new_uninterpreted ?name ?card ()
      v}
      Create an uninterpreted type. *)
  val new_uninterpreted  : ?name:string -> ?card:int -> unit -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  (**
   {v
      to_sexp ?smt2arrays t
      v}
      Convert a type to an S-expression. *)
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

  (**
   {v
      new_uninterpreted ?name tau
      v}
      Create an uninterpreted term. *)
  val new_uninterpreted  : ?name:string -> typ -> t

  (** Introduce a notation for pretty-printing a term (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  (**
   {v
      to_sexp ?smt2arrays p t
      v}
      Convert a term to an S-expression.

      smt2arrays = (mode, as_function) controls how functions are encoded:
      - mode = `Curry or `Tuple for multi-argument array encoding
      - as_function is applied to function-typed terms to decide whether their
        applications stay as function applications or are encoded as array reads. *)
  val to_sexp            : ?smt2arrays:[`Tuple | `Curry ]*(t -> bool) -> t -> Sexp.t

  (**
   {v
      is_free_termstruct ~var ts
      v}
      Check whether var occurs free in a term structure. *)
  val is_free_termstruct : var:t -> _ termstruct -> bool

  (**
   {v
      is_free ~var t
      v}
      Check whether var occurs free in t. *)
  val is_free            : var:t -> t -> bool

  (**
   {v
      fv_termstruct ts
      v}
      Free variables of a term structure. *)
  val fv_termstruct      : _ termstruct -> termset

  (**
   {v
      fv t
      v}
      Free variables of a term. *)
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

  (**
   {v
      pp model
      v}
      Pretty-print a value from a model. *)
  val pp     : model -> yval Format.printer

  (**
   {v
      pp_val model
      v}
      Pretty-print a low-level value. *)
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

  (**
   {v
      make ?support mdl
      v}
      Build a supported model. *)
  val make : ?support:term list -> model -> t

  (**
   {v
      free smodel
      v}
      Release a supported model. *)
  val free : t -> unit

  val pp :
    ?pp_start:unit Format.printer ->
    ?pp_stop:unit Format.printer ->
    ?pp_sep:unit Format.printer -> unit -> t Format.printer

  (**
   {v
      from_map ?support xs
      v}
      Build a supported model from assignments. *)
  val from_map  : ?support:term list -> (term * term) list -> t

  (**
   {v
      as_map smodel
      v}
      Return assignments as a map. *)
  val as_map  : t -> (term * term) list

  (* turns every assignment (x |-> v) into (x === v);
     unless x is Boolean in which case it produces x or (not x) depending on v;
     crashes if v cannot be expressed as a term, like algebraic numbers or functional values *)
  val as_assumptions : ?as_inequalities:bool -> t -> term list

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

    (** Pretty-print the current error report. *)
    val pp_error_report : error_report Format.printer
  end

end

open Types

module type API = sig

  include High.API with type 'a eh := 'a

  module Global : Global with type term := Term.t
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

    (** Empty assertions state. *)
    val init : t

    exception BlockingClauseUsage

    (**
     {v
        assertions st
        v}
        Flatten assertions across levels. *)
    val assertions : t -> Term.t list (* flattens all levels into a list of assertions
                                         raises BlockingClauseUsage
                                         if None is found at any level *)
    (**
     {v
        to_sexp ?smt2arrays p st
        v}
        Convert assertions to S-expressions.

        smt2arrays = (mode, as_function) controls how functions are encoded:
        - mode = `Curry or `Tuple for multi-argument array encoding
        - as_function is applied to function-typed terms to decide whether their
          applications stay as function applications or are encoded as array reads. *)
    val to_sexp : ?smt2arrays:[`Tuple | `Curry ]*(Term.t -> bool) -> t -> Sexp.t

    (** Pretty-print assertions. *)
    val pp : t Format.printer
  end
  
  module Action : sig

    type t = action

    (** Appends the action sexp(s) on top of input list.

        smt2arrays = (mode, as_function) controls how functions are encoded:
        - mode = `Curry or `Tuple for multi-argument array encoding
        - as_function is applied to function-typed terms to decide whether their
          applications stay as function applications or are encoded as array reads. *)
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

    (**
     {v
        map f s
        v}
        Map a function over a boolean structure. *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (**
     {v
        nnf negate s
        v}
        Convert to negation normal form. *)
    val nnf : bool -> 'a t -> 'a t
  end

  module Slice : sig
    type t = slice

    (**
     {v
        build ?indices t
        v}
        Create a slice descriptor. *)
    val build   : ?indices:(int*int) -> Term.t -> t

    (**
     {v
        to_term slice
        v}
        Convert a slice to a term. *)
    val to_term : t -> Term.t

    (** Pretty-print a slice. *)
    val pp      : t Format.printer

    (**
     {v
        width slice
        v}
        Bitwidth of the slice. *)
    val width   : t -> int

    (**
     {v
        is_free ~var slice
        v}
        Free-variable check for a slice. *)
    val is_free : var:Term.t -> t -> bool

    (**
     {v
        fv slice
        v}
        Free variables of a slice. *)
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

    (**
     {v
        return_block b
        v}
        Wrap a bit-structure as a block. *)
    val return_block : _ bs closed -> _ block

    (** Pretty-print an extended term. *)
    val pp           : _ base Format.printer

    (**
     {v
        to_term x
        v}
        Convert an extended term to a term. *)
    val to_term      : _ base -> Term.t

    (**
     {v
        build ts
        v}
        Close a term structure. *)
    val build        : 'a termstruct -> 'a closed

    (**
     {v
        width x
        v}
        Bitwidth of an extended term. *)
    val width        : _ base -> int

    (**
     {v
        is_free ~var x
        v}
        Free-variable check. *)
    val is_free      : var:Term.t -> _ base -> bool

    (**
     {v
        fv x
        v}
        Free variables of an extended term. *)
    val fv           : _ base -> TermSet.t

    (**
     {v
        typeof x
        v}
        Type of an extended term. *)
    val typeof       : _ base -> Type.t

    (**
     {v
        bvarray ts
        v}
        Block array from bitvector terms. *)
    val bvarray      : Term.t list -> _ block list

    module MTerm(M : Monad) : sig
      type update = { apply : 'a. 'a closed -> 'a closed M.t }

      (**
       {v
          map upd ts
          v}
          Monadic map over term structures. *)
      val map       : update ->   'a termstruct -> 'a termstruct M.t
    end

    type t  = ExtTerm  : _ closed -> t
    type yt = YExtTerm : _ termstruct -> yt

    (**
     {v
        of_yterm y
        v}
        Convert a high-level yterm to an extended term structure. *)
    val of_yterm : yterm  -> yt

    (**
     {v
        of_term t
        v}
        Convert a term to an extended term structure. *)
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
      Term.t list -> Type.t list -> bool -> unit
  end

  module Purification : Purification_types.API with type typ  := Type.t
                                                and type term := Term.t
end
