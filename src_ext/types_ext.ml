open Containers
open Sexplib

open Yices2
open Yices2.Common
open Ext
open Ext.WithExceptionsErrorHandling

(** {2 This module is here to help you build extensions to Yices.} *)

(* These are parametric mirrors of the log/trace types in Ext_types.
   We keep them in a submodule so YicesContext can refer to them without
   recursive type aliases on the same name. *)
module Log = struct
  type 'term assertions =
    | Assertions of {
        list  : 'term list option list;
        level : int;
      }

  type ('term, 'param, 'smodel) context_action =
    | Status
    | Reset
    | Push
    | Pop
    | EnableOption of string
    | DisableOption of string
    | AssertFormula of 'term
    | AssertFormulas of 'term list
    | AssertBlockingClause
    | Check of 'param option
    | CheckWithAssumptions of {
        param : 'param option;
        assumptions : 'term list;
      }
    | Stop
    | GetModel of { keep_subst : bool }
    | GetUnsatCore
    | CheckWithModel of {
        param : 'param option;
        smodel : 'smodel;
      }
    | GetModelInterpolant

  type ('term, 'typ, 'param, 'smodel) action =
    | DeclareType of 'typ * int option
    | DeclareFun of 'term * 'typ
    | DefineType of string * 'typ
    | DefineFun of string * 'term * 'typ
    | CheckWithInterpolation of {
        param : 'param option;
        build_model : bool;
        context1 : int;
        context2 : int;
      }
    | GarbageCollect of Sexp.t list
    | NewContext of { logic : string option }
    | ContextAction of {
        context_id : int;
        context_action : ('term, 'param, 'smodel) context_action;
      }
end

(* An "extended Yices" offers an API similar to the API of a Yices's context.
   You can use different terms, configs, and models from what Yices uses. *)
module type YicesContext = sig

  (* Extension-level term/type/model vocabulary *)
  type typ
  type term
  type config
  type param
  type smodel

  (* These are the parametric equivalents of Ext_types.Assertions/Action. *)
  type assertions = term Log.assertions
  type action = (term, typ, param, smodel) Log.action

  type t (* Type of contexts in the extension of Yices *)
  val pp_options        : unit HStrings.t Format.printer
  val pp_config_options : string HStrings.t Format.printer

  val assertions     : t -> assertions
  val options        : t -> unit HStrings.t
  val config         : t -> config option
  val config_options : t -> string HStrings.t
  val log            : t -> action list
  val is_alive       : t -> bool
  val is_mcsat       : t -> bool
  val id             : t -> int
  val of_id          : int -> t option
  val all            : unit -> t Seq.t
  val to_sexp        : ?smt2arrays:[`Tuple | `Curry ] * (term -> bool) -> t -> Sexp.t list

  val malloc : ?config:config -> unit -> t
  val malloc_mcsat : ?interpol:bool -> unit -> t
  val malloc_logic : string -> t
  val free   : t -> unit
  val status : t -> Types.smt_status
  val reset  : t -> unit
  val push   : t -> unit
  val pop    : t -> unit
  val goto   : t -> int -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val assert_blocking_clause : t -> unit
  val check : ?param:param -> ?assumptions:term list -> ?smodel:smodel
              -> ?as_inequalities:bool -> ?hints:term list -> t -> Types.smt_status
  val set_fixed_var_order   : t -> term list -> Types.smt_status
  val set_initial_var_order : t -> term list -> Types.smt_status
  val stop                  : t -> unit
  val get_model             : ?keep_subst:bool -> ?support:term list -> t -> smodel
  val get_unsat_core        : t -> term list
  val get_model_interpolant : t -> term
  val check_with_interpolation :
    ?build_model:bool -> ?param:param -> t -> t
    -> (term, ?support:term list -> unit -> smodel) Types.smt_status_with_answers

  val pp_log : t Format.printer
  val pp : t Format.printer
end

(* Particular case of the above module type
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardYicesContext =
  YicesContext with type typ    = Type.t
                and type term   = Term.t
                and type config = Config.t
                and type param  = Param.t
                and type smodel = SModel.t

(* Generic type of answers for satisfiability queries. *)
type ('model, 'interpolant) answer =
  | Sat of 'model
  | Unsat of 'interpolant

(* Here's what you need to implement to build an extension of Yices. *)
module type Ext = sig

  (* What needs to match the types in the solver you are extending. *)
  type old_term
  type old_typ
  type old_config
  type old_param
  type old_smodel

  (* The notions of terms, configs, and models of your extension. *)
  type term
  type typ
  type config
  type param
  type smodel
  type model

  type t (* Your extension can have a mutable state;
            use unit otherwise (see module Trivial below) *)
  val malloc : ?config:config -> unit -> old_config option * t
  val free : t -> unit
  val reset  : t -> unit
  val push   : t -> unit
  val pop    : t -> unit
  val goto   : t -> int -> unit

  (* Term translation hooks:
     - translate_assertion can expand one extension-level formula into
       zero or more old-level formulas (e.g., purification constraints).
     - translate_assumption must return a single old-level term suitable
       for assumptions/hints/variable-order. It should avoid side effects. *)
  val translate_assertion  : t -> term -> old_term list
  val translate_assumption : t -> term -> old_term

  (* Whenever the solver you're extending produces a supported model,
     if you are happy with it, please convert it to your own notion of model;
     if you are unhappy with it,
     please explain why by giving the solver you're extending a model interpolant. *)
  val check : t -> old_smodel -> (model, old_term) answer

  (* Convert old-level terms back to extension-level terms. *)
  val term_of_old : t -> old_term -> term

  (* Convert old-level types back to extension-level types.
     This is mainly used for logging/sexp conversion. *)
  val typ_of_old : t -> old_typ -> typ

  (* Param and smodel conversions for check/check_with_interpolation. *)
  val param_to_old  : t -> param -> old_param
  val smodel_to_old : t -> smodel -> old_smodel
  val smodel_of_old : t -> old_smodel -> smodel

  (* Build a supported model from the extension's model representation. *)
  val smodel_of_model : t -> ?support:term list -> model -> smodel

  (* Whenever the solver you're extending returns UNSAT, with old_term interpolant,
     you should convert that interpolant into a term interpolant. *)
  val interpolant : t -> old_term -> term

  (* Rendering hooks used by the extended context log/pp/to_sexp. *)
  val pp_term : term Format.printer
  val pp_type : typ Format.printer
  val term_to_sexp : ?smt2arrays:[`Tuple | `Curry ] * (term -> bool) -> term -> Sexp.t
  val type_to_sexp : ?smt2arrays:[`Tuple | `Curry ] -> typ -> Sexp.t
  val smodel_to_sexp : ?smt2arrays:[`Tuple | `Curry ] * (term -> bool) -> smodel -> Sexp.t

end

(* Particular case of the above module type
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardExt =
  Ext with type old_term   := Term.t
       and type old_typ    := Type.t
       and type old_config := Config.t
       and type old_param  := Param.t
       and type old_smodel := SModel.t
       and type term   := Term.t
       and type typ    := Type.t
       and type config := Config.t
       and type param  := Param.t
       and type smodel := SModel.t
       and type model  := Model.t

(* Module types for syntax extensions *)

module type TypeIndex = sig
  include Hashtbl.HashedType
  val name : string
  val pp : t Format.printer
end

module type TermIndex = sig
  include TypeIndex
  val get_type : t -> Type.t list * Type.t
end
                  
module type NewTypes = sig
  include Dmap.DORDERED
  val index : 'a t -> (module TypeIndex with type t = 'a)
end

module type NewTerms = sig
  include Dmap.DORDERED
  val index : 'a t -> (module TermIndex with type t = 'a)
end
