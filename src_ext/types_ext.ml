open Containers
open Sexplib

open Yices2
open Ext.WithExceptionsErrorHandling

(** {2 This module is here to help you build extensions to Yices.} *)

(** Shared parametric log/trace types (defined in the src "Ext_types" layer). *)
module Log = Ext_types.Log
module type Context = Ext_types.Context

(* Particular case of Ext_types.Context
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardYicesContext =
  Context with type typ    = Type.t
          and type term   = Term.t
          and type config = Config.t
          and type param  = Param.t
          and type smodel = SModel.t

(* Generic type of answers for satisfiability queries. *)
type ('model, 'interpolant) answer =
  | Sat of 'model
  | Unsat of 'interpolant
  | Unknown of string

(* Here's what you need to implement to build an extension of Yices. *)
module type Ext = sig

  (* What needs to match the types in the solver you are extending. *)
  type old_term
  type old_typ
  type old_context
  type old_config
  type old_param
  type old_smodel

  (* The notions of terms, configs, and models of your extension. *)
  type term
  type typ
  type config
  type param
  type smodel

  type t (* Your extension can have a mutable state;
            use unit otherwise (see module Trivial below) *)
  val malloc : ?config:config -> unit -> old_config option * t
  val reset  : t -> unit
  val push   : t -> unit
  val pop    : t -> unit
  val goto   : t -> int -> unit

  (* Term translation hooks:
     - translate_assertion can expand one extension-level formula into
       zero or more old-level formulas (e.g., purification constraints).
     - translate_assumption must return a single old-level term suitable
       for assumptions/hints/variable-order. It should avoid side effects. *)
  val translate_assertion  : old_context -> t -> term -> old_term list
  val translate_assumption : old_context -> t -> term -> old_term

  (* Whenever the solver you're extending produces a supported model,
     if you are happy with it, please convert it to your own notion of model;
     if you are unhappy with it, please either explain why by giving the solver
     you're extending a model interpolant, or return [Unknown reason] if the
     extension cannot soundly decide the current abstraction. *)
  val check : t -> old_smodel -> (old_smodel, old_term) answer

  (* Convert old-level terms back to extension-level terms. *)
  val term_of_old : t -> old_term -> term

  (* Convert old-level types back to extension-level types.
     This is mainly used for logging/sexp conversion. *)
  val typ_of_old : t -> old_typ -> typ

  (* Param and smodel conversions for check/check_with_interpolation. *)
  val param_to_old  : t -> param -> old_param
  val smodel_to_old : t -> smodel -> old_smodel
  val smodel_of_old : t -> old_smodel -> smodel

  (* Enrich a base supported model for this extension layer.
     If [?support] is given, it overrides the model's current support. *)
  val enrich_smodel : t -> ?support:term list -> old_smodel -> smodel

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
       and type old_context := Context.t
       and type old_config := Config.t
       and type old_param  := Param.t
       and type old_smodel := SModel.t
       and type term   := Term.t
       and type typ    := Type.t
       and type config := Config.t
       and type param  := Param.t
       and type smodel := SModel.t

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
