open Containers
open Yices2.Ext

(**************************************************************)
(* This module is here to help you build extensions to Yices. *)
(**************************************************************)

(* An "extended Yices" offers an API similar to the API of a Yices's context.
   You can use different terms, configs, and models from what Yices uses. *)
module type YicesContext = sig

  type term
  type config
  type model

  val config_set : config -> name:string -> value:string -> unit

  type t (* Type of contexts in the extension of Yices *)
  val malloc : ?config:config -> unit -> t
  val malloc_mcsat : unit -> t
  val free   : t -> unit
  val status : t -> Types.smt_status
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val check : ?param:Param.t -> t -> Types.smt_status
  val check_with_smodel : ?param:Param.t -> t -> SModel.t -> Types.smt_status
  val get_model             : t -> model
  val get_model_interpolant : t -> term

  val pp_log : t Format.printer
end

(* Particular case of the above module type
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardYicesContext =
  YicesContext with type term   = Term.t
                and type config = Config.t
                and type model  = Model.t

(* Generic type of answers for satisfiability queries. *)
type ('model, 'interpolant) answer =
  | Sat of 'model
  | Unsat of 'interpolant

(* Here's what you need to implement to build an extension of Yices. *)
module type Ext = sig

  (* What needs to match the types in the solver you are extending. *)
  type old_term
  type old_config
  type old_model

  (* The notions of terms, configs, and models of your extension. *)
  type term
  type config
  type model

  val config_set : config -> name:string -> value:string -> unit

  type t (* Your extension can have a mutable state;
            use unit otherwise (see module Trivial below) *)
  val malloc : ?config:config -> unit -> old_config option * t
  val free : t -> unit
  val push   : t -> unit
  val pop    : t -> unit

  (* Given how the solver you're extending asserts formulas,
     how do you want to assert a formula? *)
  val assert_formula : (old_term -> unit) -> t -> term -> unit

  (* Whenever the solver you're extending produces a model,
     if you are happy with is, please convert it to your own notion of model;
     if you are unhappy with it,
     please explain why by giving the solver you're extending a model interpolant. *)
  val check : t -> old_model -> (model, old_term) answer

  (* Whenver the solver you're extending returns UNSAT, with old_term interpolant,
     you should convert that interpolant into a term interpolant. *)
  val interpolant : t -> old_term -> term

end

(* Particular case of the above module type
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardExt =
  Ext with type old_term   := Term.t
       and type old_config := Config.t
       and type old_model  := Model.t
       and type term   := Term.t
       and type config := Config.t
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
