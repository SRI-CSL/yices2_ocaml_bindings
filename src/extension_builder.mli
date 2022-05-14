open Containers
open Ext_bindings

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
  val free : t -> unit
  val status : t -> Types.smt_status
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val check : ?param:Param.t -> t -> Types.smt_status
  val check_with_smodel : ?param:Param.t -> t -> SModel.t -> Types.smt_status
  val get_model : ?keep_subst:bool -> t -> model
  val get_model_interpolant : t -> term

  val pp_log : t Format.printer
end

(* Particular case of the above module type
   when your Yices extension uses the same terms, configs, and models as Yices does. *)
module type StandardYicesContext =
  YicesContext with type term   = Term.t
                and type config = Config.t
                and type model  = Model.t

(* This is the initial Yices implementation of the above.
   That's the base implementation you are extending. *)
module Context : StandardYicesContext with type t = Context.t

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

(* Here's the meat of this solver extension. It takes
   - the solver you're extending,
   - the specs of your extension,
   and it produces a new solver. *)
module Make
         (Context : YicesContext)                      (* The solver you're extending *)
         (C : Ext with type old_term   := Context.term (* The specs of your extension *)
                   and type old_config := Context.config
                   and type old_model  := Context.model) :
YicesContext with type term = C.term
              and type config = C.config
              and type model  = C.model

(* A trivial seed for your extension specs, with no state *)
module Trivial : sig
  type t = unit
  val malloc : ?config:'a -> t -> 'a option * t
  val free : 'a -> t
  val push : 'a -> t
  val pop : 'a -> t
end
