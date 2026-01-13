open! Containers
open Yices2.Ext
open WithExceptionsErrorHandling

open Types_ext
   
(** {2 This module is here to help you build extensions to Yices.} *)

(* This is the initial Yices implementation of the above.
   That's the base implementation you are extending. *)
module Context : StandardYicesContext with type t = Context.t


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

(* In order to easily define syntax extensions.
   You just have to provide NewTypes and NewTerms, then call these functors. *)
module SyntaxExtensions : sig

  exception StringAlreadyUsed of string

  module DeclareTypes(R : NewTypes) : sig
    type reveal = Reveal : 'a R.t * 'a -> reveal
    val reveal : Type.t -> reveal option
    val build  : reveal -> Type.t
  end

  module DeclareTerms(R : NewTerms) : sig
    type reveal = Reveal : 'a R.t * 'a -> reveal
    val reveal : Term.t -> (reveal * Term.t list) option
    val build  : reveal -> Term.t list -> Term.t
  end
end
