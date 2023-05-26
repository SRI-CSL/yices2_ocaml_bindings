open! Containers

open Yices2
open Ext
open WithExceptionsErrorHandling
open Types_ext

module NewTerms : sig
  type _ t = Epsilon : Types.uninterpreted t
  val compare : 'a t -> 'b t -> ('a, 'b) Dmap.cmp
  val index : 'a t -> (module TermIndex with type t = 'a)
end

module Term : sig
  include Ext_types.Term with type typ     := Type.t
                          and type t        = Term.t
                          and type termset := TermSet.t
                          and type context := Builder.Context.t
  val epsilon : t -> t -> t
  val epsilon_reveal : t -> (t * Types.uninterpreted * t) option
end

module Model : Ext_types.Model with type typ   := Type.t
                                and type term  := Term.t
                                and type t      = Model.t

module Context : YicesContext with type term = Term.t
                               and type config = Config.t
                               and type model  = Model.t
