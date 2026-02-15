open Yices2
open Yices2.Ext
open WithExceptionsErrorHandling
open Types_ext

(** Tuple blasting: eliminate tuple types/terms by rewriting them away.

    This is useful when the backend solver does not support tuples natively
    (e.g., Yices MCSAT). *)

module TopTuple : sig
  exception Map2Fail

  type 'a t =
    | Single of 'a
    | Multiple of 'a t list

  include Common.Monad with type 'a t := 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val flatten : 'a t -> 'a list
  val map_flatten : ('a -> 'b t) -> 'a list -> 'b list
end

type state

val malloc_state : unit -> state
val reset_state : state -> unit

val type_check : Type.t -> bool
val tuple_blast : state -> Term.t -> Term.t TopTuple.t
val blast_formula : state -> Term.t -> Term.t
val blast_flat : state -> Term.t -> Term.t list

(** Default tuple-blasting context: tuples are blasted only when the context is
    MCSAT (and only if tuple constructs are detected). *)
module ContextOnlyMCSAT : StandardYicesContext with type term = Term.t

(** Variant that always blasts tuples in assertions/assumptions. *)
module ContextAlways : StandardYicesContext with type term = Term.t

(** Final tuple-blasting APIs. *)
module Always : Ext_types.API
module OnlyMCSAT : Ext_types.API
