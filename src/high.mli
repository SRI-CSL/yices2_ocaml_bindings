(** Higher-level API: OCaml-friendly wrappers over the low bindings. *)
open Ctypes_static

module type API = High_types.API

include High_types.BaseAPI

open Types

module type ErrorHandling = sig
  type 'a t

  (** How to raise an error manually, originating from Yices or from the bindings *)
  val raise_yices_error    : unit -> _ t
  val raise_bindings_error : ('a, Format.formatter, unit, _ t) format4 -> 'a

  (** What to do when getting back a status *)
  val return_status : smt_status -> smt_status t

  (** What to do when getting back a value implemented as a signed int  *)
  val return_sint : 'a sintbase -> 'a sintbase t

  (** What to do when getting back a value implemented as an unsigned int  *)
  val return_uint : 'a uintbase -> 'a uintbase t

  (** What to do when getting back a pointer  *)
  val return_ptr  : 'a ptr -> 'a ptr t

  (** What to do when getting back anything else  *)
  val return      : 'a -> 'a t

  (** Error monad's bind combinator  *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module NoErrorHandling : sig
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  include ErrorHandling with type 'a t = 'a
end

module ExceptionsErrorHandling : sig
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  include ErrorHandling with type 'a t = 'a
end

module SumErrorHandling : sig
  type error = Yices of error_code * error_report | Bindings of string
  include ErrorHandling with type 'a t = ('a, error) Stdlib.Result.t
end

module Make(EH: ErrorHandling) : API with type 'a eh := 'a EH.t
