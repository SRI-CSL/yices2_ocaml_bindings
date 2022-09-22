open Ctypes_static
open Common
   
module type API = High_types.API

module LowTypes  := Low.Types
module HighTypes := High_types.Types

module Types : sig
  include module type of LowTypes
  include module type of HighTypes
end

open Types

module MType(M : Monad) : sig
    val map : (type_t -> type_t M.t) -> ytype -> ytype M.t
end

module MTerm(M : Monad) : sig
    val map : (term_t -> term_t M.t) -> 'a termstruct -> 'a termstruct M.t
end

module Algebraic : sig

  open Libpoly
     
  module DyadicRational : sig
    type t = DyadicRational.t
    val num   : t ptr -> Z.t
    val pow   : t ptr -> int
    val to_string : t ptr -> string
  end
       
  type t = AlgebraicNumber.t

  val sgn_at_a : t ptr -> bool
  val sgn_at_b : t ptr -> bool
  val is_point : t ptr -> bool

  val a_open : t ptr -> bool
  val b_open : t ptr -> bool

  val a : t ptr -> DyadicRational.t ptr
  val b : t ptr -> DyadicRational.t ptr

  val a_num : t ptr -> Z.t
  val a_pow : t ptr -> int

  val b_num : t ptr -> Z.t
  val b_pow : t ptr -> int

  val to_string : t ptr -> string
 
end

module Error : sig
  (** ********************
      ERROR REPORTING  *
   ****************** *)

  (** Error codes and the error_report data structure are defined in
      yices_types.h. When an API function is called with invalid
      arguments or when some error occurs for whatever reason, then the
      function returns a specific value (typically a negative value) and
      stores information about the error in a global error_report
      structure.  This structure can be examined by calling
      yices_error_report().  The most important component of the
      error_report is an error code that is returned by a call to
      yices_error_code().  *)

  (** Get the last error code  *)
  val code   : unit -> error_code
  (** Get the last error report  *)
  val report : unit -> error_report
  (** Clear the error report  *)
  val clear  : unit -> unit
end

(** Small abbreviation *)
val status_is_not_error : [> `STATUS_ERROR] -> bool

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
