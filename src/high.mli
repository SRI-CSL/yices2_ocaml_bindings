open Ctypes_static

module type API = High_types.API

module LowTypes  := Low.Types
module HighTypes := High_types.Types

module Types : sig
  include module type of LowTypes
  include module type of HighTypes
end

open Types

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MList(M : Monad) : sig
    val fold : ('a -> 'b -> 'a M.t) -> 'a M.t -> 'b list -> 'a M.t
    val map : ('a -> 'b M.t) -> 'a list -> 'b list M.t
end

module MType(M : Monad) : sig
    val map : (type_t -> type_t M.t) -> ytype -> ytype M.t
end

module MTerm(M : Monad) : sig
    val map : (term_t -> term_t M.t) -> 'a termstruct -> 'a termstruct M.t
end

module Algebraic : sig

  module DyadicRational : sig
    type t = lp_dyadic_rational_t ptr
    val to_string : t -> string
    val get_num   : t -> Z.t
    val get_pow   : t -> int
  end
       
  type t = lp_algebraic_number_t ptr

  val to_string  : t -> string
  val get_sign_a : t -> bool
  val get_sign_b : t -> bool
  val get_is_point : t -> bool

  val get_a_open : t -> bool
  val get_b_open : t -> bool

  val get_a : t -> DyadicRational.t
  val get_b : t -> DyadicRational.t

  val get_a_num : t -> Z.t
  val get_a_pow : t -> int

  val get_b_num : t -> Z.t
  val get_b_pow : t -> int

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

module type ErrorHandling = sig
  type 'a t

  (** How to raise an error manually, originating from Yices or from the bindings *)
  val raise_yices_error    : unit -> _ t
  val raise_bindings_error : string -> _ t

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
  val check_status : Types.smt_status -> Types.smt_status
end

module SumErrorHandling : sig
  type error = Yices of error_code * error_report | Bindings of string
  include ErrorHandling with type 'a t = ('a, error) Stdlib.Result.t
end

module Make(EH: ErrorHandling) : API with type 'a eh := 'a EH.t
