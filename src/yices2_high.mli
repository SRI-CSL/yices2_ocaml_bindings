open Ctypes
open Ctypes_static

open Yices2_high_types

val x : unit -> unit

module Types : module type of Types with type display = Types.display
                                     and type error_report = Types.error_report

open Types

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

  (** How to raise an error manually  *)
  val raise_error : string -> _ t

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

module Make(EH: ErrorHandling) : High with type 'a eh := 'a EH.t
