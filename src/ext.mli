(** Extended API: higher-level bindings plus convenience utilities. *)
open Containers
open Sexplib

module type API = Ext_types.API

include Ext_types.BaseAPI

open Types

val sexp : string -> Sexp.t list -> Sexp.t
val pp_sexp : Sexp.t Format.printer

(** Default is true: *)
val use_type_names : bool ref
val use_term_names : bool ref
val use_type_notations : bool ref
val use_term_notations : bool ref

module type ErrorHandling = High.ErrorHandling with type 'a t = 'a

(** Error handling that returns values directly and raises on errors. *)
module NoErrorHandling : sig
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  include ErrorHandling with type 'a t = 'a
end

(** Error handling that raises exceptions on errors. *)
module ExceptionsErrorHandling : sig
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  include ErrorHandling with type 'a t = 'a
end

(** Instantiate the Ext API with a custom error-handling strategy. *)
module Make(_: ErrorHandling) : Ext_types.API with type Context.t = context

(** Ext API using exception-based error handling. *)
module WithExceptionsErrorHandling : Ext_types.API with type Context.t = context
(** Ext API using direct-return error handling. *)
module WithNoErrorHandling         : Ext_types.API with type Context.t = context
