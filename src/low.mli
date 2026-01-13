(** Low-level bindings that mirror the Yices C API with minimal OCaml adaptation. *)
open Low_types

module type API  = API

include API with type 'a checkable = 'a
