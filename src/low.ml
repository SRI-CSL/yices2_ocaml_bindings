open Ctypes
open Signed
open Unsigned

let (<.>) f g x = g(f x)

module FILE = struct
  type t = [ `__IO_FILE ] structure
  include Yices_header
end

module Types = struct

  open Base.Types

  type _ checkable = sint
  let check i = Signed.SInt.(compare zero i) <= 0

  (* Opaque C types, only accessible through the API functions *)
  type unit_t = [`unit_t] checkable
  type bool_t = [`bool_t] checkable
  type term_t = [`term_t] checkable
  type type_t = [`type_t] checkable

  let null_term = Signed.SInt.minus_one
  let null_type = Signed.SInt.minus_one

  (* C's enums *)
  type smt_status_t       = uint
  type term_constructor_t = [`term_constructor_t] checkable
  type yval_tag_t         = uint
  type yices_gen_mode_t   = uint
  type error_code_t       = uint

  include Yices_header
  include Base.Types

  module type Conv64 = sig
    type t
    val to_int64 : t -> int64
    val of_int64 : int64 -> t
  end

  let unit =
    let read _ = () in
    let write () = SInt.zero in
    { read; write }

  let bool =
    let read r = not SInt.(equal r zero) in
    let write r = if r then SInt.one else SInt.zero in
    { read; write }

  let make (type a) (module M : Conv64 with type t = a) o =
    { read  = M.to_int64 <.> o#of_int;
      write = o#to_int <.> M.of_int64 }

  (* Redefining enums type handlers *)
  let smt_status       = make (module UInt) smt_status
  let term_constructor = make (module SInt) term_constructor
  let yval_tag         = make (module UInt) yval_tag
  let yices_gen_mode   = make (module UInt) yices_gen_mode
  let error_code       = make (module UInt) error_code
  
end

include Yices_header

