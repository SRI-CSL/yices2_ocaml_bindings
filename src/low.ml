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
         
  (* Opaque C types, only accessible through the API functions *)
  type term_t = sint
  type type_t = sint

  let null_term = Signed.SInt.minus_one

  (* C's enums *)
  type smt_status_t = uint
  type term_constructor_t = sint
  type yval_tag_t = uint
  type yices_gen_mode_t = uint
  type error_code_t = uint

  include Yices_header
  include Base.Types

  module type Conv64 = sig
    type t
    val to_int64 : t -> int64
    val of_int64 : int64 -> t
  end

  let make_view (type a) (module M : Conv64 with type t = a) o =
    { read  = M.to_int64 <.> o#of_int;
      write = o#to_int <.> M.of_int64 }

  (* Redefining enums type handlers *)
  let smt_status       = make_view (module UInt) smt_status
  let term_constructor = make_view (module SInt) term_constructor
  let yval_tag         = make_view (module UInt) yval_tag
  let yices_gen_mode   = make_view (module UInt) yices_gen_mode
  let error_code       = make_view (module UInt) error_code

end

include Yices_header

