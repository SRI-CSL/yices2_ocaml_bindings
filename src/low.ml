open Ctypes
open Signed
open Unsigned

module FILE = struct
  type t = [ `__IO_FILE ] structure
  include Yices_header
end

module Stdint = Yices_header

module Types = struct
  type term_t = sint
  type type_t = sint
  type smt_status_t = uint
  type term_constructor_t = sint
  type yval_tag_t = uint
  type yices_gen_mode_t = uint
  type error_code_t = uint

  include Yices_types
  include Yices_header

  let null_term = Signed.SInt.minus_one
end

module API = Yices_header

