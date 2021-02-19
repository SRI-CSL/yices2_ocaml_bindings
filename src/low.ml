open Ctypes
open Signed
open Unsigned

module type API = Low_types.API

let (<.>) f g x = g(f x)

module Types = struct

  include Low_types.BaseTypes

  let equal_sint = SInt.equal
  let equal_uint = UInt.equal
  let compare_sint = SInt.compare
  let compare_uint = UInt.compare

  (* Type of things that yices implements as a signed int, that can be checked for error *)
  type 'a sintbase = sint [@@deriving eq, ord]

  (* Type of things that yices implements as an unsigned int, that can be checked for error *)
  type 'a uintbase = uint [@@deriving eq, ord]

  let hash_sint = SInt.to_int
  let hash_uint = UInt.to_int
  
  (* Opaque C types, only accessible through the API functions *)
  type uint_t = [`uint_t] uintbase [@@deriving eq, ord]
  type sint_t = [`sint_t] sintbase [@@deriving eq, ord]
  type unit_t = [`unit_t] sintbase [@@deriving eq, ord]
  type bool_t = [`bool_t] sintbase [@@deriving eq, ord]
  type term_t = [`term_t] sintbase [@@deriving eq, ord]
  type type_t = [`type_t] sintbase [@@deriving eq, ord]

  (* C's enums *)
  type smt_status_t       = uint [@@deriving eq, ord]
  type term_constructor_t = [`term_constructor_t] sintbase [@@deriving eq, ord]
  type yval_tag_t         = uint [@@deriving eq, ord]
  type yices_gen_mode_t   = uint [@@deriving eq, ord]
  type error_code_t       = uint [@@deriving eq, ord]

end

open Types
    
let null_term = Signed.SInt.minus_one
let null_type = Signed.SInt.minus_one
let uint_t = uint
let sint_t = sint
let unit_t = sint
let bool_t = sint

type 'a checkable = 'a
let sintcheck i = Signed.SInt.(compare zero i) <= 0
let uintcheck i = Unsigned.UInt.(compare zero i) < 0
let ptrcheck  p = not(is_null p)

include Header

module type Conv64 = sig
  type t
  val to_int64 : t -> int64
  val of_int64 : int64 -> t
end

module Conv = struct
  let unit =
    let read _ = () in
    let write () = SInt.zero in
    { read; write }

  let bool =
    let read r = not SInt.(equal r zero) in
    let write r = if r then SInt.one else SInt.zero in
    { read; write }

  let sint = { read = SInt.to_int; write = SInt.of_int}
  let uint = { read = UInt.to_int; write = UInt.of_int}

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

