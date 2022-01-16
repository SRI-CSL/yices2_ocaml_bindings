
[%%import "gmp.mlh"]

[%%if gmp_present]
open Ctypes_zarith
[%%endif]

module TMP = struct

  let int    = Ctypes.typedef Ctypes.int "int"
  let size_t = Ctypes.typedef Ctypes.size_t "size_t"


  (* let lp_integer_t = Ctypes.typedef MPZ.t "lp_integer_t" *)


  (* lp_dyadic_rational_t *)
    
  let lp_dyadic_rational_struct_0 =
    let (_ctype : [ `lp_dyadic_rational_struct ] Ctypes.structure Ctypes.typ) =
      Ctypes.structure "" in
    _ctype

  let lp_dyadic_rational_struct =
    let field_0 = Ctypes.field lp_dyadic_rational_struct_0 "a" MPZ.t in
    let field_1 = Ctypes.field lp_dyadic_rational_struct_0 "n" Ctypes.ulong in
    let () = Ctypes.seal lp_dyadic_rational_struct_0 in
    object
      method ctype = lp_dyadic_rational_struct_0
      method members =
        object
          method a = field_0
          method n = field_1
        end
    end
    
  let lp_dyadic_rational_t =
    Ctypes.typedef lp_dyadic_rational_struct_0 "lp_dyadic_rational_t"



  (* lp_dyadic_interval_t *)
    

  let lp_dyadic_interval_struct_0 =
    let (_ctype : [ `lp_dyadic_interval_struct ] Ctypes.structure Ctypes.typ) =
      Ctypes.structure "lp_dyadic_interval_struct" in
    _ctype

  let lp_dyadic_interval_struct =
    let field_0 = Ctypes.field lp_dyadic_interval_struct_0 "a_open" size_t in
    let field_1 = Ctypes.field lp_dyadic_interval_struct_0 "b_open" size_t in
    let field_2 = Ctypes.field lp_dyadic_interval_struct_0 "is_point" size_t in
    let field_3 = Ctypes.field lp_dyadic_interval_struct_0 "a" lp_dyadic_rational_t in
    let field_4 = Ctypes.field lp_dyadic_interval_struct_0 "b" lp_dyadic_rational_t in
    let () = Ctypes.seal lp_dyadic_interval_struct_0 in
    object
      method ctype = lp_dyadic_interval_struct_0
      method members =
        object
          method a_open = field_0
          method b_open = field_1
          method is_point = field_2
          method a = field_3
          method b = field_4
        end
    end
    
  let lp_dyadic_interval_t =
    Ctypes.typedef lp_dyadic_interval_struct_0 "lp_dyadic_interval_t"

    

  (* Anonymous lp_upolynomial_t *)

  let lp_upolynomial_struct_0 =
    let (_ctype : [ `lp_algebraic_number_struct ] Ctypes.structure Ctypes.typ) =
      Ctypes.structure "lp_algebraic_number_struct" in
    _ctype

  let lp_upolynomial_t =
    Ctypes.typedef lp_upolynomial_struct_0 "lp_upolynomial_t"



  (* lp_algebraic_number_t *)

  let lp_algebraic_number_struct_0 =
    let (_ctype : [ `lp_algebraic_number_struct ] Ctypes.structure Ctypes.typ) =
      Ctypes.structure "lp_algebraic_number_struct" in
    _ctype

  let lp_algebraic_number_struct =
    let field_0 = Ctypes.field lp_algebraic_number_struct_0 "f" (Ctypes.ptr size_t) in
    let field_1 = Ctypes.field lp_algebraic_number_struct_0 "I" lp_dyadic_interval_t in
    let field_2 = Ctypes.field lp_algebraic_number_struct_0 "sgn_at_a" int in
    let field_3 = Ctypes.field lp_algebraic_number_struct_0 "sgn_at_b" int in
    let () = Ctypes.seal lp_algebraic_number_struct_0 in
    object
      method ctype = lp_algebraic_number_struct_0
      method members =
        object
          method f = field_0
          method _I = field_1
          method sgn_at_a = field_2
          method sgn_at_b = field_3
        end
    end
    
  let lp_algebraic_number_t =
    Ctypes.typedef lp_algebraic_number_struct_0 "lp_algebraic_number_t"


    
  let lp_algebraic_number_to_string =
    Foreign.foreign "lp_algebraic_number_to_string"
      (Ctypes.(@->)
         (Ctypes.ptr lp_algebraic_number_t)
         (Ctypes.returning (Ctypes.ptr Ctypes.char)))

  let lp_dyadic_rational_to_string =
    Foreign.foreign "lp_dyadic_rational_to_string"
      (Ctypes.(@->)
         (Ctypes.ptr lp_dyadic_rational_t)
         (Ctypes.returning (Ctypes.ptr Ctypes.char)))

end
include TMP
