[%%import "gmp.mlh"]

[%%if gmp_present]
open Ctypes_zarith
[%%endif]

(* Most of this file was automatically generated from yices_header_src.ml
   by the ctypes-of-clang ppx *)

module TMP =
  struct
    let anonymous_2 =
      let (_ctype : [ `anonymous ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "" in
      _ctype
    let anonymous_3 =
      let (_ctype : [ `anonymous_0 ] Ctypes.union Ctypes.typ) =
        Ctypes.union "" in
      _ctype
    let anonymous_4 =
      let (_ctype : [ `anonymous_1 ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "" in
      _ctype
    let _G_fpos_t_0 =
      let (_ctype : [ `_G_fpos_t ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_G_fpos_t" in
      _ctype
    let _G_fpos64_t_0 =
      let (_ctype : [ `_G_fpos64_t ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_G_fpos64_t" in
      _ctype
    let _IO_marker_0 =
      let (_ctype : [ `_IO_marker ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_IO_marker" in
      _ctype
    let _IO_codecvt_0 =
      let (_ctype : [ `_IO_codecvt ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_IO_codecvt" in
      _ctype
    let _IO_wide_data_0 =
      let (_ctype : [ `_IO_wide_data ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_IO_wide_data" in
      _ctype
    let __IO_FILE_0 =
      let (_ctype : [ `__IO_FILE ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "_IO_FILE" in
      _ctype
    let context_s_0 =
      let (_ctype : [ `context_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "context_s" in
      _ctype
    let model_s_0 =
      let (_ctype : [ `model_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "model_s" in
      _ctype
    let ctx_config_s_0 =
      let (_ctype : [ `ctx_config_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "ctx_config_s" in
      _ctype
    let param_s_0 =
      let (_ctype : [ `param_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "param_s" in
      _ctype
    let term_vector_s_0 =
      let (_ctype : [ `term_vector_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "term_vector_s" in
      _ctype
    let type_vector_s_0 =
      let (_ctype : [ `type_vector_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "type_vector_s" in
      _ctype
    let yval_s_0 =
      let (_ctype : [ `yval_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "yval_s" in
      _ctype
    let yval_vector_s_0 =
      let (_ctype : [ `yval_vector_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "yval_vector_s" in
      _ctype
    let error_report_s_0 =
      let (_ctype : [ `error_report_s ] Ctypes.structure Ctypes.typ) =
        Ctypes.structure "error_report_s" in
      _ctype
    let __u_char = Ctypes.typedef Ctypes.uchar "__u_char"
    let __u_short = Ctypes.typedef Ctypes.ushort "__u_short"
    let __u_int = Ctypes.typedef Ctypes.uint "__u_int"
    let __u_long = Ctypes.typedef Ctypes.ulong "__u_long"
    let __int8_t = Ctypes.typedef Ctypes.schar "__int8_t"
    let __uint8_t = Ctypes.typedef Ctypes.uchar "__uint8_t"
    let __int16_t = Ctypes.typedef Ctypes.short "__int16_t"
    let __uint16_t = Ctypes.typedef Ctypes.ushort "__uint16_t"
    let __int32_t = Ctypes.typedef Ctypes.sint "__int32_t"
    let __uint32_t = Ctypes.typedef Ctypes.uint "__uint32_t"
    let __int64_t = Ctypes.typedef Ctypes.long "__int64_t"
    let __uint64_t = Ctypes.typedef Ctypes.ulong "__uint64_t"
    let __int_least8_t = Ctypes.typedef __int8_t "__int_least8_t"
    let __uint_least8_t = Ctypes.typedef __uint8_t "__uint_least8_t"
    let __int_least16_t = Ctypes.typedef __int16_t "__int_least16_t"
    let __uint_least16_t = Ctypes.typedef __uint16_t "__uint_least16_t"
    let __int_least32_t = Ctypes.typedef __int32_t "__int_least32_t"
    let __uint_least32_t = Ctypes.typedef __uint32_t "__uint_least32_t"
    let __int_least64_t = Ctypes.typedef __int64_t "__int_least64_t"
    let __uint_least64_t = Ctypes.typedef __uint64_t "__uint_least64_t"
    let __quad_t = Ctypes.typedef Ctypes.long "__quad_t"
    let __u_quad_t = Ctypes.typedef Ctypes.ulong "__u_quad_t"
    let __intmax_t = Ctypes.typedef Ctypes.long "__intmax_t"
    let __uintmax_t = Ctypes.typedef Ctypes.ulong "__uintmax_t"
    let __dev_t = Ctypes.typedef Ctypes.ulong "__dev_t"
    let __uid_t = Ctypes.typedef Ctypes.uint "__uid_t"
    let __gid_t = Ctypes.typedef Ctypes.uint "__gid_t"
    let __ino_t = Ctypes.typedef Ctypes.ulong "__ino_t"
    let __ino64_t = Ctypes.typedef Ctypes.ulong "__ino64_t"
    let __mode_t = Ctypes.typedef Ctypes.uint "__mode_t"
    let __nlink_t = Ctypes.typedef Ctypes.ulong "__nlink_t"
    let __off_t = Ctypes.typedef Ctypes.long "__off_t"
    let __off64_t = Ctypes.typedef Ctypes.long "__off64_t"
    let __pid_t = Ctypes.typedef Ctypes.sint "__pid_t"
    let anonymous =
      let field_0 =
        Ctypes.field anonymous_2 "__val" (Ctypes.array 2 Ctypes.sint) in
      let () = Ctypes.seal anonymous_2 in
      object
        method ctype = anonymous_2
        method members = object method __val = field_0 end
      end
    let __fsid_t = Ctypes.typedef anonymous_2 "__fsid_t"
    let __clock_t = Ctypes.typedef Ctypes.long "__clock_t"
    let __rlim_t = Ctypes.typedef Ctypes.ulong "__rlim_t"
    let __rlim64_t = Ctypes.typedef Ctypes.ulong "__rlim64_t"
    let __id_t = Ctypes.typedef Ctypes.uint "__id_t"
    let __time_t = Ctypes.typedef Ctypes.long "__time_t"
    let __useconds_t = Ctypes.typedef Ctypes.uint "__useconds_t"
    let __suseconds_t = Ctypes.typedef Ctypes.long "__suseconds_t"
    let __daddr_t = Ctypes.typedef Ctypes.sint "__daddr_t"
    let __key_t = Ctypes.typedef Ctypes.sint "__key_t"
    let __clockid_t = Ctypes.typedef Ctypes.sint "__clockid_t"
    let __timer_t = Ctypes.typedef (Ctypes.ptr Ctypes.void) "__timer_t"
    let __blksize_t = Ctypes.typedef Ctypes.long "__blksize_t"
    let __blkcnt_t = Ctypes.typedef Ctypes.long "__blkcnt_t"
    let __blkcnt64_t = Ctypes.typedef Ctypes.long "__blkcnt64_t"
    let __fsblkcnt_t = Ctypes.typedef Ctypes.ulong "__fsblkcnt_t"
    let __fsblkcnt64_t = Ctypes.typedef Ctypes.ulong "__fsblkcnt64_t"
    let __fsfilcnt_t = Ctypes.typedef Ctypes.ulong "__fsfilcnt_t"
    let __fsfilcnt64_t = Ctypes.typedef Ctypes.ulong "__fsfilcnt64_t"
    let __fsword_t = Ctypes.typedef Ctypes.long "__fsword_t"
    let __ssize_t = Ctypes.typedef Ctypes.long "__ssize_t"
    let __syscall_slong_t = Ctypes.typedef Ctypes.long "__syscall_slong_t"
    let __syscall_ulong_t = Ctypes.typedef Ctypes.ulong "__syscall_ulong_t"
    let __loff_t = Ctypes.typedef __off64_t "__loff_t"
    let __caddr_t = Ctypes.typedef (Ctypes.ptr Ctypes.char) "__caddr_t"
    let __intptr_t = Ctypes.typedef Ctypes.long "__intptr_t"
    let __socklen_t = Ctypes.typedef Ctypes.uint "__socklen_t"
    let __sig_atomic_t = Ctypes.typedef Ctypes.sint "__sig_atomic_t"
    let int8_t = Ctypes.typedef __int8_t "int8_t"
    let int16_t = Ctypes.typedef __int16_t "int16_t"
    let int32_t = Ctypes.typedef __int32_t "int32_t"
    let int64_t = Ctypes.typedef __int64_t "int64_t"
    let uint8_t = Ctypes.typedef __uint8_t "uint8_t"
    let uint16_t = Ctypes.typedef __uint16_t "uint16_t"
    let uint32_t = Ctypes.typedef __uint32_t "uint32_t"
    let uint64_t = Ctypes.typedef __uint64_t "uint64_t"
    let int_least8_t = Ctypes.typedef __int_least8_t "int_least8_t"
    let int_least16_t = Ctypes.typedef __int_least16_t "int_least16_t"
    let int_least32_t = Ctypes.typedef __int_least32_t "int_least32_t"
    let int_least64_t = Ctypes.typedef __int_least64_t "int_least64_t"
    let uint_least8_t = Ctypes.typedef __uint_least8_t "uint_least8_t"
    let uint_least16_t = Ctypes.typedef __uint_least16_t "uint_least16_t"
    let uint_least32_t = Ctypes.typedef __uint_least32_t "uint_least32_t"
    let uint_least64_t = Ctypes.typedef __uint_least64_t "uint_least64_t"
    let int_fast8_t = Ctypes.typedef Ctypes.schar "int_fast8_t"
    let int_fast16_t = Ctypes.typedef Ctypes.long "int_fast16_t"
    let int_fast32_t = Ctypes.typedef Ctypes.long "int_fast32_t"
    let int_fast64_t = Ctypes.typedef Ctypes.long "int_fast64_t"
    let uint_fast8_t = Ctypes.typedef Ctypes.uchar "uint_fast8_t"
    let uint_fast16_t = Ctypes.typedef Ctypes.ulong "uint_fast16_t"
    let uint_fast32_t = Ctypes.typedef Ctypes.ulong "uint_fast32_t"
    let uint_fast64_t = Ctypes.typedef Ctypes.ulong "uint_fast64_t"
    let intptr_t = Ctypes.typedef Ctypes.long "intptr_t"
    let uintptr_t = Ctypes.typedef Ctypes.ulong "uintptr_t"
    let intmax_t = Ctypes.typedef __intmax_t "intmax_t"
    let uintmax_t = Ctypes.typedef __uintmax_t "uintmax_t"
    let size_t = Ctypes.typedef Ctypes.ulong "size_t"
    let anonymous_0 =
      let field_0 = Ctypes.field anonymous_3 "__wch" Ctypes.uint in
      let field_1 =
        Ctypes.field anonymous_3 "__wchb" (Ctypes.array 4 Ctypes.char) in
      let () = Ctypes.seal anonymous_3 in
      object
        method ctype = anonymous_3
        method members =
          object method __wch = field_0 method __wchb = field_1 end
      end
    let anonymous_1 =
      let field_0 = Ctypes.field anonymous_4 "__count" Ctypes.sint in
      let field_1 = Ctypes.field anonymous_4 "__value" anonymous_3 in
      let () = Ctypes.seal anonymous_4 in
      object
        method ctype = anonymous_4
        method members =
          object method __count = field_0 method __value = field_1 end
      end
    let __mbstate_t = Ctypes.typedef anonymous_4 "__mbstate_t"
    let _G_fpos_t =
      let field_0 = Ctypes.field _G_fpos_t_0 "__pos" __off_t in
      let field_1 = Ctypes.field _G_fpos_t_0 "__state" __mbstate_t in
      let () = Ctypes.seal _G_fpos_t_0 in
      object
        method ctype = _G_fpos_t_0
        method members =
          object method __pos = field_0 method __state = field_1 end
      end
    let __fpos_t = Ctypes.typedef _G_fpos_t_0 "__fpos_t"
    let _G_fpos64_t =
      let field_0 = Ctypes.field _G_fpos64_t_0 "__pos" __off64_t in
      let field_1 = Ctypes.field _G_fpos64_t_0 "__state" __mbstate_t in
      let () = Ctypes.seal _G_fpos64_t_0 in
      object
        method ctype = _G_fpos64_t_0
        method members =
          object method __pos = field_0 method __state = field_1 end
      end
    let __fpos64_t = Ctypes.typedef _G_fpos64_t_0 "__fpos64_t"
    let ___FILE = Ctypes.typedef __IO_FILE_0 "__FILE"
    let _FILE = Ctypes.typedef __IO_FILE_0 "FILE"
    let _IO_marker =
      object method ctype = _IO_marker_0 method members = object  end end
    let _IO_codecvt =
      object method ctype = _IO_codecvt_0 method members = object  end end
    let _IO_wide_data =
      object method ctype = _IO_wide_data_0 method members = object  end end
    let _IO_lock_t = Ctypes.typedef Ctypes.void "_IO_lock_t"
    let __IO_FILE =
      let field_0 = Ctypes.field __IO_FILE_0 "_flags" Ctypes.sint in
      let field_1 =
        Ctypes.field __IO_FILE_0 "_IO_read_ptr" (Ctypes.ptr Ctypes.char) in
      let field_2 =
        Ctypes.field __IO_FILE_0 "_IO_read_end" (Ctypes.ptr Ctypes.char) in
      let field_3 =
        Ctypes.field __IO_FILE_0 "_IO_read_base" (Ctypes.ptr Ctypes.char) in
      let field_4 =
        Ctypes.field __IO_FILE_0 "_IO_write_base" (Ctypes.ptr Ctypes.char) in
      let field_5 =
        Ctypes.field __IO_FILE_0 "_IO_write_ptr" (Ctypes.ptr Ctypes.char) in
      let field_6 =
        Ctypes.field __IO_FILE_0 "_IO_write_end" (Ctypes.ptr Ctypes.char) in
      let field_7 =
        Ctypes.field __IO_FILE_0 "_IO_buf_base" (Ctypes.ptr Ctypes.char) in
      let field_8 =
        Ctypes.field __IO_FILE_0 "_IO_buf_end" (Ctypes.ptr Ctypes.char) in
      let field_9 =
        Ctypes.field __IO_FILE_0 "_IO_save_base" (Ctypes.ptr Ctypes.char) in
      let field_10 =
        Ctypes.field __IO_FILE_0 "_IO_backup_base" (Ctypes.ptr Ctypes.char) in
      let field_11 =
        Ctypes.field __IO_FILE_0 "_IO_save_end" (Ctypes.ptr Ctypes.char) in
      let field_12 =
        Ctypes.field __IO_FILE_0 "_markers" (Ctypes.ptr _IO_marker_0) in
      let field_13 =
        Ctypes.field __IO_FILE_0 "_chain" (Ctypes.ptr __IO_FILE_0) in
      let field_14 = Ctypes.field __IO_FILE_0 "_fileno" Ctypes.sint in
      let field_15 = Ctypes.field __IO_FILE_0 "_flags2" Ctypes.sint in
      let field_16 = Ctypes.field __IO_FILE_0 "_old_offset" __off_t in
      let field_17 = Ctypes.field __IO_FILE_0 "_cur_column" Ctypes.ushort in
      let field_18 = Ctypes.field __IO_FILE_0 "_vtable_offset" Ctypes.schar in
      let field_19 =
        Ctypes.field __IO_FILE_0 "_shortbuf" (Ctypes.array 1 Ctypes.char) in
      let field_20 = Ctypes.field __IO_FILE_0 "_lock" (Ctypes.ptr _IO_lock_t) in
      let field_21 = Ctypes.field __IO_FILE_0 "_offset" __off64_t in
      let field_22 =
        Ctypes.field __IO_FILE_0 "_codecvt" (Ctypes.ptr _IO_codecvt_0) in
      let field_23 =
        Ctypes.field __IO_FILE_0 "_wide_data" (Ctypes.ptr _IO_wide_data_0) in
      let field_24 =
        Ctypes.field __IO_FILE_0 "_freeres_list" (Ctypes.ptr __IO_FILE_0) in
      let field_25 =
        Ctypes.field __IO_FILE_0 "_freeres_buf" (Ctypes.ptr Ctypes.void) in
      let field_26 = Ctypes.field __IO_FILE_0 "__pad5" size_t in
      let field_27 = Ctypes.field __IO_FILE_0 "_mode" Ctypes.sint in
      let field_28 =
        Ctypes.field __IO_FILE_0 "_unused2" (Ctypes.array 20 Ctypes.char) in
      let () = Ctypes.seal __IO_FILE_0 in
      object
        method ctype = __IO_FILE_0
        method members =
          object
            method _flags = field_0
            method _IO_read_ptr = field_1
            method _IO_read_end = field_2
            method _IO_read_base = field_3
            method _IO_write_base = field_4
            method _IO_write_ptr = field_5
            method _IO_write_end = field_6
            method _IO_buf_base = field_7
            method _IO_buf_end = field_8
            method _IO_save_base = field_9
            method _IO_backup_base = field_10
            method _IO_save_end = field_11
            method _markers = field_12
            method _chain = field_13
            method _fileno = field_14
            method _flags2 = field_15
            method _old_offset = field_16
            method _cur_column = field_17
            method _vtable_offset = field_18
            method _shortbuf = field_19
            method _lock = field_20
            method _offset = field_21
            method _codecvt = field_22
            method _wide_data = field_23
            method _freeres_list = field_24
            method _freeres_buf = field_25
            method __pad5 = field_26
            method _mode = field_27
            method _unused2 = field_28
          end
      end
    let off_t = Ctypes.typedef __off_t "off_t"
    let ssize_t = Ctypes.typedef __ssize_t "ssize_t"
    let fpos_t = Ctypes.typedef __fpos_t "fpos_t"
    let term_t = Ctypes.typedef int32_t "term_t"
    let type_t = Ctypes.typedef int32_t "type_t"
    let context_s =
      object method ctype = context_s_0 method members = object  end end
    let context_t = Ctypes.typedef context_s_0 "context_t"
    let model_s =
      object method ctype = model_s_0 method members = object  end end
    let model_t = Ctypes.typedef model_s_0 "model_t"
    let ctx_config_s =
      object method ctype = ctx_config_s_0 method members = object  end end
    let ctx_config_t = Ctypes.typedef ctx_config_s_0 "ctx_config_t"
    let param_s =
      object method ctype = param_s_0 method members = object  end end
    let param_t = Ctypes.typedef param_s_0 "param_t"
    let smt_status =
      let (to_int, of_int) =
        ((function
          | `STATUS_IDLE -> 0L
          | `STATUS_SEARCHING -> 1L
          | `STATUS_UNKNOWN -> 2L
          | `STATUS_SAT -> 3L
          | `STATUS_UNSAT -> 4L
          | `STATUS_INTERRUPTED -> 5L
          | `STATUS_ERROR -> 6L),
          (function
           | 0L -> `STATUS_IDLE
           | 1L -> `STATUS_SEARCHING
           | 2L -> `STATUS_UNKNOWN
           | 3L -> `STATUS_SAT
           | 4L -> `STATUS_UNSAT
           | 5L -> `STATUS_INTERRUPTED
           | 6L -> `STATUS_ERROR
           | _ -> failwith "enum to_int")) in
      object
        method ctype = Ctypes.uint
        method to_int = to_int
        method of_int = of_int
      end
    let smt_status_t = Ctypes.typedef smt_status#ctype "smt_status_t"
    let term_vector_s =
      let field_0 = Ctypes.field term_vector_s_0 "capacity" uint32_t in
      let field_1 = Ctypes.field term_vector_s_0 "size" uint32_t in
      let field_2 = Ctypes.field term_vector_s_0 "data" (Ctypes.ptr term_t) in
      let () = Ctypes.seal term_vector_s_0 in
      object
        method ctype = term_vector_s_0
        method members =
          object
            method capacity = field_0
            method size = field_1
            method data = field_2
          end
      end
    let term_vector_t = Ctypes.typedef term_vector_s_0 "term_vector_t"
    let type_vector_s =
      let field_0 = Ctypes.field type_vector_s_0 "capacity" uint32_t in
      let field_1 = Ctypes.field type_vector_s_0 "size" uint32_t in
      let field_2 = Ctypes.field type_vector_s_0 "data" (Ctypes.ptr type_t) in
      let () = Ctypes.seal type_vector_s_0 in
      object
        method ctype = type_vector_s_0
        method members =
          object
            method capacity = field_0
            method size = field_1
            method data = field_2
          end
      end
    let type_vector_t = Ctypes.typedef type_vector_s_0 "type_vector_t"
    let term_constructor =
      let (to_int, of_int) =
        ((function
          | `YICES_CONSTRUCTOR_ERROR -> (-1L)
          | `YICES_BOOL_CONSTANT -> 0L
          | `YICES_ARITH_CONSTANT -> 1L
          | `YICES_BV_CONSTANT -> 2L
          | `YICES_SCALAR_CONSTANT -> 3L
          | `YICES_VARIABLE -> 4L
          | `YICES_UNINTERPRETED_TERM -> 5L
          | `YICES_ITE_TERM -> 6L
          | `YICES_APP_TERM -> 7L
          | `YICES_UPDATE_TERM -> 8L
          | `YICES_TUPLE_TERM -> 9L
          | `YICES_EQ_TERM -> 10L
          | `YICES_DISTINCT_TERM -> 11L
          | `YICES_FORALL_TERM -> 12L
          | `YICES_LAMBDA_TERM -> 13L
          | `YICES_NOT_TERM -> 14L
          | `YICES_OR_TERM -> 15L
          | `YICES_XOR_TERM -> 16L
          | `YICES_BV_ARRAY -> 17L
          | `YICES_BV_DIV -> 18L
          | `YICES_BV_REM -> 19L
          | `YICES_BV_SDIV -> 20L
          | `YICES_BV_SREM -> 21L
          | `YICES_BV_SMOD -> 22L
          | `YICES_BV_SHL -> 23L
          | `YICES_BV_LSHR -> 24L
          | `YICES_BV_ASHR -> 25L
          | `YICES_BV_GE_ATOM -> 26L
          | `YICES_BV_SGE_ATOM -> 27L
          | `YICES_ARITH_GE_ATOM -> 28L
          | `YICES_ARITH_ROOT_ATOM -> 29L
          | `YICES_ABS -> 30L
          | `YICES_CEIL -> 31L
          | `YICES_FLOOR -> 32L
          | `YICES_RDIV -> 33L
          | `YICES_IDIV -> 34L
          | `YICES_IMOD -> 35L
          | `YICES_IS_INT_ATOM -> 36L
          | `YICES_DIVIDES_ATOM -> 37L
          | `YICES_SELECT_TERM -> 38L
          | `YICES_BIT_TERM -> 39L
          | `YICES_BV_SUM -> 40L
          | `YICES_ARITH_SUM -> 41L
          | `YICES_POWER_PRODUCT -> 42L),
          (function
           | (-1L) -> `YICES_CONSTRUCTOR_ERROR
           | 0L -> `YICES_BOOL_CONSTANT
           | 1L -> `YICES_ARITH_CONSTANT
           | 2L -> `YICES_BV_CONSTANT
           | 3L -> `YICES_SCALAR_CONSTANT
           | 4L -> `YICES_VARIABLE
           | 5L -> `YICES_UNINTERPRETED_TERM
           | 6L -> `YICES_ITE_TERM
           | 7L -> `YICES_APP_TERM
           | 8L -> `YICES_UPDATE_TERM
           | 9L -> `YICES_TUPLE_TERM
           | 10L -> `YICES_EQ_TERM
           | 11L -> `YICES_DISTINCT_TERM
           | 12L -> `YICES_FORALL_TERM
           | 13L -> `YICES_LAMBDA_TERM
           | 14L -> `YICES_NOT_TERM
           | 15L -> `YICES_OR_TERM
           | 16L -> `YICES_XOR_TERM
           | 17L -> `YICES_BV_ARRAY
           | 18L -> `YICES_BV_DIV
           | 19L -> `YICES_BV_REM
           | 20L -> `YICES_BV_SDIV
           | 21L -> `YICES_BV_SREM
           | 22L -> `YICES_BV_SMOD
           | 23L -> `YICES_BV_SHL
           | 24L -> `YICES_BV_LSHR
           | 25L -> `YICES_BV_ASHR
           | 26L -> `YICES_BV_GE_ATOM
           | 27L -> `YICES_BV_SGE_ATOM
           | 28L -> `YICES_ARITH_GE_ATOM
           | 29L -> `YICES_ARITH_ROOT_ATOM
           | 30L -> `YICES_ABS
           | 31L -> `YICES_CEIL
           | 32L -> `YICES_FLOOR
           | 33L -> `YICES_RDIV
           | 34L -> `YICES_IDIV
           | 35L -> `YICES_IMOD
           | 36L -> `YICES_IS_INT_ATOM
           | 37L -> `YICES_DIVIDES_ATOM
           | 38L -> `YICES_SELECT_TERM
           | 39L -> `YICES_BIT_TERM
           | 40L -> `YICES_BV_SUM
           | 41L -> `YICES_ARITH_SUM
           | 42L -> `YICES_POWER_PRODUCT
           | _ -> failwith "enum to_int")) in
      object
        method ctype = Ctypes.sint
        method to_int = to_int
        method of_int = of_int
      end
    let term_constructor_t =
      Ctypes.typedef term_constructor#ctype "term_constructor_t"
    let yval_tag =
      let (to_int, of_int) =
        ((function
          | `YVAL_UNKNOWN -> 0L
          | `YVAL_BOOL -> 1L
          | `YVAL_RATIONAL -> 2L
          | `YVAL_ALGEBRAIC -> 3L
          | `YVAL_BV -> 4L
          | `YVAL_SCALAR -> 5L
          | `YVAL_TUPLE -> 6L
          | `YVAL_FUNCTION -> 7L
          | `YVAL_MAPPING -> 8L),
          (function
           | 0L -> `YVAL_UNKNOWN
           | 1L -> `YVAL_BOOL
           | 2L -> `YVAL_RATIONAL
           | 3L -> `YVAL_ALGEBRAIC
           | 4L -> `YVAL_BV
           | 5L -> `YVAL_SCALAR
           | 6L -> `YVAL_TUPLE
           | 7L -> `YVAL_FUNCTION
           | 8L -> `YVAL_MAPPING
           | _ -> failwith "enum to_int")) in
      object
        method ctype = Ctypes.uint
        method to_int = to_int
        method of_int = of_int
      end
    let yval_tag_t = Ctypes.typedef yval_tag#ctype "yval_tag_t"
    let yval_s =
      let field_0 = Ctypes.field yval_s_0 "node_id" int32_t in
      let field_1 = Ctypes.field yval_s_0 "node_tag" yval_tag_t in
      let () = Ctypes.seal yval_s_0 in
      object
        method ctype = yval_s_0
        method members =
          object method node_id = field_0 method node_tag = field_1 end
      end
    let yval_t = Ctypes.typedef yval_s_0 "yval_t"
    let yval_vector_s =
      let field_0 = Ctypes.field yval_vector_s_0 "capacity" uint32_t in
      let field_1 = Ctypes.field yval_vector_s_0 "size" uint32_t in
      let field_2 = Ctypes.field yval_vector_s_0 "data" (Ctypes.ptr yval_t) in
      let () = Ctypes.seal yval_vector_s_0 in
      object
        method ctype = yval_vector_s_0
        method members =
          object
            method capacity = field_0
            method size = field_1
            method data = field_2
          end
      end
    let yval_vector_t = Ctypes.typedef yval_vector_s_0 "yval_vector_t"
    let yices_gen_mode =
      let (to_int, of_int) =
        ((function
          | `YICES_GEN_DEFAULT -> 0L
          | `YICES_GEN_BY_SUBST -> 1L
          | `YICES_GEN_BY_PROJ -> 2L),
          (function
           | 0L -> `YICES_GEN_DEFAULT
           | 1L -> `YICES_GEN_BY_SUBST
           | 2L -> `YICES_GEN_BY_PROJ
           | _ -> failwith "enum to_int")) in
      object
        method ctype = Ctypes.uint
        method to_int = to_int
        method of_int = of_int
      end
    let yices_gen_mode_t =
      Ctypes.typedef yices_gen_mode#ctype "yices_gen_mode_t"
    let error_code =
      let (to_int, of_int) =
        ((function
          | `NO_ERROR -> 0L
          | `INVALID_TYPE -> 1L
          | `INVALID_TERM -> 2L
          | `INVALID_CONSTANT_INDEX -> 3L
          | `INVALID_VAR_INDEX -> 4L
          | `INVALID_TUPLE_INDEX -> 5L
          | `INVALID_RATIONAL_FORMAT -> 6L
          | `INVALID_FLOAT_FORMAT -> 7L
          | `INVALID_BVBIN_FORMAT -> 8L
          | `INVALID_BVHEX_FORMAT -> 9L
          | `INVALID_BITSHIFT -> 10L
          | `INVALID_BVEXTRACT -> 11L
          | `INVALID_BITEXTRACT -> 12L
          | `TOO_MANY_ARGUMENTS -> 13L
          | `TOO_MANY_VARS -> 14L
          | `MAX_BVSIZE_EXCEEDED -> 15L
          | `DEGREE_OVERFLOW -> 16L
          | `DIVISION_BY_ZERO -> 17L
          | `POS_INT_REQUIRED -> 18L
          | `NONNEG_INT_REQUIRED -> 19L
          | `SCALAR_OR_UTYPE_REQUIRED -> 20L
          | `FUNCTION_REQUIRED -> 21L
          | `TUPLE_REQUIRED -> 22L
          | `VARIABLE_REQUIRED -> 23L
          | `ARITHTERM_REQUIRED -> 24L
          | `BITVECTOR_REQUIRED -> 25L
          | `SCALAR_TERM_REQUIRED -> 26L
          | `WRONG_NUMBER_OF_ARGUMENTS -> 27L
          | `TYPE_MISMATCH -> 28L
          | `INCOMPATIBLE_TYPES -> 29L
          | `DUPLICATE_VARIABLE -> 30L
          | `INCOMPATIBLE_BVSIZES -> 31L
          | `EMPTY_BITVECTOR -> 32L
          | `ARITHCONSTANT_REQUIRED -> 33L
          | `INVALID_MACRO -> 34L
          | `TOO_MANY_MACRO_PARAMS -> 35L
          | `TYPE_VAR_REQUIRED -> 36L
          | `DUPLICATE_TYPE_VAR -> 37L
          | `BVTYPE_REQUIRED -> 38L
          | `BAD_TERM_DECREF -> 39L
          | `BAD_TYPE_DECREF -> 40L
          | `INVALID_TYPE_OP -> 41L
          | `INVALID_TERM_OP -> 42L
          | `INVALID_TOKEN -> 100L
          | `SYNTAX_ERROR -> 101L
          | `UNDEFINED_TYPE_NAME -> 102L
          | `UNDEFINED_TERM_NAME -> 103L
          | `REDEFINED_TYPE_NAME -> 104L
          | `REDEFINED_TERM_NAME -> 105L
          | `DUPLICATE_NAME_IN_SCALAR -> 106L
          | `DUPLICATE_VAR_NAME -> 107L
          | `INTEGER_OVERFLOW -> 108L
          | `INTEGER_REQUIRED -> 109L
          | `RATIONAL_REQUIRED -> 110L
          | `SYMBOL_REQUIRED -> 111L
          | `TYPE_REQUIRED -> 112L
          | `NON_CONSTANT_DIVISOR -> 113L
          | `NEGATIVE_BVSIZE -> 114L
          | `INVALID_BVCONSTANT -> 115L
          | `TYPE_MISMATCH_IN_DEF -> 116L
          | `ARITH_ERROR -> 117L
          | `BVARITH_ERROR -> 118L
          | `CTX_FREE_VAR_IN_FORMULA -> 300L
          | `CTX_LOGIC_NOT_SUPPORTED -> 301L
          | `CTX_UF_NOT_SUPPORTED -> 302L
          | `CTX_ARITH_NOT_SUPPORTED -> 303L
          | `CTX_BV_NOT_SUPPORTED -> 304L
          | `CTX_ARRAYS_NOT_SUPPORTED -> 305L
          | `CTX_QUANTIFIERS_NOT_SUPPORTED -> 306L
          | `CTX_LAMBDAS_NOT_SUPPORTED -> 307L
          | `CTX_NONLINEAR_ARITH_NOT_SUPPORTED -> 308L
          | `CTX_FORMULA_NOT_IDL -> 309L
          | `CTX_FORMULA_NOT_RDL -> 310L
          | `CTX_TOO_MANY_ARITH_VARS -> 311L
          | `CTX_TOO_MANY_ARITH_ATOMS -> 312L
          | `CTX_TOO_MANY_BV_VARS -> 313L
          | `CTX_TOO_MANY_BV_ATOMS -> 314L
          | `CTX_ARITH_SOLVER_EXCEPTION -> 315L
          | `CTX_BV_SOLVER_EXCEPTION -> 316L
          | `CTX_ARRAY_SOLVER_EXCEPTION -> 317L
          | `CTX_SCALAR_NOT_SUPPORTED -> 318L
          | `CTX_TUPLE_NOT_SUPPORTED -> 319L
          | `CTX_UTYPE_NOT_SUPPORTED -> 320L
          | `CTX_INVALID_OPERATION -> 400L
          | `CTX_OPERATION_NOT_SUPPORTED -> 401L
          | `CTX_UNKNOWN_DELEGATE -> 420L
          | `CTX_DELEGATE_NOT_AVAILABLE -> 421L
          | `CTX_INVALID_CONFIG -> 500L
          | `CTX_UNKNOWN_PARAMETER -> 501L
          | `CTX_INVALID_PARAMETER_VALUE -> 502L
          | `CTX_UNKNOWN_LOGIC -> 503L
          | `EVAL_UNKNOWN_TERM -> 600L
          | `EVAL_FREEVAR_IN_TERM -> 601L
          | `EVAL_QUANTIFIER -> 602L
          | `EVAL_LAMBDA -> 603L
          | `EVAL_OVERFLOW -> 604L
          | `EVAL_FAILED -> 605L
          | `EVAL_CONVERSION_FAILED -> 606L
          | `EVAL_NO_IMPLICANT -> 607L
          | `EVAL_NOT_SUPPORTED -> 608L
          | `MDL_UNINT_REQUIRED -> 700L
          | `MDL_CONSTANT_REQUIRED -> 701L
          | `MDL_DUPLICATE_VAR -> 702L
          | `MDL_FTYPE_NOT_ALLOWED -> 703L
          | `MDL_CONSTRUCTION_FAILED -> 704L
          | `YVAL_INVALID_OP -> 800L
          | `YVAL_OVERFLOW -> 801L
          | `YVAL_NOT_SUPPORTED -> 802L
          | `MDL_GEN_TYPE_NOT_SUPPORTED -> 900L
          | `MDL_GEN_NONLINEAR -> 901L
          | `MDL_GEN_FAILED -> 902L
          | `MCSAT_ERROR_UNSUPPORTED_THEORY -> 1000L
          | `OUTPUT_ERROR -> 9000L
          | `INTERNAL_EXCEPTION -> 9999L),
          (function
           | 0L -> `NO_ERROR
           | 1L -> `INVALID_TYPE
           | 2L -> `INVALID_TERM
           | 3L -> `INVALID_CONSTANT_INDEX
           | 4L -> `INVALID_VAR_INDEX
           | 5L -> `INVALID_TUPLE_INDEX
           | 6L -> `INVALID_RATIONAL_FORMAT
           | 7L -> `INVALID_FLOAT_FORMAT
           | 8L -> `INVALID_BVBIN_FORMAT
           | 9L -> `INVALID_BVHEX_FORMAT
           | 10L -> `INVALID_BITSHIFT
           | 11L -> `INVALID_BVEXTRACT
           | 12L -> `INVALID_BITEXTRACT
           | 13L -> `TOO_MANY_ARGUMENTS
           | 14L -> `TOO_MANY_VARS
           | 15L -> `MAX_BVSIZE_EXCEEDED
           | 16L -> `DEGREE_OVERFLOW
           | 17L -> `DIVISION_BY_ZERO
           | 18L -> `POS_INT_REQUIRED
           | 19L -> `NONNEG_INT_REQUIRED
           | 20L -> `SCALAR_OR_UTYPE_REQUIRED
           | 21L -> `FUNCTION_REQUIRED
           | 22L -> `TUPLE_REQUIRED
           | 23L -> `VARIABLE_REQUIRED
           | 24L -> `ARITHTERM_REQUIRED
           | 25L -> `BITVECTOR_REQUIRED
           | 26L -> `SCALAR_TERM_REQUIRED
           | 27L -> `WRONG_NUMBER_OF_ARGUMENTS
           | 28L -> `TYPE_MISMATCH
           | 29L -> `INCOMPATIBLE_TYPES
           | 30L -> `DUPLICATE_VARIABLE
           | 31L -> `INCOMPATIBLE_BVSIZES
           | 32L -> `EMPTY_BITVECTOR
           | 33L -> `ARITHCONSTANT_REQUIRED
           | 34L -> `INVALID_MACRO
           | 35L -> `TOO_MANY_MACRO_PARAMS
           | 36L -> `TYPE_VAR_REQUIRED
           | 37L -> `DUPLICATE_TYPE_VAR
           | 38L -> `BVTYPE_REQUIRED
           | 39L -> `BAD_TERM_DECREF
           | 40L -> `BAD_TYPE_DECREF
           | 41L -> `INVALID_TYPE_OP
           | 42L -> `INVALID_TERM_OP
           | 100L -> `INVALID_TOKEN
           | 101L -> `SYNTAX_ERROR
           | 102L -> `UNDEFINED_TYPE_NAME
           | 103L -> `UNDEFINED_TERM_NAME
           | 104L -> `REDEFINED_TYPE_NAME
           | 105L -> `REDEFINED_TERM_NAME
           | 106L -> `DUPLICATE_NAME_IN_SCALAR
           | 107L -> `DUPLICATE_VAR_NAME
           | 108L -> `INTEGER_OVERFLOW
           | 109L -> `INTEGER_REQUIRED
           | 110L -> `RATIONAL_REQUIRED
           | 111L -> `SYMBOL_REQUIRED
           | 112L -> `TYPE_REQUIRED
           | 113L -> `NON_CONSTANT_DIVISOR
           | 114L -> `NEGATIVE_BVSIZE
           | 115L -> `INVALID_BVCONSTANT
           | 116L -> `TYPE_MISMATCH_IN_DEF
           | 117L -> `ARITH_ERROR
           | 118L -> `BVARITH_ERROR
           | 300L -> `CTX_FREE_VAR_IN_FORMULA
           | 301L -> `CTX_LOGIC_NOT_SUPPORTED
           | 302L -> `CTX_UF_NOT_SUPPORTED
           | 303L -> `CTX_ARITH_NOT_SUPPORTED
           | 304L -> `CTX_BV_NOT_SUPPORTED
           | 305L -> `CTX_ARRAYS_NOT_SUPPORTED
           | 306L -> `CTX_QUANTIFIERS_NOT_SUPPORTED
           | 307L -> `CTX_LAMBDAS_NOT_SUPPORTED
           | 308L -> `CTX_NONLINEAR_ARITH_NOT_SUPPORTED
           | 309L -> `CTX_FORMULA_NOT_IDL
           | 310L -> `CTX_FORMULA_NOT_RDL
           | 311L -> `CTX_TOO_MANY_ARITH_VARS
           | 312L -> `CTX_TOO_MANY_ARITH_ATOMS
           | 313L -> `CTX_TOO_MANY_BV_VARS
           | 314L -> `CTX_TOO_MANY_BV_ATOMS
           | 315L -> `CTX_ARITH_SOLVER_EXCEPTION
           | 316L -> `CTX_BV_SOLVER_EXCEPTION
           | 317L -> `CTX_ARRAY_SOLVER_EXCEPTION
           | 318L -> `CTX_SCALAR_NOT_SUPPORTED
           | 319L -> `CTX_TUPLE_NOT_SUPPORTED
           | 320L -> `CTX_UTYPE_NOT_SUPPORTED
           | 400L -> `CTX_INVALID_OPERATION
           | 401L -> `CTX_OPERATION_NOT_SUPPORTED
           | 420L -> `CTX_UNKNOWN_DELEGATE 
           | 421L -> `CTX_DELEGATE_NOT_AVAILABLE
           | 500L -> `CTX_INVALID_CONFIG
           | 501L -> `CTX_UNKNOWN_PARAMETER
           | 502L -> `CTX_INVALID_PARAMETER_VALUE
           | 503L -> `CTX_UNKNOWN_LOGIC
           | 600L -> `EVAL_UNKNOWN_TERM
           | 601L -> `EVAL_FREEVAR_IN_TERM
           | 602L -> `EVAL_QUANTIFIER
           | 603L -> `EVAL_LAMBDA
           | 604L -> `EVAL_OVERFLOW
           | 605L -> `EVAL_FAILED
           | 606L -> `EVAL_CONVERSION_FAILED
           | 607L -> `EVAL_NO_IMPLICANT
           | 608L -> `EVAL_NOT_SUPPORTED
           | 700L -> `MDL_UNINT_REQUIRED
           | 701L -> `MDL_CONSTANT_REQUIRED
           | 702L -> `MDL_DUPLICATE_VAR
           | 703L -> `MDL_FTYPE_NOT_ALLOWED
           | 704L -> `MDL_CONSTRUCTION_FAILED
           | 800L -> `YVAL_INVALID_OP
           | 801L -> `YVAL_OVERFLOW
           | 802L -> `YVAL_NOT_SUPPORTED
           | 900L -> `MDL_GEN_TYPE_NOT_SUPPORTED
           | 901L -> `MDL_GEN_NONLINEAR
           | 902L -> `MDL_GEN_FAILED
           | 1000L -> `MCSAT_ERROR_UNSUPPORTED_THEORY
           | 9000L -> `OUTPUT_ERROR
           | 9999L -> `INTERNAL_EXCEPTION
           | _ -> failwith "enum to_int")) in
      object
        method ctype = Ctypes.uint
        method to_int = to_int
        method of_int = of_int
      end
    let error_code_t = Ctypes.typedef error_code#ctype "error_code_t"
    let error_report_s =
      let field_0 = Ctypes.field error_report_s_0 "code" error_code_t in
      let field_1 = Ctypes.field error_report_s_0 "line" uint32_t in
      let field_2 = Ctypes.field error_report_s_0 "column" uint32_t in
      let field_3 = Ctypes.field error_report_s_0 "term1" term_t in
      let field_4 = Ctypes.field error_report_s_0 "type1" type_t in
      let field_5 = Ctypes.field error_report_s_0 "term2" term_t in
      let field_6 = Ctypes.field error_report_s_0 "type2" type_t in
      let field_7 = Ctypes.field error_report_s_0 "badval" int64_t in
      let () = Ctypes.seal error_report_s_0 in
      object
        method ctype = error_report_s_0
        method members =
          object
            method code = field_0
            method line = field_1
            method column = field_2
            method term1 = field_3
            method type1 = field_4
            method term2 = field_5
            method type2 = field_6
            method badval = field_7
          end
      end
    let error_report_t = Ctypes.typedef error_report_s_0 "error_report_t"
    let yices_version =
      Foreign.foreign_value "yices_version" (Ctypes.ptr Ctypes.char)
    let yices_build_arch =
      Foreign.foreign_value "yices_build_arch" (Ctypes.ptr Ctypes.char)
    let yices_build_mode =
      Foreign.foreign_value "yices_build_mode" (Ctypes.ptr Ctypes.char)
    let yices_build_date =
      Foreign.foreign_value "yices_build_date" (Ctypes.ptr Ctypes.char)
    let yices_has_mcsat =
      Foreign.foreign "yices_has_mcsat"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning int32_t))
    let yices_is_thread_safe =
      Foreign.foreign "yices_is_thread_safe"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning int32_t))
    let yices_init =
      Foreign.foreign "yices_init"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.void))
    let yices_exit =
      Foreign.foreign "yices_exit"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.void))
    let yices_reset =
      Foreign.foreign "yices_reset"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.void))
    let yices_free_string =
      Foreign.foreign "yices_free_string"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning Ctypes.void))
    let yices_set_out_of_mem_callback =
      Foreign.foreign "yices_set_out_of_mem_callback"
        (Ctypes.(@->)
           (Ctypes.static_funptr
              (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.void)))
           (Ctypes.returning Ctypes.void))
    let yices_error_code =
      Foreign.foreign "yices_error_code"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning error_code_t))
    let yices_error_report =
      Foreign.foreign "yices_error_report"
        (Ctypes.(@->) Ctypes.void
           (Ctypes.returning (Ctypes.ptr error_report_t)))
    let yices_clear_error =
      Foreign.foreign "yices_clear_error"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning Ctypes.void))
    let yices_print_error =
      Foreign.foreign "yices_print_error"
        (Ctypes.(@->) (Ctypes.ptr _FILE) (Ctypes.returning int32_t))
    let yices_print_error_fd =
      Foreign.foreign "yices_print_error_fd"
        (Ctypes.(@->) Ctypes.sint (Ctypes.returning int32_t))
    let yices_error_string =
      Foreign.foreign "yices_error_string"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning (Ctypes.ptr Ctypes.char)))
    let yices_init_term_vector =
      Foreign.foreign "yices_init_term_vector"
        (Ctypes.(@->) (Ctypes.ptr term_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_init_type_vector =
      Foreign.foreign "yices_init_type_vector"
        (Ctypes.(@->) (Ctypes.ptr type_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_delete_term_vector =
      Foreign.foreign "yices_delete_term_vector"
        (Ctypes.(@->) (Ctypes.ptr term_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_delete_type_vector =
      Foreign.foreign "yices_delete_type_vector"
        (Ctypes.(@->) (Ctypes.ptr type_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_reset_term_vector =
      Foreign.foreign "yices_reset_term_vector"
        (Ctypes.(@->) (Ctypes.ptr term_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_reset_type_vector =
      Foreign.foreign "yices_reset_type_vector"
        (Ctypes.(@->) (Ctypes.ptr type_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_bool_type =
      Foreign.foreign "yices_bool_type"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning type_t))
    let yices_int_type =
      Foreign.foreign "yices_int_type"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning type_t))
    let yices_real_type =
      Foreign.foreign "yices_real_type"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning type_t))
    let yices_bv_type =
      Foreign.foreign "yices_bv_type"
        (Ctypes.(@->) uint32_t (Ctypes.returning type_t))
    let yices_new_scalar_type =
      Foreign.foreign "yices_new_scalar_type"
        (Ctypes.(@->) uint32_t (Ctypes.returning type_t))
    let yices_new_uninterpreted_type =
      Foreign.foreign "yices_new_uninterpreted_type"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning type_t))
    let yices_tuple_type =
      Foreign.foreign "yices_tuple_type"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr type_t) (Ctypes.returning type_t)))
    let yices_tuple_type1 =
      Foreign.foreign "yices_tuple_type1"
        (Ctypes.(@->) type_t (Ctypes.returning type_t))
    let yices_tuple_type2 =
      Foreign.foreign "yices_tuple_type2"
        (Ctypes.(@->) type_t (Ctypes.(@->) type_t (Ctypes.returning type_t)))
    let yices_tuple_type3 =
      Foreign.foreign "yices_tuple_type3"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) type_t
              (Ctypes.(@->) type_t (Ctypes.returning type_t))))
    let yices_function_type =
      Foreign.foreign "yices_function_type"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr type_t)
              (Ctypes.(@->) type_t (Ctypes.returning type_t))))
    let yices_function_type1 =
      Foreign.foreign "yices_function_type1"
        (Ctypes.(@->) type_t (Ctypes.(@->) type_t (Ctypes.returning type_t)))
    let yices_function_type2 =
      Foreign.foreign "yices_function_type2"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) type_t
              (Ctypes.(@->) type_t (Ctypes.returning type_t))))
    let yices_function_type3 =
      Foreign.foreign "yices_function_type3"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) type_t
              (Ctypes.(@->) type_t
                 (Ctypes.(@->) type_t (Ctypes.returning type_t)))))
    let yices_type_is_bool =
      Foreign.foreign "yices_type_is_bool"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_int =
      Foreign.foreign "yices_type_is_int"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_real =
      Foreign.foreign "yices_type_is_real"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_arithmetic =
      Foreign.foreign "yices_type_is_arithmetic"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_bitvector =
      Foreign.foreign "yices_type_is_bitvector"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_tuple =
      Foreign.foreign "yices_type_is_tuple"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_function =
      Foreign.foreign "yices_type_is_function"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_scalar =
      Foreign.foreign "yices_type_is_scalar"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_is_uninterpreted =
      Foreign.foreign "yices_type_is_uninterpreted"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_test_subtype =
      Foreign.foreign "yices_test_subtype"
        (Ctypes.(@->) type_t (Ctypes.(@->) type_t (Ctypes.returning int32_t)))
    let yices_compatible_types =
      Foreign.foreign "yices_compatible_types"
        (Ctypes.(@->) type_t (Ctypes.(@->) type_t (Ctypes.returning int32_t)))
    let yices_bvtype_size =
      Foreign.foreign "yices_bvtype_size"
        (Ctypes.(@->) type_t (Ctypes.returning uint32_t))
    let yices_scalar_type_card =
      Foreign.foreign "yices_scalar_type_card"
        (Ctypes.(@->) type_t (Ctypes.returning uint32_t))
    let yices_type_num_children =
      Foreign.foreign "yices_type_num_children"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_type_child =
      Foreign.foreign "yices_type_child"
        (Ctypes.(@->) type_t (Ctypes.(@->) int32_t (Ctypes.returning type_t)))
    let yices_type_children =
      Foreign.foreign "yices_type_children"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) (Ctypes.ptr type_vector_t)
              (Ctypes.returning int32_t)))
    let yices_true =
      Foreign.foreign "yices_true"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning term_t))
    let yices_false =
      Foreign.foreign "yices_false"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning term_t))
    let yices_constant =
      Foreign.foreign "yices_constant"
        (Ctypes.(@->) type_t (Ctypes.(@->) int32_t (Ctypes.returning term_t)))
    let yices_new_uninterpreted_term =
      Foreign.foreign "yices_new_uninterpreted_term"
        (Ctypes.(@->) type_t (Ctypes.returning term_t))
    let yices_new_variable =
      Foreign.foreign "yices_new_variable"
        (Ctypes.(@->) type_t (Ctypes.returning term_t))
    let yices_application =
      Foreign.foreign "yices_application"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t))))
    let yices_application1 =
      Foreign.foreign "yices_application1"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_application2 =
      Foreign.foreign "yices_application2"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_application3 =
      Foreign.foreign "yices_application3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t
                 (Ctypes.(@->) term_t (Ctypes.returning term_t)))))
    let yices_ite =
      Foreign.foreign "yices_ite"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_eq =
      Foreign.foreign "yices_eq"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_neq =
      Foreign.foreign "yices_neq"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_not =
      Foreign.foreign "yices_not"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_or =
      Foreign.foreign "yices_or"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_and =
      Foreign.foreign "yices_and"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_xor =
      Foreign.foreign "yices_xor"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_or2 =
      Foreign.foreign "yices_or2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_and2 =
      Foreign.foreign "yices_and2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_xor2 =
      Foreign.foreign "yices_xor2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_or3 =
      Foreign.foreign "yices_or3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_and3 =
      Foreign.foreign "yices_and3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_xor3 =
      Foreign.foreign "yices_xor3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_iff =
      Foreign.foreign "yices_iff"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_implies =
      Foreign.foreign "yices_implies"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_tuple =
      Foreign.foreign "yices_tuple"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_pair =
      Foreign.foreign "yices_pair"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_triple =
      Foreign.foreign "yices_triple"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_select =
      Foreign.foreign "yices_select"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_tuple_update =
      Foreign.foreign "yices_tuple_update"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_update =
      Foreign.foreign "yices_update"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) term_t (Ctypes.returning term_t)))))
    let yices_update1 =
      Foreign.foreign "yices_update1"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_update2 =
      Foreign.foreign "yices_update2"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t
                 (Ctypes.(@->) term_t (Ctypes.returning term_t)))))
    let yices_update3 =
      Foreign.foreign "yices_update3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t
                 (Ctypes.(@->) term_t
                    (Ctypes.(@->) term_t (Ctypes.returning term_t))))))
    let yices_distinct =
      Foreign.foreign "yices_distinct"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_forall =
      Foreign.foreign "yices_forall"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_exists =
      Foreign.foreign "yices_exists"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_lambda =
      Foreign.foreign "yices_lambda"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_zero =
      Foreign.foreign "yices_zero"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning term_t))
    let yices_int32 =
      Foreign.foreign "yices_int32"
        (Ctypes.(@->) int32_t (Ctypes.returning term_t))
    let yices_int64 =
      Foreign.foreign "yices_int64"
        (Ctypes.(@->) int64_t (Ctypes.returning term_t))
    let yices_rational32 =
      Foreign.foreign "yices_rational32"
        (Ctypes.(@->) int32_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_rational64 =
      Foreign.foreign "yices_rational64"
        (Ctypes.(@->) int64_t
           (Ctypes.(@->) uint64_t (Ctypes.returning term_t)))
[%%if gmp_present]
    let yices_mpz =
      Foreign.foreign "yices_mpz"
        (Ctypes.(@->) MPZ.t_ptr
           (Ctypes.returning term_t))
    let yices_mpq =
      Foreign.foreign "yices_mpq"
        (Ctypes.(@->) MPQ.t_ptr
           (Ctypes.returning term_t))
[%%endif]
    let yices_parse_rational =
      Foreign.foreign "yices_parse_rational"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_parse_float =
      Foreign.foreign "yices_parse_float"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_add =
      Foreign.foreign "yices_add"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_sub =
      Foreign.foreign "yices_sub"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_neg =
      Foreign.foreign "yices_neg"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_mul =
      Foreign.foreign "yices_mul"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_square =
      Foreign.foreign "yices_square"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_power =
      Foreign.foreign "yices_power"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_sum =
      Foreign.foreign "yices_sum"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_product =
      Foreign.foreign "yices_product"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_division =
      Foreign.foreign "yices_division"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_idiv =
      Foreign.foreign "yices_idiv"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_imod =
      Foreign.foreign "yices_imod"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_divides_atom =
      Foreign.foreign "yices_divides_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_is_int_atom =
      Foreign.foreign "yices_is_int_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_abs =
      Foreign.foreign "yices_abs"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_floor =
      Foreign.foreign "yices_floor"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_ceil =
      Foreign.foreign "yices_ceil"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_poly_int32 =
      Foreign.foreign "yices_poly_int32"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr int32_t)
              (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t))))
    let yices_poly_int64 =
      Foreign.foreign "yices_poly_int64"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr int64_t)
              (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t))))
    let yices_poly_rational32 =
      Foreign.foreign "yices_poly_rational32"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr int32_t)
              (Ctypes.(@->) (Ctypes.ptr uint32_t)
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))))
    let yices_poly_rational64 =
      Foreign.foreign "yices_poly_rational64"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr int64_t)
              (Ctypes.(@->) (Ctypes.ptr uint64_t)
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))))
[%%if gmp_present]
    let yices_poly_mpz =
      Foreign.foreign "yices_poly_mpz"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) MPZ.t_ptr
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t))))
    let yices_poly_mpq =
      Foreign.foreign "yices_poly_mpq"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) MPQ.t_ptr
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t))))
[%%endif]
    let yices_arith_eq_atom =
      Foreign.foreign "yices_arith_eq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_neq_atom =
      Foreign.foreign "yices_arith_neq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_geq_atom =
      Foreign.foreign "yices_arith_geq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_leq_atom =
      Foreign.foreign "yices_arith_leq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_gt_atom =
      Foreign.foreign "yices_arith_gt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_lt_atom =
      Foreign.foreign "yices_arith_lt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_arith_eq0_atom =
      Foreign.foreign "yices_arith_eq0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_arith_neq0_atom =
      Foreign.foreign "yices_arith_neq0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_arith_geq0_atom =
      Foreign.foreign "yices_arith_geq0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_arith_leq0_atom =
      Foreign.foreign "yices_arith_leq0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_arith_gt0_atom =
      Foreign.foreign "yices_arith_gt0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_arith_lt0_atom =
      Foreign.foreign "yices_arith_lt0_atom"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_bvconst_uint32 =
      Foreign.foreign "yices_bvconst_uint32"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_bvconst_uint64 =
      Foreign.foreign "yices_bvconst_uint64"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) uint64_t (Ctypes.returning term_t)))
    let yices_bvconst_int32 =
      Foreign.foreign "yices_bvconst_int32"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) int32_t (Ctypes.returning term_t)))
    let yices_bvconst_int64 =
      Foreign.foreign "yices_bvconst_int64"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) int64_t (Ctypes.returning term_t)))
[%%if gmp_present]
    let yices_bvconst_mpz =
      Foreign.foreign "yices_bvconst_mpz"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) MPZ.t_ptr (Ctypes.returning term_t)))
[%%endif]
    let yices_bvconst_zero =
      Foreign.foreign "yices_bvconst_zero"
        (Ctypes.(@->) uint32_t (Ctypes.returning term_t))
    let yices_bvconst_one =
      Foreign.foreign "yices_bvconst_one"
        (Ctypes.(@->) uint32_t (Ctypes.returning term_t))
    let yices_bvconst_minus_one =
      Foreign.foreign "yices_bvconst_minus_one"
        (Ctypes.(@->) uint32_t (Ctypes.returning term_t))
    let yices_bvconst_from_array =
      Foreign.foreign "yices_bvconst_from_array"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning term_t)))
    let yices_parse_bvbin =
      Foreign.foreign "yices_parse_bvbin"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_parse_bvhex =
      Foreign.foreign "yices_parse_bvhex"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_bvadd =
      Foreign.foreign "yices_bvadd"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsub =
      Foreign.foreign "yices_bvsub"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvneg =
      Foreign.foreign "yices_bvneg"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_bvmul =
      Foreign.foreign "yices_bvmul"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsquare =
      Foreign.foreign "yices_bvsquare"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_bvpower =
      Foreign.foreign "yices_bvpower"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_bvdiv =
      Foreign.foreign "yices_bvdiv"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvrem =
      Foreign.foreign "yices_bvrem"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsdiv =
      Foreign.foreign "yices_bvsdiv"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsrem =
      Foreign.foreign "yices_bvsrem"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsmod =
      Foreign.foreign "yices_bvsmod"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvnot =
      Foreign.foreign "yices_bvnot"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_bvnand =
      Foreign.foreign "yices_bvnand"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvnor =
      Foreign.foreign "yices_bvnor"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvxnor =
      Foreign.foreign "yices_bvxnor"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvshl =
      Foreign.foreign "yices_bvshl"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvlshr =
      Foreign.foreign "yices_bvlshr"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvashr =
      Foreign.foreign "yices_bvashr"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvand =
      Foreign.foreign "yices_bvand"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bvor =
      Foreign.foreign "yices_bvor"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bvxor =
      Foreign.foreign "yices_bvxor"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bvand2 =
      Foreign.foreign "yices_bvand2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvor2 =
      Foreign.foreign "yices_bvor2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvxor2 =
      Foreign.foreign "yices_bvxor2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvand3 =
      Foreign.foreign "yices_bvand3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_bvor3 =
      Foreign.foreign "yices_bvor3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_bvxor3 =
      Foreign.foreign "yices_bvxor3"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) term_t
              (Ctypes.(@->) term_t (Ctypes.returning term_t))))
    let yices_bvsum =
      Foreign.foreign "yices_bvsum"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bvproduct =
      Foreign.foreign "yices_bvproduct"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_shift_left0 =
      Foreign.foreign "yices_shift_left0"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_shift_left1 =
      Foreign.foreign "yices_shift_left1"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_shift_right0 =
      Foreign.foreign "yices_shift_right0"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_shift_right1 =
      Foreign.foreign "yices_shift_right1"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_ashift_right =
      Foreign.foreign "yices_ashift_right"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_rotate_left =
      Foreign.foreign "yices_rotate_left"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_rotate_right =
      Foreign.foreign "yices_rotate_right"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_bvextract =
      Foreign.foreign "yices_bvextract"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) uint32_t (Ctypes.returning term_t))))
    let yices_bvconcat2 =
      Foreign.foreign "yices_bvconcat2"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvconcat =
      Foreign.foreign "yices_bvconcat"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bvrepeat =
      Foreign.foreign "yices_bvrepeat"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_sign_extend =
      Foreign.foreign "yices_sign_extend"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_zero_extend =
      Foreign.foreign "yices_zero_extend"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_redand =
      Foreign.foreign "yices_redand"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_redor =
      Foreign.foreign "yices_redor"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_redcomp =
      Foreign.foreign "yices_redcomp"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvarray =
      Foreign.foreign "yices_bvarray"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning term_t)))
    let yices_bitextract =
      Foreign.foreign "yices_bitextract"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t (Ctypes.returning term_t)))
    let yices_bveq_atom =
      Foreign.foreign "yices_bveq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvneq_atom =
      Foreign.foreign "yices_bvneq_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvge_atom =
      Foreign.foreign "yices_bvge_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvgt_atom =
      Foreign.foreign "yices_bvgt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvle_atom =
      Foreign.foreign "yices_bvle_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvlt_atom =
      Foreign.foreign "yices_bvlt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsge_atom =
      Foreign.foreign "yices_bvsge_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsgt_atom =
      Foreign.foreign "yices_bvsgt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvsle_atom =
      Foreign.foreign "yices_bvsle_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_bvslt_atom =
      Foreign.foreign "yices_bvslt_atom"
        (Ctypes.(@->) term_t (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_parse_type =
      Foreign.foreign "yices_parse_type"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning type_t))
    let yices_parse_term =
      Foreign.foreign "yices_parse_term"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_subst_term =
      Foreign.foreign "yices_subst_term"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) term_t (Ctypes.returning term_t)))))
    let yices_subst_term_array =
      Foreign.foreign "yices_subst_term_array"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) (Ctypes.ptr term_t)
                       (Ctypes.returning int32_t))))))
    let yices_set_type_name =
      Foreign.foreign "yices_set_type_name"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning int32_t)))
    let yices_set_term_name =
      Foreign.foreign "yices_set_term_name"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning int32_t)))
    let yices_remove_type_name =
      Foreign.foreign "yices_remove_type_name"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning Ctypes.void))
    let yices_remove_term_name =
      Foreign.foreign "yices_remove_term_name"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning Ctypes.void))
    let yices_get_type_by_name =
      Foreign.foreign "yices_get_type_by_name"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning type_t))
    let yices_get_term_by_name =
      Foreign.foreign "yices_get_term_by_name"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning term_t))
    let yices_clear_type_name =
      Foreign.foreign "yices_clear_type_name"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_clear_term_name =
      Foreign.foreign "yices_clear_term_name"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_get_type_name =
      Foreign.foreign "yices_get_type_name"
        (Ctypes.(@->) type_t (Ctypes.returning (Ctypes.ptr Ctypes.char)))
    let yices_get_term_name =
      Foreign.foreign "yices_get_term_name"
        (Ctypes.(@->) term_t (Ctypes.returning (Ctypes.ptr Ctypes.char)))
    let yices_type_of_term =
      Foreign.foreign "yices_type_of_term"
        (Ctypes.(@->) term_t (Ctypes.returning type_t))
    let yices_term_is_bool =
      Foreign.foreign "yices_term_is_bool"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_int =
      Foreign.foreign "yices_term_is_int"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_real =
      Foreign.foreign "yices_term_is_real"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_arithmetic =
      Foreign.foreign "yices_term_is_arithmetic"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_bitvector =
      Foreign.foreign "yices_term_is_bitvector"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_tuple =
      Foreign.foreign "yices_term_is_tuple"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_function =
      Foreign.foreign "yices_term_is_function"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_scalar =
      Foreign.foreign "yices_term_is_scalar"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_bitsize =
      Foreign.foreign "yices_term_bitsize"
        (Ctypes.(@->) term_t (Ctypes.returning uint32_t))
    let yices_term_is_ground =
      Foreign.foreign "yices_term_is_ground"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_atomic =
      Foreign.foreign "yices_term_is_atomic"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_composite =
      Foreign.foreign "yices_term_is_composite"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_projection =
      Foreign.foreign "yices_term_is_projection"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_sum =
      Foreign.foreign "yices_term_is_sum"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_bvsum =
      Foreign.foreign "yices_term_is_bvsum"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_is_product =
      Foreign.foreign "yices_term_is_product"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_constructor =
      Foreign.foreign "yices_term_constructor"
        (Ctypes.(@->) term_t (Ctypes.returning term_constructor_t))
    let yices_term_num_children =
      Foreign.foreign "yices_term_num_children"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_term_child =
      Foreign.foreign "yices_term_child"
        (Ctypes.(@->) term_t (Ctypes.(@->) int32_t (Ctypes.returning term_t)))
    let yices_term_children =
      Foreign.foreign "yices_term_children"
        (Ctypes.(@->) term_t (Ctypes.(@->) (Ctypes.ptr term_vector_t) (Ctypes.returning int32_t)))
    let yices_proj_index =
      Foreign.foreign "yices_proj_index"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_proj_arg =
      Foreign.foreign "yices_proj_arg"
        (Ctypes.(@->) term_t (Ctypes.returning term_t))
    let yices_bool_const_value =
      Foreign.foreign "yices_bool_const_value"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t)))
    let yices_bv_const_value =
      Foreign.foreign "yices_bv_const_value"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t)))
    let yices_scalar_const_value =
      Foreign.foreign "yices_scalar_const_value"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t)))
[%%if gmp_present]
    let yices_rational_const_value =
      Foreign.foreign "yices_rational_const_value"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) MPQ.t_ptr (Ctypes.returning int32_t)))
    let yices_sum_component =
      Foreign.foreign "yices_sum_component"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) int32_t
              (Ctypes.(@->) MPQ.t_ptr
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning int32_t)))))
[%%endif]
    let yices_bvsum_component =
      Foreign.foreign "yices_bvsum_component"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) int32_t
              (Ctypes.(@->) (Ctypes.ptr int32_t)
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning int32_t)))))
    let yices_product_component =
      Foreign.foreign "yices_product_component"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) int32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) (Ctypes.ptr uint32_t)
                    (Ctypes.returning int32_t)))))
    let yices_num_terms =
      Foreign.foreign "yices_num_terms"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning uint32_t))
    let yices_num_types =
      Foreign.foreign "yices_num_types"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning uint32_t))
    let yices_incref_term =
      Foreign.foreign "yices_incref_term"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_decref_term =
      Foreign.foreign "yices_decref_term"
        (Ctypes.(@->) term_t (Ctypes.returning int32_t))
    let yices_incref_type =
      Foreign.foreign "yices_incref_type"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_decref_type =
      Foreign.foreign "yices_decref_type"
        (Ctypes.(@->) type_t (Ctypes.returning int32_t))
    let yices_num_posref_terms =
      Foreign.foreign "yices_num_posref_terms"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning uint32_t))
    let yices_num_posref_types =
      Foreign.foreign "yices_num_posref_types"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning uint32_t))
    let yices_garbage_collect =
      Foreign.foreign "yices_garbage_collect"
        (Ctypes.(@->) (Ctypes.ptr term_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr type_t)
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) int32_t (Ctypes.returning Ctypes.void))))))
    let yices_new_config =
      Foreign.foreign "yices_new_config"
        (Ctypes.(@->) Ctypes.void
           (Ctypes.returning (Ctypes.ptr ctx_config_t)))
    let yices_free_config =
      Foreign.foreign "yices_free_config"
        (Ctypes.(@->) (Ctypes.ptr ctx_config_t)
           (Ctypes.returning Ctypes.void))
    let yices_set_config =
      Foreign.foreign "yices_set_config"
        (Ctypes.(@->) (Ctypes.ptr ctx_config_t)
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
              (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                 (Ctypes.returning int32_t))))
    let yices_default_config_for_logic =
      Foreign.foreign "yices_default_config_for_logic"
        (Ctypes.(@->) (Ctypes.ptr ctx_config_t)
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning int32_t)))
    let yices_new_context =
      Foreign.foreign "yices_new_context"
        (Ctypes.(@->) (Ctypes.ptr ctx_config_t)
           (Ctypes.returning (Ctypes.ptr context_t)))
    let yices_free_context =
      Foreign.foreign "yices_free_context"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning Ctypes.void))
    let yices_context_status =
      Foreign.foreign "yices_context_status"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning smt_status_t))
    let yices_reset_context =
      Foreign.foreign "yices_reset_context"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning Ctypes.void))
    let yices_push =
      Foreign.foreign "yices_push"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning int32_t))
    let yices_pop =
      Foreign.foreign "yices_pop"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning int32_t))
    let yices_context_enable_option =
      Foreign.foreign "yices_context_enable_option"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning int32_t)))
    let yices_context_disable_option =
      Foreign.foreign "yices_context_disable_option"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char) (Ctypes.returning int32_t)))
    let yices_assert_formula =
      Foreign.foreign "yices_assert_formula"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) term_t (Ctypes.returning int32_t)))
    let yices_assert_formulas =
      Foreign.foreign "yices_assert_formulas"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning int32_t))))
    let yices_check_context =
      Foreign.foreign "yices_check_context"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr param_t) (Ctypes.returning smt_status_t)))
    let yices_check_context_with_assumptions =
      Foreign.foreign "yices_check_context_with_assumptions"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr param_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.returning smt_status_t)))))
    let yices_assert_blocking_clause =
      Foreign.foreign "yices_assert_blocking_clause"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning int32_t))
    let yices_stop_search =
      Foreign.foreign "yices_stop_search"
        (Ctypes.(@->) (Ctypes.ptr context_t) (Ctypes.returning Ctypes.void))
    let yices_new_param_record =
      Foreign.foreign "yices_new_param_record"
        (Ctypes.(@->) Ctypes.void (Ctypes.returning (Ctypes.ptr param_t)))
    let yices_default_params_for_context =
      Foreign.foreign "yices_default_params_for_context"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr param_t) (Ctypes.returning Ctypes.void)))
    let yices_set_param =
      Foreign.foreign "yices_set_param"
        (Ctypes.(@->) (Ctypes.ptr param_t)
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
              (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                 (Ctypes.returning int32_t))))
    let yices_free_param_record =
      Foreign.foreign "yices_free_param_record"
        (Ctypes.(@->) (Ctypes.ptr param_t) (Ctypes.returning Ctypes.void))
    let yices_get_unsat_core =
      Foreign.foreign "yices_get_unsat_core"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) (Ctypes.ptr term_vector_t)
              (Ctypes.returning int32_t)))
    let yices_get_model =
      Foreign.foreign "yices_get_model"
        (Ctypes.(@->) (Ctypes.ptr context_t)
           (Ctypes.(@->) int32_t (Ctypes.returning (Ctypes.ptr model_t))))
    let yices_free_model =
      Foreign.foreign "yices_free_model"
        (Ctypes.(@->) (Ctypes.ptr model_t) (Ctypes.returning Ctypes.void))
    let yices_model_from_map =
      Foreign.foreign "yices_model_from_map"
        (Ctypes.(@->) uint32_t
           (Ctypes.(@->) (Ctypes.ptr term_t)
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.returning (Ctypes.ptr model_t)))))
    let yices_model_collect_defined_terms =
      Foreign.foreign "yices_model_collect_defined_terms"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr term_vector_t)
              (Ctypes.returning Ctypes.void)))
    let yices_check_formula =
      Foreign.foreign "yices_check_formula"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
              (Ctypes.(@->) (Ctypes.ptr (Ctypes.ptr model_t))
                 (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                    (Ctypes.returning smt_status_t)))))
    let yices_check_formulas =
      Foreign.foreign "yices_check_formulas"
        (Ctypes.(@->) (Ctypes.ptr term_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                 (Ctypes.(@->) (Ctypes.ptr (Ctypes.ptr model_t))
                    (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                       (Ctypes.returning smt_status_t))))))
    let yices_has_delegate =
      Foreign.foreign "yices_has_delegate"
        (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
           (Ctypes.returning int32_t))
    let yices_export_formula_to_dimacs =
      Foreign.foreign "yices_export_formula_to_dimacs"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
              (Ctypes.(@->) int32_t
                 (Ctypes.(@->) (Ctypes.ptr smt_status_t)
                    (Ctypes.returning int32_t)))))
    let yices_export_formulas_to_dimacs =
      Foreign.foreign "yices_export_formulas_to_dimacs"
        (Ctypes.(@->) (Ctypes.ptr term_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr Ctypes.char)
                 (Ctypes.(@->) int32_t
                    (Ctypes.(@->) (Ctypes.ptr smt_status_t)
                       (Ctypes.returning int32_t))))))
    let yices_get_bool_value =
      Foreign.foreign "yices_get_bool_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_get_int32_value =
      Foreign.foreign "yices_get_int32_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_get_int64_value =
      Foreign.foreign "yices_get_int64_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int64_t) (Ctypes.returning int32_t))))
    let yices_get_rational32_value =
      Foreign.foreign "yices_get_rational32_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int32_t)
                 (Ctypes.(@->) (Ctypes.ptr uint32_t)
                    (Ctypes.returning int32_t)))))
    let yices_get_rational64_value =
      Foreign.foreign "yices_get_rational64_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int64_t)
                 (Ctypes.(@->) (Ctypes.ptr uint64_t)
                    (Ctypes.returning int32_t)))))
    let yices_get_double_value =
      Foreign.foreign "yices_get_double_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr Ctypes.double)
                 (Ctypes.returning int32_t))))
[%%if gmp_present]
    let yices_get_mpz_value =
      Foreign.foreign "yices_get_mpz_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) MPZ.t_ptr
                 (Ctypes.returning int32_t))))
    let yices_get_mpq_value =
      Foreign.foreign "yices_get_mpq_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) MPQ.t_ptr
                 (Ctypes.returning int32_t))))
[%%endif]
    let yices_get_bv_value =
      Foreign.foreign "yices_get_bv_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_get_scalar_value =
      Foreign.foreign "yices_get_scalar_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_init_yval_vector =
      Foreign.foreign "yices_init_yval_vector"
        (Ctypes.(@->) (Ctypes.ptr yval_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_delete_yval_vector =
      Foreign.foreign "yices_delete_yval_vector"
        (Ctypes.(@->) (Ctypes.ptr yval_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_reset_yval_vector =
      Foreign.foreign "yices_reset_yval_vector"
        (Ctypes.(@->) (Ctypes.ptr yval_vector_t)
           (Ctypes.returning Ctypes.void))
    let yices_get_value =
      Foreign.foreign "yices_get_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t))))
    let yices_val_is_int32 =
      Foreign.foreign "yices_val_is_int32"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))
    let yices_val_is_int64 =
      Foreign.foreign "yices_val_is_int64"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))
    let yices_val_is_rational32 =
      Foreign.foreign "yices_val_is_rational32"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))
    let yices_val_is_rational64 =
      Foreign.foreign "yices_val_is_rational64"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))
    let yices_val_is_integer =
      Foreign.foreign "yices_val_is_integer"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))
    let yices_val_bitsize =
      Foreign.foreign "yices_val_bitsize"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning uint32_t)))
    let yices_val_tuple_arity =
      Foreign.foreign "yices_val_tuple_arity"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning uint32_t)))
    let yices_val_mapping_arity =
      Foreign.foreign "yices_val_mapping_arity"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning uint32_t)))
    let yices_val_function_arity =
      Foreign.foreign "yices_val_function_arity"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning uint32_t)))
    let yices_val_function_type =
      Foreign.foreign "yices_val_function_type"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning type_t)))
    let yices_val_get_bool =
      Foreign.foreign "yices_val_get_bool"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_val_get_int32 =
      Foreign.foreign "yices_val_get_int32"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_val_get_int64 =
      Foreign.foreign "yices_val_get_int64"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int64_t) (Ctypes.returning int32_t))))
    let yices_val_get_rational32 =
      Foreign.foreign "yices_val_get_rational32"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int32_t)
                 (Ctypes.(@->) (Ctypes.ptr uint32_t)
                    (Ctypes.returning int32_t)))))
    let yices_val_get_rational64 =
      Foreign.foreign "yices_val_get_rational64"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int64_t)
                 (Ctypes.(@->) (Ctypes.ptr uint64_t)
                    (Ctypes.returning int32_t)))))
    let yices_val_get_double =
      Foreign.foreign "yices_val_get_double"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr Ctypes.double)
                 (Ctypes.returning int32_t))))
[%%if gmp_present]
    let yices_val_get_mpz =
      Foreign.foreign "yices_val_get_mpz"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) MPZ.t_ptr
                 (Ctypes.returning int32_t))))
    let yices_val_get_mpq =
      Foreign.foreign "yices_val_get_mpq"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) MPQ.t_ptr
                 (Ctypes.returning int32_t))))
[%%endif]
    let yices_val_get_bv =
      Foreign.foreign "yices_val_get_bv"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int32_t) (Ctypes.returning int32_t))))
    let yices_val_get_scalar =
      Foreign.foreign "yices_val_get_scalar"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr int32_t)
                 (Ctypes.(@->) (Ctypes.ptr type_t) (Ctypes.returning int32_t)))))
    let yices_val_expand_tuple =
      Foreign.foreign "yices_val_expand_tuple"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t))))
    let yices_val_expand_function =
      Foreign.foreign "yices_val_expand_function"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr yval_t)
                 (Ctypes.(@->) (Ctypes.ptr yval_vector_t)
                    (Ctypes.returning int32_t)))))
    let yices_val_expand_mapping =
      Foreign.foreign "yices_val_expand_mapping"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) (Ctypes.ptr yval_t)
              (Ctypes.(@->) (Ctypes.ptr yval_t)
                 (Ctypes.(@->) (Ctypes.ptr yval_t) (Ctypes.returning int32_t)))))
    let yices_formula_true_in_model =
      Foreign.foreign "yices_formula_true_in_model"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t (Ctypes.returning int32_t)))
    let yices_formulas_true_in_model =
      Foreign.foreign "yices_formulas_true_in_model"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning int32_t))))
    let yices_get_value_as_term =
      Foreign.foreign "yices_get_value_as_term"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t (Ctypes.returning term_t)))
    let yices_term_array_value =
      Foreign.foreign "yices_term_array_value"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) (Ctypes.ptr term_t) (Ctypes.returning int32_t)))))
    let yices_model_term_support =
      Foreign.foreign "yices_model_term_support"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                 (Ctypes.returning int32_t))))
    let yices_model_term_array_support =
      Foreign.foreign "yices_model_term_array_support"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                    (Ctypes.returning int32_t)))))
    let yices_implicant_for_formula =
      Foreign.foreign "yices_implicant_for_formula"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                 (Ctypes.returning int32_t))))
    let yices_implicant_for_formulas =
      Foreign.foreign "yices_implicant_for_formulas"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                    (Ctypes.returning int32_t)))))
    let yices_generalize_model =
      Foreign.foreign "yices_generalize_model"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.(@->) yices_gen_mode_t
                       (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                          (Ctypes.returning int32_t)))))))
    let yices_generalize_model_array =
      Foreign.foreign "yices_generalize_model_array"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) (Ctypes.ptr term_t)
                       (Ctypes.(@->) yices_gen_mode_t
                          (Ctypes.(@->) (Ctypes.ptr term_vector_t)
                             (Ctypes.returning int32_t))))))))
    let yices_pp_type =
      Foreign.foreign "yices_pp_type"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) type_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_pp_term =
      Foreign.foreign "yices_pp_term"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) term_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_pp_term_array =
      Foreign.foreign "yices_pp_term_array"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t
                       (Ctypes.(@->) uint32_t
                          (Ctypes.(@->) int32_t (Ctypes.returning int32_t))))))))
    let yices_print_model =
      Foreign.foreign "yices_print_model"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) (Ctypes.ptr model_t) (Ctypes.returning Ctypes.void)))
    let yices_pp_model =
      Foreign.foreign "yices_pp_model"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_print_term_values =
      Foreign.foreign "yices_print_term_values"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.returning int32_t)))))
    let yices_pp_term_values =
      Foreign.foreign "yices_pp_term_values"
        (Ctypes.(@->) (Ctypes.ptr _FILE)
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.(@->) uint32_t
                       (Ctypes.(@->) uint32_t
                          (Ctypes.(@->) uint32_t
                             (Ctypes.returning int32_t))))))))
    let yices_pp_type_fd =
      Foreign.foreign "yices_pp_type_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) type_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_pp_term_fd =
      Foreign.foreign "yices_pp_term_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) term_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_pp_term_array_fd =
      Foreign.foreign "yices_pp_term_array_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) (Ctypes.ptr term_t)
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t
                       (Ctypes.(@->) uint32_t
                          (Ctypes.(@->) int32_t (Ctypes.returning int32_t))))))))
    let yices_print_model_fd =
      Foreign.foreign "yices_print_model_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) (Ctypes.ptr model_t) (Ctypes.returning int32_t)))
    let yices_pp_model_fd =
      Foreign.foreign "yices_pp_model_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))
    let yices_print_term_values_fd =
      Foreign.foreign "yices_print_term_values_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.returning int32_t)))))
    let yices_pp_term_values_fd =
      Foreign.foreign "yices_print_term_values_fd"
        (Ctypes.(@->) Ctypes.sint
           (Ctypes.(@->) (Ctypes.ptr model_t)
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) (Ctypes.ptr term_t)
                    (Ctypes.(@->) uint32_t
                       (Ctypes.(@->) uint32_t
                          (Ctypes.(@->) uint32_t (Ctypes.returning int32_t))))))))
    let yices_type_to_string =
      Foreign.foreign "yices_type_to_string"
        (Ctypes.(@->) type_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.returning (Ctypes.ptr Ctypes.char))))))
    let yices_term_to_string =
      Foreign.foreign "yices_term_to_string"
        (Ctypes.(@->) term_t
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.returning (Ctypes.ptr Ctypes.char))))))
    let yices_model_to_string =
      Foreign.foreign "yices_model_to_string"
        (Ctypes.(@->) (Ctypes.ptr model_t)
           (Ctypes.(@->) uint32_t
              (Ctypes.(@->) uint32_t
                 (Ctypes.(@->) uint32_t
                    (Ctypes.returning (Ctypes.ptr Ctypes.char))))))
  end
include TMP
