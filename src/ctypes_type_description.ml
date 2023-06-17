open Ctypes

[%%import "gmp.mlh"]

module Types (F : TYPE) = struct
  open F

  let term_t = typedef sint "term_t"
  let type_t = typedef sint "type_t"

  let __IO_FILE_0 =
    let (_ctype : [ `__IO_FILE ] structure typ) =
      structure "_IO_FILE" in
    _ctype

  let context_s_0 =
    let (_ctype : [ `context_s ] structure typ) =
      structure "context_s" in
    _ctype
  let model_s_0 =
    let (_ctype : [ `model_s ] structure typ) =
      structure "model_s" in
    _ctype
  let ctx_config_s_0 =
    let (_ctype : [ `ctx_config_s ] structure typ) =
      structure "ctx_config_s" in
    _ctype
  let param_s_0 =
    let (_ctype : [ `param_s ] structure typ) =
      structure "param_s" in
    _ctype
  let term_vector_s_0 =
    let (_ctype : [ `term_vector_s ] structure typ) =
      structure "term_vector_s" in
    _ctype
  let type_vector_s_0 =
    let (_ctype : [ `type_vector_s ] structure typ) =
      structure "type_vector_s" in
    _ctype
  let yval_s_0 =
    let (_ctype : [ `yval_s ] structure typ) =
      structure "yval_s" in
    _ctype
  let yval_vector_s_0 =
    let (_ctype : [ `yval_vector_s ] structure typ) =
      structure "yval_vector_s" in
    _ctype
  let error_report_s_0 =
    let (_ctype : [ `error_report_s ] structure typ) =
      structure "error_report_s" in
    _ctype

  (* Hand-written *)
  let interpolation_context_s_0 =
    let (_ctype : [ `interpolation_context_s ] structure typ) =
      structure "interpolation_context_s" in
    _ctype


  let _FILE = typedef __IO_FILE_0 "FILE"    


  let context_s =
    object method ctype = context_s_0 method members = object  end end
  let context_t = typedef context_s_0 "context_t"

  let model_s =
    object method ctype = model_s_0 method members = object  end end
  let model_t = typedef model_s_0 "model_t"

  let ctx_config_s =
    object method ctype = ctx_config_s_0 method members = object  end end
  let ctx_config_t = typedef ctx_config_s_0 "ctx_config_t"

  let param_s =
    object method ctype = param_s_0 method members = object  end end
  let param_t = typedef param_s_0 "param_t"

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
      method ctype = uint
      method to_int = to_int
      method of_int = of_int
    end
  let smt_status_t = typedef smt_status#ctype "smt_status_t"

  let term_vector_s =
    let field_0 = field term_vector_s_0 "capacity" uint in
    let field_1 = field term_vector_s_0 "size" uint in
    let field_2 = field term_vector_s_0 "data" (ptr term_t) in
    let () = seal term_vector_s_0 in
    object
      method ctype = term_vector_s_0
      method members =
        object
          method capacity = field_0
          method size = field_1
          method data = field_2
        end
    end
  let term_vector_t = typedef term_vector_s_0 "term_vector_t"

  let type_vector_s =
    let field_0 = field type_vector_s_0 "capacity" uint in
    let field_1 = field type_vector_s_0 "size" uint in
    let field_2 = field type_vector_s_0 "data" (ptr type_t) in
    let () = seal type_vector_s_0 in
    object
      method ctype = type_vector_s_0
      method members =
        object
          method capacity = field_0
          method size = field_1
          method data = field_2
        end
    end
  let type_vector_t = typedef type_vector_s_0 "type_vector_t"

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
      method ctype = sint
      method to_int = to_int
      method of_int = of_int
    end
  let term_constructor_t =
    typedef term_constructor#ctype "term_constructor_t"

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
      method ctype = uint
      method to_int = to_int
      method of_int = of_int
    end
  let yval_tag_t = typedef yval_tag#ctype "yval_tag_t"

  let yval_s =
    let field_0 = field yval_s_0 "node_id" sint in
    let field_1 = field yval_s_0 "node_tag" yval_tag_t in
    let () = seal yval_s_0 in
    object
      method ctype = yval_s_0
      method members =
        object method node_id = field_0 method node_tag = field_1 end
    end
  let yval_t = typedef yval_s_0 "yval_t"

  let yval_vector_s =
    let field_0 = field yval_vector_s_0 "capacity" uint in
    let field_1 = field yval_vector_s_0 "size" uint in
    let field_2 = field yval_vector_s_0 "data" (ptr yval_t) in
    let () = seal yval_vector_s_0 in
    object
      method ctype = yval_vector_s_0
      method members =
        object
          method capacity = field_0
          method size = field_1
          method data = field_2
        end
    end
  let yval_vector_t = typedef yval_vector_s_0 "yval_vector_t"

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
      method ctype = uint
      method to_int = to_int
      method of_int = of_int
    end
  let yices_gen_mode_t =
    typedef yices_gen_mode#ctype "yices_gen_mode_t"

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
        | `CTX_HIGH_ORDER_FUN_NOT_SUPPORTED -> 321L
        | `CTX_INVALID_OPERATION -> 400L
        | `CTX_OPERATION_NOT_SUPPORTED -> 401L
        | `CTX_UNKNOWN_DELEGATE -> 420L
        | `CTX_DELEGATE_NOT_AVAILABLE -> 421L
        | `CTX_EF_ASSERTIONS_CONTAIN_UF -> 440L 
        | `CTX_EF_NOT_EXISTS_FORALL -> 441L
        | `CTX_EF_HIGH_ORDER_VARS -> 442L
        | `CTX_EF_INTERNAL_ERROR -> 443L
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
        | `MDL_NONNEG_INT_REQUIRED -> 705L
        | `YVAL_INVALID_OP -> 800L
        | `YVAL_OVERFLOW -> 801L
        | `YVAL_NOT_SUPPORTED -> 802L
        | `MDL_GEN_TYPE_NOT_SUPPORTED -> 900L
        | `MDL_GEN_NONLINEAR -> 901L
        | `MDL_GEN_FAILED -> 902L
        | `MDL_GEN_UNSUPPORTED_TERM -> 903L
        | `MCSAT_ERROR_UNSUPPORTED_THEORY -> 1000L
        | `MCSAT_ERROR_ASSUMPTION_TERM_NOT_SUPPORTED -> 1001L
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
        | 321L -> `CTX_HIGH_ORDER_FUN_NOT_SUPPORTED
        | 400L -> `CTX_INVALID_OPERATION
        | 401L -> `CTX_OPERATION_NOT_SUPPORTED
        | 420L -> `CTX_UNKNOWN_DELEGATE 
        | 421L -> `CTX_DELEGATE_NOT_AVAILABLE
        | 440L -> `CTX_EF_ASSERTIONS_CONTAIN_UF
        | 441L -> `CTX_EF_NOT_EXISTS_FORALL
        | 442L -> `CTX_EF_HIGH_ORDER_VARS
        | 443L -> `CTX_EF_INTERNAL_ERROR
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
        | 705L -> `MDL_NONNEG_INT_REQUIRED
        | 800L -> `YVAL_INVALID_OP
        | 801L -> `YVAL_OVERFLOW
        | 802L -> `YVAL_NOT_SUPPORTED
        | 900L -> `MDL_GEN_TYPE_NOT_SUPPORTED
        | 901L -> `MDL_GEN_NONLINEAR
        | 902L -> `MDL_GEN_FAILED
        | 903L -> `MDL_GEN_UNSUPPORTED_TERM
        | 1000L -> `MCSAT_ERROR_UNSUPPORTED_THEORY
        | 9000L -> `OUTPUT_ERROR
        | 9999L -> `INTERNAL_EXCEPTION
        | _ -> failwith "enum to_int")) in
    object
      method ctype = uint
      method to_int = to_int
      method of_int = of_int
    end
  let error_code_t = typedef error_code#ctype "error_code_t"

  let error_report_s =
    let field_0 = field error_report_s_0 "code" error_code_t in
    let field_1 = field error_report_s_0 "line" uint in
    let field_2 = field error_report_s_0 "column" uint in
    let field_3 = field error_report_s_0 "term1" term_t in
    let field_4 = field error_report_s_0 "type1" type_t in
    let field_5 = field error_report_s_0 "term2" term_t in
    let field_6 = field error_report_s_0 "type2" type_t in
    let field_7 = field error_report_s_0 "badval" long in
    let () = seal error_report_s_0 in
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
  let error_report_t = typedef error_report_s_0 "error_report_t"

  (* Hand-written *)
  let interpolation_context_s =
    let field_0 = field interpolation_context_s_0 "ctx_A" (ptr context_t) in
    let field_1 = field interpolation_context_s_0 "ctx_B" (ptr context_t) in
    let field_2 = field interpolation_context_s_0 "interpolant" term_t in
    let field_3 = field interpolation_context_s_0 "model" (ptr model_t) in
    let () = seal interpolation_context_s_0 in
    object
      method ctype = interpolation_context_s_0
      method members =
        object
          method ctx_A = field_0
          method ctx_B = field_1
          method interpolant = field_2
          method model = field_3
        end
    end
  let interpolation_context_t =
    typedef interpolation_context_s_0 "interpolation_context_t"

end
