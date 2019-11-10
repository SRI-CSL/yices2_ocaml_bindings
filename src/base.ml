open Ctypes

module Types = struct

  type context_t      = [ `context_s ] structure
  type model_t        = [ `model_s ] structure
  type ctx_config_t   = [ `ctx_config_s ] structure
  type param_t        = [ `param_s ] structure
  type term_vector_t  = [ `term_vector_s ] structure
  type type_vector_t  = [ `type_vector_s ] structure
  type yval_t         = [ `yval_s ] structure
  type yval_vector_t  = [ `yval_vector_s ] structure
  type error_report_t = [ `error_report_s ] structure

  type smt_status = 
    [ `STATUS_ERROR
    | `STATUS_IDLE
    | `STATUS_INTERRUPTED
    | `STATUS_SAT
    | `STATUS_SEARCHING
    | `STATUS_UNKNOWN
    | `STATUS_UNSAT ]

  type term_constructor =
    [ `YICES_ABS
    | `YICES_APP_TERM
    | `YICES_ARITH_CONSTANT
    | `YICES_ARITH_GE_ATOM
    | `YICES_ARITH_ROOT_ATOM
    | `YICES_ARITH_SUM
    | `YICES_BIT_TERM
    | `YICES_BOOL_CONSTANT
    | `YICES_BV_ARRAY
    | `YICES_BV_ASHR
    | `YICES_BV_CONSTANT
    | `YICES_BV_DIV
    | `YICES_BV_GE_ATOM
    | `YICES_BV_LSHR
    | `YICES_BV_REM
    | `YICES_BV_SDIV
    | `YICES_BV_SGE_ATOM
    | `YICES_BV_SHL
    | `YICES_BV_SMOD
    | `YICES_BV_SREM
    | `YICES_BV_SUM
    | `YICES_CEIL
    | `YICES_CONSTRUCTOR_ERROR
    | `YICES_DISTINCT_TERM
    | `YICES_DIVIDES_ATOM
    | `YICES_EQ_TERM
    | `YICES_FLOOR
    | `YICES_FORALL_TERM
    | `YICES_IDIV
    | `YICES_IMOD
    | `YICES_IS_INT_ATOM
    | `YICES_ITE_TERM
    | `YICES_LAMBDA_TERM
    | `YICES_NOT_TERM
    | `YICES_OR_TERM
    | `YICES_POWER_PRODUCT
    | `YICES_RDIV
    | `YICES_SCALAR_CONSTANT
    | `YICES_SELECT_TERM
    | `YICES_TUPLE_TERM
    | `YICES_UNINTERPRETED_TERM
    | `YICES_UPDATE_TERM
    | `YICES_VARIABLE
    | `YICES_XOR_TERM ]

  type yval_tag =
    [ `YVAL_ALGEBRAIC
    | `YVAL_BOOL
    | `YVAL_BV
    | `YVAL_FUNCTION
    | `YVAL_MAPPING
    | `YVAL_RATIONAL
    | `YVAL_SCALAR
    | `YVAL_TUPLE
    | `YVAL_UNKNOWN ]

  type yices_gen_mode =
    [ `YICES_GEN_BY_PROJ
    | `YICES_GEN_BY_SUBST
    | `YICES_GEN_DEFAULT ]

  type error_code = 
    [ `ARITHCONSTANT_REQUIRED
    | `ARITHTERM_REQUIRED
    | `ARITH_ERROR
    | `BAD_TERM_DECREF
    | `BAD_TYPE_DECREF
    | `BITVECTOR_REQUIRED
    | `BVARITH_ERROR
    | `BVTYPE_REQUIRED
    | `CTX_ARITH_NOT_SUPPORTED
    | `CTX_ARITH_SOLVER_EXCEPTION
    | `CTX_ARRAYS_NOT_SUPPORTED
    | `CTX_ARRAY_SOLVER_EXCEPTION
    | `CTX_BV_NOT_SUPPORTED
    | `CTX_BV_SOLVER_EXCEPTION
    | `CTX_FORMULA_NOT_IDL
    | `CTX_FORMULA_NOT_RDL
    | `CTX_FREE_VAR_IN_FORMULA
    | `CTX_INVALID_CONFIG
    | `CTX_INVALID_OPERATION
    | `CTX_INVALID_PARAMETER_VALUE
    | `CTX_LAMBDAS_NOT_SUPPORTED
    | `CTX_LOGIC_NOT_SUPPORTED
    | `CTX_NONLINEAR_ARITH_NOT_SUPPORTED
    | `CTX_OPERATION_NOT_SUPPORTED
    | `CTX_QUANTIFIERS_NOT_SUPPORTED
    | `CTX_SCALAR_NOT_SUPPORTED
    | `CTX_TOO_MANY_ARITH_ATOMS
    | `CTX_TOO_MANY_ARITH_VARS
    | `CTX_TOO_MANY_BV_ATOMS
    | `CTX_TOO_MANY_BV_VARS
    | `CTX_TUPLE_NOT_SUPPORTED
    | `CTX_UF_NOT_SUPPORTED
    | `CTX_UNKNOWN_LOGIC
    | `CTX_UNKNOWN_PARAMETER
    | `CTX_UTYPE_NOT_SUPPORTED
    | `DEGREE_OVERFLOW
    | `DIVISION_BY_ZERO
    | `DUPLICATE_NAME_IN_SCALAR
    | `DUPLICATE_TYPE_VAR
    | `DUPLICATE_VARIABLE
    | `DUPLICATE_VAR_NAME
    | `EMPTY_BITVECTOR
    | `EVAL_CONVERSION_FAILED
    | `EVAL_FAILED
    | `EVAL_FREEVAR_IN_TERM
    | `EVAL_LAMBDA
    | `EVAL_NOT_SUPPORTED
    | `EVAL_NO_IMPLICANT
    | `EVAL_OVERFLOW
    | `EVAL_QUANTIFIER
    | `EVAL_UNKNOWN_TERM
    | `FUNCTION_REQUIRED
    | `INCOMPATIBLE_BVSIZES
    | `INCOMPATIBLE_TYPES
    | `INTEGER_OVERFLOW
    | `INTEGER_REQUIRED
    | `INTERNAL_EXCEPTION
    | `INVALID_BITEXTRACT
    | `INVALID_BITSHIFT
    | `INVALID_BVBIN_FORMAT
    | `INVALID_BVCONSTANT
    | `INVALID_BVEXTRACT
    | `INVALID_BVHEX_FORMAT
    | `INVALID_CONSTANT_INDEX
    | `INVALID_FLOAT_FORMAT
    | `INVALID_MACRO
    | `INVALID_RATIONAL_FORMAT
    | `INVALID_TERM
    | `INVALID_TERM_OP
    | `INVALID_TOKEN
    | `INVALID_TUPLE_INDEX
    | `INVALID_TYPE
    | `INVALID_TYPE_OP
    | `INVALID_VAR_INDEX
    | `MAX_BVSIZE_EXCEEDED
    | `MCSAT_ERROR_UNSUPPORTED_THEORY
    | `MDL_CONSTANT_REQUIRED
    | `MDL_CONSTRUCTION_FAILED
    | `MDL_DUPLICATE_VAR
    | `MDL_FTYPE_NOT_ALLOWED
    | `MDL_GEN_FAILED
    | `MDL_GEN_NONLINEAR
    | `MDL_GEN_TYPE_NOT_SUPPORTED
    | `MDL_UNINT_REQUIRED
    | `NEGATIVE_BVSIZE
    | `NONNEG_INT_REQUIRED
    | `NON_CONSTANT_DIVISOR
    | `NO_ERROR
    | `OUTPUT_ERROR
    | `POS_INT_REQUIRED
    | `RATIONAL_REQUIRED
    | `REDEFINED_TERM_NAME
    | `REDEFINED_TYPE_NAME
    | `SCALAR_OR_UTYPE_REQUIRED
    | `SCALAR_TERM_REQUIRED
    | `SYMBOL_REQUIRED
    | `SYNTAX_ERROR
    | `TOO_MANY_ARGUMENTS
    | `TOO_MANY_MACRO_PARAMS
    | `TOO_MANY_VARS
    | `TUPLE_REQUIRED
    | `TYPE_MISMATCH
    | `TYPE_MISMATCH_IN_DEF
    | `TYPE_REQUIRED
    | `TYPE_VAR_REQUIRED
    | `UNDEFINED_TERM_NAME
    | `UNDEFINED_TYPE_NAME
    | `VARIABLE_REQUIRED
    | `WRONG_NUMBER_OF_ARGUMENTS
    | `YVAL_INVALID_OP
    | `YVAL_NOT_SUPPORTED
    | `YVAL_OVERFLOW ]

  type ('a,'b) converter = { read  : 'a -> 'b;
                             write : 'b -> 'a }

end

module type Types = sig

  (* Type of things that can be checked for error *)
  type _ checkable
  val check : _ checkable -> bool (* The said checking operation *)
  
  (* Opaque C types, only accessible through the API functions *)
  type unit_t = [`unit_t] checkable
  type bool_t = [`bool_t] checkable
  type term_t = [`term_t] checkable
  type type_t = [`type_t] checkable
  val term_t : term_t typ
  val type_t : type_t typ

  val null_term : term_t
  val null_type : type_t

  (* C's enums *)
  type smt_status_t
  type term_constructor_t = [`term_constructor_t] checkable
  type yval_tag_t
  type yices_gen_mode_t
  type error_code_t

  val smt_status_t       : smt_status_t       typ
  val term_constructor_t : term_constructor_t typ
  val yval_tag_t         : yval_tag_t         typ
  val yices_gen_mode_t   : yices_gen_mode_t   typ
  val error_code_t       : error_code_t       typ

  include module type of Types

  (* OCaml's enums, as views of the C's enums;
     ocaml types have been introduced in Yices_types *)
  val unit             : (unit_t            , unit)             converter
  val bool             : (bool_t            , bool)             converter
  val smt_status       : (smt_status_t      , smt_status)       converter
  val term_constructor : (term_constructor_t, term_constructor) converter
  val yval_tag         : (yval_tag_t        , yval_tag)         converter
  val yices_gen_mode   : (yices_gen_mode_t  , yices_gen_mode)   converter
  val error_code       : (error_code_t      , error_code)       converter

  (* Opaque C structure types with no visible member,
     only accessible through the API functions;
     types have been introduced in Yices_types *)
  val context_t    : context_t    typ
  val model_t      : model_t      typ
  val ctx_config_t : ctx_config_t typ
  val param_t      : param_t      typ

  (* C structure types with some visible members;
     types have been introduced in Yices_types *)

  (* Term vectors and Type vectors *)
  val term_vector_s :
    < ctype : term_vector_t typ;
      members : < capacity : (Unsigned.uint, term_vector_t)
                             field;
                  data : (term_t ptr, term_vector_t)
                         field;
                  size : (Unsigned.uint, term_vector_t) field > >
  val term_vector_t : term_vector_t typ
  val type_vector_s :
    < ctype : type_vector_t typ;
      members : < capacity : (Unsigned.uint, type_vector_t)
                             field;
                  data : (type_t ptr, type_vector_t)
                         field;
                  size : (Unsigned.uint, type_vector_t) field > >
  val type_vector_t : type_vector_t typ

  val yval_tag_t : yval_tag_t typ
  val yval_s :
    < ctype : yval_t typ;
      members : < node_id : (Signed.sint, yval_t) field;
                  node_tag : (yval_tag_t, yval_t) field > >
  val yval_t : yval_t typ
  val yval_vector_s :
    < ctype : yval_vector_t typ;
      members : < capacity : (Unsigned.uint, yval_vector_t)
                             field;
                  data : (yval_t ptr, yval_vector_t)
                         field;
                  size : (Unsigned.uint, yval_vector_t) field > >
  val yval_vector_t : yval_vector_t typ
  val error_report_s :
    < ctype : error_report_t typ;
      members : < badval : (Signed.long, error_report_t) field;
                  code : (Unsigned.uint, error_report_t) field;
                  column : (Unsigned.uint, error_report_t)
                           field;
                  line : (Unsigned.uint, error_report_t) field;
                  term1 : (term_t, error_report_t) field;
                  term2 : (term_t, error_report_t) field;
                  type1 : (type_t, error_report_t) field;
                  type2 : (type_t, error_report_t) field > >
  val error_report_t : error_report_t typ

end
