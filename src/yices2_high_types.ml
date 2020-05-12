open Ctypes
open Unsigned
open Signed

module Types = struct

  include Yices2_low.Types

  type scalar = type_t [@@deriving eq]
  type uninterpreted = type_t [@@deriving eq]

  type ytype =
    | Bool
    | Int
    | Real
    | BV of int
    | Scalar of scalar
    | Uninterpreted of uninterpreted
    | Tuple of type_t list
    | Fun of { dom : type_t list; codom : type_t } [@@deriving eq]

  type 'a composite = private Composite

  (** Internal term structure:

      - atomic terms are Boolean, bitvector, arithmetic constants,
        and variables and uninterpreted terms (i.e., terms that
        don't have subterms)

      - composite terms are of the form
        (constructor, number-of-children, list-of-children)

      - projection terms are of the form
        (constructor, index, child)

      - a sum is a term of the form
          a_0 t_0 + ... + a_n t_n
        where a_0 ... a_n are rational coefficients (constant)
        and t_0 ... t_n are arithmetic terms

      - a bitvector sum is a sum
          a_0 t_0 + ... + a_n t_n
        where the coefficients a_0 ... a_n are bitvector constants
        and t_0 ... t_n are bitvector terms

      - a product is a term of the form t_0^d_0 x ... x t_n ^d_n
        where d_0 ... d_n are positive exponents,
        and t_0 ... t_n are either all arithmetic terms or all
        bitvector terms

      - the number of terms in a sum, bitvector sum, or product
        is always positive, but it may be equal to 1. For
        example, the expression (- x) is internally represented
        as a sum with one monomial (-1 * x).


      Composite terms:

       if-then-else: (if c t1 t2)
      - three children
      - the first child is the condition c
      - the second child is the 'then' part t1
      - the third child is the 'else' part  t2

       function application:  (apply f t1 .. t_n)
      - n+1 children if f has arity n
      - the first child is the function f
      - the other children are the arguments t_1 ... t_n

       function update: (update f t1 .. t_n v)
      - n+2 children if f has arity n
      - the first child is the function f
      - the next n children = arguments
      - last children = new value

       tuple: (tuple t1 ... t_n)

       equality: (eq t1 t2)

       distinct: (distinct t1 ... t_n)

       forall: (forall x_1 ... x_n p)
      - the variables are the n first children
      - the body p is the last child

       lambda: (lambda x_1 ... x_n t)
      - the variables are the n first children
      - the body t is the last child

       negation: (not t)

       disjunction: (or t1 ... t_n)

       exclusive or: (xor t1 ... t_n)

       bit array: (bv-array t1 ... t_n)
      - each t_i is a Boolean term
      - this uses little-endian form: the first child is the
         low-order bit, the last child is the high-order bit.

       bitvector operators:
         (bvdiv  t1 t2)    unsigned division
         (bvrem  t1 t2)    unsigned remainder
         (bvsdiv t1 t2)    signed division
         (bvsrem t1 t2)    signed remainder (rounding to 0)
         (bvsmod t1 t2)    signed remainder (rounding to -infinity)
         (bvshl  t1 t2)    shift left
         (bvlshr t1 t2)    logical shift right
         (bvashr t1 t2)    arithmetic shift right
         (bvge   t1 t2)    unsigned comparison: (t1 >= t2)
         (bvsge  t1 t2)    signed comparison: (t1 >= t2)

       arithmetic atom:
         (ge t1 t2)   t1 >= t2


      Projection terms:

       tuple projection:  (select i t)
      - the child t is a tuple term (of type tau_1 x ... x tau_n)
      - i is an index between 1 and n

       bit selection:  (bit i t)
      - the child t is a bitvector term of n bits
      - i is an index between 0 and n-1  *)
  type 'a termstruct =
    | A0 : [ `YICES_BOOL_CONSTANT
           | `YICES_ARITH_CONSTANT
           | `YICES_BV_CONSTANT
           | `YICES_SCALAR_CONSTANT
           | `YICES_VARIABLE
           | `YICES_UNINTERPRETED_TERM ]
           * term_t                        -> [`a0] termstruct
    | A1 : [ `YICES_NOT_TERM
           | `YICES_ABS
           | `YICES_CEIL 
           | `YICES_FLOOR 
           | `YICES_ARITH_ROOT_ATOM 
           | `YICES_IS_INT_ATOM ] * term_t -> [`a1] composite termstruct
    | A2 : [ `YICES_EQ_TERM 
           | `YICES_BV_ASHR 
           | `YICES_BV_DIV 
           | `YICES_BV_GE_ATOM 
           | `YICES_BV_LSHR 
           | `YICES_BV_REM 
           | `YICES_BV_SDIV 
           | `YICES_BV_SGE_ATOM 
           | `YICES_BV_SHL 
           | `YICES_BV_SMOD 
           | `YICES_BV_SREM
           | `YICES_ARITH_GE_ATOM
           | `YICES_DIVIDES_ATOM 
           | `YICES_IDIV 
           | `YICES_IMOD 
           | `YICES_RDIV ]
           * term_t * term_t         -> [`a2] composite termstruct
    | ITE : term_t * term_t * term_t -> [`a3] composite termstruct
    | Astar : [ `YICES_TUPLE_TERM
              | `YICES_DISTINCT_TERM 
              | `YICES_OR_TERM 
              | `YICES_XOR_TERM
              | `YICES_BV_ARRAY ] * term_t list -> [`astar] composite termstruct
    | Bindings : { c    : [ `YICES_FORALL_TERM | `YICES_LAMBDA_TERM ];
                   vars : term_t list;
                   body : term_t }              -> [`bindings] composite termstruct
    | App    : term_t * term_t list             -> [`app]      composite termstruct
    | Update : { array : term_t; index : term_t list; value : term_t}      -> [`update] composite termstruct
    | Projection : [ `YICES_SELECT_TERM | `YICES_BIT_TERM ] * int * term_t -> [`projection] termstruct
    | BV_Sum    : (sint * (term_t option)) list -> [`bvsum] termstruct
    | Sum       : (sint * (term_t option)) list -> [`sum]   termstruct
    | Product   : (term_t * int) list           -> [`prod]  termstruct

  type yterm = Term : _ termstruct -> yterm [@@unboxed]

    (** Pretty printing uses a rectangular display area, characterized
        by its width, height, and offset as follows.

                        <----------- width ------------->
                         _______________________________
       <---- offset --->|                               |   ^
                        |                               |   |
                        |                               | Height
                        |                               |   |
                        |                               |   v
                         -------------------------------
        *)
  type display = {
    width:int;
    height:int;
    offset:int
  }  

  type error_report = {
    badval : int;
    code   : error_code;
    column : int;
    line  : int;
    term1 : term_t;
    term2 : term_t;
    type1 : type_t;
    type2 : type_t;
  }
end

module type High = sig

  open Types

  type 'a eh

  module type Names = sig

    (** *********
        NAMES   *
     ******** *)

    (** It's possible to assign names to terms and types, and later
        retrieve the term or type from these names.

        For each term and type, Yices stores a base name that's
        used for pretty printing. By default, the base name is NULL.
        The base name is set on the first call to yices_set_term_name or
        yices_set_type_name.

        In addition, Yices stores two symbol tables that maps names to
        terms and types, respectively. The name spaces for types and terms
        are disjoint. The term or type that a name refers to can be changed,
        and Yices provides a scoping mechanism:
        - when function  yices_set_term_name(t, name) is called,
          then the previous mapping for 'name' (if any) is hidden and now
          'name' refers to term 't'.
        - if function yices_remove_term_name(name) is called, then the current
          mapping for 'name' is removed and the previous mapping (if any)
          is restored.  *)

    type t

    (** The following functions attach a name to a type or a term
        - name  must be a '\0'-terminated string
        - if tau or t does not have a base name yet, then name is stored
          as base name for tau or t.
        - if name referred to another term or another type, then this
          previous mapping is hidden

        The functions return -1 and set the error report if the term or
        type is invalid. Otherwise they return 0.

        A copy of string name is made internally.  *)
    val set : t -> string -> unit eh

    (** Remove the current mapping of name
        - no effect if name is not assigned to a term or type
        - if name is assigned to some term t or type tau, then this current
          mapping is removed. If name was previously mapped to another term
          or type, then the previous mapping is restored.  *)
    val remove : string -> unit

    (** Get type or term of the given name
        - return NULL_TYPE or NULL_TERM if there's no type or term with that name  *)
    val of_name : string -> t eh

    (** Remove the base name of a type tau or of a term t.

        The functions return -1 and set the error report if the
        type or term is invalid. Otherwise, they return 0.

        If tau or t doesn't have a base name, the functions do
        nothing and return 0.

        Otherwise, the mapping from the base_name to tau or t is removed
        from the symbol table for terms or types, and the base_name of
        tau or t is set to NULL (i.e., tau or t don't have a base name anymore).  *)
    val clear : t -> unit eh

    (** Get the base name of a term or type

        The functions return NULL if the  term or type has no name,
        or if the term or type is not valid. The error report is set
        to INVALID_TERM or INVALID_TYPE in such cases.  *)
    val to_name : t -> string eh

  end


  module Global : sig
    (** The version as a string "x.y.z"  *)
    val version : string eh

    (** More details about the release:
        - build_arch is a string like "x86_64-unknown-linux-gnu"
        - build_mode is "release" or "debug"
        - build_date is the compilation date as in "2014-01-27"  *)

    val build_arch : string eh
    val build_mode : string eh
    val build_date : string eh

    (** Check whether the library was compiled with MCSAT support (i.e.,
        it can deal with nonlinear arithmetic).  *)
    val has_mcsat : unit -> bool eh

    (** Check whether the library was compiled in THREAD_SAFE mode.  *)
    val is_thread_safe : unit -> bool eh

    (** ************************************
        GLOBAL INITIALIZATION AND CLEANUP  *
     *********************************** *)

    (** This function must be called before anything else to initialize
        internal data structures.  *)
    val init : unit -> unit

    (** Delete all internal data structures and objects
        - this must be called to avoid memory leaks  *)
    val exit : unit -> unit

    (** Full reset
        - delete all terms and types and reset the symbol tables
        - delete all contexts, models, configuration descriptors and
          parameter records.  *)
    val reset : unit -> unit

    (** ************************
        OUT-OF-MEMORY CALLBACK  *
     *********************** *)

    (** By default, when Yices runs out of memory, it
        first prints an error message on stderr; then it calls
        exit(YICES_EXIT_OUT_OF_MEMORY).  This kills the whole process.

        The following function allows you to register a callback to invoke
        if Yices runs out of memory.  The callback function takes no
        arguments and returns nothing.

        Installing an out-of-memory callback allows you to do something a
        bit less brutal than killing the process. If there's a callback,
        yices will call it first when it runs out of memory.  The callback
        function should not return. If it does, yices will call exit as
        previously.

        In other words, the code that handles out-of-memory is as follows: *)
     (*
         if (callback != NULL) {
           callback();
         } else {
           fprintf(stderr, ...);
         }
         exit(YICES_EXIT_OUT_OF_MEMORY); *)

    (**
       IMPORTANT
       ---------
       After Yices runs out of memory, its internal data structures may be
       left in an inconsistent/corrupted state. The API is effectively
       unusable at this point and nothing can be done to recover cleanly.
       Evan a call to yices_exit() may cause a seg fault. The callback
       should not try to cleanup anything, or call any function from the API.

       A plausible use of this callback feature is to implement an
       exception mechanism using setjmp/longjmp.  *)
    val set_out_of_mem_callback : (unit -> unit) static_funptr -> unit

  end

  (** ************************
   *  ERROR REPORTING PRINTS *
   ************************ *)

  module ErrorPrint : sig

    (** Print an error message on stream f.  This converts the current error
        code + error report structure into an error message.
        - f must be a non-NULL open stream (writable)

        Return -1 if there's an error while writing to f (as reported by fprintf).
        Return 0 otherwise.

        If there's an error, errno, perror, and friends can be used for diagnostic.  *)
    val print : FILE.t ptr -> unit_t eh

    (** Print an error message on file descriptor fd.  This converts the current error
        code + error report structure into an error message.
        - fd must be an open file descriptor (writable)

        Return -1 if there's an error while writing to fd.
        Return 0 otherwise.

        If there's an error, errno, perror, and friends can be used for diagnostic.  *)
    val print_fd : sint -> unit_t eh

    (** Build a string from the current error code + error report structure.  *)
    val string : unit -> string eh

  end

  module Type : sig

    type t = type_t [@@deriving eq]

    (** ********************
        TYPE CONSTRUCTORS  *
     ******************* *)

    (** All constructors return NULL_TYPE (-1) if the type definition is wrong.  *)

    (** Built-in types bool, int, real.  *)

    val bool : unit -> type_t eh
    val int  : unit -> type_t eh
    val real : unit -> type_t eh

    (** Bitvectors of given size (number of bits)
        Requires size > 0

        If size = 0, the error report is set
         code = POS_INT_REQUIRED
         badval = size
        If size > YICES_MAX_BVSIZE
         code = MAX_BVSIZE_EXCEEDED
         badval = size  *)
    val bv : int -> type_t eh

    (** New scalar type of given cardinality.
        Requires card > 0

        If card = 0, set error report to
         code = POS_INT_REQUIRED
         badval = size  *)
    val new_scalar : card:int -> type_t eh

    (** New uninterpreted type. No error report.  *)
    val new_uninterpreted : unit -> type_t eh

    (** Tuple type tau[0] x ... x tau[n-1].
        Requires n>0 and tau[0] ... tau[n-1] to be well defined types.

        Error report
        if n == 0,
         code = POS_INT_REQUIRED
         badval = n
        if n > YICES_MAX_ARITY
         code = TOO_MANY_ARGUMENTS
         badval = n
        if tau[i] is not well defined (and tau[0] .. tau[i-1] are)
         code = INVALID_TYPE
         type1 = tau[i]  *)
    val tuple : type_t list -> type_t eh


    (** Function type: dom[0] ... dom[n-1] -> range
        Requires n>0, and dom[0] ... dom[n-1] and range to be well defined

        Error report
        if n == 0,
         code = POS_INT_REQUIRED
         badval = n
        if n > YICES_MAX_ARITY
         code = TOO_MANY_ARGUMENTS
         badval = n
        if range undefined
         code = INVALID_TYPE
         type1 = range
        if dom[i] is undefined (and dom[0] ... dom[i-1] are)
         code = INVALID_TYPE
         type1 = dom[i]  *)
    val func : type_t list -> type_t -> type_t eh

    (** **********************
         TYPE EXPLORATION    *
     ********************* *)

    (** Check whether tau is a subtype of sigma

        If tau or sigma is not a valid type, the function returns false
        and sets the error report:
         code = INVALID_TYPE
         type1 = tau or sigma  *)
    val test_subtype : type_t -> type_t -> bool eh

    (** Check whether tau and sigma are compatible

        If tau or sigma is not a valid type, the function returns 0 and
        sets the error report:
         code = INVALID_TYPE
         type1 = tau or sigma  *)
    val compatible_types : type_t -> type_t -> bool eh

    module Names : Names with type t := type_t

    val reveal : type_t -> ytype eh
    val build  : ytype -> type_t eh

    (** Parsing uses the Yices language (cf. doc/YICES-LANGUAGE)
        - convert an input string s to a type or term.
        - s must be terminated by '\0'

        The parsing function return NULL_TYPE or NULL_TERM if there's an
        error and set the error report. The line and column fields of the
        error report give information about the error location.  *)
    val parse : string -> type_t eh

  end


  module Term : sig

    type t = term_t [@@deriving eq]

    (** ********************
        TERM CONSTRUCTORS  *
     ******************* *)

    (** Constructors do type checking and simplification.
        They return NULL_TERM (< 0) if there's a type error.

        Type checking rules for function applications:
        - if f has type [tau_1 ... tau_n -> u]
          x_1 has type sigma_1, ..., x_n has type sigma_n
        - then (f x1 ... xn) is type correct if sigma_i
          is a subtype of tau_i for i=1,...,n.
          Examples:
        - x_i has type int and tau_i is real: OK
        - x_i has type real and tau_i is int: type error  *)

    (** Boolean constants: no error report  *)

    val ytrue  : unit -> term_t eh
    val yfalse : unit -> term_t eh

    (** Constant of type tau and id = index
        - tau must be a scalar type or an uninterpreted type
        - index must be non-negative, and, if tau is scalar,
          index must be less than tau's cardinality.

        Each constant is identified by its index. Two constants
        of type tau that have different indices are distinct.

        Error report:
        if tau is undefined
         code = INVALID_TYPE
         type1 = tau
        if tau is not scalar or uninterpreted,
         code = SCALAR_OR_UTYPE_REQUIRED
         type1 = tau
        if the index is negative or too large
         code = INVALID_CONSTANT_INDEX
         type1 = tau
         badval = index  *)
    val constant : type_t -> id:int -> term_t eh

    (** Uninterpreted term of type tau

        An uninterpreted term is like a global variable of type tau. But, we
        don't call it a variable, because variables have a different meaning
        in Yices (see next function).

        If tau is a function type, then this creates an uninterpreted
        function (see yices_application).

        Error report:
        if tau is undefined
         code = INVALID_TYPE
         type1 = tau  *)
    val new_uninterpreted_term : type_t -> term_t eh

    (** Variable of type tau. This creates a new variable.

        Variables are different form uninterpreted terms. They are used
        in quantifiers and to support substitutions.

        Error report:
        if tau is undefined
         code = INVALID_TYPE
         type1 = tau  *)
    val new_variable : type_t -> term_t eh

    (** Application of an uninterpreted function to n arguments.

        Error report:
        if n == 0,
         code = POS_INT_REQUIRED
         badval = n
        if fun or arg[i] is not defined
         code = INVALID_TERM
         term1 = fun or arg[i]
        if fun is not a function
         code = FUNCTION_REQUIRED
         term1 = fun
        if n != number of arguments required for fun
         code = WRONG_NUMBER_OF_ARGUMENTS
         type1 = type of fun
         badval = n
        if arg[i] has a wrong type
         code = TYPE_MISMATCH
         term1 = arg[i]
         type1 = expected type  *)
    val application : term_t -> term_t list -> term_t eh

    (** if-then-else

        Error report:
        if cond, then_term, or else_term is not a valid term
         code = INVALID_TERM
         term1 = whichever of cond, then_term, or else_term is invalid
        if cond is not boolean
         code = TYPE_MISMATCH
         term1 = cond
         type1 = bool (expected type)
        if then_term and else_term have incompatible types
         code = INCOMPATIBLE_TYPES
         term1 = then_term
         type1 = term1's type
         term2 = else_term
         type2 = term2's type  *)
    val ite : term_t -> term_t -> term_t -> term_t eh

    (** Equality (= left right)
        Disequality (/= left right)

        Error report:
        if left or right is not a valid term
         code = INVALID_TERM
         term1 = left or right
        if left and right do not have compatible types
         code = INCOMPATIBLE_TYPES
         term1 = left
         type1 = term1's type
         term2 = right
         type2 = term2's type  *)

    val (===) : term_t -> term_t -> term_t eh
    val (=/=) : term_t -> term_t -> term_t eh

    (** (not arg)

        Error report:
        if arg is invalid
          code = INVALID_TERM
          term1 = arg
        if arg is not boolean
          code = TYPE_MISMATCH
          term1 = arg
          type1 = bool (expected type)  *)
    val (!!) : term_t -> term_t eh

    (** (or  arg[0] ... arg[n-1])
        (and arg[0] ... arg[n-1])
        (xor arg[0] ... arg[n-1])

        NOTE: ARRAY ARG MAY BE MODIFIED.

        Error report:
        if n > YICES_MAX_ARITY
         code = TOO_MANY_ARGUMENTS
         badval = n
        if arg[i] is not a valid term
         code = INVALID_TERM
         term1 = arg[i]
        if arg[i] is not boolean
         code = TYPE_MISMATCH
         term1 = arg[i]
         type1 = bool (expected type)  *)

    val (!|) : term_t list -> term_t eh
    val (!&) : term_t list -> term_t eh
    val (!*) : term_t list -> term_t eh


    (** (iff left right)
        (implies left right)

        Error report:
        if left or right is invalid
          code = INVALID_TERM
          term1 = left/right
        if left or right is not boolean
          code = TYPE_MISMATCH
          term1 = left/right
          type1 = bool (expected type)  *)

    val (<=>) : term_t -> term_t -> term_t eh
    val (=>)  : term_t -> term_t -> term_t eh

    (** Tuple constructor

        Error report:
        if n == 0
         code = POS_INT_REQUIRED
         badval = n
        if n > YICES_MAX_ARITY
         code = TOO_MANY_ARGUMENTS
         badval = n
        if one arg[i] is invalid
         code = INVALID_TERM
         term1 = arg[i]  *)
    val tuple : term_t list -> term_t eh

    (** Tuple projection

        The index must be between 1 and n (where n = number of components in tuple)

        Error report:
        if tuple is invalid
          code = INVALID_TERM
          term1 = tuple
        if tuple does not have a tuple type
          code = TUPLE_REQUIRED
          term1 = tuple
        if index = 0 or index > number of components in tuple
          code = INVALID_TUPLE_INDEX
          type1 = type of tuple
          badval = index  *)
    val select : int -> term_t -> term_t eh

    (** Tuple update: replace component i of tuple by new_v

        The index must be between 1 and n (where n = number of components in tuple)

        Error report
        if tuple or new_v is invalid
          code = INVALID_TERM
          term1 = tuple/new_v
        if tuple doesn't have a tuple type
          code = TUPLE_REQUIRED
          term1 = tuple
        if index = 0 or index > number of components in tuple
          code = INVALID_TUPLE_INDEX
          type1 = tuple's type
          badval = index
        if new_v has a wrong type
          code = TYPE_MISMATCH
          term1 = new_v
          type1 = expected type (i-th component type in tuple)  *)
    val tuple_update : term_t -> int -> term_t -> term_t eh

    (** Function update

        Error report:
        if n = 0
          code = POS_INT_REQUIRED
          badval = n
        if fun or new_v, or one of arg[i] is invalid
          code = INVALID_TERM
          term1 = fun, new_v, or arg[i]
        if fun does not have a function type
          code = FUNCTION_REQUIRED
          term1 = fun
        if n != number of arguments for fun
          code = WRONG_NUMBER_OF_ARGUMENTS
          type1 = type of fun
          badval = n
        if new_v has a wrong type (not a subtype of fun's range)
          code = TYPE_MISMATCH
          term1 = new_v
          type1 = fun's range (expected type)
        if arg[i] has a wrong type for i-th arg of fun
          code = TYPE_MISMATCH
          term1 = arg[i]
          type1 = expected type  *)
    val update : term_t -> term_t list -> term_t -> term_t eh


    (** Distinct

        NOTE: ARG MANY BE MODIFIED

        Error report:
        if n == 0
          code = POS_INT_REQUIRED
          badval = n
        if n > YICES_MAX_ARITY
          code = TOO_MANY_ARGUMENTS
          badval = n
        if arg[i] is not a valid term
          code = INVALID_TERM
          term1 = arg[i]
        if two terms arg[i] and arg[j] don't have compatible types
          code = INCOMPATIBLE_TYPES
          term1 = arg[i]
          type1 = term1's type
          term2 = arg[j]
          type2 = term2's type  *)
    val distinct : term_t list -> term_t eh

    (** Quantified terms
        (forall (var[0] ... var[n-1]) body)
        (exists (var[0] ... var[n-1]) body)

        NOTE: ARRAY VAR MAY BE MODIFIED

        Error report:
        if n == 0
          code = POS_INT_REQUIRED
          badval = n
        if n > YICES_MAX_VARS
          code = TOO_MANY_VARS
          badval = n
        if body or one of var[i] is invalid
          code = INVALID_TERM
          term1 = body or var[i]
        if body is not boolean
          code = TYPE_MISMATCH
          term1 = body
          type1 = bool (expected type)
        if one of var[i] is not a variable
          code = VARIABLE_REQUIRED
          term1 = var[i]
        if one variable occurs twice in var
          code = DUPLICATE_VARIABLE
          term1 = var[i]  *)

    val forall : term_t list -> term_t -> term_t eh
    val exists : term_t list -> term_t -> term_t eh

    (** Lambda terms

        Error report:
        if n == 0
          code = POS_INT_REQUIRED
          badval = n
        if n > YICES_MAX_VARS
          code = TOO_MANY_VARS
          badval = n
        if body or one of var[i] is invalid
          code = INVALID_TERM
          term1 = body or var[i]
        if one of var[i] is not a variable
          code = VARIABLE_REQUIRED
          term1 = var[i]
        if one variable occurs twice in var
          code = DUPLICATE_VARIABLE
          term1 = var[i]
        *)
    val lambda : term_t list -> term_t -> term_t eh


    module Arith : sig

      (** *******************************
       *  ARITHMETIC TERM CONSTRUCTORS  *
       ******************************* *)

      (** RATIONAL/INTEGER CONSTANTS
       *
       * Constant terms can be constructed from integers, GMP numbers,
       * or by parsing strings.
       *
       * The constant term constructors return NULL_TERM (-1) if there's
       * an error and set the error report.  *)

      (** Zero: no error  *)
      val zero : unit -> term_t eh

      (** Integer constants  *)

      val int32 : sint -> term_t eh
      val int64 : long -> term_t eh

      (** Rational constants
       * - den must be non-zero
       * - common factors are removed
       *
       * Error report:
       * if den is zero
       *   code = DIVISION_BY_ZERO  *)

      val rational32 : sint -> uint -> term_t eh
      val rational64 : long -> ulong -> term_t eh

      val mpz : Z.t -> term_t eh
      val mpq : Q.t -> term_t eh
      
      (** Convert a string to a rational or integer term.
       * The string format is
       *     <optional_sign> <numerator>/<denominator>
       *  or <optional_sign> <numerator>
       *
       * where <optional_sign> is + or - or nothing
       * <numerator> and <denominator> are sequences of
       * decimal digits.
       *
       * Error report:
       *   code = INVALID_RATIONAL_FORMAT if s is not in this format
       *   code = DIVISION_BY_ZERO if the denominator is zero  *)
      val parse_rational : string -> term_t eh

      (** Convert a string in floating point format to a rational
       * The string must be in one of the following formats:
       *   <optional sign> <integer part> . <fractional part>
       *   <optional sign> <integer part> <exp> <optional sign> <integer>
       *   <optional sign> <integer part> . <fractional part> <exp> <optional sign> <integer>
       *
       * where <optional sign> is + or - or nothing
       *       <exp> is either 'e' or 'E'
       *
       * Error report:
       * code = INVALID_FLOAT_FORMAT  *)
      val parse_float : string -> term_t eh

      (** ARITHMETIC OPERATIONS  *)

      (** All operations return NULL_TERM if there's an error (NULL_TERM = -1)
       *
       * Error reports:
       * if t1 or t2 is not valid
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not an arithmetic term
       *   code = ARITHTERM_REQUIRED
       *   term1 = t1 or t2
       *
       * for yices_mul, yices_square, and yices_power,
       * if the result's degree is too large,
       * then the error report is
       *   code = DEGREE_OVERFLOW
       *   badval = product degree  *)

      val (+) : term_t -> term_t -> term_t eh
      val (-) : term_t -> term_t -> term_t eh
      val (!-) : term_t -> term_t eh
      val ( * ) : term_t -> term_t -> term_t eh
      val square : term_t -> term_t eh
      val (^) : term_t -> uint -> term_t eh

      (** Sum of n arithmetic terms t[0] ... t[n-1]
       *
       * Return NULL_TERM if there's an error
       *
       * Error reports:
       * if t[i] is not valid
       *   code = INVALID_TERM
       *   term1 = t[i]
       * if t[i] is not an arithmetic term
       *   code = ARITHTERM_REQUIRED
       *   term1 = t[i]  *)
      val (!+) : term_t list -> term_t eh

      (** Product of n arithmetic terms t[0] ... t[n-1]
       *
       * Return NULL_TERM if there's an error
       *
       * Error reports:
       * if t[i] is not valid
       *   code = INVALID_TERM
       *   term1 = t[i]
       * if t[i] is not an arithmetic term
       *   code = ARITHTERM_REQUIRED
       *   term1 = t[i]
       * if the result has degree > YICES_MAX_DEGREE
       *   code = DEGREE OVERFLOW
       *   badval = degree  *)
      val (!* ) : term_t list -> term_t eh

      (** Division:  t1/t2
       *
       * t1 and t2 must be arithmetic terms
       *
       * NOTE: Until Yices 2.5.0, t2 was required to be a non-zero constant.
       * This is no longer the case: t2 can be any arithmetic term.
       *
       * Return NULL_TERM if there's an error
       *
       * Error report:
       * if t1 or t2 is not valid
       *    code = INVALID_TERM
       *    term1 = t1 or t2
       * if t1 or t2 is not an arithmetic term
       *    code = ARITHTERM_REQUIRED
       *    term1 = t1 or t2  *)
      val (/) : term_t -> term_t -> term_t eh

      (** Integer division and modulo
       *
       * t1 and t2 must arithmetic terms
       *
       * The semantics is as defined in SMT-LIB 2.0 (theory Ints),
       * except that t1 and t2 are not required to be integer.
       *
       * NOTE: Until Yices 2.5.0, t2 was required to be a non-zero constant.
       * This is no longer the case: t2 can be any arithmetic term.
       *
       * The functions (div t1 t2) and (mod t1 t2) satisfy the following
       * constraints:
       *    t1 = (div t1 t2) * t2 + (mod t1 t2)
       *    0 <= (mod t1 t2) < (abs t2)
       *    (div t1 t2) is an integer
       *
       * The functions return NULL_TERM if there's an error.
       *
       * Error report:
       * if t1 or t2 is not valid
       *    code = INVALID_TERM
       *    term1 = t1 or t2
       * if t1 or t2 is not an arithmetic term
       *    code = ARITHTERM_REQUIRED
       *    term1 = t1 or t2  *)

      val ( /. ) : term_t -> term_t -> term_t eh
      val ( %. ) : term_t -> term_t -> term_t eh

      (** Divisibility test:
       *
       * t1 must be an arihtmetic constant.
       * t2 must be an arithmetic term.
       *
       * This function constructs the atom (divides t1 t2).
       * The semantics is
       *   (divides t1 t2) IFF (there is an integer k such that t2 = k * t1)
       *
       * The functions return NULL_TERM if there's an error.
       *
       * Error report:
       * if t1 or t2 is not valid
       *    code = INVALID_TERM
       *    term1 = t1 or t2
       * if t1 is not an arithmetic term
       *    code = ARITHTERM_REQUIRED
       *    term1 = t1
       * if t2 is not an arithmetic constant
       *    code = ARITHCONSTANT_REQUIRED
       *    term1 = t2  *)
      val divides_atom : term_t -> term_t -> term_t eh

      (** Integrality test:
       *
       * t must be an arithmetic term.
       *
       * This function constructs the atom (is-int t) as defined in
       * SMT-LIB 2: (is-int t) is true iff t is an integer. Also, we have
       * (is-int t) iff (divides 1 t).
       *
       * The function returns NULL_TERM if there's an error.
       *
       * Error report:
       * if t is not valid
       *    code = INVALID_TERM
       *    term1 = t
       * if t is not an arithmetic term
       *    code = ARITHTERM_REQUIRED
       *    term1 = t
       *  *)
      val is_int_atom : term_t -> term_t eh

      (** Absolute value, floor, ceiling
       *
       * t must be an arithmetic term
       *
       * floor t is the largest integer that's less than or equal to t
       * ceiling t is the smallest integer that's greater than or equal to t
       * The functions return NULL_TERM if there's an error.
       *
       * Error report:
       * if t is not valid
       *    code = INVALID_TERM
       *    term1 = t
       * if t is not an arithmetic term
       *    code = ARITHTERM_REQUIRED
       *    term1 = t  *)

      val abs   : term_t -> term_t eh
      val floor : term_t -> term_t eh
      val ceil  : term_t -> term_t eh

      (** POLYNOMIALS  *)

      (** The functions below construct the term a_0 t_0 + ... + a_{n-1} t_{n-1}
       * given n constant coefficients a_0, ..., a_{n-1} and
       *       n arithmetic terms t_0, ..., t_{n-1}.
       *
       * If there's an error, the functions return NULL_TERM (-1).
       *
       * Error reports:
       * if t[i] is not valid
       *   code = INVALID_TERM
       *   term1 = t[i]
       * if t[i] is not an arithmetic term
       *   code = ARITHTERM_REQUIRED
       *   term1 = t[i]  *)

      (** Polynomial with integer coefficients
       * - a and t must both be arrays of size n  *)

      val poly_int32 : (sint*term_t) list -> term_t eh
      val poly_int64 : (long*term_t) list -> term_t eh

      (** Polynomial with rational coefficients
       * - den, num, and t must be arrays of size n
       * - the coefficient a_i is num[i]/den[i]
       *
       * Error report:
       * if den[i] is 0
       *   code = DIVISION_BY_ZERO  *)

      val poly_rational32 : (sint*uint*term_t) list -> term_t eh
      val poly_rational64 : (long*ulong*term_t) list -> term_t eh

      (** Coefficients are GMP integers or rationals. *)

      val poly_mpz : (Z.t * term_t) list -> term_t eh
      val poly_mpq : (Q.t * term_t) list -> term_t eh

      (** ARITHMETIC ATOMS  *)

      (** All operations return NULL_TERM if there's an error (NULL_TERM = -1)
       *
       * Error reports
       * if t1 or t2 is not valid
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not an arithmetic term
       *   code = ARITHTERM_REQUIRED
       *   term1 = t1 or t2  *)

      val eq  : term_t -> term_t -> term_t eh
      val neq : term_t -> term_t -> term_t eh
      val geq : term_t -> term_t -> term_t eh
      val leq : term_t -> term_t -> term_t eh
      val gt  : term_t -> term_t -> term_t eh
      val lt  : term_t -> term_t -> term_t eh

      (** Comparison with 0:
       *
       * Return NULL_TERM if there's an error.
       *
       * Error report:
       * if t is not valid:
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not an arithmetic term
       *   code = ARITH_TERM_REQUIRED
       *   term1 = t  *)

      val eq0  : term_t -> term_t eh
      val neq0 : term_t -> term_t eh
      val geq0 : term_t -> term_t eh
      val leq0 : term_t -> term_t eh
      val gt0  : term_t -> term_t eh
      val lt0  : term_t -> term_t eh
    end

    module BV : sig
      (** ******************************
       *  BITVECTOR TERM CONSTRUCTORS  *
       ****************************** *)

      (** BITVECTOR CONSTANTS
       *
       * Constants can be constructed from C integers (32 or 64 bits),
       * from GMP integers, from arrays, or by parsing strings.
       *
       * The constant constructors return NULL_TERM (-1) if there's
       * an error and set the error report.  *)

      (** Conversion of an integer to a bitvector constant.
       * - n = number of bits
       * - x = value
       * The low-order bit of x is bit 0 of the constant.
       *
       * For yices_bvconst_uint32:
       * - if n is less than 32, then the value of x is truncated to
       *   n bits (i.e., only the n least significant bits of x are considered)
       * - if n is more than 32, then the value of x is zero-extended to
       *   n bits.
       *
       * For yices_bvconst_uint64:
       * - if n is less than 64, then the value of x is truncated to
       *   n bits (i.e., only the n least significant bits of x are considered)
       * - if n is more than 64, then the value of x is zero-extended to
       *   n bits.
       *
       * For yices_bvconst_int32:
       * - if n is less than 32, then the value of x is truncated to
       *   n bits (i.e., only the n least significant bits of x are considered)
       * - if n is more than 32, then the value of x is sign-extended to
       *   n bits.
       *
       * For yices_bvconst_int64:
       * - if n is less than 64, then the value of x is truncated to
       *   n bits (i.e., only the n least significant bits of x are considered)
       * - if n is more than 64, then the value of x is sign-extended to
       *   n bits.
       *
       * For yices_bvconst_mpz:
       * - x is interpreted as a signed number in 2-s complement
       * - if x has fewer than n bits (in 2's complement), then the value is sign-extended
       * - if x has more than n bits (in 2's complement) then the value is truncated
       *   (to n least significant bits).
       *
       * Error report:
       * if n = 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if n > YICES_MAX_BVSIZE
       *    code = MAX_BVSIZE_EXCEEDED
       *    badval = n  *)

      val bvconst_uint32 : width:int -> uint -> term_t eh
      val bvconst_uint64 : width:int -> ulong -> term_t eh
      val bvconst_int32  : width:int -> sint -> term_t eh
      val bvconst_mpz    : width:int -> Z.t -> term_t eh

      (** bvconst_zero: set all bits to 0
       * bvconst_one: set low-order bit to 1, all the other bits to 0
       * bvconst_minus_one: set all bits to 1
       *
       * Error report:
       * if n = 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if n > YICES_MAX_BVSIZE
       *    code = MAX_BVSIZE_EXCEEDED
       *    badval = n  *)

      val bvconst_zero      : width:int -> term_t eh
      val bvconst_one       : width:int -> term_t eh
      val bvconst_minus_one : width:int -> term_t eh

      (** Construction from an integer array
       * bit i of the constant is 0 if a[i] == 0
       * bit i of the constant is 1 if a[i] != 0
       *
       * Error report:
       * if n = 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if n > YICES_MAX_BVSIZE
       *    code = MAX_BVSIZE_EXCEEDED
       *    badval = n  *)
      val bvconst_from_array : bool list -> term_t eh

      (** Parsing from a string of characters '0' and '1'
       * First character = high-order bit
       * Last character = low-order bit
       * The constant has n bits if the strings has n characters.
       *
       * Error report:
       * if the format is incorrect:
       *   code = INVALID_BVBIN_FORMAT
       * if the string has more than YICES_MAX_BVSIZE digits
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = n  *)
      val parse_bvbin : string -> term_t eh

      (** Parsing from a hexadecimal string
       * All characters must be '0' to '9' or 'a' to 'f' or 'A' to 'F'
       * - First character = 4 high-order bits
       * - Last character = 4 low-order bits
       * The constant has 4n bits if s has n characters.
       *
       * Error report:
       * if the format is incorrect:
       *   code = INVALID_BVHEX_FORMAT
       * if the result would have more than YICES_MAX_BVSIZE digits
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = 4n  *)
      val parse_bvhex : string -> term_t eh

      (** BIT-VECTOR ARITHMETIC  *)

      (** Binary operations: both arguments must be bitvector terms of the same size.
       * The functions return NULL_TERM (-1) if there's an error.
       *
       * Error reports
       * if t1 or t2 is not valid
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t1 or t2
       * if t1 and t2 do not have the same bitvector type
       *   code = INCOMPATIBLE_TYPES
       *   term1 = t1
       *   type1 = type of t1
       *   term2 = t2
       *   type2 = type of t2
       *
       * For bvmul, bvsquare, or bvpower, if the degree is too large
       *   code = DEGREE_OVERFLOW
       *
       *
       * In case of division by 0, Yices uses the following conventions:
       *
       *   (bvdiv  x 0b00...0) is the  largest unsigned integer that can be represented using n bits
       *                       (i.e., 0b111....1)
       *
       *   (bvrem  x 0b00...0) is x
       *
       *   (bvsdiv x 0b00...0) is   0b00..01 (i.e., +1) if x's sign bit is 1
       *                       and  0b111111 (i.e., -1) if x's sign bit is 0
       *
       *   (bvsrem x 0b00...0) is x
       *
       *   (bvsmod x 0b00...0) is x
       *  *)

      val bvadd : term_t -> term_t -> term_t eh
      val bvsub : term_t -> term_t -> term_t eh
      val bvneg : term_t -> term_t eh
      val bvmul : term_t -> term_t -> term_t eh
      val bvsquare : term_t -> term_t eh
      val bvpower : term_t -> int -> term_t eh
      val bvdiv  : term_t -> term_t -> term_t eh
      val bvrem  : term_t -> term_t -> term_t eh
      val bvsdiv : term_t -> term_t -> term_t eh
      val bvsrem : term_t -> term_t -> term_t eh
      val bvsmod : term_t -> term_t -> term_t eh
      val bvnot  : term_t -> term_t eh
      val bvnand : term_t -> term_t -> term_t eh
      val bvnor  : term_t -> term_t -> term_t eh
      val bvxnor : term_t -> term_t -> term_t eh
      val bvshl  : term_t -> term_t -> term_t eh
      val bvlshr : term_t -> term_t -> term_t eh
      val bvashr : term_t -> term_t -> term_t eh

      (** Bitvector and/or/xor
       *
       * The general form takes an array t[0 ...n-1] as argument (n must be positive).
       * - all t[i]s must be bitvector term of the same type (i.e., the same number of bits).
       * - special forms are provided for convenience for n=2 and 3.
       *
       * These function return NULL_TERM if there's an error.
       *
       * Error reports:
       * if n == 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if t[i] is not valid
       *    code = INVALID_TERM
       *    term1 = t[i]
       * if t[i] is not a bitvector term
       *    code = BITVECTOR_REQUIRED
       *    badval = n
       * if t[0] and t[i] don't have the same bitvector type
       *    code = INCOMPATIBLE_TYPES
       *    term1 = t[0]
       *    type1 = type of t[0]
       *    term2 = t[i]
       *    type2 = type of t[i]
       *  *)

      val bvand  : term_t list -> term_t eh
      val bvor   : term_t list -> term_t eh
      val bvxor  : term_t list -> term_t eh

      (** Sum of n bitvector terms t[0] ... t[n-1]
       * - n must be positive
       * - all t[i]s must be bitvector terms of the same type (same number of bits)
       *
       * Return NULL_TERM if there's an error.
       *
       * Error reports:
       * if n == 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if t[i] is not valid
       *    code = INVALID_TERM
       *    term1 = t[i]
       * if t[i] is not a bitvector term
       *    code = BITVECTOR_REQUIRED
       *    badval = n
       * if t[0] and t[i] don't have the same bitvector type
       *    code = INCOMPATIBLE_TYPES
       *    term1 = t[0]
       *    type1 = type of t[0]
       *    term2 = t[i]
       *    type2 = type of t[i]  *)
      val bvsum  : term_t list -> term_t eh

      (** Product of n bitvector terms t[0] ... t[n-1]
       *
       * - n must be positive
       * - all t[i]s must be bitvector terms of the same type (same number of bits)
       *
       * Return NULL_TERM if there's an error.
       *
       * Error reports:
       * if n == 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if t[i] is not valid
       *    code = INVALID_TERM
       *    term1 = t[i]
       * if t[i] is not a bitvector term
       *    code = BITVECTOR_REQUIRED
       *    term1 = t[i]
       * if t[0] and t[i] don't have the same bitvector type
       *    code = INCOMPATIBLE_TYPES
       *    term1 = t[0]
       *    type1 = type of t[0]
       *    term2 = t[i]
       *    type2 = type of t[i]
       * if the result has degree > YICES_MAX_DEGREE
       *    code = DEGREE_OVERFLOW
       *    badval = degree  *)
      val bvproduct : term_t list -> term_t eh

      (** Shift or rotation by an integer constant n
       * - shift_left0 sets the low-order bits to zero
       * - shift_left1 sets the low-order bits to one
       * - shift_right0 sets the high-order bits to zero
       * - shift_right1 sets the high-order bits to one
       * - ashift_right is arithmetic shift, it copies the sign bit
       * - rotate_left: circular rotation
       * - rotate_right: circular rotation
       *
       * If t is a vector of m bits, then n must satisfy 0 <= n <= m.
       *
       * The functions return NULL_TERM (-1) if there's an error.
       *
       * Error reports:
       * if t is not valid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t
       * if n > size of t
       *   code = INVALID_BITSHIFT
       *   badval = n  *)

      val shift_left0  : term_t -> int -> term_t eh
      val shift_left1  : term_t -> int -> term_t eh
      val shift_right0 : term_t -> int -> term_t eh
      val shift_right1 : term_t -> int -> term_t eh
      val ashift_right : term_t -> int -> term_t eh
      val rotate_left  : term_t -> int -> term_t eh
      val rotate_right : term_t -> int -> term_t eh

      (** Extract a subvector of t
       * - t must be a bitvector term of size m
       * - i and j must satisfy i <= j <= m-1
       * The result is the bits i to j of t.
       *
       * Return NULL_TERM (-1) if there's an error.
       *
       * Error reports:
       * if t is not valid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t
       * if i <= j <= m-1 does not hold
       *   code = INVALID_BVEXTRACT  *)
      val bvextract : term_t -> int -> int -> term_t eh

      (** Concatenation
       * - t1 and t2 must be bitvector terms
       *
       * NOTE: t1 is the high-order part of the result, t2 is the low-order part.
       * For example, if t1 is 0b0000 and t2 is 0b11111, then the function will
       * construct 0b000011111.
       *
       * Return NULL_TERM (-1) if there's an error.
       *
       * Error reports
       * if t1 or t2 is not a valid term
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t1 or t2
       * if the size of the result would be larger than MAX_BVSIZE
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = n1 + n2 (n1 = size of t1, n2 = size of t2)  *)
      val bvconcat2 : term_t -> term_t -> term_t eh

      (** General form of concatenation: the input is an array of n bitvector terms
       * - n must be positive.
       *
       * NOTE: t[0] is the high-order part of the result, and t[n-1] is the low-order
       * part. For example, if n=3, t[0] is 0b000, t[1] is 0b111, and t[2] is 0b01, then
       * the function constructs 0b00011101.
       *
       * Error reports:
       * if n == 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if t[i] is not valid
       *    code = INVALID_TERM
       *    term1 = t[i]
       * if t[i] is not a bitvector term
       *    code = BITVECTOR_REQUIRED
       *    term1 = t[i]
       * if the size of the result would be more than YICES_MAX_BVSIZE
       *    code = MAX_BVSIZE_EXCEEDED
       *    badval = sum of the size of t[i]s  *)
      val bvconcat : term_t list -> term_t eh

      (** Repeated concatenation:
       * - make n copies of t and concatenate them
       * - n must be positive
       *
       * Return NULL_TERM (-1) if there's an error
       *
       * Error report:
       * if t is not valid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t
       * if n == 0
       *   code = POS_INT_REQUIRED
       *   badval = n
       * if n * size of t > MAX_BVSIZE
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = n * size of t  *)
      val bvrepeat : term_t -> int -> term_t eh

      (** Sign extension
       * - add n copies of t's sign bit
       *
       * Return NULL_TERM if there's an error.
       *
       * Error reports:
       * if t is invalid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector
       *   code = BITVECTOR_REQUIRED
       *   term1 = t
       * if n + size of t > MAX_BVSIZE
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = n * size of t  *)
      val sign_extend : term_t -> int -> term_t eh

      (** Zero extension
       * - add n zeros to t
       *
       * Return NULL_TERM if there's an error.
       *
       * Error reports:
       * if t is invalid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector
       *   code = BITVECTOR_REQUIRED
       *   term1 = t
       * if n + size of t > MAX_BVSIZE
       *   code = MAX_BVSIZE_EXCEEDED
       *   badval = n * size of t  *)
      val zero_extend : term_t -> int -> term_t eh

      (** AND-reduction:
       * if t is b[m-1] ... b[0], then the result is a bit-vector of 1 bit
       * equal to the conjunction of all bits of t (i.e., (and b[0] ... b[m-1])
       *
       * OR-reduction: compute (or b[0] ... b[m-1])
       *
       * Return NULL_TERM if there's an error
       *
       * Error reports:
       * if t is invalid
       *   code = INVALID_TERM
       *   term1 = t
       * if t is not a bitvector
       *   code = BITVECTOR_REQUIRED
       *   term1 = t  *)

      val redand : term_t -> term_t eh
      val redor  : term_t -> term_t eh

      (** Bitwise equality comparison: if t1 and t2 are bitvectors of size n,
       * construct (bvredand (bvxnor t1 t2))
       *
       * Return NULL_TERM if there's an error
       *
       * Error reports:
       * if t1 or t2 is not valid
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t1 or t2
       * if t1 and t2 do not have the same bitvector type
       *   code = INCOMPATIBLE_TYPES
       *   term1 = t1
       *   type1 = type of t1
       *   term2 = t2
       *   type2 = type of t2  *)
      val redcomp : term_t -> term_t -> term_t eh

      (** Convert an array of boolean terms arg[0 ... n-1] into
       * a bitvector term of n bits
       * - arg[0] = low-order bit of the result
       * - arg[n-1] = high-order bit
       *
       * Error report:
       * if n == 0
       *    code = POS_INT_REQUIRED
       *    badval = n
       * if n > YICES_MAX_BVSIZE
       *    code = MAX_BVSIZE_EXCEEDED
       *    badval = size
       * if arg[i] is invalid
       *    code = INVALID_TERM
       *    term1 = arg[i]
       * if arg[i] is not a boolean
       *    code = TYPE_MISMATCH
       *    term1 = arg[i]
       *    type1 = bool  *)
      val bvarray : term_t list -> term_t eh

      (** Extract bit i of vector t (as a boolean)
       * - if t is a bitvector of n bits, then i must be between 0 and n-1
       * - the low-order bit of t has index 0
       * - the high-order bit of t has index (n-1)
       *
       * Error report:
       * if t is invalid
       *    code = INVALID_TERM
       *    term1 = t
       * if t is not a bitvector term
       *    code = BITVECTOR_REQUIRED
       *    term1 = t
       * if i >= t's bitsize
       *    code = INVALID_BITEXTRACT  *)
      val bitextract : term_t -> int -> term_t eh

      (** BITVECTOR ATOMS  *)

      (** All operations return NULL_TERM (i.e., a negative index) on error
       *
       * Error reports
       * if t1 or t2 is not valid
       *   code = INVALID_TERM
       *   term1 = t1 or t2
       * if t1 or t2 is not a bitvector term
       *   code = BITVECTOR_REQUIRED
       *   term1 = t1 or t2
       * if t1 and t2 do not have the same bitvector type
       *   code = INCOMPATIBLE_TYPES
       *   term1 = t1
       *   type1 = type of t1
       *   term2 = t2
       *   type2 = type of t2  *)

      (** Equality and disequality  *)

      val bveq : term_t -> term_t -> term_t eh
      val bvneq : term_t -> term_t -> term_t eh

      (** Unsigned inequalities  *)

      val bvge : term_t -> term_t -> term_t eh
      val bvgt : term_t -> term_t -> term_t eh
      val bvle : term_t -> term_t -> term_t eh
      val bvlt : term_t -> term_t -> term_t eh

      (** Signed inequalities  *)

      val bvsge : term_t -> term_t -> term_t eh
      val bvsgt : term_t -> term_t -> term_t eh
      val bvsle : term_t -> term_t -> term_t eh
      val bvslt : term_t -> term_t -> term_t eh
    end

    (** ****************
        SUBSTITUTIONS  *
     *************** *)

    (** Apply the substitution defined by arrays var and map to a term t
        - var must be an array of n terms. Each element of var must
          be either an uninterpreted term or a variable.
          (cf. yices_new_uninterpreted_term and yices_new_variable).
        - map must be an array of n terms
        - the type of map[i] must be a subtype of var[i]'s type
        - every occurrence of var[i] in t is replaced by map[i]
        - if a variable occurs several times in v, the last occurrence
          counts. (e.g., if v[i] = x and v[j] = x with i < j, and
          there are no other occurrences of x in v, then x is
          replaced by map[j]).

        Return the resulting term or NULL_TERM if there's an error.

        Error codes:
        - INVALID_TERM if var[i] or map[i] is not valid
        - VARIABLE_REQUIRED if var[i] is not a variable or an uninterpreted term
        - TYPE_MISMATCH if map[i]'s type is not a subtype of var[i]'s type
        - DEGREE_OVERFLOW if the substitution causes an overflow  *)
    val subst_term : (term_t*term_t) list -> term_t -> term_t eh

    (** Apply a substitution to m terms in parallel
        - the substitution is defined by arrays var and map:
          var must be an array of n variables or uninterpreted terms
          map must be an array of n terms
          map[i]'s type must be a subtype of var[i]'s type
        - the substitution is applied to terms t[0] ... t[m-1]
        - on entry to the function: t[i] must be a valid term
          the function applies the substitution to t[i]
          then store the result in place (i.e., t[i] := subst(n, var, map, t[i])).

        Note: it's more efficient to call this function than to call
        yices_subst_term m times.

        Return code:
        0 if all goes well
        -1 if there's an error

        Error codes: as above  *)
    val subst_terms : (term_t*term_t) list -> term_t list -> term_t list eh


    (** ********************
        TERM EXPLORATION   *
     ******************* *)

    (** Get the type of term t
        returns NULL_TYPE if t is not a valid term
        and sets the error report:
         code = INVALID_TERM
         term1 = t  *)
    val type_of_term : term_t -> type_t eh

    (** Check the type of a term t:
        - returns 0 for false, 1 for true

        - term_is_arithmetic checks whether t's type is either int or real
        - term_is_real checks whether t's type is real
        - term_is_int checks whether t's type is int
        - term_is_scalar checks whether t has a scalar or uninterpreted type

        If t is not a valid term, the check functions return false
        and set the error report as above.  *)

    val is_bool       : term_t -> bool eh
    val is_int        : term_t -> bool eh
    val is_real       : term_t -> bool eh
    val is_arithmetic : term_t -> bool eh
    val is_bitvector  : term_t -> bool eh
    val is_tuple      : term_t -> bool eh
    val is_function   : term_t -> bool eh
    val is_scalar     : term_t -> bool eh

    (** Size of a bitvector term (i.e., number of bits)
        - returns 0 if there's an error

        Error report:
        if t is not a valid term
          code = INVALID_TERM
          term1 = t
        if t is not a bitvector term
          code = BITVECTOR_REQUIRED
          term1 = t  *)
    val bitsize : term_t -> int eh

    (** Check whether t is a ground term (i.e., does not have free variables)

        Also return false and set the error report if t is not valid  *)
    val is_ground : term_t -> bool eh


    (** The following functions check the structure of a term_t.
        They return 0 for false, 1 for true.

        If t is not a valid term, then the functions return 0
        and set the error report:
          code = INVALID_TERM
          term1 = t  *)

    val is_atomic     : term_t -> bool eh
    val is_composite  : term_t -> bool eh
    val is_projection : term_t -> bool eh
    val is_sum        : term_t -> bool eh
    val is_bvsum      : term_t -> bool eh
    val is_product    : term_t -> bool eh

    val reveal : term_t -> yterm eh


    (** Constructor of term t:
        - if t is a valid term, the function returns one of the following codes
          defined in yices_types.h:

         if t is atomic:

          YICES_BOOL_CONSTANT        boolean constant
          YICES_ARITH_CONSTANT       rational constant
          YICES_BV_CONSTANT          bitvector constant
          YICES_SCALAR_CONSTANT      constant of uninterpreted/scalar
          YICES_VARIABLE             variable in quantifiers/lambda terms
          YICES_UNINTERPRETED_TERM   global variables

         if t is a composite terms:

          YICES_ITE_TERM             if-then-else
          YICES_APP_TERM             application of an uninterpreted function
          YICES_UPDATE_TERM          function update
          YICES_TUPLE_TERM           tuple constructor
          YICES_EQ_TERM              equality
          YICES_DISTINCT_TERM        (distinct t_1 ... t_n)
          YICES_FORALL_TERM          quantifier
          YICES_LAMBDA_TERM          lambda
          YICES_NOT_TERM             (not t)
          YICES_OR_TERM              n-ary OR
          YICES_XOR_TERM             n-ary XOR

          YICES_BV_ARRAY             array of boolean terms
          YICES_BV_DIV               unsigned division
          YICES_BV_REM               unsigned remainder
          YICES_BV_SDIV              signed division
          YICES_BV_SREM              remainder in signed division (rounding to 0)
          YICES_BV_SMOD              remainder in signed division (rounding to -infinity)
          YICES_BV_SHL               shift left (padding with 0)
          YICES_BV_LSHR              logical shift right (padding with 0)
          YICES_BV_ASHR              arithmetic shift right (padding with sign bit)
          YICES_BV_GE_ATOM           unsigned bitvector comparison: (t1 >= t2)
          YICES_BV_SGE_ATOM          signed bitvector comparison (t1 >= t2)

          YICES_ARITH_GE_ATOM        arithmetic comparison (t1 >= t2)
          YICES_ARITH_ROOT_ATOM      nonlinear arithmetic constraint

         if t is a projection

          YICES_SELECT_TERM          tuple projection
          YICES_BIT_TERM             bit-select: extract the i-th bit of a bitvector

         if t is a sum

          YICES_BV_SUM               sum of pairs a * t where a is a bitvector constant (and t is a bitvector term)
          YICES_ARITH_SUM            sum of pairs a * t where a is a rational (and t is an arithmetic term)

        if t is a product

          YICES_POWER_PRODUCT         power products: (t1^d1 * ... * t_n^d_n)

        If t is not a valid term, the function returns a negative number,
        (i.e., YICES_CONSTRUCTOR_ERROR) and sets the error report.
          code = INVALID_TERM
          term1 = t  *)
    val constructor : term_t -> term_constructor eh

    (** Number of children of term t
        - for atomic terms, returns 0
        - for composite terms, returns the number of children
        - for projections, returns 1
        - for sums, returns the number of summands
        - for products, returns the number of factors

        - returns -1 if t is not a valid term and sets the error report  *)
    val num_children : term_t -> int eh

    (** Get i-th child of a composite term
        - if t has n children (as returned by yices_term_num_children)
          then i must be between 0 and n-1.

        - the function returns NULL_TERM if there's an error.

        Error codes:
        if t is not valid
          code = INVALID_TERM
          term1 = t
        if t is not a composite, or i is not in the range [0 .. n-1]
          code = INVALID_TERM_OP  *)
    val child : term_t -> int -> term_t eh

    (** Collect all the children of composite term t in vector *v
      - v must be initialized by calling yices_init_term_vector
      - if t is not valid or is not a composite term, the function
        returns -1 and leaves *v unchanged
      - otherwise, the children are stored in *v:
         v->size = number of children
         v->data[0 ... v->size-1] = the children
     
      The vector->size is equal to yices_term_num_children(t).
      The children are stored in the same order as given by yices_term_child:
         v->data[i] = child of index i.
     
      Error report:
      if t is not a valid type
        code = INVALID_TERM
        term1 = t
      if t is not a composite term
        code = INVALID_TERM_OP
     
      Since 2.6.2. *)
    val children : term_t -> term_t list eh

    (** Get the argument and index of a projection
        - if t is invalid or not a projection term then
           yices_proj_index returns -1
           yices_proj_arg returns NULL_TERM

        Error codes:
        if t is not valid
          code = INVALID_TERM
          term1 = t
        if t is not a projection
          code = INVALID_TERM_OP  *)

    val proj_index : term_t -> int eh
    val proj_arg   : term_t -> term_t eh

    (** Value of a constant term:
        - these functions return 0 if t is a valid term and store t's value
          in *val (or in q)
        - if t is invalid or it's not the right kind of term, then the
          functions return -1 and leave *val unchanged.

        For yices_rational_const_value, q must be initialized.

        Error codes:
        if t is not valid
          code = INVALID_TERM
          term1 = t
        if t is not of the right kind
          code = INVALID_TERM_OP  *)

    val bool_const_value     : term_t -> bool eh
    val bv_const_value       : term_t -> sint eh
    val scalar_const_value   : term_t -> sint eh
    val rational_const_value : term_t -> Q.t eh

    (** Components of a sum t
        - i = index (must be between 0 and t's number of children - 1)
        - for an arithmetic sum, each component is a pair (rational, term)
        - for a bitvector sum, each component is a pair (bvconstant, term)
        - if the term in the pair is NULL_TERM then the component consists of
          only the constant
        - the number of bits in the bvconstant is the same as in t

        These two functions return 0 on success and -1 on error.

        Error codes:
        if t is not valid
          code = INVALID_TERM
          term1 = t
        if t is not of the right kind of the index is invalid
          code = INVALID_TERM_OP  *)
    val sum_component   : term_t -> int -> (Q.t  * (term_t option)) eh
    val bvsum_component : term_t -> int -> (sint * (term_t option)) eh

    (** Component of power product t
        - i = index (must be between 0 and t's arity - 1)
        - the component is of the form (term, exponent)
          (where exponent is a positive integer)

        The function returns 0 on success and -1 on error.

        Error codes:
        if t is not valid
          code = INVALID_TERM
          term1 = t
        if t is not of the right kind or i is invalid
          code = INVALID_TERM_OP  *)
    val product_component : term_t -> int -> (term_t * int) eh

    module Names : Names with type t := term_t

    (** Parsing uses the Yices language (cf. doc/YICES-LANGUAGE)
        - convert an input string s to a type or term.
        - s must be terminated by '\0'

        The parsing function return NULL_TYPE or NULL_TERM if there's an
        error and set the error report. The line and column fields of the
        error report give information about the error location.  *)
    val parse : string -> term_t eh

  end


  module GC : sig

    (** **********************
        GARBAGE COLLECTION   *
     ********************* *)

    (** By default, Yices never deletes any terms or types. All terms and
        types returned by the functions above can always be used by the
        application. There's no explicit term or type deletion function.

        If you want to delete terms or types that are no longer useful, you
        must make an explicit call to the garbage collector by calling
        function yices_garbage_collect.

        Yices uses a mark-and-sweep garbage collector. Given a set of root
        terms and types that must be preserved, Yices marks the roots and
        all the terms and types on which the roots depend.  After this
        marking phase, all unmarked terms and types are deleted. Yices
        includes several methods to specify the root terms and types that
        you want to keep.

        First, there's a set of default roots, namely, every term or type
        that is used by a live context or model. For example, if you call
        yices_new_context to obtain a new context and assert formulas in
        this context, then all these formulas are considered root terms
        until you delete the context using yices_free_context.

        In addition, you can specify more roots using any of the following
        mechanisms (they can be combined).

        1) give a list of root terms and types as arguments to
          yices_garbage_collect.

        2) set parameter 'keep_named' to true when calling
          yices_garbage_collect.

          If this flag is true, any term or type that is stored in the
          symbol tables is added to the set of roots.

        3) maintain reference counts for terms and types, using the functions
            yices_incref_term
            yices_decref_term
            yices_incref_type
            yices_decref_type

          When yices_garbage_collect is called, all terms and all types with
          a positive reference count are considered roots. If you never
          call yices_incref_term or yices_incref_type, then the reference
          counting is disabled and it's not taken into account when calling
          yices_garbage_collect.

        Remember that nothing is deleted until the call to yices_garbage_collect.  *)


    (** The following functions can be used to get the number of terms
        and types currently stored in the internal data structures.  *)

    val num_terms : unit -> int
    val num_types : unit -> int

    (** Reference counting support:
        - the functions return -1 if there's an error, 0 otherwise

        Error reports:
        - INVALID_TERM or INVALID_TYPE if the argument is not valid

        The decref functions also report an error if the argument has a
        current reference count of zero. The error report's code is set to
        BAD_TERM_DECREF or BAD_TYPE_DECREF in such a case.  *)

    val incref_term : term_t -> unit eh
    val decref_term : term_t -> unit eh
    val incref_type : type_t -> unit eh
    val decref_type : type_t -> unit eh

    (** The following functions give the number of terms and types
        that have a positive reference count. They return 0 if
        no call to yices_incref_term or yices_incref_type has been
        made.  *)

    val num_posref_terms : unit -> int
    val num_posref_types : unit -> int

    (** Call the garbage collector.
        - t = optional array of terms
        - nt = size of t
        - tau = optional array of types
        - ntau = size of tau
        - keep_named specifies whether the named terms and types should
          all be preserved

        The set of roots is determined as follows:
        1) all terms/types used by contexts and models are roots.
        2) if t is non NULL, then all elements in t[0 ... nt-1] are added to
          the set of root terms.
        3) if tau is non NULL, then all elements in tau[0 ... ntau - 1] are added
          to the set of root types.
        4) if keep_named is non zero (i.e., true), then all terms and types
          that are referenced in the symbol tables are added to the set of
          roots.
        5) all terms and types with a positive reference count are added to
          the set of roots.

        The function silently ignores any term t[i] or any type tau[j] that's not valid.  *)

    val garbage_collect : term_t ptr -> uint -> type_t ptr -> uint -> sint -> unit

  end

  module Config : sig

    type t = ctx_config_t ptr

    (** *************************
        CONTEXT CONFIGURATION   *
     ************************ *)

    (** When a context is created, it is possible to configure it to use a
        specific solver or a specific combination of solvers.  It is also
        possible to specify whether or not the context should support
        features such as push and pop.

        There are two solver types:
        - dpllt: default solver based on DPLL modulo theories
        - mcsat: solver based on the Model-Constructing Satisfiability Calculus

        The "mcsat" solver is required for formulas that use non-linear
        arithmetic. Currently the mcsat solver does not support push and
        pop. If you select "mcsat" as the solver type, no other
        configuration is necessary.

        If you select "dpllt" as the solver type, then you can define the
        combination of theory solvers you want to include.

        The following theory solvers are currently available:
        - egraph (solver for uninterpreted functions)
        - bitvector solver
        - array solver
        - solver for linear arithmetic based on simplex
        - solver for integer difference logic (based on Floyd-Warshall)
        - solver for real difference logic (also based on Floyd-Warshall)

        The following combinations of theory solvers can be used:
        - no solvers at all
        - egraph alone
        - bitvector solver alone
        - simplex solver alone
        - integer Floyd-Warshall solver alone
        - real Floyd-Warshall solver alone
        - egraph + bitvector solver
        - egraph + simplex solver
        - egraph + array solver
        - egraph + bitvector + array solver
        - egraph + simplex + array solver
        - egraph + simplex + bitvector + array solver

        If no solvers are used, the context can deal only with Boolean
        formulas.

        When the simplex solver is used, it's also possible to
        specify which arithmetic fragment is intended, namely:
        - integer difference logic              (IDL)
        - real difference logic                 (RDL)
        - real linear arithmetic                (LRA)
        - integer linear arithmetic             (LIA)
        - mixed integer/real linear arithmetic  (LIRA)

        In addition to the solver combination, a context can be configured
        for different usages:
        - one-shot mode: check satisfiability of one set of formulas
        - multiple checks: repeated calls to assert/check are allowed
        - push/pop: push and pop are supported (implies multiple checks)
        - clean interrupts are supported (implies push/pop)
          Currently, the Floyd-Warshall solvers can only be used in one-shot mode.

        By default, a new solver is configured as follows:
        - solvers: egraph + simplex + bitvector + array solver
        - usage: push/pop supported

        To specify another configuration, one must pass a configuration
        descriptor to function yices_new_context. A configuration descriptor
        is an opaque structure that includes the following fields:
        - arith-fragment: either IDL, RDL, LRA, LIA, or LIRA
        - uf-solver: either NONE, DEFAULT
        - bv-solver: either NONE, DEFAULT
        - array-solver: either NONE, DEFAULT
        - arith-solver: either NONE, DEFAULT, IFW, RFW, SIMPLEX
        - mode: either ONE-SHOT, MULTI-CHECKS, PUSH-POP, INTERACTIVE

        This is done as follows:
        1) allocate a configuration descriptor via yices_new_config
        2) set the configuration parameters by repeated calls to yices_set_config
          or using yices_default_config_for_logic
        3) create one or more context with this configuration by passing the
          descriptor to yices_new_context
        4) free the configuration descriptor when it's no longer needed  *)

    (** Allocate a configuration descriptor:
        - the descriptor is set to the default configuration  *)
    val malloc : unit -> t eh

    (** Deletion  *)
    val free   : t -> unit

    (** Set a configuration parameter:
        - name = the parameter name
        - value = the value

        The following table specifies the parameters and allowed values for each parameter name:

                  name    |    value            |      meaning
         ----------------------------------------------------------------------------------------
                  "mode"  | "one-shot"          |  only one call to check is supported
                          |                     |
                          | "multi-checks"      |  several calls to assert and check are
                          |                     |  possible
                          |                     |
                          | "push-pop"          |  like multi-check and with support for
                          |                     |  retracting assertions (via push/pop)
                          |                     |
                          | "interactive"       |  like push-pop, but with automatic context clean
                          |                     |  up when search is interrupted.
         ----------------------------------------------------------------------------------------
          "solver-type"   | "dpllt"             | DPLL(T) style solver (default)
                          | "mcsat"             | MCSat style solver
         ----------------------------------------------------------------------------------------
          "uf-solver"     | "default"           |  the uf-solver is included (i.e., the egraph)
                          | "none"              |  no uf-solver
         ----------------------------------------------------------------------------------------
          "bv-solver"     | "default"           |  the bitvector solver is included
                          | "none"              |  no bitvector solver
         ----------------------------------------------------------------------------------------
          "array-solver"  | "default"           |  the array solver is included
                          | "none"              |  no array solver
         ----------------------------------------------------------------------------------------
          "arith-solver"  | "ifw"               |  solver for IDL, based on the Floyd-Warshall
                          |                     |  algorithm
                          |                     |
                          | "rfw"               |  solver for RDL, based on Floyd-Warshall
                          |                     |
                          | "simplex"           |  solver for linear arithmetic, based on Simplex
                          |                     |
                          | "default"           |  same as "simplex"
                          |                     |
                          | "auto"              |  same as "simplex" unless mode="one-shot" and
                          |                     |  logic is QF_IDL or QF_RDL, in which case the
                          |                     |  solver is determined after the first call to
                          |                     |  yices_assert_formula(s).
                          |                     |
                          | "none"              |  no arithmetic solver
         ----------------------------------------------------------------------------------------
         "arith-fragment" | "IDL"               |  integer difference logic
                          | "RDL"               |  real difference logic
                          | "LIA"               |  linear integer arithmetic
                          | "LRA"               |  linear real arithmetic
                          | "LIRA"              |  mixed linear arithmetic (real + integer variables)



        The function returns -1 if there's an error, 0 otherwise.

        Error codes:
        CTX_UNKNOWN_PARAMETER if name is not a known parameter name
        CTX_INVALID_PARAMETER_VALUE if name is known but value does not match the parameter type  *)
    val set : t -> name:string -> value:string -> unit eh

    (** Set config to a default solver type or solver combination for the given logic
        - return -1 if there's an error
        - return 0 otherwise

        The logic must be given as a string, using the SMT-LIB conventions.
        Currently, Yices recognizes and supports the following logics:

         NONE:        no theories (i.e., propositional logic only)

         QF_AX:       arrays with extensionality
         QF_BV:       bitvectors
         QF_IDL:      integer difference logic
         QF_RDL:      real difference logic
         QF_LIA:      linear integer arithmetic
         QF_LRA:      linear real arithmetic
         QF_LIRA:     mixed linear arithmetic
         QF_UF:       uninterpreted functions

         QF_ABV:      arrays and bitvectors
         QF_ALIA:     arrays + linear integer arithmetic
         QF_ALRA:     arrays + linear real arithmetic
         QF_ALIRA:    arrays + mixed linear arithmetic

         QF_AUF:      arrays + uninterpreted functions
         QF_AUFBV:    arrays, bitvectors, uninterpreted functions
         QF_AUFLIA:   arrays, uninterpreted functions, and linear integer arithmetic
         QF_AUFLRA:   arrays, uninterpreted functions, and linear real arithmetic
         QF_AUFLIRA:  arrays, uninterpreted functions, and mixed linear arithmetic

         QF_UFBV:     uninterpreted functions + bitvectors
         QF_UFIDL:    uninterpreted functions + integer difference logic
         QF_UFLIA:    uninterpreted functions + linear integer arithmetic
         QF_UFLRA:    uninterpreted functions + linear real arithmetic
         QF_UFLIRA:   uninterpreted functions + mixed linear arithmetic
         QF_UFRDL:    uninterpreted functions + real difference logic

         QF_NIA:      non-linear integer arithmetic
         QF_NRA:      non-linear real arithmetic
         QF_NIRA:     non-linear mixed arithmetic

         QF_UFNIA:    uninterpreted functions + non-linear integer arithmetic
         QF_UFNRA:    uninterpreted functions + non-linear real arithmetic
         QF_UFNIRA:   uninterpreted functions + mixed, non-linear arithmetic

        In all these logics, QF means quantifier-free.

        For future extensions, Yices also recognizes the following names
        for logics that Yices does not support yet. (They combine arrays and
        non-linear arithmetic).

         QF_ANIA:     arrays + non-linear integer arithmetic
         QF_ANRA:     arrays + non-linear real arithmetic
         QF_ANIRA:    arrays + mixed/non-linear arithmetic

         QF_AUFNIA:   arrays + uninterpreted functions + non-linear integer arithmetic
         QF_AUFNRA:   arrays + uninterpreted functions + non-linear real arithmetic
         QF_AUFNIRA:  arrays + uninterpreted functions + mixed, non-linear arithmetic

        For every QF logic listed above, Yices also recognizes the full logic (i.e.,
        with quantifiers). This is for future extension. Yices does not include support
        for quantifiers yet. For example, Yices recognizes AUFLIRA as a valid logic name
        (arrays + uninterpreted functions + mixed linear arithmetic), but this logic is
        not currently supported.

        Since release 2.6.0, you can also use the name "ALL" as a logic
        name.  This has the same interpretation as using (set-logic ALL)
        in yices-smt2. For the current release, this logic name specifies
        quantifier-free combination of arrays + uninterpreted functions +
        mixed linear arithmetic + bitvectors.


        Error codes:

        CTX_UNKNOWN_LOGIC if logic is not a valid name
        CTX_LOGIC_NOT_SUPPORTED if logic is known but not supported  *)
    val default : t -> logic:string -> unit eh
  end


  module Model : sig

    type t = model_t ptr
    (** ***********
         MODELS   *
     ********** *)

    (** Delete model mdl  *)
    val free : t -> unit

    (** Build a model from a term-to-term mapping:
        - the mapping is defined by two arrays var[] and map[]
        - every element of var must be an uninterpreted term
          every element of map must be a constant of primitive or tuple type
          map[i]'s type must be a subtype of var[i]
        - there must not be duplicates in array var

        The function returns NULL and sets up the error report if something
        goes wrong. It allocates and creates a new model otherwise. When
        the model is no longer used, it must be deleted by calling yices_free_model.

        Error report:
        - code = INVALID_TERM if var[i] or map[i] is not valid
        - code = TYPE_MISMATCH if map[i]'s type is not a subtype of var[i]'s type
        - code = MDL_UNINT_REQUIRED if var[i] is not an uninterpreted term
        - code = MDL_CONSTANT_REQUIRED if map[i] is not a constant
        - code = MDL_DUPLICATE_VAR if var contains duplicate elements
        - code = MDL_FTYPE_NOT_ALLOWED if one of var[i] has a function type
        - code = MDL_CONSTRUCTION_FAILED: something else went wrong  *)
    val from_map : (term_t * term_t) list -> t eh

    (** Collect all the uninterpreted terms that have a value in model mdl.
        - these terms are returned in vector v
        - v must be an initialized term_vector

        If the model was produced from a context (i.e., returned by
        yices_get_model(context_t ctx, ...))  then vector v will contain
        all unintepreted terms that occurred in the asserted formulas.
        Warning: formula simplification may remove some uninterpreted terms.

        Trivial example:

         (assert (and (> y 0) (= x x)))

        is simplified to

         (assert (> y 0))

        then variable 'x' does not occur in the simplified assertions and will
        not be included in vector 'v'.  *)
    val collect_defined_terms : t -> term_t list eh



    (** ********************
        VALUES IN A MODEL  *
     ******************* *)

    (** Evaluation functions. Once a model is constructed, it's possible
        to query for the value of a term t in that model. The following
        functions do that for different term types.

        The evaluation functions return -1 if the value of t is unknown
        or can't be computed in the model. Otherwise, they return 0 (except
        function yices_get_value_as_term).

        Possible error codes:
        If t is not valid:
         code = INVALID_TERM
         term1 = t
        If t contains a subterm whose value is not known
         code = EVAL_UNKNOWN_TERM
        If t contains free variables
         code = EVAL_FREEVAR_IN_TERM
        If t contains quantifier(s)
         code = EVAL_QUANTIFIER
        If t contains lambda terms
         code = EVAL_LAMBDA
        If the evaluation fails for other reasons:
         code = EVAL_FAILED

        Other codes are possible depending on the specific evaluation function.  *)


    (** EVALUATION FOR SIMPLE TYPES  *)

    (** Value of boolean term t: returned as an integer val

        Error codes:
        If t is not boolean
         code = TYPE_MISMATCH
         term1 = t
         type1 = bool (expected type)
        + the other evaluation error codes above.  *)
    val get_bool_value : t -> term_t -> bool eh

    (** Value of arithmetic term t. The value can be returned as an integer, a
        rational (pair num/den), converted to a double, or using the GMP
        mpz_t and mpq_t representations, or as a libpoly algebraic number.

        Error codes:
        If t is not an arithmetic term:
         code = ARITHTERM_REQUIRED
         term1 = t
        If t's value does not fit in the *val object
         code = EVAL_OVERFLOW  *)

    val get_int32_value      : t -> term_t -> sint eh
    val get_int64_value      : t -> term_t -> long eh
    val get_rational32_value : t -> term_t -> (sint*uint) eh
    val get_rational64_value : t -> term_t -> (long*ulong) eh
    val get_double_value     : t -> term_t -> float eh
    val get_mpz_value        : t -> term_t -> Z.t eh
    val get_mpq_value        : t -> term_t -> Q.t eh

    (** UNSUPPORTED (extract from Yices's API)
     *
     * (\*  * Conversion to an algebraic number.
     *  *
     *  * t must be an arithmetic term.
     *  *
     *  * Error codes:
     *  * - if t's value is rational:
     *  *    code = EVAL_CONVERSION_FAILED
     *  * - if yices is compiled without support for MCSAT
     *  *    code = EVAL_NOT_SUPPORTED
     *  *\)
     * #ifdef LIBPOLY_VERSION
     * __YICES_DLLSPEC__ extern int32_t yices_get_algebraic_number_value(model_t *mdl, term_t t, lp_algebraic_number_t *a);
     * #endif *)

    (** Value of bitvector term t in mdl
        - the value is returned in array val
        - val must be an integer array of sufficient size to store all bits of t
          (the number of bits of t can be found by calling yices_term_bitsize).
        - bit i of t is stored in val[i] (val[i] is either 0 or 1)
        - the value is returned using small-endian convention:
          val[0] is the low-order bit
          ...
          val[n-1] is the high-order bit

        If t is not a bitvector term
         code = BITVECTOR_REQUIRED
         term1 = t  *)
    val get_bv_value : t -> term_t -> sint eh

    (** Value of term t of uninterpreted or scalar type
        - the value is returned as a constant index in *val
          (with the same meaning as in function yices_constant):
        - if t has type tau and tau is a scalar type of size n then
          the function returns an index k between 0 and n-1
        - if tau is an uninterpreted type, then the function returns an
          integer index k

        The index k is a unique identifier: if two terms t1 and t2 are not
        equal in the model mdl, then their values will be distinct indices k.

        Error codes:
        - if t does not have a scalar or uninterpreted type:
          code = SCALAR_TERM_REQUIRED
          term1 = t  *)
    val get_scalar_value : t -> term_t -> sint eh


    (** GENERIC FORM: VALUE DESCRIPTORS AND NODES  *)

    (** The previous functions work for terms t of atomic types, but they
        can't be used if t has a tuple or function type. Internally, yices
        represent the tuple and function values as nodes in a DAG. The
        following functions allows one to query and explore this DAG.
        A node in the DAG is represented by a structure of type yval_t defined
        as follows in yices_types.h: *)

    (*  typedef struct yval_s {
         int32_t node_id;
         yval_tag_t node_tag;
        } yval_t;
    *)

    (** This descriptor includes the node id (all nodes have a unique id) and
        a tag that identifies the node type. Leaf nodes represent atomic constants.
        Non-leaf nodes represent tuples and functions.

        The possible tags for a leaf node are:

        YVAL_BOOL       Boolean constant
        YVAL_RATIONAL   Rational (or integer) constant
        YVAL_ALGEBRAIC  Algebraic number
        YVAL_BV         Bitvector constant
        YVAL_SCALAR     Constant of a scalar or uninterpreted type

        The following tags are used for non-leaf nodes:

        YVAL_TUPLE      Constant tuple
        YVAL_FUNCTION   Function
        YVAL_MAPPING    Mapping of the form [val_1 .. val_k -> val]

        There is also the special leaf node to indicate an error or that a value
        is not known:

        YVAL_UNKNOWN


        The children of a tuple node denote the tuple components. For
        example Yices will represent the tuple (true, -1/2, 0b0011) as a
        node with tag YVAL_TUPLE and three children. Each children is a
        leaf node in this case.

        All functions used in the model have a simple form. They are defined
        by a finite list of mappings and a default value. Each mapping specifies the
        value of the function at a single point in its domain. For example, we could
        have a function f of type [int, int -> int] defined by the clauses:
         f(0, 0) = 0
         f(3, 1) = 1
         f(x, y) = -2 in all other cases.

        Yices represents such a function as a node with tag YVAL_FUNCTION
        and with three children. Two of these children are nodes with tag
        YVAL_MAPPING that represent the mappings:
          [0, 0 -> 0]
          [3, 1 -> 1]
        The third children represents the default value for f. In this case,
        it's a leaf node for the constant -2 (tag YVAL_RATIONAL and value -2).

        The following functions return the value of a term t as a node in
        the DAG, and allow one to query and collect the children of
        non-leaf nodes.  *)

    (** Value of term t as a node descriptor.

        The function returns 0 it t's value can be computed, -1 otherwise.
        If t's value can be computed, the corresponding node descriptor is
        returned in *val.

        Error codes are as in all evaluation functions.
        If t is not valid:
         code = INVALID_TERM
         term1 = t
        If t contains a subterm whose value is not known
         code = EVAL_UNKNOWN_TERM
        If t contains free variables
         code = EVAL_FREEVAR_IN_TERM
        If t contains quantifier(s)
         code = EVAL_QUANTIFIER
        If t contains lambda terms
         code = EVAL_LAMBDA
        If the evaluation fails for other reasons:
         code = EVAL_FAILED  *)
    val get_value : t -> term_t -> yval_t ptr eh

    (** Queries on the value of a rational node:
        - if v->node_tag is YVAL_RATIONAL, the functions below check whether v's value
          can be converted to an integer or a pair num/den of the given size.
        - if v->node_tag != YVAL_RATIONAL, these functions return false (i.e. 0).

        yices_val_is_int32: check whether v's value fits in a signed, 32bit integer

        yices_val_is_int64: check whether v's value fits in a signed, 64bit integer

        yices_val_is_rational32: check whether v's value can be written num/den where num
          is a signed 32bit integer and den is an unsigned 32bit integer

        yices_val_is_rational64: check whether v's value can be written num/den where num
          is a signed 64bit integer and den is an unsigned 64bit integer

        yices_val_is_integer: check whether v's value is an integer  *)

    val val_is_int32      : t -> yval_t ptr -> bool eh
    val val_is_int64      : t -> yval_t ptr -> bool eh
    val val_is_rational32 : t -> yval_t ptr -> bool eh
    val val_is_rational64 : t -> yval_t ptr -> bool eh
    val val_is_integer    : t -> yval_t ptr -> bool eh


    (** Get the number of bits in a bv constant, the number of components in a tuple,
        or the arity of a mapping. These function return 0 if v has the wrong tag (i.e.,
        not a bitvector constant, or not a tuple, or not a mapping).  *)

    val val_bitsize       : t -> yval_t ptr -> int eh
    val val_tuple_arity   : t -> yval_t ptr -> int eh
    val val_mapping_arity : t -> yval_t ptr -> int eh

    (** Arity of a function node. This function returns 0 if v has tag
        other than YVAL_FUNCTION, otherwise it returns the function's
        arity (i.e., the number of parameters that the function takes).  *)
    val val_function_arity : t -> yval_t ptr -> int eh

    (** Type of a function node. This function returns -1 if v has tag
        other than YVAL_FUNCTION. Otherwise, it returns the type of the
        object v.
        Since 2.6.2.  *)
    val val_function_type : t -> yval_t ptr -> type_t eh

    (** Get the value of a Boolean node v.
        - returns 0 if there's no error and store v's value in *val:
         *val is either 0 (for false) or 1 (for true).
        - returns -1 if v does not have tag YVAL_BOOL and sets the error code
          to YVAL_INVALID_OP.  *)
    val val_get_bool : t -> yval_t ptr -> bool eh

    (** Get the value of a rational node v
        - the functions return 0 if there's no error and store v's value in *val
          or in the pair *num, *den (v's value is ( *num )/( *den ).
        - they return -1 if there's an error.

        The error code is set to YVAL_INVALID_OP if v's tag is not YVAL_RATIONAL.
        The error code is set to YVAL_OVERFLOW if v's value does not fit in
        ( *val ) or in ( *num )/( *den ).  *)
    val val_get_int32      : t -> yval_t ptr -> sint eh
    val val_get_int64      : t -> yval_t ptr -> long eh
    val val_get_rational32 : t -> yval_t ptr -> (sint*uint) eh
    val val_get_rational64 : t -> yval_t ptr -> (long*ulong) eh

    (** Value converted to a floating point number  *)
    val val_get_double : t -> yval_t ptr -> float eh

    (** GMP values *)

    val val_get_mpz : model_t ptr -> yval_t ptr -> Z.t eh
    val val_get_mpq : model_t ptr -> yval_t ptr -> Q.t eh

    (** NOT SUPPORTED in yices bindings

       (**  * Export an algebraic number
        * - v->tag must be YVAL_ALGEBRAIC
        * - return a copy of the algebraic number in *a
        *
        * Error reports:
        * - if v is not an algebraic number:
        *    code = YVAL_INVALID_OP
        * - if MCSAT is not supported by the yices library
        *    code = YVAL_NOT_SUPPORTED
         *)
        #ifdef LIBPOLY_VERSION
        __YICES_DLLSPEC__ extern int32_t yices_val_get_algebraic_number(model_t *mdl, const yval_t *v, lp_algebraic_number_t *a);
        #endif  *)

    (** Get the value of a bitvector node:
        - val must have size at least equal to n = yices_val_bitsize(mdl, v)
        - v's value is returned in val[0] = low-order bit, ..., val[n-1] = high-order bit.
          every val[i] is either 0 or 1.
        - the function returns 0 if v has tag YVAL_BV
        - it returns -1 if v has another tag and sets the error code to YVAL_INVALID_OP.  *)
    val val_get_bv : t -> yval_t ptr -> sint eh

    (** Get the value of a scalar node:
        - the function returns 0 if v's tag is YVAL_SCALAR
          the index and type of the scalar/uninterpreted constant are stored in *val and *tau, respectively.
        - the function returns -1 if v's tag is not YVAL_SCALAR and sets the error code to YVAL_INVALID_OP.  *)
    val val_get_scalar : t -> yval_t ptr -> (sint*type_t) eh

    (** Expand a tuple node:
        - child must be an array large enough to store all children of v (i.e.,
          at least n elements where n = yices_val_tuple_arity(mdl, v))
        - the children nodes of v are stored in child[0 ... n-1]

        Return code = 0 if v's tag is YVAL_TUPLE.
        Return code = -1 otherwise and the error code is then set to YVAL_INVALID_OP.  *)
    val val_expand_tuple : t -> yval_t ptr -> yval_t ptr list eh

    (** Expand a function node f
        - the default value for f is stored in *def
        - the set of mappings for f is stored in vector *v.
          This vector must be initialized using yices_init_yval_vector.
          The number of mappings is v->size and the mappings are stored
          in v->data[0 ... n-1] where n = v->size

        Return code = 0 if v's tag is YVAL_FUNCTION.
        Return code = -1 otherwise and the error code is then set to YVAL_INVALID_OP.  *)
    val val_expand_function : t -> yval_t ptr -> ((yval_t ptr) * (yval_t ptr list)) eh

    (** Expand a mapping node m
        - the mapping is of the form [x_1 ... x_k -> v] where k = yices_val_mapping_arity(mdl, m)
        - tup must be an array of size at least k
        - the nodes (x_1 ... x_k) are stored in tup[0 ... k-1]
          the node v is stored in val.

        Return code = 0 if v's tag is YVAL_MAPPING.
        Return code = -1 otherwise and the error code is then set to YVAL_INVALID_OP.  *)
    val val_expand_mapping : t -> yval_t ptr -> ((yval_t ptr list) * (yval_t ptr)) eh

    (** CHECK THE VALUE OF BOOLEAN FORMULAS  *)

    (** Check whether f is true in mdl
        - the returned value is
           1 if f is true in mdl,
           0 if f is false in mdl,
          -1 if f's value can't be evaluated (then an error code is set)

        Error codes:
        - same as yices_get_bool_value  *)
    val formula_true_in_model : t -> term_t -> bool eh

    (** Check whether f[0 ... n-1] are all true in mdl
        - the returned value is as in the previous function:
           1 if all f[i] are true
           0 if one f[i] is false (and f[0 ... i-1] are all true)
          -1 if one f[i] can't be evaluated (and f[0 ... i-1] are all true)

        Error codes:
        - same as yices_get_bool_value

        NOTE: if n>1, it's more efficient to call this function once than to
        call the previous function n times.  *)
    val formulas_true_in_model : t -> term_t list -> bool eh

    (** CONVERSION OF VALUES TO CONSTANT TERMS  *)

    (** Value of term t converted to a constant term val.

        For primitive types, this is the same as extracting the value
        then converting it to a constant term:
        - if t is a Boolean term, then val is either true or false (as
          returned by functions yices_true() or yices_false()).
        - if t is an arithmetic term, then val is a rational or integer constant
          (as built by functions yices_mpq or yices_mpz).
        - if t has uninterpreted or scalar type, then val is a constant term
          of that type (as built by function yices_constant).
        - if t has a bitvector type, then val is a bitvector constant term
          (as in yices_bvconst_from_array)

        Conversion of function values is not supported currently. So the
        function fails and returns NULL_TERM if t has a function type.

        If t has tuple type tau, then val is a tuple of constants (provided
        tau does not contain any function type).

        The function returns val, or NULL_TERM if there's an error.

        Error report
         code = EVAL_CONVERSION_FAILED,
         if the conversion to term fails (because it would require
         converting a function to a term).
        *)   
    val get_value_as_term : t -> term_t -> term_t eh

    (** Get the values of terms a[0 .. n-1] in mdl and convert the values to terms.
        - a must be an array of n terms
        - b must be large enough to store n terms

        This function has the same behavior and limitations as yices_get_value_as_term.
        If there's no error, the function returns 0 and store the values in array b:
        - b[i] = value of a[i] in mdl, converted to a term

        Otherwise, the function returns -1 and sets the error report.
        The error codes are the same as for yices_get_value_as_term.  *)
    val terms_value : t -> term_t list -> term_t list eh

    (** SUPPORTS *)

    (** Given a term t and a model mdl, the support of t in mdl is a set of uninterpreted
      terms whose values are sufficient to fix the value of t in mdl. For example, if
      t is (if x>0 then x+z else y) and x has value 1 in mdl, then the value of t doesn't depend
      on the value of y in mdl. In this case, support(t) = { x, z }.
     
      This extends to an array of terms a[0 ... n-1]. The support of a is a set of terms whose
      values in mdl are sufficient to determine the values of a[0] .... a[n-1]. *)

    (** Get the support of a term t in mdl
      - the support is returned in vector *v; v must be initialized by calling yices_init_term_vector.
      - if t is not a valid term, the function returns -1 and leaves v unchanged.
      - otherwise, the function returns 0 and the support of t is stored in *v:
         v->size = number of terms in the support
         v->data[0 ... v->size-1] = the terms
     
      Error report:
      if t is not a valid term:
         code = INVALID_TERM
         term1 = t
     
      Since 2.6.2. *)
    val model_term_support : t -> term_t -> term_t list eh

    (** Get the support of terms a[0...n-1] in mdl
      - the support is returned in vector *v;
        v must be initialized by calling yices_init_term_vector.
      - if one  is not a valid term, the function returns -1 and leaves v unchanged.
      - otherwise, the function returns 0 and the support is stored in *v:
         v->size = number of terms in the support
         v->data[0 ... v->size-1] = the terms
     
      Error report:
      if a[i] is not a valid term,
        code = INVALID_TERM
        term1 = a[i]
     
      Since 2.6.2. *)
    val model_terms_support :
      t -> term_t list -> term_t list eh

    (** IMPLICANTS  *)

    (** Compute an implicant for t in mdl
        - t must be a Boolean term that's true in mdl
        - the implicant is a list of Boolean terms a[0] ... a[n-1] such that
          1) a[i] is a literal (atom or negation of an atom)
          2) a[i] is true in mdl
          3) the conjunction a[0] /\ ... /\ a[n-1] implies t

        The implicant is returned in vector v, which must be initialized by
        yices_init_term_vector:
          v->size is the number of literals in the implicant (i.e., n)
          v->data[0] ... v->data[n-1] = the n literals
        If there's an error (return code -1) then v is empty:
          v->size is set to 0.

        The function returns 0 if the implicant can be computed. Otherwise
        it returns -1.

        Error report:
        if t is not valid
         code = INVALID_TERM
         term1 = t
        if t is not a Boolean term
         code = TYPE_MISMATCH
         term1 = t
         type1 = bool
        if t is false in the model:
         code = EVAL_NO_IMPLICANT
        any of the error codes for evaluation functions is also possible:
         EVAL_UNKNOWN_TERM
         EVAL_FREEVAR_IN_TERM
         EVAL_QUANTIFIER
         EVAL_LAMBDA
         EVAL_FAILED  *)
    val implicant_for_formula : t -> term_t -> term_t list eh

    (** Variant: compute an implicant for an array of formulas in mdl.
        - n = size of the array
        - a[0 ... n-1] = n input terms.
          each a[i] must be a Boolean term and must be true in mdl

        The function computes an implicant for the conjunction (and a[0] ... a[n-1]).

        Return codes and errors are as in the previous function.
        The implicant is returned in vector v.

        If the return code is 0, then
          v->size = number of literals
          v->data contains the array of literals.
        Otherwise, v->size is set to 0.  *)
    val implicant_for_formulas : t -> term_t list -> term_t list eh

    (** MODEL GENERALIZATION  *)

    (** Given a model mdl for a formula F(X, Y). The following generalization functions
        eliminate variables Y from F(X, Y) in a way that is guided by the model.

        The result is a formula G(X) such that:
        1) mdl satisfies G(X)
        2) G(X) implies (exists Y. F(X, Y))

        Yices supports the following generalization methods:

        1) generalization by substitution: eliminate the Y variables
          by replacing them by their value in mdl
          (this is the simplest approach)

        2) generalization by projection:
        - first compute an implicant for formula F(X, Y)
            this produces a set of literals L_1(X, Y) .... L_k(X, Y)
        - then Y is eliminated from the literals by projection
            (this is a hybrid of Fourier-Motzkin elimination
             and virtual term substitution)

        In the functions below, the generalization method can be selected
        by setting parameter mode to one of the following values:

         mode = YICES_GEN_BY_SUBST  ---> generalize by substitution
         mode = YICES_GEN_BY_PROJ   ---> projection
         mode = YICES_GEN_DEFAULT   ---> automatically choose the mode
                                         depending on the variables to eliminate

        Any value other than these is interpreted the same as YICES_GEN_DEFAULT  *)

    (** Compute a generalization of mdl for formula t
        - nelims = number of variables to eliminate
        - elim = variables to eliminate
        - each term in elim[i] must be an uninterpreted term (as returned by yices_new_uninterpreted_term)
          of one of the following types: Boolean, (bitvector k), or Real
        - mode defines the generalization algorithm
        - v: term_vector to return the result

        The generalization G(X) is returned in term_vector v that must be initialized
        using yices_init_term_vector. G(X) is the conjunction of all formulas in v.
          v->size = number of formulas returned
          v->data[0] ....  v->data[v->size-1] = the formulas themselves.

        If mode = YICES_GEN_BY_PROJ, then every element of v is guaranteed to be a literal

        Important: t must be true in mdl, otherwise, the returned data may be garbage.

        Returned code:
         0 means success
        -1 means that the generalization failed.  *)
    val generalize_model : t -> term_t -> term_t list -> yices_gen_mode -> term_t list eh

    (** Compute a generalization of mdl for the conjunct (a[0] /\ ... /\ a[n-1])  *)
    val generalize_model_list : t -> term_t list -> term_t list -> yices_gen_mode -> term_t list eh

  end

    (** ********************
   *  CHECK FORMULA(S)  *
   *********************)

    (** Check whether a formula is satisfiable
      - f = formula
      - logic = SMT name for a logic (or NULL)
      - model = resulting model (or NULL if no model is needed)
      - delegate = external solver to use or NULL
     
      This function first checks whether f is trivially sat or trivially unsat.
      If not, it constructs a context configured for the specified logic, then
      asserts f in this context and checks whether the context is satisfiable.
     
      The return value is
        STATUS_SAT if f is satisfiable,
        STATUS_UNSAT if f is not satisifiable
        STATUS_ERROR if something goes wrong
     
      If the formula is satisfiable and model != NULL, then a model of f is returned in *model.
      That model must be deleted when no-longer needed by calling yices_free_model.
     
      The logic must be either NULL or the name of an SMT logic supported by Yices.
      If the logic is NULL, the function uses a default context configuration.
      Ohterwise, the function uses a context specialized for the logic.
     
      The delegate is an optional argument used only when logic is "QF_BV".
      If is ignored otherwise. It must either be NULL or be the name of an
      external SAT solver to use after bit-blasting. Valid delegates
      are "cadical", "cryptominisat", and "y2sat".
      If delegate is NULL, the default SAT solver is used.
     
      Support for "cadical" and "cryptominisat" must be enabled at compilation
      time. The "y2sat" solver is always available. The function will return STATUS_ERROR
      and store an error code if the requested delegate is not available.
     
      Error codes:
     
      if f is invalid
        code = INVALID_TERM
        term1 = f
     
      if f is not Boolean
        code = TYPE_MISMATCH
        term1 = t
        type1 = bool
     
      if logic is not a known logic name
        code = CTX_UNKNOWN_LOGIC
     
      if the logic is known but not supported by Yices
        code = CTX_LOGIC_NOT_SUPPORTED
     
      if delegate is not one of "cadical", "cryptominisat", "y2sat"
        code = CTX_UNKNOWN_DELEGATE
     
      if delegate is "cadical" or "cryptominisat" but support for these SAT solvers
      was not implemented at compile time,
        code = CTX_DELEGATE_NOT_AVAILABLE
     
      other error codes are possible if the formula is not in the specified logic (cf. yices_assert_formula)
     
      Since 2.6.2. *)
  val check_formula :
    ?logic:string -> ?model:bool -> ?delegate:string -> term_t -> smt_status_t * Model.t option

    (** Check whether n formulas are satisfiable.
      - f = array of n Boolean terms
      - n = number of elements in f
     
      This is similar to yices_check_formula except that it checks whether
      the conjunction of f[0] ... f[n-1] is satisfiable.
     
      Since 2.6.2.  *)
    val check_formulas :
      ?logic:string -> ?model:bool -> ?delegate:string -> term_t list
      -> smt_status_t * Model.t option

    (** Check whether the given delegate is supported
      - return 0 if it's not supported.
      - return 1 if delegate is NULL or it's the name of a supported delegate
     
      Which delegate is supported depends on how this version of Yices was compiled.
     
      Since 2.6.2. *)
    val has_delegate : string -> bool eh

    (** **********************************
     *  BIT-BLAST AND EXPORT TO DIMACS  *
     ***********************************)

    (** Bit-blast then export the CNF to a file
      - f = a Boolean formula (in the QF_BV theory)
      - filename = name of the ouput file
      - simplify_cnf = boolean flag
      - status = pointer to a variable that stores the formula's status
     
      The function bitblasts formula f and exports the resulting CNF to a file in DIMACS format.
      - filename = name of this file
      - simplify_cnf is a flag to enable CNF simplification using the y2sat SAT solver.
        If simplify_cnf is 0, no CNF simplifcation is applied
        If simplify_cnf is not 0, CNF simplification is applied
     
      The bit-vector solver applies various simplifications and preprocessing that may detect
      that f is SAT or UNSAT without generating a CNF. In this case, the function does not
      produce a DIMACS file and the formula status (either STATUS_SAT or STATUS_UNSAT) is
      returned in variable *status.
     
      If simplify_cnf is non-zero, it is also possible for CNF simplification to detect
      that the CNF is sat or unsat. In this case, no DIMACS file is produced and the status
      is returne in variable *status.
     
      Return code:
        1 if the DIMACS file was constructed
        0 if the formula is solved without CNF or after simplifying
       -1 if there's an error
     
      Error reports:
      if f is not a valid term:
        code = INVALID_TERM
        term1 = f
      if f is not a Boolean term
        code = TYPE_MISMATCH
        term1 = f
        type1 = bool (expected type)
      if there's an error when opening or writing to filename
        code = OUTPUT_ERROR
     
      Other errors are possible if f can't be processed by the bitvector solver.
     
      Since 2.6.2. *)

    val export_formula_to_dimacs :
      term_t -> filename:string -> simplify:bool -> (smt_status_t * bool) eh
    (** Bit-blast n formulas then export the CNF to a file
      - f = array of n Boolean formula (in the QF_BV theory)
      - n = number of formulas in f
      - filename = name of the ouput file
      - simplify_cnf = boolean flag
      - stat = pointer to a variable that stores the formula's status
     
      Return code:
        1 if the DIMACS file was constructed
        0 if the formula is solved without CNF or after simplifying
       -1 if there's an error
     
      Error reports: same as for yices_export_formula_to_dimacs.
     
      Since 2.6.2. *)
    val export_formulas_to_dimacs :
      term_t list -> filename:string -> simplify:bool -> (smt_status_t * bool) eh

  module Context : sig

    type t = context_t ptr

    (** ************
        CONTEXTS   *
     *********** *)

    (** A context is a stack of assertions.

        The intended use is:
        1) create a context (empty)
        2) assert one or more formulas in the context.
          (it's allowed to call assert several times before check).
        3) check satisfiability
        4) if the context is satisfiable, optionally build a model
        5) reset the context or call push or pop, then go back to 2
        6) delete the context


        A context can be in one of the following states:
        1) STATUS_IDLE: this is the initial state.
          In this state, it's possible to assert formulas.
          After assertions, the status may change to STATUS_UNSAT (if
          the assertions are trivially unsatisfiable). Otherwise
          the state remains STATUS_IDLE.

        2) STATUS_SEARCHING: this is the context status during search.
          The context moves into that state after a call to 'check'
          and remains in that state until the solver completes
          or the search is interrupted.

        3) STATUS_SAT/STATUS_UNSAT/STATUS_UNKNOWN: status returned after a search
        - STATUS_UNSAT means the assertions are not satisfiable.
        - STATUS_SAT means they are satisfiable.
        - STATUS_UNKNOWN means that the solver could not determine whether
            the assertions are satisfiable or not. This may happen if
            Yices is not complete for the specific logic used (e.g.,
            if the formula includes quantifiers).

        4) STATUS_INTERRUPTED: if the context is in the STATUS_SEARCHING state,
          then it can be interrupted via a call to stop_search.
          The status STATUS_INTERRUPTED indicates that.

        For fine tuning: there are options that determine which internal
        simplifications are applied when formulas are asserted, and
        other options to control heuristics used by the solver.  *)

    (** Create a new context:
        - config is an optional argument that defines the context configuration
        - the configuration specifies which components the context should
          include (e.g., egraph, bv_solver, simplex_solver, etc),
          and which features should be supported (e.g., whether push/pop are
          needed).

        If config is NULL, the default configuration is used:
         push/pop are enabled
         the solvers are: egraph + array solver + bv solver + simplex
         mixed real/integer linear arithmetic is supported

        Otherwise the context is configured as specified by config, provided
        that configuration is valid.

        If there's an error (i.e., the configuration is not supported), the
        function returns NULL and set an error code: CTX_INVALID_CONFIG.  *)
    val malloc : Config.t -> t eh

    (** Deletion  *)
    val free : t -> unit

    (** Get status: return the context's status flag
        - return one of the codes defined in yices_types.h,
          namely one of the constants

          STATUS_IDLE
          STATUS_SEARCHING
          STATUS_UNKNOWN
          STATUS_SAT
          STATUS_UNSAT
          STATUS_INTERRUPTED
        *)
    val status : t -> smt_status

    (** Reset: remove all assertions and restore ctx's
        status to STATUS_IDLE.  *)
    val reset : t -> unit

    (** Push: mark a backtrack point
        - return 0 if this operation is supported by the context
               -1 otherwise

        Error report:
        - if the context is not configured to support push/pop
          code = CTX_OPERATION_NOT_SUPPORTED
        - if the context status is STATUS_UNSAT or STATUS_SEARCHING or STATUS_INTERRUPTED
          code = CTX_INVALID_OPERATION  *)
    val push : t -> unit eh

    (** Pop: backtrack to the previous backtrack point (i.e., the matching
        call to yices_push).
        - return 0 if the operation succeeds, -1 otherwise.

        Error report:
        - if the context is not configured to support push/pop
          code = CTX_OPERATION_NOT_SUPPORTED
        - if there's no matching push (i.e., the context stack is empty)
          or if the context's status is STATUS_SEARCHING
          code = CTX_INVALID_OPERATION  *)
    val pop  : t -> unit eh

    (** Several options determine how much simplification is performed
        when formulas are asserted. It's best to leave them untouched
        unless you really know what you're doing.

        The following functions selectively enable/disable a preprocessing
        option. The current options include:

         var-elim: whether to eliminate variables by substitution

         arith-elim: more variable elimination for arithmetic (Gaussian elimination)

         bvarith-elim: more variable elimination for bitvector arithmetic

         eager-arith-lemmas: if enabled and the simplex solver is used, the simplex
         solver will eagerly generate lemmas such as (x >= 1) => (x >= 0) (i.e.,
         the lemmas that involve two inequalities on the same variable x).

         flatten: whether to flatten nested (or ...)
         if this is enabled the term (or (or a b) (or c d) ) is
         flattened to (or a b c d)

         learn-eq: enable/disable heuristics to learn implied equalities

         keep-ite: whether to eliminate term if-then-else or keep them as terms
        - this requires the context to include the egraph

         break-symmetries: attempt to detect symmetries and add constraints
         to remove them (this can be used only if the context is created for QF_UF)

         assert-ite-bounds: try to determine upper and lower bound on if-then-else
         terms and assert these bounds. For example, if term t is defined as
         (ite c 10 (ite d 3 20)), then the context with include the assertion
         3 <= t <= 20.

        The parameter must be given as a string. For example, to disable var-elim,
        call  yices_context_disable_option(ctx, "var-elim")

        The two functions return -1 if there's an error, 0 otherwise.

        Error codes:
        CTX_UNKNOWN_PARAMETER if the option name is not one of the above.  *)

    val enable_option  : t -> option:string -> unit eh
    val disable_option : t -> option:string -> unit eh

    (** Assert formula t in ctx
        - ctx status must be STATUS_IDLE or STATUS_UNSAT or STATUS_SAT or STATUS_UNKNOWN
        - t must be a boolean term

        If ctx's status is STATUS_UNSAT, nothing is done.

        If ctx's status is STATUS_IDLE, STATUS_SAT, or STATUS_UNKNOWN, then
        the formula is simplified and  asserted in the context. The context
        status is changed  to STATUS_UNSAT if the formula  is simplified to
        'false' or to STATUS_IDLE otherwise.

        This returns 0 if there's no error or -1 if there's an error.

        Error report:
        if t is invalid
         code = INVALID_TERM
         term1 = t
        if t is not boolean
         code = TYPE_MISMATCH
         term1 = t
         type1 = bool (expected type)
        if ctx's status is not STATUS_IDLE or STATUS_UNSAT or STATUS_SAT or STATUS_UNKNOWN
         code = CTX_INVALID_OPERATION
        if ctx's status is neither STATUS_IDLE nor STATUS_UNSAT, and the context is
        not configured for multiple checks
         code = CTX_OPERATION_NOT_SUPPORTED

        Other error codes are defined in yices_types.h to report that t is
        outside the logic supported by ctx.  *)
    val assert_formula : t -> term_t -> unit eh

    (** Assert an array of n formulas t[0 ... n-1]
        - ctx's status must be STATUS_IDLE or STATUS_UNSAT or STATUS_SAT or STATUS_UNKNOWN
        - all t[i]'s must be valid boolean terms.

        The function returns -1 on error, 0 otherwise.

        The error report is set as in the previous function.  *)
    val assert_formulas : t -> term_t list -> unit eh

    (** Add a blocking clause: this is intended to help enumerate different models
        for a set of assertions.
        - if ctx's status is STATUS_SAT or STATUS_UNKNOWN, then a new clause is added to ctx
          to remove the current truth assignment from the search space. After this
          clause is added, the next call to yices_check_context will either produce
          a different truth assignment (hence a different model) or return STATUS_UNSAT.

        - ctx's status flag is updated to STATUS_IDLE (if the new clause is not empty) or
          to STATUS_UNSAT (if the new clause is the empty clause).

        Return code: 0 if there's no error, -1 if there's an error.

        Error report:
        if ctx's status is different from STATUS_SAT or STATUS_UNKNOWN
          code = CTX_INVALID_OPERATION
        if ctx is not configured to support multiple checks
          code = CTX_OPERATION_NOT_SUPPORTED  *)
    val assert_blocking_clause : t -> unit eh

    (** Check satisfiability: check whether the assertions stored in ctx
        are satisfiable.
        - params is an optional structure that stores heuristic parameters.
        - if params is NULL, default parameter settings are used.

        It's better to keep params=NULL unless you encounter performance
        problems.  Then you may want to play with the heuristics to see if
        performance improves.

        The behavior and returned value depend on ctx's current status.

        1) If ctx's status is STATUS_SAT, STATUS_UNSAT, or STATUS_UNKNOWN, the function
          does nothing and just returns the status.

        2) If ctx's status is STATUS_IDLE, then the solver searches for a
          satisfying assignment. If param != NULL, the search parameters
          defined by params are used.

          The function returns one of the following codes:
        - STATUS_SAT: the context is satisfiable
        - STATUS_UNSAT: the context is not satisfiable
        - STATUS_UNKNOWN: satisfiability can't be proved or disproved
        - STATUS_INTERRUPTED: the search was interrupted

          The returned status is also stored as the new ctx's status flag,
          with the following exception. If the context was built with
          mode = INTERACTIVE and the search was interrupted, then the
          function returns STATUS_INTERRUPTED but the ctx's state is restored to
          what it was before the call to 'yices_check_context' and the
          status flag is reset to STATUS_IDLE.

        3) Otherwise, the function does nothing and returns 'STATUS_ERROR',
          it also sets the yices error report (code = CTX_INVALID_OPERATION).  *)
    val check : ?param:param_t ptr -> t -> smt_status

    (** Check satisfiability under assumptions: check whether the
        assertions stored in ctx conjoined with n assumptions is
        satisfiable.
        - params is an optional structure to store heuristic parameters
        - if params is NULL, default parameter settings are used.
        - n = number of assumptions
        - t = array of n assumptions
        - the assumptions t[0] ... t[n-1] must all be valid Boolean terms

        It behaves the same as the previous function.

        If this function returns STATUS_UNSAT, then one can construct an unsat core by
        calling function yices_get_unsat_core. The unsat core is a subset of t[0] ... t[n-1]
        that's inconsistent with ctx.  *)
    val check_with_assumptions : ?param:param_t ptr -> t -> term_t list -> smt_status

    (** Interrupt the search:
        - this can be called from a signal handler to stop the search,
          after a call to yices_check_context to interrupt the solver.

        If ctx's status is STATUS_SEARCHING, then the current search is
        interrupted. Otherwise, the function does nothing.  *)
    val stop : t -> unit

    (** Build a model from ctx
        - keep_subst indicates whether the model should include
          the eliminated variables:
          keep_subst = 0 means don't keep substitutions,
          keep_subst != 0 means keep them
        - ctx status must be STATUS_SAT or STATUS_UNKNOWN

        The function returns NULL if the status isn't SAT or STATUS_UNKNOWN
        and sets an error report (code = CTX_INVALID_OPERATION).

        When assertions are added to the context, the simplifications may
        eliminate variables (cf. simplification options above).  The flag
        'keep_subst' indicates whether the model should keep track of these
        eliminated variables and include their value.

        Example: after the following assertions

          (= x (bv-add y z))
          (bv-gt y 0b000)
          (bg-gt z 0b000)

        variable 'x' gets eliminated. Then a call to 'check_context' will
        return STATUS_SAT and we can ask for a model 'M'
        - if 'keep_subst' is false then the value of 'x' in 'M' is unavailable.
        - if 'keep_subst' is true then the value of 'x' in 'M' is computed,
          based on the value of 'y' and 'z' in 'M'.

        It's always better to set 'keep_subst' true. The only exceptions
        are some of the large SMT_LIB benchmarks where millions of variables
        are eliminated.  In such cases, it saves memory to set 'keep_subst'
        false, and model construction is faster too.  *)
    val get_model : t -> keep_subst:bool -> Model.t eh

    (** *************
        UNSAT CORE  *
     ************ *)

    (** Construct an unsat core and store the result in vector *v.
        - v must be an initialized term_vector

        If ctx status is unsat, this function stores an unsat core in v,
        and returns 0. Otherwise, it sets an error core an returns -1.

        This is intended to be used after a call to
        yices_check_context_with_assumptions that returned STATUS_UNSAT. In
        this case, the function builds an unsat core, which is a subset of
        the assumptions. If there were no assumptions or if the context is UNSAT
        for another reason, an empty core is returned (i.e., v->size is set to 0).

        Error code:
        - CTX_INVALID_OPERATION if the context's status is not STATUS_UNSAT.  *)
    val get_unsat_core : t -> term_t list eh

  end

  module Param : sig

    type t = param_t ptr

    (** SEARCH PARAMETERS  *)

    (** A parameter record is an opaque object that stores various
        search parameters and options that control the heuristics used by
        the solver.

        A parameter structure is created by calling
        - yices_new_param_record(void)
          This returns a parameter structure initialized with default
          settings.

        Then individual parameters can be set using function
        - yices_set_param(s, name, value) where both name and value are
          character strings.
        - an unknown/unsupported parameter name is ignored

        Then the param object can be passed on as argument to yices_check_context.

        When it's no longer needed, the object must be deleted by
        calling yices_free_param_structure(param).  *)

    (** Return a parameter record initialized with default settings.  *)
    val malloc : unit -> t eh

    (** Delete the record param  *)
    val free : t -> unit

    (** Set default search parameters for ctx.  *)
    val default : Context.t -> t -> unit

    (** Set a parameter in record p
        - pname = parameter name
        - value = setting

        The parameters are explained in doc/YICES-LANGUAGE
        (and at http://yices.csl.sri.com/doc/parameters.html)

        Return -1 if there's an error, 0 otherwise.

        Error codes:
        - CTX_UNKNOWN_PARAMETER if pname is not a known parameter name
        - CTX_INVALID_PARAMETER_VALUE if value is not valid for the parameter  *)
    val set : t -> name:string -> value:string -> unit eh

  end

  module PP : sig

    (** *******************
        PRETTY PRINTING   *
     ****************** *)

    (** Pretty print type tau or term t on file f
        - width, height, offset define the print area
        - f = output file to use.
          f must be open and writable.

        - return -1 on error
        - return 0 otherwise.

        - possible error report for yices_pp_type
          code = INVALID_TYPE
          type1 = tau

        - possible error report for yices_pp_term
          code = INVALID_TERM
          term1 = t

        - other errors (for both)
          code = OUTPUT_ERROR if writing to file f failed.
          in this case, errno, perror, etc. can be used for diagnostic.  *)

    val type_file : FILE.t ptr -> type_t -> display:display -> unit eh
    val term_file : FILE.t ptr -> term_t -> display:display -> unit eh

    (** Pretty print an array of terms:
        - f = output file to use
        - n = number of terms in the array a
        - a = array of terms
        - width, height, offset define the print area
        - horiz = Boolean flag that determines the layout

        If horiz is true (non-zero), the terms are printed as follows
           a[0]  a[1] .... a[k]
           a[k+1] ... a[n-1]

        If horiz is false (zero), the terms are printed as follows
           a[0]
           a[1]
            ...
           a[n-1]

        The function first checks whether all terms in a[0... n-1] are
        valid.  If not, it sets the error report:
          code = INVALID_TERM
          term = a[i] (first invalid term in the array)
        and returns -1. Nothing is printed in this case.

        Otherwise, the terms a[0... n-1] are printed in the specified
        print area (some terms may be omitted if the area is too small).
        The function returns 0 unless there's an error while writing to
        file f. In such as case, the function returns -1 and
        set the error report to:
          code = OUTPUT_ERROR
        *)
    val terms_file : FILE.t ptr -> term_t list -> display:display -> layout:bool -> unit eh

    (** Print model mdl on FILE f
        - f must be open/writable

        The model stores a mapping from uninterpreted terms to values.
        This function will print the value of these uninterpreted terms,
        but it will skip the ones that don't have a name.

        To see the value of uninterpreted term x in the model, you have to
        give a name to 'x'. For example, this can be done by creating 'x'
        as follows:

         x = yices_new_uninterpreted_term(<some type>)
         yices_set_term_name(x, "x")

      This function will print a line of the form
         (= <name of x> <value of x in mdl>)
      for every uninterpreted term 'x' that has a  value in the model,
      unless 'x' is a  function term.
     
      For function terms (i.e., if 'x' has a function type), then the yices_print_model
      will show a function definition as follows:
    
        (function f
          (type (-> t_0 ... t_k s))
          (= (f <x_00> ... <x_0k>) v_0)
            ...
          (- (f <x_m0> ... <x_mk>) v_m)
          (default w))
     
      where f is the function name.
      - the type (-> t_0 ... t_k s) is the type of f: the domain is t_0 .... t_k
        and the range is s.
      - in every line (= (f <x_i0> ... x_ik>) v_i):
           x_i0 is a constant of type t_0
           ...
           x_ik is a constant of type t_k
           v_i  is a constant of type s
      - each such line specifies the value v_i of f at some point in its domain.
      - the last entry:  (default w)
        is the default value of f at all points that are not listed above.
     
      Example:
     
        (function b
          (type (-> int bool))
            (= (b 0) true)
            (= (b 1) true)
           (default false))
     
      means that b is a function from int to boo, such that (b 0) and (b 1) are true
      and (b i) is false for any i not equal to 0 or 1.  *)
    
    (** Pretty printing:
        - f = output file to use
        - width, height, offset define the print area

        return -1 on error, 0 otherwise

        Like yices_print_model, this function ignores the uninterpreted terms
        that don't have a name.

        On error:
         code = OUTPUT_ERROR (means that writing to f failed)
         in this case, errno, perror, etc. can be used for diagnostic.  *)
    val model_file : FILE.t ptr -> ?display:display -> model_t ptr -> unit eh

    (** Print the values of n terms in  a model
      - f = output file
      - mdl = model
      - n = number of terms
      - a - array of n terms
     
      The function returns -1 on error, 0 otherwise.
     
      Error report:
      if a[i] is not a valid term:
        code = INVALID_TERM
        term1 = a[i]
     
      Since 2.6.2. *)

    (** Pretty print the values of n terms in  a model
      - f = output file
      - mdl = model
      - n = number of terms
      - a - array of n terms
      - width, height, offset define the print area.
     
      This function is like yices_print_term_values except that is uses pretty printing.
     
      Return code: -1 on error, 0 otherwise
     
     
      Error report:
      if a[i] is not a valid term:
        code = INVALID_TERM
        term1 = a[i]
      if writing to f fails,
        code = OUTPUT_ERROR
        in this case, errno, perror, etc. can be used for diagnostic.
     
      Since 2.6.2. *)
    val term_values_file :
      FILE.t ptr -> ?display:display -> model_t ptr -> term_t list -> unit eh

    (** Variants of the above functions that use file descriptors rather than file pointers.

        These functions return 0 if successful or -1 if there's an error.
        Error codes are set as in the corresponding functions above.

        In particular, if fd is not a valid file descriptor or some other IO error happens,
          code is set to OUTPUT_ERROR
          and errno, perror can be used for diagnostic. *)
    val type_fd        : sint -> type_t -> display:display -> unit eh
    val term_fd        : sint -> term_t -> display:display -> unit eh
    val terms_fd       : sint -> term_t list -> display:display -> layout:bool -> unit eh
    val model_fd       : sint -> ?display:display -> model_t ptr -> unit eh
    (** Since 2.6.2. *)
    val term_values_fd : sint -> ?display:display -> model_t ptr -> term_t list -> unit eh

    
    (** Convert type tau or term t to a string using the pretty printer.
        - width, height, offset define the print area as above.

        - return NULL on error
        - return a '\0' terminated string otherwise
          this string must be deleted by calling yices_free_string when it's no longer used

        - possible error report for yices_type_to_string
          code = INVALID_TYPE
          type1 = tau

        - possible error report for yices_term_to_string
          code = INVALID_TERM
          term1 = t
        *)

    val type_string : type_t -> display:display -> string eh
    val term_string : term_t -> display:display -> string eh

    (** Convert model to a string using the pretty printer.
        - width, height, offset define the print area
          Returns a '\0'-terminated string otherwise. This string must be deleted
          when no longer needed by calling yices_free_string.  *)
    val model_string : model_t ptr -> display:display -> string eh
  end
end
