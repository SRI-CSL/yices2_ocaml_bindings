open Ctypes
open Signed
open Unsigned
open Low

open Types
open API

(* Mnemotechnic: ! represents OCaml's int *)
let (!>)  = UInt.of_int
let (!<)  = UInt.to_int
let (!>>) = ULong.of_int
let (!<<) = ULong.to_int

let toBool r = not Signed.SInt.(equal r zero)
let toBool1 f a   = f a |> toBool
let toBool2 f a b = f a b |> toBool
let toBool3 f a b c = f a b c |> toBool

let (?<) = coerce (ptr char) string
let (?</) x = let r = coerce (ptr char) string x in yices_free_string x; r
let (??</) x =
  match coerce (ptr char) string_opt x with
  | None -> None
  | Some s -> yices_free_string x; Some s

(* Mnemotechnic: ? represents Ocaml's string *)
let (?>) = coerce string (ptr char)
let ofString f x = ?>x |> f

let carray t l = CArray.(let c = of_list t l in !>(length c), start c)

module DepList(M : sig type 'a t end) = struct
  type _ t =
    | El : unit t
    | Cons : 'a M.t * 'b t -> ('a * 'b) t
  let build1 a = Cons(a,El)
  let build2 (a,b) = Cons(a,Cons(b,El))
  let build3 (a,b,c) = Cons(a,Cons(b,Cons(c,El)))
end

module TypList = DepList(struct type 'a t = 'a typ end)
module AList   = DepList(struct type 'a t = 'a end)
module PtrList = DepList(struct type 'a t = 'a ptr end)

let multipack (ids: 'a TypList.t) (l: 'a AList.t list) =
  let module ListList = DepList(struct type 'a t = 'a list end) in
  let rec init : type a. a TypList.t -> a ListList.t = function
    | El -> El
    | Cons(a,b) -> Cons([], init b)
  in
  let rec aux : type a. a ListList.t -> a AList.t -> a ListList.t = fun a b ->
    match a,b with
    | El, El -> El
    | Cons(a',a), Cons(b',b) -> Cons((b'::a'),aux a b)
  in
  let rec aux2 : type a. a TypList.t -> a ListList.t -> a PtrList.t =
    fun a b ->
      match a, b with
      | El,El -> El
      | Cons(a',a),Cons(b',b) -> Cons(CArray.(start(of_list a' b')), aux2 a b)
  in
  aux2 ids (List.fold_left aux (init ids) l)

let toList1 t f l = 
  let PtrList.(Cons(b1,El)) = multipack (TypList.build1 t) (List.map AList.build1 l)
  in f !>(List.length l) b1

let toList2 t1 t2 f l =
  let PtrList.(Cons(b1,Cons(b2,El))) =
    multipack (TypList.build2(t1,t2)) (List.map AList.build2 l)
  in f !>(List.length l) b1 b2

let toList3 t1 t2 t3 f l =
  let PtrList.(Cons(b1,Cons(b2,Cons(b3,El)))) =
    multipack (TypList.build3(t1,t2,t3)) (List.map AList.build3 l)
  in f !>(List.length l) b1 b2 b3

module Global = struct

  let version = ?< !@ yices_version
  let build_arch = ?< !@ yices_build_arch
  let build_mode = ?< !@ yices_build_mode
  let build_date = ?< !@ yices_build_date
  let has_mcsat = toBool1 yices_has_mcsat
  let is_thread_safe = toBool1 yices_is_thread_safe

  let init = yices_init
  let exit = yices_exit
  let reset = yices_reset

  let set_out_of_mem_callback = yices_set_out_of_mem_callback
end

module Error = struct
  let code = yices_error_code <.> coerce error_code_t error_code
  let report = yices_error_report
  let clear = yices_clear_error
  let print = yices_print_error
  let print_fd = yices_print_error_fd
  let string() = ?</(yices_error_string())
end

module TypeVector = struct

  let make () =
    let result = make type_vector_t in
    yices_init_type_vector (addr result);
    result

  let delete tv = yices_delete_type_vector (addr tv)
  let reset tv = yices_reset_type_vector (addr tv)
      
  let size (tv : type_vector_t) = getf tv (type_vector_s#members#size)
  let data (tv : type_vector_t) = getf tv (type_vector_s#members#data)
  let to_array (tv : type_vector_t) = CArray.from_ptr (data tv) (UInt.to_int (size tv))
  let to_list tv = tv |> to_array |> CArray.to_list

  (* let init_type_vector = yices_init_type_vector *)
  (* let delete_type_vector = yices_delete_type_vector *)
  (* let reset_type_vector = yices_reset_type_vector *)
end

module TermVector = struct

  let make () =
    let result = make term_vector_t in
    yices_init_term_vector (addr result);
    result

  let delete tv = yices_delete_term_vector (addr tv)
  let reset tv = yices_reset_term_vector (addr tv)
      
  let size (tv : term_vector_t) = getf tv (term_vector_s#members#size)
  let data (tv : term_vector_t) = getf tv (term_vector_s#members#data)
  let to_array (tv : term_vector_t) = CArray.from_ptr (data tv) (UInt.to_int (size tv))
  let to_list tv = tv |> to_array |> CArray.to_list

  (* let init_term_vector = yices_init_term_vector *)
  (* let delete_term_vector = yices_delete_term_vector *)
  (* let reset_term_vector = yices_reset_term_vector *)
end


module Type = struct

  let bool_type = yices_bool_type
  let int_type = yices_int_type
  let real_type = yices_real_type
  let bv_type = yices_bv_type
  let new_scalar_type ~card = yices_new_scalar_type card
  let new_uninterpreted_type = yices_new_uninterpreted_type
  let tuple_type = toList1 type_t yices_tuple_type
  let function_type = toList1 type_t yices_function_type

  let type_is_bool       = toBool1 yices_type_is_bool
  let type_is_int        = toBool1 yices_type_is_int
  let type_is_real       = toBool1 yices_type_is_real
  let type_is_arithmetic = toBool1 yices_type_is_arithmetic
  let type_is_bitvector  = toBool1 yices_type_is_bitvector
  let type_is_tuple      = toBool1 yices_type_is_tuple
  let type_is_function   = toBool1 yices_type_is_function
  let type_is_scalar     = toBool1 yices_type_is_scalar
  let type_is_uninterpreted = toBool1 yices_type_is_uninterpreted

  let test_subtype       = toBool2 yices_test_subtype
  let compatible_types   = toBool2 yices_compatible_types

  let bvtype_size = yices_bvtype_size
  let scalar_type_card = yices_scalar_type_card
  let type_num_children = yices_type_num_children
  let type_child = yices_type_child
  let type_children = yices_type_children

  type scalar = type_t
  type uninterpreted = type_t
  
  type nonrec t =
    | Bool
    | Int
    | Real
    | BV of int
    | Scalar of scalar
    | Uninterpreted of uninterpreted
    | Tuple of type_t list
    | Fun of { dom : type_t list; codom : type_t }

  let reveal t =
    if type_is_bool t then Bool
    else if type_is_int t then Int
    else if type_is_real t then Real
    else if type_is_bitvector t then BV !<(bvtype_size t)
    else if type_is_scalar t then Scalar t
    else if type_is_uninterpreted t then Uninterpreted t
    else if type_is_tuple t then
      let arg = TypeVector.make() in
      let _ = type_children t (addr arg) in
      let a = TypeVector.to_list arg in
      TypeVector.delete arg;
      Tuple(a)
    else if type_is_function t then
      let arg = TypeVector.make() in
      let _ = type_children t (addr arg) in
      let a = TypeVector.to_list arg in
      TypeVector.delete arg;
      match List.rev a with
      | [] -> assert false
      | codom::dom -> let dom = List.rev dom in Fun{dom; codom}
    else assert false

  let build = function
    | Bool -> bool_type()
    | Int  -> int_type()
    | Real -> real_type()
    | BV n -> bv_type !>n
    | Scalar self -> self
    | Uninterpreted self -> self
    | Tuple l -> tuple_type l
    | Fun{dom; codom} -> function_type dom codom
      
end

module Term = struct

  let yices_true = yices_true
  let yices_false = yices_false
  let constant t ~id = yices_constant t id
  let new_uninterpreted_term = yices_new_uninterpreted_term
  let new_variable = yices_new_variable
  let application a = toList1 term_t (yices_application a)
  
  let ite = yices_ite
  let (===) = yices_eq
  let (=/=) = yices_neq
  let (!!) = yices_not
  let (!|) = toList1 term_t yices_or
  let (!&) = toList1 term_t yices_and
  let (!*) = toList1 term_t yices_xor
  let (<=>) = yices_iff
  let (=>)  = yices_implies
  let tuple = toList1 term_t yices_tuple
  let select i = yices_select !>i
  let tuple_update v i = yices_tuple_update v !>i
  let update f = toList1 term_t (yices_update f)
  let distinct = toList1 term_t yices_distinct
  let forall = toList1 term_t yices_forall
  let exists = toList1 term_t yices_exists
  let lambda = toList1 term_t yices_lambda

  module Arith = struct
    let zero = yices_zero
    let int32 = yices_int32
    let int64 = yices_int64
    let rational32 = yices_rational32
    let rational64 = yices_rational64
    let parse_rational = ofString yices_parse_rational
    let parse_float = ofString yices_parse_float
    let (+) = yices_add
    let (-) = yices_sub
    let (!-) = yices_neg
    let ( * ) = yices_mul
    let square = yices_square
    let (^) = yices_power
    let (!+) = toList1 term_t yices_sum
    let (!*) = toList1 term_t yices_product
    let (/) = yices_division
    let (/.) = yices_idiv
    let (%.) = yices_imod
    let divides_atom = yices_divides_atom

    let is_int_atom = yices_is_int_atom
    let abs = yices_abs
    let floor = yices_floor
    let ceil = yices_ceil

    let poly_int32 = toList2 sint term_t yices_poly_int32
    let poly_int64 = toList2 long term_t yices_poly_int64
    let poly_rational32 = toList3 sint uint term_t yices_poly_rational32
    let poly_rational64 = toList3 long ulong term_t yices_poly_rational64
    let eq = yices_arith_eq_atom
    let neq = yices_arith_neq_atom
    let geq = yices_arith_geq_atom
    let leq = yices_arith_leq_atom
    let gt = yices_arith_gt_atom
    let lt = yices_arith_lt_atom
    let eq0 = yices_arith_eq0_atom
    let neq0 = yices_arith_neq0_atom
    let geq0 = yices_arith_geq0_atom
    let leq0 = yices_arith_leq0_atom
    let gt0 = yices_arith_gt0_atom
    let lt0 = yices_arith_lt0_atom

  end

  module BV = struct
    let bvconst_uint32 ~width = yices_bvconst_uint32 !> width
    let bvconst_uint64 ~width = yices_bvconst_uint64 !> width
    let bvconst_int32 ~width = yices_bvconst_int32 !> width
    let bvconst_int64 ~width = yices_bvconst_int64 !> width
    let bvconst_zero ~width = yices_bvconst_zero !> width
    let bvconst_one ~width = yices_bvconst_one !> width
    let bvconst_minus_one ~width = yices_bvconst_minus_one !> width
    let bvconst_from_array l =
      let l = List.map (fun b -> if b then Signed.SInt.one else Signed.SInt.zero) l in
      toList1 sint yices_bvconst_from_array l
    let parse_bvbin = ofString yices_parse_bvbin
    let parse_bvhex = ofString yices_parse_bvhex
    let bvadd = yices_bvadd
    let bvsub = yices_bvsub
    let bvneg = yices_bvneg
    let bvmul = yices_bvmul
    let bvsquare = yices_bvsquare
    let bvpower a i = yices_bvpower a !>i
    let bvdiv = yices_bvdiv
    let bvrem = yices_bvrem
    let bvsdiv = yices_bvsdiv
    let bvsrem = yices_bvsrem
    let bvsmod = yices_bvsmod
    let bvnot = yices_bvnot
    let bvnand = yices_bvnand
    let bvnor = yices_bvnor
    let bvxnor = yices_bvxnor
    let bvshl = yices_bvshl
    let bvlshr = yices_bvlshr
    let bvashr = yices_bvashr
    let bvand = toList1 term_t yices_bvand
    let bvor  = toList1 term_t yices_bvor
    let bvxor = toList1 term_t yices_bvxor
    let bvsum = toList1 term_t yices_bvsum
    let bvproduct = toList1 term_t yices_bvproduct
    let shift_left0 t i = yices_shift_left0 t !>i
    let shift_left1 t i = yices_shift_left1 t !>i
    let shift_right0 t i = yices_shift_right0 t !>i
    let shift_right1 t i = yices_shift_right1 t !>i
    let ashift_right t i = yices_ashift_right t !>i
    let rotate_left t i = yices_rotate_left t !>i
    let rotate_right t i = yices_rotate_right t !>i
    let bvextract t i j = yices_bvextract t !>i !>j
    let bvconcat2 = yices_bvconcat2
    let bvconcat = toList1 term_t yices_bvconcat
    let bvrepeat t i = yices_bvrepeat t !>i
    let sign_extend t i = yices_sign_extend t !>i
    let zero_extend t i = yices_zero_extend t !>i
    let redand = yices_redand
    let redor = yices_redor
    let redcomp = yices_redcomp
    let bvarray = toList1 term_t yices_bvarray
    let bitextract t i = yices_bitextract t !>i
    let bveq_atom = yices_bveq_atom
    let bvneq_atom = yices_bvneq_atom
    let bvge_atom = yices_bvge_atom
    let bvgt_atom = yices_bvgt_atom
    let bvle_atom = yices_bvle_atom
    let bvlt_atom = yices_bvlt_atom
    let bvsge_atom = yices_bvsge_atom
    let bvsgt_atom = yices_bvsgt_atom
    let bvsle_atom = yices_bvsle_atom
    let bvslt_atom = yices_bvslt_atom
  end
  
  let subst_term = toList2 term_t term_t yices_subst_term
  let subst_term_array l =
    toList1 term_t (toList2 term_t term_t yices_subst_term_array l)
      
  let type_of_term = yices_type_of_term

  let term_is_bool       = toBool1 yices_term_is_bool
  let term_is_int        = toBool1 yices_term_is_int
  let term_is_real       = toBool1 yices_term_is_real
  let term_is_arithmetic = toBool1 yices_term_is_arithmetic
  let term_is_bitvector  = toBool1 yices_term_is_bitvector
  let term_is_tuple      = toBool1 yices_term_is_tuple
  let term_is_function   = toBool1 yices_term_is_function
  let term_is_scalar     = toBool1 yices_term_is_scalar

  let term_bitsize t = !<(yices_term_bitsize t)

  let term_is_ground     = toBool1 yices_term_is_ground
  let term_is_atomic     = toBool1 yices_term_is_atomic
  let term_is_composite  = toBool1 yices_term_is_composite
  let term_is_projection = toBool1 yices_term_is_projection
  let term_is_sum        = toBool1 yices_term_is_sum
  let term_is_bvsum      = toBool1 yices_term_is_bvsum
  let term_is_product    = toBool1 yices_term_is_product

  let bool_const_value = yices_bool_const_value
  let bv_const_value = yices_bv_const_value
  let scalar_const_value = yices_scalar_const_value

  let term_num_children = yices_term_num_children <.> SInt.to_int

  let term_child t        = SInt.of_int <.> yices_term_child t
  let bvsum_component t i =
    let coeff_ptr = allocate_n sint 1 in
    let term_ptr = allocate_n term_t 1 in
    let _ = (SInt.of_int <.> yices_bvsum_component t) i coeff_ptr term_ptr in
    let t = !@ term_ptr in
    let t = if t = null_term then None else Some t in
    !@ coeff_ptr, t
  let product_component t i =
    let exp_ptr = allocate_n uint 1 in
    let term_ptr = allocate_n term_t 1 in
    let _ = (SInt.of_int <.> yices_product_component t) i term_ptr exp_ptr in
    !@ term_ptr, !< !@ exp_ptr

  let term_args f t =
    let rec aux accu i = if i < 0 then accu else aux ((f t i)::accu) (i-1) in
    aux [] (term_num_children t)
  let term_children = term_args term_child
  let bvsum_components = term_args bvsum_component
  let product_components = term_args product_component
  let sum_components t = assert false (* need GMP *)

  let proj_index = yices_proj_index <.> SInt.to_int
  let proj_arg = yices_proj_arg

  let term_constructor = yices_term_constructor <.> coerce term_constructor_t term_constructor

  type a0 = private A0
  type a1 = private A1
  type a2 = private A2
  type a3 = private A3
  type astar = private Astar
  type bindings = private Bindings
  type projection = private Projection
  type app = private App
  type update = private Update
  type sum = private Sum
  type bvsum = private BVSum
  type prod = private Prod
  type 'a composite = private Composite
    
  type 'a termstruct =
    | A0 : [ `YICES_BOOL_CONSTANT
           | `YICES_ARITH_CONSTANT
           | `YICES_BV_CONSTANT
           | `YICES_SCALAR_CONSTANT
           | `YICES_VARIABLE
           | `YICES_UNINTERPRETED_TERM ]
           * term_t -> a0 termstruct
    | A1 : [ `YICES_NOT_TERM
           | `YICES_ABS
           | `YICES_CEIL 
           | `YICES_FLOOR 
           | `YICES_ARITH_ROOT_ATOM 
           | `YICES_IS_INT_ATOM ] * term_t -> a1 composite termstruct
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
           * term_t * term_t -> a2 composite termstruct
    | ITE : term_t * term_t * term_t -> a3 composite termstruct
    | Astar : [ `YICES_TUPLE_TERM
              | `YICES_DISTINCT_TERM 
              | `YICES_OR_TERM 
              | `YICES_XOR_TERM
              | `YICES_BV_ARRAY ] * term_t list -> astar composite termstruct
    | Bindings : { c    : [ `YICES_FORALL_TERM | `YICES_LAMBDA_TERM ];
                   vars : term_t list;
                   body : term_t } -> bindings composite termstruct
    | App    : term_t * term_t list -> app termstruct
    | Update : { array : term_t; index : term_t list; value : term_t} -> update composite termstruct
    | Projection : [ `YICES_SELECT_TERM | `YICES_BIT_TERM ] * int * term_t -> projection termstruct
    | BV_Sum    : (sint * (term_t option)) list -> bvsum termstruct
    | Sum       : (sint * (term_t option)) list -> sum termstruct
    | Product   : (term_t * int) list -> prod termstruct

  type t = Term : _ termstruct -> t [@@unboxed]

  let get_last l =
    let rec aux accu = function
      | []   -> assert false
      | [h]  -> accu, h
      | h::l -> aux (h::accu) l
    in
    let index, last = aux [] l in
    List.rev index, last

  let reveal t = match term_constructor t with
    | `YICES_CONSTRUCTOR_ERROR -> assert false
    | `YICES_BOOL_CONSTANT
    | `YICES_ARITH_CONSTANT
    | `YICES_BV_CONSTANT
    | `YICES_SCALAR_CONSTANT
    | `YICES_VARIABLE
    | `YICES_UNINTERPRETED_TERM as c -> Term(A0(c,t))
    | `YICES_SELECT_TERM
    | `YICES_BIT_TERM as c -> Term(Projection(c, proj_index t, proj_arg t))
    | `YICES_BV_SUM        -> Term(BV_Sum(bvsum_components t))
    | `YICES_ARITH_SUM     -> Term(Sum(sum_components t))
    | `YICES_POWER_PRODUCT -> Term(Product(product_components t))
    | `YICES_NOT_TERM
    | `YICES_ABS
    | `YICES_CEIL 
    | `YICES_FLOOR 
    | `YICES_ARITH_ROOT_ATOM 
    | `YICES_IS_INT_ATOM  as c -> (match term_children t with [t] -> Term(A1(c,t)) | _ -> assert false)
    | `YICES_EQ_TERM 
    | `YICES_ARITH_GE_ATOM 
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
    | `YICES_DIVIDES_ATOM 
    | `YICES_IDIV 
    | `YICES_IMOD 
    | `YICES_RDIV as c -> (match term_children t with [a;b] -> Term(A2(c,a,b)) | _ -> assert false)
    | `YICES_ITE_TERM -> (match term_children t with [a;b;c] -> Term(ITE(a,b,c)) | _ -> assert false)
    | `YICES_TUPLE_TERM
    | `YICES_DISTINCT_TERM 
    | `YICES_OR_TERM 
    | `YICES_XOR_TERM
    | `YICES_BV_ARRAY as c -> Term(Astar(c,term_children t))
    | `YICES_FORALL_TERM
    | `YICES_LAMBDA_TERM as c -> let vars, body = get_last(term_children t) in Term(Bindings{ c; vars; body})
    | `YICES_APP_TERM         -> (match term_children t with h::l -> Term(App(h,l)) | _ -> assert false)
    | `YICES_UPDATE_TERM ->
      (match term_children t with array::l -> let index, value = get_last l in Term(Update{array; index; value}) | _ -> assert false)


end

module Name = struct

  let set_type_name x = ofString(yices_set_type_name x)
  let set_term_name x = ofString(yices_set_term_name x)
  let remove_type_name = ofString yices_remove_type_name
  let remove_term_name = ofString yices_remove_term_name
  let get_type_by_name = ofString yices_get_type_by_name
  let get_term_by_name = ofString yices_get_term_by_name
  let clear_type_name = yices_clear_type_name
  let clear_term_name = yices_clear_term_name
  let get_type_name x = ??</(yices_get_type_name x)
  let get_term_name x = ??</(yices_get_term_name x)

end

let parse_type = ofString yices_parse_type
let parse_term = ofString yices_parse_term

module GC = struct

  let num_terms = yices_num_terms
  let num_types = yices_num_types
  let incref_term = yices_incref_term
  let decref_term = yices_decref_term
  let incref_type = yices_incref_type
  let decref_type = yices_decref_type
  let num_posref_terms = yices_num_posref_terms
  let num_posref_types = yices_num_posref_types
  let garbage_collect = yices_garbage_collect

end

module Config = struct
  let new_config = yices_new_config
  let free_config = yices_free_config
  let set_config c ~name ~value = yices_set_config c ?>name ?>value
  let default_config_for_logic c ~logic = yices_default_config_for_logic c ?>logic
end

let new_context = yices_new_context
let free_context = yices_free_context
let context_status = yices_context_status <.> coerce smt_status_t smt_status
let reset_context = yices_reset_context
let push = yices_push
let pop = yices_pop
let context_enable_option c ~option = yices_context_enable_option c ?>option
let context_disable_option c ~option = yices_context_disable_option c ?>option
let assert_formula = yices_assert_formula
let assert_formulas = yices_assert_formulas
let check_context a b = yices_check_context a b |> coerce smt_status_t smt_status
let check_context_with_assumptions a b c d = yices_check_context_with_assumptions a b c d |> coerce smt_status_t smt_status
let assert_blocking_clause = yices_assert_blocking_clause
let stop_search = yices_stop_search
let new_param_record = yices_new_param_record
let default_params_for_context = yices_default_params_for_context
let set_param p ~name ~value = yices_set_param p ?>name ?>value
let free_param_record = yices_free_param_record
let get_unsat_core = yices_get_unsat_core

module Model = struct

  let get_model = yices_get_model
  let free_model = yices_free_model
  let model_from_map = yices_model_from_map
  let model_collect_defined_terms = yices_model_collect_defined_terms
  let get_bool_value = toBool3 yices_get_bool_value
  let get_int32_value = yices_get_int32_value
  let get_int64_value = yices_get_int64_value
  let get_rational32_value = yices_get_rational32_value
  let get_rational64_value = yices_get_rational64_value
  let get_double_value = yices_get_double_value
  let get_bv_value = yices_get_bv_value
  let get_scalar_value = yices_get_scalar_value
  let init_yval_vector = yices_init_yval_vector
  let delete_yval_vector = yices_delete_yval_vector
  let reset_yval_vector = yices_reset_yval_vector
  let get_value = yices_get_value

  let val_is_int32 = toBool2 yices_val_is_int32
  let val_is_int64 = toBool2 yices_val_is_int64
  let val_is_rational32 = toBool2 yices_val_is_rational32
  let val_is_rational64 = toBool2 yices_val_is_rational64
  let val_is_integer = toBool2 yices_val_is_integer

  let val_bitsize = yices_val_bitsize
  let val_tuple_arity = yices_val_tuple_arity
  let val_mapping_arity = yices_val_mapping_arity
  let val_function_arity = yices_val_function_arity
  let val_function_type = yices_val_function_type
  let val_get_bool = yices_val_get_bool
  let val_get_int32 = yices_val_get_int32
  let val_get_int64 = yices_val_get_int64
  let val_get_rational32 = yices_val_get_rational32
  let val_get_rational64 = yices_val_get_rational64
  let val_get_double = yices_val_get_double
  let val_get_bv = yices_val_get_bv
  let val_get_scalar = yices_val_get_scalar
  let val_expand_tuple = yices_val_expand_tuple
  let val_expand_function = yices_val_expand_function
  let val_expand_mapping = yices_val_expand_mapping
  let formula_true_in_model = yices_formula_true_in_model
  let formulas_true_in_model = yices_formulas_true_in_model
  let get_value_as_term = yices_get_value_as_term
  let term_array_value = yices_term_array_value
  let implicant_for_formula = yices_implicant_for_formula
  let implicant_for_formulas = yices_implicant_for_formulas
  let generalize_model m t l gen = toList1 term_t (yices_generalize_model m t) l (coerce yices_gen_mode yices_gen_mode_t gen)
  let generalize_model_array  m l1 l2 gen = toList1 term_t (toList1 term_t (yices_generalize_model_array m) l1) l2 (coerce yices_gen_mode yices_gen_mode_t gen)

end

module PP = struct

  let pp_type file t ~width ~height ~offset = yices_pp_type file t !>width !>height !>offset
  let pp_term file t ~width ~height ~offset = yices_pp_term file t !>width !>height !>offset
  let pp_term_array file l ~width ~height ~offset = toList1 term_t (yices_pp_term_array file) l !>width !>height !>offset
  let print_model = yices_print_model
  let pp_model file t ~width ~height ~offset = yices_pp_model file t !>width !>height !>offset
  let pp_type_fd file t ~width ~height ~offset = yices_pp_type_fd file t !>width !>height !>offset
  let pp_term_fd file t ~width ~height ~offset = yices_pp_term_fd file t !>width !>height !>offset
  let pp_term_array_fd file t ~width ~height ~offset = toList1 term_t (yices_pp_term_array_fd file) t !>width !>height !>offset
  let print_model_fd = yices_print_model_fd
  let pp_model_fd file t ~width ~height ~offset = yices_pp_model_fd file t !>width !>height !>offset
  let type_to_string t ~width ~height ~offset = ?<(yices_type_to_string t !>width !>height !>offset)
  let term_to_string t ~width ~height ~offset = ?<(yices_term_to_string t !>width !>height !>offset)
  let model_to_string t ~width ~height ~offset = ?<(yices_model_to_string t !>width !>height !>offset)

end
