open Containers
open Ctypes
open Ctypes_zarith
open Signed
open Unsigned
open Common
open Low

(* Abbreviation *)
module type API = High_types.API

module Types = struct
  include Low.Types
  include High_types.Types
end

open Types

module Array = CArray

(* Monadic map on types *)
module MType(M : Monad) = struct
  open M
  let (let+) = bind
  open MList(M)

  let map f = function
    | Tuple list         -> let+ list = map f list in return(Tuple list) 
    | Fun { dom; codom } ->
       let+ dom = map f dom in
       let+ codom = f codom in
       return(Fun{dom; codom})
    | ty -> return ty

end

exception KN_UNIMPLEMENTED

(* Monadic map on terms *)
module MTerm(M : Monad) = struct
  open M
  let (let+) = bind
  open MList(M)

  let polymap f l =
    let aux (coeff,term_o) =
      let+ t = match term_o with
        | Some t -> let+ t = f t in return(Some t)
        | None -> return None
      in
      return(coeff, t)
    in
    map aux l

  let map : type a. (term_t -> term_t M.t) -> a termstruct -> a termstruct M.t =
    fun f -> function
      | A0 _ as t   -> return t
      | A1(c,t)     -> let+ t = f t in return (A1(c,t))
      | A2(c,t1,t2) ->
        let+ t1 = f t1 in
        let+ t2 = f t2 in
        return (A2(c,t1,t2))
      | ITE(c, tb, eb) -> 
        let+ c  = f c in
        let+ tb = f tb in
        let+ eb = f eb in
        return (ITE(c,tb,eb))
      | Astar(c,l) ->
        let+ l = map f l in
        return (Astar(c,l))
      | Bindings{c;vars;body} ->
        let+ vars = map f vars in
        let+ body = f body in
        return(Bindings{c;vars;body})
      | App(a,l) ->
        let+ a = f a in
        let+ l = map f l in
        return(App(a,l))
      | Update {array; index; value} ->
        let+ array = f array in
        let+ index = map f index in
        let+ value = f value in
        return(Update {array; index; value})
      | Projection(c,i,t) ->
        let+ t = f t in
        return(Projection(c,i,t))
      | BV_Sum l -> let+ l = polymap f l in return(BV_Sum l)
      | Sum l    -> let+ l = polymap f l in return(Sum l)
      | FF_Sum l -> raise KN_UNIMPLEMENTED
      | Product(isBV, l) ->
        let aux (t,p) =
          let+ t = f t in
          return(t,p)
        in
        let+ l = map aux l in
        return(Product(isBV, l))
end

                        
(* Mnemotechnic: ! represents OCaml's int.
   No possibility of error checking in unsigned int conversion *)
let (!>)  = UInt.of_int
let (!<)  = UInt.to_int
(* let (!>>) = ULong.of_int *)
(* let (!<<) = ULong.to_int *)

(* The null pointer for a particular type *)
let null typ = null |> from_voidp typ

(* Composition operators *)
let (<.>) f g x = g(f x)
let (<..>) f g x1 x2 = g (f x1 x2)
let (<...>) f g x1 x2 x3 = g (f x1 x2 x3)

module Algebraic = struct

  open Libpoly

  module DyadicRational = struct
    include DyadicRational
    let to_string = DyadicRational.to_string
    let num = DyadicRational.a
    let pow = DyadicRational.n
  end

  include AlgebraicNumber

  let a_open = interval <.> DyadicInterval.a_open
  let b_open = interval <.> DyadicInterval.b_open 
  let is_point = interval <.> DyadicInterval.is_point
  let a = interval <.> DyadicInterval.a
  let b = interval <.> DyadicInterval.b

  let a_num = a <.> DyadicRational.num
  let a_pow = a <.> DyadicRational.pow
  let b_num = b <.> DyadicRational.num
  let b_pow = b <.> DyadicRational.pow

end
                 

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
  let rec init : type a b. a TypList.t -> (a ListList.t -> b ListList.t) -> b ListList.t
    = fun l cont -> match l with
    | El -> cont El
    | Cons(_,b) -> init b (fun r -> cont(Cons([], r)))
  in
  let rec aux : type a b. a ListList.t -> a AList.t -> (a ListList.t -> b ListList.t) -> b ListList.t
    = fun a b cont -> match a,b with
    | El, El -> cont El
    | Cons(a',a), Cons(b',b) -> aux a b (fun r -> cont(Cons((b'::a'),r)))
  in
  let rec aux2 : type a b. a TypList.t -> a ListList.t -> (a PtrList.t -> b PtrList.t) -> b PtrList.t
    = fun a b cont -> match a, b with
    | El,El -> cont El
    | Cons(a',a),Cons(b',b) -> aux2 a b (fun r -> cont(Cons(Array.(start(of_list a' b')), r)))
  in
  let id x = x in
  aux2 ids (List.fold_left (fun x y -> aux x y id) (init ids id) (List.rev l)) id

let ofList1 t f l = 
  let PtrList.(Cons(b1,El)) = multipack (TypList.build1 t) (List.map AList.build1 l)
  in f !>(List.length l) b1

let ofList2 t1 t2 f l =
  let PtrList.(Cons(b1,Cons(b2,El))) =
    multipack (TypList.build2(t1,t2)) (List.map AList.build2 l)
  in f !>(List.length l) b1 b2

let ofList3 t1 t2 t3 f l =
  let PtrList.(Cons(b1,Cons(b2,Cons(b3,El)))) =
    multipack (TypList.build3(t1,t2,t3)) (List.map AList.build3 l)
  in f !>(List.length l) b1 b2 b3

let swap f a b = f b a

let ofZ f = MPZ.of_z <.> f 
let ofQ f = MPQ.of_q <.> f

module Error = struct
  let code     = yices_error_code <.> Conv.error_code.read
  let report () =
    let r = yices_error_report () in
    let badval = getf !@r (error_report_s#members#badval) |> Long.to_int in
    let code   = getf !@r (error_report_s#members#code) |> Conv.error_code.read in
    let column = getf !@r (error_report_s#members#column) |> UInt.to_int in
    let line   = getf !@r (error_report_s#members#line) |> UInt.to_int in
    let term1  = getf !@r (error_report_s#members#term1) in
    let term2  = getf !@r (error_report_s#members#term2) in
    let type1  = getf !@r (error_report_s#members#type1) in
    let type2  = getf !@r (error_report_s#members#type2) in
    { badval; code; column; line; term1; term2; type1; type2 }
    
  let clear    = yices_clear_error
end

let status_is_not_error = function
  | `STATUS_ERROR -> false
  | _ -> true

module type SafeErrorHandling = sig
  type 'a checkable
  type 'a t
  val raise_yices_error    : unit -> _ t
  val raise_bindings_error : ('a, Format.formatter, unit, _ t) format4 -> 'a
  val return_status : smt_status -> smt_status t
  val return_sint : 'a sintbase checkable -> 'a sintbase t
  val return_uint : 'a uintbase checkable -> 'a uintbase t
  val return_ptr  : 'a ptr checkable -> 'a ptr t
  val return      : 'a -> 'a t
  val bind        : 'a t -> ('a -> 'b t) -> 'b t
end

module type ErrorHandling = SafeErrorHandling with type 'a checkable := 'a

module NoErrorHandling = struct
  type 'a t = 'a
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  let raise_yices_error ()   = raise(YicesException(Error.code(),Error.report()))
  let raise_bindings_error a = Format.ksprintf ~f:(fun s -> raise(YicesBindingsException s)) a
  let return_status t = t
  let return_sint t = t
  let return_uint t = t
  let return_ptr t  = t
  let return x = x
  let bind x f = f x
end

module ExceptionsErrorHandling = struct
  type 'a t = 'a
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  let raise_yices_error ()   = raise(YicesException(Error.code(),Error.report()))
  let raise_bindings_error a = Format.ksprintf ~f:(fun s -> raise(YicesBindingsException s)) a
  let aux check t = if check t then t else raise_yices_error ()
  let return_status = aux status_is_not_error
  let return_sint t = aux sintcheck t
  let return_uint t = aux uintcheck t
  let return_ptr t  = aux ptrcheck t
  let return x = x
  let bind x f = f x
end

module SumErrorHandling = struct
  type error = Yices of error_code * error_report | Bindings of string [@@ show]
  type 'a t = ('a, error) Result.t
  let raise_yices_error ()   = Error(Yices(Error.code(),Error.report()))
  let raise_bindings_error a = Format.ksprintf ~f:(fun s -> Error(Bindings s)) a
  let aux check t =
    if check t then Ok t
    else Error(Yices(Error.code(),Error.report()))
  let return_status = aux status_is_not_error
  let return_sint t = aux sintcheck t
  let return_uint t = aux uintcheck t
  let return_ptr t  = aux ptrcheck t
  let return = Result.return
  let bind = Result.(let*)
end

module SafeMake
    (L : Low_types.API with type 'a Types.sintbase = 'a sintbase
                        and type 'a Types.uintbase = 'a uintbase)
    (EH: SafeErrorHandling with type 'a checkable := 'a L.checkable) = struct

  open L
  open EH

  (* Monadic let constructions for error handling *)
  let (let+) = bind
  let (let<=) b f = (let+) (return_sint b) f
  let (let/=) b f = (let+) (return_uint b) f
  let (let*) b f  = (let+) (return_ptr b) f

  (* Composition operators for error handling *)
  let (|+>) x f            = let+ x = x in f x
  let (<+>) f g x1         = let+ x = f x1 in g x
  let (<++>) f g x1 x2     = let+ x = f x1 x2 in g x
  (* let (<+++>) f g x1 x2 x3 = let+ x = f x1 x2 x3 in g x *)

  (* Monadic fold and map *)
  module ML = MList(EH)
  open ML

  (* Conversions to/from bools *)
  let toBool  x     = let<= x = x in return(Conv.bool.read x)
  let toBool1 f a   = f a       |> toBool
  (* let toBool2 f a b = f a b     |> toBool
   * let toBool3 f a b c = f a b c |> toBool *)

  (* Conversions to/from strings *)
  (* Mnemotechnic: ? represents Ocaml's string, / represents freeing memory *)

  (* Raw conversion from char pointers to strings *)
  let toStringR x = return(coerce (ptr char) string x)

  (* Conversion from char pointers with error handling to a string; the char pointer is freed *)
  let toString x =
    let* x = x in
    let+ r = toStringR x in
    (* yices_free_string x; *) (* This failed when I tried *)
    return r

  (* Conversion from string to char pointer;
     I think the char pointer is automatically freed by garbage collection *)
  let (?>) = coerce string (ptr char)
  let ofString f x = f ?>x

  (* Conversions to unit *)
  let toUnit (x : unit_t checkable) = let<= _ = x in return()

  (* Conversions to int *)
  let toInts x = let<= x = x in return(Conv.sint.read x)
  let toIntu x = let/= x = x in return(Conv.uint.read x)

  (* Turn list into size+pointer; t specifies the type of elements *)
  (* let carray t l = Array.(let c = of_list t l in !>(length c), start c) *)

  (* Useful abbreviations *)
  type 'a vector = ('a, [`Struct]) structured
  type mpz_array = MPZ.t abstract Array.t Array.t
  type mpq_array = MPQ.t abstract Array.t Array.t

  (* Malloc memory cell(s) for a C function to place its result. *)
  module Alloc  : sig

    type ('a,'b) t (* Implemented as a pair of 'a and 'b; we hide this for safety. *)

    val load : 'b -> (unit,'b) t
    (* val apply  : ('a, 'b -> 'c) t -> 'b -> ('a, 'c) t *)

    (* This is the interesting type:
       a function of type ('a,'b,'c) apply
       takes as input a pair of a value v:'a and a function f : 'b -> 'c,
       where f encapsulates a C function
       and 'b is some sort of C-allocated pointer that the C function will use to write an output;
       the function of type ('a,'b,'c) apply allocates memory for that 'b output and call f on it.
       The result is v:'a, the allocated 'b pointer, and the functional result of type 'c.
       The value v:'a does not really play a role in the call, it is there as an accumulator,
       as functions of type ('a,'b,'c) apply are chained.
       Note that the C function actually doing the writes may be called at the end of the chain. 
    *)
    type ('a, 'b, 'c) alloc = ('a, 'b -> 'c) t -> ('a * 'b, 'c) t

    val custom  : (unit -> 'b) -> ('a, 'b, 'c) alloc
    val alloc   :        'b typ -> ?finalise: ('b ptr     -> unit) -> ('a, 'b ptr,     'c) alloc
    val allocN  : int -> 'b typ -> ?finalise: ('b Array.t -> unit) -> ('a, 'b Array.t, 'c) alloc
    val allocZ  :        ('a, MPZ.ptr,   'c) alloc
    val allocQ  :        ('a, MPQ.ptr,   'c) alloc
    val allocZn : int -> ('a, mpz_array, 'c) alloc
    val allocQn : int -> ('a, mpq_array, 'c) alloc

    (* For vectors, things are a bit different:
       the first argument is the make function, producing a vector,
       but the C function expects a pointer.
     *)
    val allocV : (unit -> 'b vector) -> ('a, 'b vector ptr -> 'c) t -> ('a * 'b vector, 'c) t

    (* What to do at the end of the chain: we recover the results and we can do some checking *)
    val nocheck: ('c -> 'a -> 'b)          -> ('a, 'c) t -> 'b
    val check  : ('c sintbase -> 'a -> 'b) -> ('a, 'c sintbase checkable) t -> 'b EH.t
    val check1 : ('a -> 'b)                -> (unit*'a, unit_t checkable) t -> 'b EH.t
    val check2 : ('a1 -> 'a2 -> 'b) -> ((unit*'a1)*'a2, unit_t checkable) t -> 'b EH.t
  end = struct
    
    type ('a,'b) t = 'a * 'b
    type ('a, 'b, 'c) alloc = ('a, 'b -> 'c) t -> ('a * 'b, 'c) t

    let load f = (),f
    (* let apply (y,f) x = y,f x *)

    let aux preproc alloc (y,f) =
      let x = alloc () in
      (y,x), (x |> preproc |> f)

    let id x = x

    let custom make = aux id make

    let alloc hdl ?finalise    = custom (fun () -> allocate_n hdl ~count:1 ?finalise)
    let allocN n hdl ?finalise = custom (fun () -> Array.make hdl n ?finalise)

    let allocZ a = custom MPZ.make a
    let allocQ a = custom MPQ.make a

    let allocZn n =
      let finalise = CArray.(iter (start <.> MPZ.clear)) in
      let make () = 
        let r = Array.make ~finalise (array 1 MPZ.t) n in
        Array.(iter (start <.> MPZ.init)) r;
        r
      in
      custom make
      
    let allocQn n a =
      let finalise = CArray.(iter (start <.> MPQ.clear)) in
      let make () = 
        let r = Array.make ~finalise (array 1 MPQ.t) n in
        Array.(iter (start <.> MPQ.init)) r;
        r
      in
      custom make a
    let allocV make  = aux addr make
    
    let nocheck cont (y,f) = cont f y 
    let check cont (y,f) =
      let<= x = f in
      return (cont x y)

    let check1 cont = check (fun (_ : unit_t) ((),x) -> cont x)
    let check2 cont = check (fun (_ : unit_t) (((),x1),x2) -> cont x1 x2)

  end

  (* Get 1 result *)
  let alloc1 t f     = Alloc.(load f |> alloc t |> check1 (fun x -> !@x))
  (* Get 2 results *)
  let alloc2 t1 t2 f = Alloc.(load f |> alloc t1 |> alloc t2 |> check2 (fun x1 x2 -> !@x1, !@x2))
  (* Get a list of results *)
  let allocL ~n t f   = Alloc.(load (Array.start <.> f)
                               |> allocN n t
                               |> check1 Array.to_list)

  (* Conversion to Z.t *)
  let toZ1 f = Alloc.(load f |> allocZ |> check1 MPZ.to_z)
  (* Conversion to Q.t *)
  let toQ1 f = Alloc.(load f |> allocQ |> check1 MPQ.to_q)
  
  module type Vector = sig
    type t
    type e
    val make     : unit -> t
    val reset    : t -> unit
    val to_array : t -> e Array.t
    val to_list  : t -> e list
    val toList   : (t ptr -> unit_t checkable) -> e list EH.t
  end

  module type VectorArg = sig
    type a
    type t := a vector
    type e
    val o: < ctype   : t typ;
             members : < capacity : (uint, t) field;
                         data : (e ptr, t) field;
                         size : (uint, t) field > >
    val init   : t ptr -> unit
    val delete : t ptr -> unit
    val reset  : t ptr -> unit
  end

  module Vector_Make(M : VectorArg) : Vector with type t := M.a vector
                                              and type e := M.e
  = struct
    let finalise tv = M.delete (addr tv)
    let make () =
      let result = make ~finalise M.o#ctype in
      M.init (addr result);
      result
    let reset tv = M.reset (addr tv)
    let size tv  = getf tv (M.o#members#size)
    let data tv  = getf tv (M.o#members#data)
    let to_array tv = Array.from_ptr (data tv) (UInt.to_int (size tv))
    let to_list  = to_array <.> Array.to_list
    let toList f = Alloc.(load f |> allocV make |> check1 to_list)
  end

  module TypeVector : Vector with type t := type_vector_t and type e := type_t
    = Vector_Make(struct
      type a = [`type_vector_s]
      type e = type_t
      let init   = yices_init_type_vector
      let delete = yices_delete_type_vector
      let reset  = yices_reset_type_vector
      let o      = type_vector_s
    end)

  module TermVector : Vector with type t := term_vector_t and type e := term_t
    = Vector_Make(struct
      type a = [`term_vector_s]
      type e = term_t
      let init   = yices_init_term_vector
      let delete = yices_delete_term_vector
      let reset  = yices_reset_term_vector
      let o      = term_vector_s
    end)

  module YValVector : Vector with type t := yval_vector_t and type e := yval_t
    = Vector_Make(struct
      type a = [`yval_vector_s]
      type e = yval_t
      let init   = yices_init_yval_vector
      let delete = yices_delete_yval_vector
      let reset  = yices_reset_yval_vector
      let o      = yval_vector_s
    end)

  (* Yices High-level bindings *)

  module PP = struct

    let ddisplay = {
      width=80;
      height=10;
      offset=0
    }

    let type_file file ?(display=ddisplay) t
      = yices_pp_type file t !>(display.width) !>(display.height) !>(display.offset) |> toUnit
    let term_file file ?(display=ddisplay) t
      = yices_pp_term file t !>(display.width) !>(display.height) !>(display.offset) |> toUnit
    let terms_file file ?(display=ddisplay) l ~layout
      = (yices_pp_term_array file |>
         ofList1 term_t) l !>(display.width) !>(display.height) !>(display.offset)
        (Conv.bool.write layout) |> toUnit
    let model_file file ?display t
      = match display with
      | Some {width; height; offset} ->
        yices_pp_model file t !>width !>height !>offset |> toUnit
      | _ ->  return(yices_print_model file t)
    let term_values_file file ?display model terms =
      match display with
      | Some {width; height; offset} ->
        (yices_pp_term_values file model |> ofList1 term_t) terms
          !>width !>height !>offset |> toUnit
      | None ->
        (yices_print_term_values file model |> ofList1 term_t) terms
        |> toUnit
    
    let type_fd fd ?(display=ddisplay) t
      = yices_pp_type_fd fd t !>(display.width) !>(display.height) !>(display.offset) |> toUnit
    let term_fd fd ?(display=ddisplay) t
      = yices_pp_term_fd fd t !>(display.width) !>(display.height) !>(display.offset) |> toUnit
    let terms_fd fd ?(display=ddisplay) t ~layout
      = (yices_pp_term_array_fd fd |> ofList1 term_t) t
        !>(display.width) !>(display.height) !>(display.offset)
        (Conv.bool.write layout) |> toUnit
    let model_fd fd ?display t
      = match display with
      | Some {width; height; offset} ->
        yices_pp_model_fd fd t !>width !>height !>offset |> toUnit
      | _ ->  yices_print_model_fd fd t |> toUnit
    let term_values_fd fd ?display model terms =
      match display with
      | Some {width; height; offset} ->
        (yices_pp_term_values_fd fd model |> ofList1 term_t) terms
          !>width !>height !>offset |> toUnit
      | None ->
        (yices_print_term_values_fd fd model |> ofList1 term_t) terms
        |> toUnit

    let type_string ?(display=ddisplay) t
      = yices_type_to_string t !>(display.width) !>(display.height) !>(display.offset)
        |> toString
    let term_string ?(display=ddisplay) t
      = yices_term_to_string t !>(display.width) !>(display.height) !>(display.offset)
        |> toString
    let model_string ?(display=ddisplay) t
      = yices_model_to_string t !>(display.width) !>(display.height) !>(display.offset)
        |> toString

  end

  module ErrorPrint = struct
    let print    = yices_print_error    <.> return_sint
    let print_fd = yices_print_error_fd <.> return_sint
    let string   = yices_error_string   <.> toString
  end

  let rec ifseries t = function
    | [] -> assert false
    | (f,x)::tail -> let+ b = f t in
                     if b then Lazy.force x else ifseries t tail

  module Type = struct

    type t = type_t [@@deriving eq, ord]
    let hash = hash_sint
    let of_hash = of_hash_sint
    let is_good = sintcheck

    module Names = struct
      let set x     = ofString(yices_set_type_name x) <.> toUnit
      let remove    = ofString yices_remove_type_name
      let clear     = yices_clear_type_name <.> toUnit
      let is_name   = (ofString yices_get_type_by_name) <.> sintcheck
      let of_name   = (ofString yices_get_type_by_name) <.> return_sint
      let has_name  = yices_get_type_name <.> ptrcheck
      let to_name   = yices_get_type_name <.> toString
    end

    let parse = ofString yices_parse_type <.> return_sint

    let bool = yices_bool_type <.> return_sint
    let int  = yices_int_type  <.> return_sint
    let real = yices_real_type <.> return_sint
    let bv i = yices_bv_type !>i |> return_sint
    let new_uninterpreted ?name ?card () =
      let+ r =
        (match card with
         | None      -> yices_new_uninterpreted_type()
         | Some card -> yices_new_scalar_type !>card)
        |> return_sint
      in
      match name with
      | Some name -> let+ () = Names.set r name in return r
      | None -> return r

    let tuple = ofList1 type_t yices_tuple_type     <.> return_sint
    let func  = ofList1 type_t yices_function_type <..> return_sint

    let is_bool       = yices_type_is_bool <.> toBool
    let is_int        = yices_type_is_int  <.> toBool
    let is_real       = yices_type_is_real <.> toBool
    let is_arithmetic = yices_type_is_arithmetic <.> toBool
    let is_bitvector  = yices_type_is_bitvector  <.> toBool
    let is_tuple      = yices_type_is_tuple      <.> toBool
    let is_function   = yices_type_is_function   <.> toBool
    let is_scalar     = yices_type_is_scalar <.> toBool
    let is_uninterpreted = yices_type_is_uninterpreted <.> toBool
    let test_subtype     = yices_test_subtype     <..> toBool
    let compatible_types = yices_compatible_types <..> toBool

    let bvsize       = yices_bvtype_size       <.> toIntu
    let scalar_card  = yices_scalar_type_card  <.> toIntu
    let num_children = yices_type_num_children <.> toInts
    let child t      = SInt.of_int <.> yices_type_child t <.> return_sint
    let children     = yices_type_children <.> TypeVector.toList

    let reveal t = ifseries t
        [is_bool, lazy(return Bool);
         is_int,  lazy(return Int);
         is_real, lazy(return Real);
         is_bitvector, lazy(let+ x = bvsize t in return(BV x));
         is_scalar, lazy(return(Scalar t));
         is_uninterpreted, lazy(return(Uninterpreted t));
         is_tuple, lazy(let+ l = children t in return(Tuple l));
         is_function, lazy(let+ x = children t in
                           match List.rev x with
                           | [] -> raise_bindings_error "functions must have at least 1 child"
                           | codom::dom -> let dom = List.rev dom in return(Fun{dom; codom}))]

    let build = function
      | Bool -> bool()
      | Int  -> int()
      | Real -> real()
      | BV n -> bv n
      | Scalar self -> return self
      | Uninterpreted self -> return self
      | Tuple l -> tuple l
      | Fun{dom; codom} -> func dom codom

    include MType(EH)

  end

  module Term = struct

    type t = term_t [@@deriving eq,ord]
    let hash = hash_sint
    let of_hash = of_hash_sint
    let is_good = strict_sintcheck

    module Names = struct
      let set     = yices_set_term_name <.> ofString <..> toUnit
      let remove  = yices_remove_term_name |> ofString
      let clear   = yices_clear_term_name <.> toUnit
      let is_name   = (ofString yices_get_term_by_name) <.> sintcheck
      let of_name   = (ofString yices_get_term_by_name) <.> return_sint
      let has_name  = yices_get_term_name <.> ptrcheck
      let to_name   = yices_get_term_name <.> toString
    end

    let parse = ofString yices_parse_term <.> return_sint

    let true0  = yices_true  <.> return_sint
    let false0 = yices_false <.> return_sint
    let bool = function
      | true -> true0()
      | false -> false0()
    let constant t ~id = yices_constant t (SInt.of_int id) |> return_sint
    let new_uninterpreted ?name typ =
      let+ r = yices_new_uninterpreted_term typ |> return_sint in
      match name with
      | Some name -> let+ () = Names.set r name in return r
      | None -> return r

    let new_variable = yices_new_variable <.> return_sint
    let application a = ofList1 term_t (yices_application a) <.> return_sint

    let ite   = yices_ite <...> return_sint
    let eq    = yices_eq   <..> return_sint
    let neq   = yices_neq  <..> return_sint
    let not1  = yices_not   <.> return_sint
    let or2   = yices_or2  <..> return_sint
    let and2  = yices_and2 <..> return_sint
    let xor2  = yices_xor2 <..> return_sint
    let orN   = ofList1 term_t yices_or  <.> return_sint
    let andN  = ofList1 term_t yices_and <.> return_sint
    let xorN  = ofList1 term_t yices_xor <.> return_sint
    let iff   = yices_iff     <..> return_sint
    let implies = yices_implies <..> return_sint
    let (===) = eq
    let (=/=) = neq
    let (!!)  = not1
    let (|||) = or2
    let (&&&) = and2
    let ( *** ) = xor2
    let (!|)  = orN
    let (!&)  = andN
    let (!^)  = xorN
    let (<=>) = iff
    let (==>) = implies
    let tuple = ofList1 term_t yices_tuple <.> return_sint
    let select i = yices_select !>i        <.> return_sint
    let tuple_update v i = yices_tuple_update v !>i <.> return_sint
    let update f = ofList1 term_t (yices_update f) <..> return_sint
    let distinct = ofList1 term_t yices_distinct <.> return_sint
    let forall = ofList1 term_t yices_forall <..> return_sint
    let exists = ofList1 term_t yices_exists <..> return_sint
    let lambda = ofList1 term_t yices_lambda <..> return_sint

    module Arith = struct
      let zero       = yices_zero  <.> return_sint
      let int32      = yices_int32 <.> return_sint
      let int64      = yices_int64 <.> return_sint
      let int        = Long.of_int <.> int64
      let rational32 = yices_rational32 <..> return_sint
      let rational64 = yices_rational64 <..> return_sint
      let rational n d = rational64 (Long.of_int n) (ULong.of_int d)
      let mpz        = yices_mpz |> ofZ <.> return_sint
      let mpq        = yices_mpq |> ofQ <.> return_sint
      let parse_rational = ofString yices_parse_rational <.> return_sint
      let parse_float = ofString yices_parse_float <.> return_sint
      let add   = yices_add <..> return_sint
      let sub   = yices_sub <..> return_sint
      let neg   = yices_neg <.> return_sint
      let mul   = yices_mul <..> return_sint
      let (++)   = add
      let (--)   = sub
      let (!-)   = neg
      let ( ** ) = mul
      let square = yices_square <.> return_sint
      let power32 = yices_power <..> return_sint
      let power t = UInt.of_int <.> power32 t
      let sum      = ofList1 term_t yices_sum <.> return_sint
      let product  = ofList1 term_t yices_product <.> return_sint
      let division = yices_division <..> return_sint
      let idiv  = yices_idiv <..> return_sint
      let imod  = yices_imod <..> return_sint
      let divides_atom = yices_divides_atom <..> return_sint
      let (^^)  = power
      let (!+)  = sum
      let (!*)  = product
      let (//)  = division
      let (/.)  = idiv
      let (%.)  = imod
      let (||.) = divides_atom

      let is_int_atom = yices_is_int_atom <.> return_sint
      let abs   = yices_abs   <.> return_sint
      let floor = yices_floor <.> return_sint
      let ceil  = yices_ceil  <.> return_sint

      let poly_int32 = ofList2 sint term_t yices_poly_int32 <.> return_sint
      let poly_int64 = ofList2 long term_t yices_poly_int64 <.> return_sint
      let poly_int   = List.map (fun (i,t) -> Long.of_int i, t) <.> poly_int64
      let poly_rational32 = ofList3 sint uint term_t yices_poly_rational32 <.> return_sint
      let poly_rational64 = ofList3 long ulong term_t yices_poly_rational64 <.> return_sint
      let poly_rational   = List.map (fun (n,d,t) -> Long.of_int n, ULong.of_int d, t)
                            <.> poly_rational64

      let poly_mpz l =
        let length = List.length l in
        let to_load zz tt =
          let aux i (z,t) =
            MPZ.set z Array.(start zz.(i));
            Array.set tt i t
          in
          List.iteri aux l;
          yices_poly_mpz !>length (Array.start zz.(0)) (Array.start tt)
        in
        Alloc.(load to_load
               |> allocZn length
               |> allocN length term_t
               |> check (fun r _ -> r))

      let poly_mpq l =
        let length = List.length l in
        let to_load qq tt =
          let aux i (z,t) =
            MPQ.set z Array.(start qq.(i));
            Array.set tt i t
          in
          List.iteri aux l;
          yices_poly_mpq !>length (Array.start qq.(0)) (Array.start tt)
        in
        Alloc.(load to_load
               |> allocQn length
               |> allocN length term_t
               |> check (fun r _ -> r))

      let arith_eq  = yices_arith_eq_atom <..> return_sint
      let arith_neq = yices_arith_neq_atom <..> return_sint
      let geq  = yices_arith_geq_atom <..> return_sint
      let leq  = yices_arith_leq_atom <..> return_sint
      let gt   = yices_arith_gt_atom <..> return_sint
      let lt   = yices_arith_lt_atom <..> return_sint
      let eq0  = yices_arith_eq0_atom <.> return_sint
      let neq0 = yices_arith_neq0_atom <.> return_sint
      let geq0 = yices_arith_geq0_atom <.> return_sint
      let leq0 = yices_arith_leq0_atom <.> return_sint
      let gt0  = yices_arith_gt0_atom <.> return_sint
      let lt0  = yices_arith_lt0_atom <.> return_sint
    end

    module BV = struct
      let bvconst_uint32 ~width = yices_bvconst_uint32 !> width <.> return_sint
      let bvconst_uint64 ~width = yices_bvconst_uint64 !> width <.> return_sint
      let bvconst_int32  ~width = yices_bvconst_int32  !> width <.> return_sint
      let bvconst_int64  ~width = yices_bvconst_int64  !> width <.> return_sint
      let bvconst_int    ~width = Long.of_int <.> bvconst_int64 ~width
      let bvconst_mpz    ~width = yices_bvconst_mpz    !> width |> ofZ <.> return_sint
      let bvconst_zero   ~width = yices_bvconst_zero   !> width |> return_sint
      let bvconst_one    ~width = yices_bvconst_one    !> width |> return_sint
      let bvconst_minus_one ~width = yices_bvconst_minus_one !> width |> return_sint
      let bvconst_from_list =
        List.map Conv.bool.write
        <.> ofList1 bool_t yices_bvconst_from_array
        <.> return_sint
      let parse_bvbin = ofString yices_parse_bvbin <.> return_sint
      let parse_bvhex = ofString yices_parse_bvhex <.> return_sint
      let bvadd = yices_bvadd <..> return_sint
      let bvsub = yices_bvsub <..> return_sint
      let bvneg = yices_bvneg <.> return_sint
      let bvmul = yices_bvmul <..> return_sint
      let bvsquare = yices_bvsquare <.> return_sint
      let bvpower32 = yices_bvpower <..> return_sint
      let bvpower t = UInt.of_int <.> bvpower32 t
      let bvdiv = yices_bvdiv <..> return_sint
      let bvrem = yices_bvrem <..> return_sint
      let bvsdiv = yices_bvsdiv <..> return_sint
      let bvsrem = yices_bvsrem <..> return_sint
      let bvsmod = yices_bvsmod <..> return_sint
      let bvnot = yices_bvnot <.> return_sint
      let bvnand = yices_bvnand <..> return_sint
      let bvnor = yices_bvnor <..> return_sint
      let bvxnor = yices_bvxnor <..> return_sint
      let bvshl = yices_bvshl <..> return_sint
      let bvlshr = yices_bvlshr <..> return_sint
      let bvashr = yices_bvashr <..> return_sint
      let bvand = ofList1 term_t yices_bvand <.> return_sint
      let bvor  = ofList1 term_t yices_bvor <.> return_sint
      let bvxor = ofList1 term_t yices_bvxor <.> return_sint
      let bvsum = ofList1 term_t yices_bvsum <.> return_sint
      let bvproduct = ofList1 term_t yices_bvproduct <.> return_sint
      let shift_left0 t i = yices_shift_left0 t !>i |> return_sint
      let shift_left1 t i = yices_shift_left1 t !>i |> return_sint
      let shift_right0 t i = yices_shift_right0 t !>i |> return_sint
      let shift_right1 t i = yices_shift_right1 t !>i |> return_sint
      let ashift_right t i = yices_ashift_right t !>i |> return_sint
      let rotate_left t i = yices_rotate_left t !>i |> return_sint
      let rotate_right t i = yices_rotate_right t !>i |> return_sint
      let bvextract t i j = yices_bvextract t !>i !>j |> return_sint
      let bvconcat2 = yices_bvconcat2 <..> return_sint
      let bvconcat = ofList1 term_t yices_bvconcat <.> return_sint
      let bvrepeat t i = yices_bvrepeat t !>i |> return_sint
      let sign_extend t i = yices_sign_extend t !>i |> return_sint
      let zero_extend t i = yices_zero_extend t !>i |> return_sint
      let redand = yices_redand <.> return_sint
      let redor = yices_redor <.> return_sint
      let redcomp = yices_redcomp <..> return_sint
      let bvarray = ofList1 term_t yices_bvarray <.> return_sint
      let bitextract t i = yices_bitextract t !>i |> return_sint
      let bveq = yices_bveq_atom <..> return_sint
      let bvneq = yices_bvneq_atom <..> return_sint
      let bvge = yices_bvge_atom <..> return_sint
      let bvgt = yices_bvgt_atom <..> return_sint
      let bvle = yices_bvle_atom <..> return_sint
      let bvlt = yices_bvlt_atom <..> return_sint
      let bvsge = yices_bvsge_atom <..> return_sint
      let bvsgt = yices_bvsgt_atom <..> return_sint
      let bvsle = yices_bvsle_atom <..> return_sint
      let bvslt = yices_bvslt_atom <..> return_sint
    end

    let subst_term = ofList2 term_t term_t yices_subst_term <..> return_sint
    let subst_terms l =
      let aux len ptr =
        let+ () = ofList2 term_t term_t yices_subst_term_array l len ptr |> toUnit in
        Array.(from_ptr ptr !<len |> to_list |> return)
      in 
      ofList1 term_t aux

    let type_of_term = yices_type_of_term <.> return_sint

    let is_bool       = yices_term_is_bool <.> toBool
    let is_int        = yices_term_is_int  <.> toBool
    let is_real       = yices_term_is_real <.> toBool
    let is_arithmetic = yices_term_is_arithmetic <.> toBool
    let is_bitvector  = yices_term_is_bitvector  <.> toBool
    let is_tuple      = yices_term_is_tuple      <.> toBool
    let is_function   = yices_term_is_function   <.> toBool
    let is_scalar     = yices_term_is_scalar     <.> toBool

    let bitsize       = yices_term_bitsize <.> toIntu

    let is_ground     = yices_term_is_ground <.> toBool
    let is_atomic     = yices_term_is_atomic <.> toBool
    let is_composite  = yices_term_is_composite  <.> toBool
    let is_projection = yices_term_is_projection <.> toBool
    let is_sum        = yices_term_is_sum     <.> toBool
    let is_bvsum      = yices_term_is_bvsum   <.> toBool
    let is_product    = yices_term_is_product <.> toBool

    let num_children  = yices_term_num_children <.> toInts

    let child t       = SInt.of_int <.> yices_term_child t <.> return_sint
    (* let children      = yices_term_children <.> TermVector.toList *)
    let children t    =
      let+ n = num_children t in
      let aux sofar i =
        let+ child = child t i in
        return (child::sofar)
      in
      let+ l = fold aux (return []) (List.init n (fun i -> i)) in
      return (List.rev l)

    let term_ptr2term_opt t = if equal t null_term then None else Some t
    
    let sum_component term i =
      let cont q t = MPQ.to_q q, term_ptr2term_opt (!@ t) in
      Alloc.(load ((SInt.of_int <.> yices_sum_component term) i)
                       |> allocQ
             |> alloc term_t
             |> Alloc.check2 cont)

    let bvsum_component term i =
      let cont carray t = Array.to_list carray |> List.map Conv.bool.read, term_ptr2term_opt (!@ t) in
      let+ bitwidth = bitsize term in
      Alloc.(load (Array.start <.> (SInt.of_int <.> yices_bvsum_component term) i)
             |> allocN bitwidth bool_t 
             |> alloc term_t
             |> check2 cont)

    let product_component t i =
      let cont t p = !@ t, !@ p in
      Alloc.(load ((SInt.of_int <.> yices_product_component t) i)
             |> alloc term_t
             |> alloc uint
             |> check2 cont)

    let args f t =
      let+ x = num_children t in
      let rec aux accu i = if i < 0 then return accu else
                             let+ call = f t i in
                             aux (call::accu) (i-1)
      in
      aux [] (x-1)
    let bvsum_components   = args bvsum_component
    let product_components = args product_component
    let sum_components     = args sum_component

    let proj_index = yices_proj_index <.> toInts
    let proj_arg   = yices_proj_arg   <.> return_sint

    let constructor x =
      let<= x = yices_term_constructor x in
      return(Conv.term_constructor.read x)

    let get_last l =
      let rec aux accu = function
        | []   -> raise_bindings_error "Term.get_last expects at least 1 argument, got 0"
        | [h]  -> return(accu, h)
        | h::l -> aux (h::accu) l
      in
      let+ index, last = aux [] l in
      return(List.rev index, last)

    (* Complain about wrong number of arguments for constructor *)
    let raise_args ?(at_least=false) constructor expected args =
      raise_bindings_error
            "Term.reveal expected %s%i argument for %a, got %i of them instead"
            (if at_least then "at least " else "")
            expected
            Types.pp_term_constructor constructor
            (List.length args)
    
    let reveal t = let+ c = constructor t in
      match c with
      | `YICES_CONSTRUCTOR_ERROR -> raise_yices_error ()
      | `YICES_BOOL_CONSTANT
      | `YICES_ARITH_CONSTANT
      | `YICES_BV_CONSTANT
      | `YICES_SCALAR_CONSTANT
      | `YICES_VARIABLE
      | `YICES_UNINTERPRETED_TERM as c -> return(Term(A0(c,t)))
      | `YICES_SELECT_TERM
      | `YICES_BIT_TERM as c ->
        let+ index = proj_index t in
        let+ arg   = proj_arg t in
        return(Term(Projection(c, index, arg)))
      | `YICES_BV_SUM        -> let+ x = bvsum_components t in return(Term(BV_Sum x))
      | `YICES_ARITH_SUM     -> let+ x = sum_components t in
                                return(Term(Sum x))
      | `YICES_ARITH_FF_CONSTANT | `YICES_ARITH_FF_SUM  -> raise KN_UNIMPLEMENTED

      | `YICES_POWER_PRODUCT ->
        let+ x = product_components t in
        let+ typ = type_of_term t in
        let+ b = Type.is_bitvector typ in
        return(Term(Product(b, x)))
      | `YICES_NOT_TERM
      | `YICES_ABS
      | `YICES_CEIL 
      | `YICES_FLOOR 
      | `YICES_IS_INT_ATOM  as c -> let+ children = children t in
        begin match children with
          | [t] -> return(Term(A1(c,t)))
          | l   -> raise_args c 1 l
        end
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
      | `YICES_RDIV
      | `YICES_ARITH_ROOT_ATOM as c -> let+ children = children t in
        begin match children with
          | [a;b] -> return(Term(A2(c,a,b)))
          | l     -> raise_args c 2 l
        end
      | `YICES_ITE_TERM -> let+ children = children t in
        begin match children with
          | [a;b;c] -> return(Term(ITE(a,b,c)))
          | l       -> raise_args c 3 l
        end
      | `YICES_TUPLE_TERM
      | `YICES_DISTINCT_TERM 
      | `YICES_OR_TERM 
      | `YICES_XOR_TERM
      | `YICES_BV_ARRAY as c ->  let+ children = children t in
        return(Term(Astar(c,children)))
      | `YICES_FORALL_TERM
      | `YICES_LAMBDA_TERM as c -> let+ children = children t in
        let+ vars, body = get_last children in
        return(Term(Bindings{ c; vars; body}))
      | `YICES_APP_TERM -> let+ children = children t in
        begin match children with
          | h::l -> return(Term(App(h,l)))
          | []   -> raise_args ~at_least:true c 1 []
        end
      | `YICES_UPDATE_TERM -> let+ children = children t in
        begin match children with
          | array::l -> let+ index, value = get_last l in
            return(Term(Update{array; index; value}))
          | [] -> raise_args ~at_least:true c 2 []
        end

    let build : type a. a termstruct -> term_t EH.t = function
      | A0(_,t) -> return t
      | A1(c,t) ->
        begin
          match c with
          | `YICES_NOT_TERM    -> not1 t
          | `YICES_ABS         -> Arith.abs t
          | `YICES_CEIL        -> Arith.ceil t
          | `YICES_FLOOR       -> Arith.floor t
          | `YICES_IS_INT_ATOM -> Arith.is_int_atom t
        end
      | A2(c,t1,t2) ->
        begin
          match c with
          | `YICES_EQ_TERM       -> eq t1 t2
          | `YICES_ARITH_GE_ATOM -> Arith.geq t1 t2
          | `YICES_BV_ASHR       -> BV.bvashr t1 t2
          | `YICES_BV_DIV        -> BV.bvdiv t1 t2
          | `YICES_BV_GE_ATOM    -> BV.bvge t1 t2
          | `YICES_BV_LSHR       -> BV.bvlshr t1 t2
          | `YICES_BV_REM        -> BV.bvrem t1 t2
          | `YICES_BV_SDIV       -> BV.bvsdiv t1 t2
          | `YICES_BV_SGE_ATOM   -> BV.bvsge t1 t2
          | `YICES_BV_SHL        -> BV.bvshl t1 t2
          | `YICES_BV_SMOD       -> BV.bvsmod t1 t2
          | `YICES_BV_SREM       -> BV.bvsrem t1 t2
          | `YICES_DIVIDES_ATOM  -> Arith.divides_atom t1 t2
          | `YICES_IDIV          -> Arith.idiv t1 t2
          | `YICES_IMOD          -> Arith.imod t1 t2
          | `YICES_RDIV          -> Arith.division t1 t2
          | `YICES_ARITH_ROOT_ATOM -> raise_bindings_error "Cannot build an algebraic term"
        end
      | ITE(c, tb, eb) -> ite c tb eb
      | Astar(c,l) ->
        begin
          match c with
          | `YICES_TUPLE_TERM    -> tuple l
          | `YICES_DISTINCT_TERM -> distinct l
          | `YICES_OR_TERM       -> orN l
          | `YICES_XOR_TERM      -> xorN l
          | `YICES_BV_ARRAY      -> BV.bvarray l
        end
      | Bindings{c;vars;body} ->
        begin
          match c with
          | `YICES_FORALL_TERM -> forall vars body
          | `YICES_LAMBDA_TERM -> lambda vars body
        end
      | App(f,l) -> application f l
      | Update { array; index; value} -> update array index value
      | Projection(c,i,t) ->
        begin
          match c with
          | `YICES_SELECT_TERM -> select i t
          | `YICES_BIT_TERM    -> BV.bitextract t i
        end
      | BV_Sum l ->
        let aux sofar (coeff, term) =
          let+ sofar = sofar in
          let+ coeff_as_term = BV.bvconst_from_list coeff in
          let+ summand = match term with
            | Some term -> BV.bvmul coeff_as_term term
            | None -> return coeff_as_term
          in
          return(summand::sofar)
        in
        List.rev l |> List.fold_left aux (return []) |+> BV.bvsum
      | FF_Sum _ -> raise KN_UNIMPLEMENTED
      | Sum l ->
        let aux sofar (coeff, term) =
          let+ sofar = sofar in
          let+ coeff_as_term = Arith.mpq coeff in
          let+ summand = match term with
            | Some term -> Arith.mul coeff_as_term term
            | None -> return coeff_as_term
          in
          return(summand::sofar)
        in
        List.rev l |> List.fold_left aux (return []) |+> Arith.sum
        (* let+ one = Arith.int 1 in
         * let l = List.map (function coeff, Some t -> coeff, t | coeff, None -> coeff, one) l in
         * Arith.poly_mpq l *)
      | Product(isBV, l) ->
        let generic power32 product =
          let aux sofar (t,p) =
            let+ sofar = sofar in
            let+ pp = power32 t p in
            return(pp::sofar)
          in
          List.rev l |> List.fold_left aux (return []) |+> product
        in
        if isBV then generic BV.bvpower32 BV.bvproduct else generic Arith.power32 Arith.product

    include MTerm(EH)
      
    let bool_const_value = yices_bool_const_value
                           <.> alloc1 bool_t
                           <+> (Conv.bool.read <.> return)
    let bv_const_value t =
      let+ n = bitsize t in
      t |> yices_bv_const_value |> allocL ~n bool_t |+> (List.map Conv.bool.read <.> return)

    let scalar_const_value   = yices_scalar_const_value
                               <.> alloc1 sint
                               <+> (SInt.to_int <.> return)
    let rational_const_value = yices_rational_const_value <.> toQ1

    let const_value (A0(c, t)) =
      match c with
      | `YICES_BOOL_CONSTANT   -> let+ c = bool_const_value t     in return(`Bool c)
      | `YICES_ARITH_CONSTANT  -> let+ c = rational_const_value t in return(`Rational c) 
      | `YICES_BV_CONSTANT     ->
         let+ typ = type_of_term t in
         let+ bitwidth = Type.bvsize typ in
         let+ c = bv_const_value t in
         return(`BV(bitwidth, c))
      | `YICES_SCALAR_CONSTANT ->
         let+ typ = type_of_term t in
         let+ c = scalar_const_value t in
         return(`Scalar(typ, c))
      | `YICES_VARIABLE
        | `YICES_UNINTERPRETED_TERM -> return `SYMBOLIC
      | `YICES_ARITH_FF_CONSTANT -> raise KN_UNIMPLEMENTED

    let const_as_term : atomic_const -> t EH.t = function
      | `Bool true  -> true0()
      | `Bool false -> false0()
      | `Rational q -> Arith.mpq q
      | `BV(_,l)    -> BV.bvconst_from_list l
      | `Scalar(typ, id) -> constant typ ~id

    let yval_as_term val_as_term : yval -> t EH.t = function
      | `Bool _ | `Rational _ | `BV _ | `Scalar _ as s ->
         const_as_term s
      | `Tuple(_,l) ->
         ML.map val_as_term l |+> tuple
      | `Fun { mappings; default; typ; _ } ->
         let+ input_types =
           let+ ytyp = Type.reveal typ in
           match ytyp with
           | Fun{ dom; _ } -> return dom
           | _ -> raise_bindings_error "fun val should have fun type"
         in
         let+ variables = ML.map new_variable input_types in
         let default = val_as_term default in
         let aux sofar mapping =
           let rec aux accu vars vals =
             match vars, vals with
             | [], [] -> return accu
             | var::vars, v::vals ->
                let+ v = val_as_term v in
                let+ cond = (var === v) in
                aux (cond::accu) vars vals
             | _ -> failwith "bug in yval_as_term"
           in
           let+ cond = aux [] variables mapping.args |+> andN in
           let+ value = val_as_term mapping.value in
           ite cond value sofar
         in
         fold aux default mappings |+> lambda variables
      | `Algebraic _algebraic ->
         raise_bindings_error "Cannot convert an algebraic value to a term"

  end

  module Global = struct

    let toString2 x =
      let* x = x in
      let x = !@x in
      if Ctypes.is_null x then EH.raise_bindings_error "String is null pointer"
      else toStringR x

    let version    = toString2 yices_version
    let build_arch = toString2 yices_build_arch
    let build_mode = toString2 yices_build_mode
    let build_date = toString2 yices_build_date
    let has_mcsat      = toBool1 yices_has_mcsat
    let is_thread_safe = toBool1 yices_is_thread_safe

    let cleanup_ocaml = ref (fun ~after:_ -> ())

    let register_cleanup f =
      let current = !cleanup_ocaml in
      let aux ~after = current ~after; f ~after in
      cleanup_ocaml := aux
      
    let create create reset ?after_gc:f capacity =
      let r = create capacity in
      let reset = match f with
        | Some f ->
           (fun ~after   ->
             match after with
             | `GC -> f r
             | _   -> reset r)
        | None   -> fun ~after:_ -> reset r
      in
      register_cleanup reset;
      r

    let hTypes_create ?after_gc cap = create HTypes.create HTypes.reset ?after_gc cap
    let hTerms_create ?after_gc cap = create HTerms.create HTerms.reset ?after_gc cap

    let cleanup_ocaml ~after = !cleanup_ocaml ~after

    let init() =
      yices_init();
      cleanup_ocaml ~after:`Init  

    let reset() =
      yices_reset();
      cleanup_ocaml ~after:`Reset

    let exit  = yices_exit

    let set_out_of_mem_callback = yices_set_out_of_mem_callback
  end

  module GC = struct
    let num_terms   = yices_num_terms <.> UInt.to_int
    let num_types   = yices_num_types <.> UInt.to_int
    let incref_term = yices_incref_term <.> toUnit
    let decref_term = yices_decref_term <.> toUnit
    let incref_type = yices_incref_type <.> toUnit
    let decref_type = yices_decref_type <.> toUnit
    let num_posref_terms = yices_num_posref_terms <.> UInt.to_int
    let num_posref_types = yices_num_posref_types <.> UInt.to_int
    let garbage_collect =
      yices_garbage_collect |> swap |> ofList1 term_t
      <.> swap <.> ofList1 type_t
      <..> (fun f -> Conv.bool.write <.> f)

    let garbage_collect terms types keep_named =
      garbage_collect terms types keep_named;
      Global.cleanup_ocaml ~after:`GC
  end

  module Config = struct
    type t = ctx_config_t ptr
    let malloc = yices_new_config <.> return_ptr
    let free   = yices_free_config
    let set     c ~name ~value = yices_set_config c ?>name ?>value |> toUnit
    let default ?(logic="NONE") c = yices_default_config_for_logic c ?>logic |> toUnit
  end

  module Model = struct
    type t = model_t ptr

    let free     = yices_free_model
    let from_map = yices_model_from_map |> ofList2 term_t term_t <.> return_ptr
    let collect_defined_terms m =
      Alloc.(load (yices_model_collect_defined_terms m)
             |> allocV TermVector.make
             |> nocheck (fun () ((),x) -> TermVector.to_list x))

    let empty = yices_new_model <.> return_ptr

    let set_bool model x = Conv.bool.write <.> yices_model_set_bool model x <.> toUnit
    let set_int32        = yices_model_set_int32 <...> toUnit
    let set_int64        = yices_model_set_int64 <...> toUnit
    let set_int model x  = Long.of_int <.> set_int64 model x
    let set_rational32 model = yices_model_set_rational32 model <...> toUnit
    let set_rational64 model = yices_model_set_rational64 model <...> toUnit
    let set_rational model x n d = set_rational64 model x (Long.of_int n) (ULong.of_int d)
    let set_mpz model x = yices_model_set_mpz model x |> ofZ <.> toUnit
    let set_mpq model x = yices_model_set_mpq model x |> ofQ <.> toUnit

    let set_algebraic_number model x = yices_model_set_algebraic_number model x <.> toUnit

    let set_bv_int32       = yices_model_set_bv_int32 <...> toUnit
    let set_bv_int64       = yices_model_set_bv_int64 <...> toUnit
    let set_bv_int model x = Long.of_int <.> set_bv_int64 model x
    let set_bv_uint32      = yices_model_set_bv_uint32 <...> toUnit
    let set_bv_uint64      = yices_model_set_bv_uint64 <...> toUnit
    let set_bv_mpz model x = yices_model_set_bv_mpz model x |> ofZ <.> toUnit

    let set_bv_from_list model x =
      List.map Conv.bool.write
      <.> ofList1 bool_t (yices_model_set_bv_from_array model x)
      <.> toUnit


    let get_bool_value  = yices_get_bool_value
                          <..> alloc1 bool_t
                          <++> (Conv.bool.read <.> return)
    let get_int32_value = yices_get_int32_value <..> alloc1 sint
    let get_int64_value = yices_get_int64_value <..> alloc1 long
    let get_rational32_value = yices_get_rational32_value <..> alloc2 sint uint
    let get_rational64_value = yices_get_rational64_value <..> alloc2 long ulong
    let get_double_value = yices_get_double_value <..> alloc1 float
    let get_mpz_value    = yices_get_mpz_value <..> toZ1
    let get_mpq_value    = yices_get_mpq_value <..> toQ1

    let algebraic_treat libpoly =
      let a_num = Algebraic.a_num libpoly in
      let a_pow = Algebraic.a_pow libpoly in
      let b_num = Algebraic.b_num libpoly in
      let b_pow = Algebraic.b_pow libpoly in
      let a = Q.((of_bigint a_num) asr a_pow) in
      let b = Q.((of_bigint b_num) asr b_pow) in
      let a_open = Algebraic.a_open libpoly in
      let b_open = Algebraic.b_open libpoly in
      if Algebraic.is_point libpoly
      then { libpoly; a; b; a_open; b_open; degree = -1; coeffs = [] }
      else      
        let f = Libpoly.AlgebraicNumber.f libpoly in
        let degree = Libpoly.UPolynomial.degree f in
        let to_load carray = Libpoly.UPolynomial.unpack f (CArray.start carray) in
        let n = degree+1 in
        let get a i = CArray.get a i |> addr |> MPZ.to_z in
        let coeffs = Alloc.(load to_load
                            |> allocN n MPZ.t
                            |> nocheck (fun () ((),a) -> List.init n (get a))) in
        { libpoly; a; b; a_open; b_open; degree; coeffs }
        

    let get_algebraic_number_value model x =
      Alloc.(load (yices_get_algebraic_number_value model x)
             |> custom Libpoly.AlgebraicNumber.make
             |> check1 algebraic_treat)

    let get_bv_value m t =
      let+ n = Term.bitsize t in
      t |> yices_get_bv_value m |> allocL ~n bool_t |+> (List.map Conv.bool.read <.> return)

    let get_scalar_value = yices_get_scalar_value <..> alloc1 sint
    let get_value m t    = Alloc.(load (yices_get_value m t)
                                  |> alloc yval_t
                                  |> check1 (fun x -> x))

    let val_is_int32       = yices_val_is_int32      <..> toBool
    let val_is_int64       = yices_val_is_int64      <..> toBool
    let val_is_rational32  = yices_val_is_rational32 <..> toBool
    let val_is_rational64  = yices_val_is_rational64 <..> toBool
    let val_is_integer     = yices_val_is_integer    <..> toBool

    let val_bitsize        = yices_val_bitsize        <..> toIntu
    let val_tuple_arity    = yices_val_tuple_arity    <..> toIntu
    let val_mapping_arity  = yices_val_mapping_arity  <..> toIntu
    let val_function_arity = yices_val_function_arity <..> toIntu
    let val_function_type  = yices_val_function_type  <..> return_sint

    let val_get_bool       = yices_val_get_bool
                             <..> alloc1 bool_t
                             <++> (Conv.bool.read <.> return)
    let val_get_int32      = yices_val_get_int32 <..> alloc1 sint
    let val_get_int64      = yices_val_get_int64 <..> alloc1 long
    let val_get_int        = val_get_int64 <++> (Long.to_int <.> return)
    let val_get_rational32 = yices_val_get_rational32 <..> alloc2 sint uint
    let val_get_rational64 = yices_val_get_rational64 <..> alloc2 long ulong
    let val_get_double     = yices_val_get_double <..> alloc1 float
    let val_get_mpz        = yices_val_get_mpz    <..> toZ1
    let val_get_mpq        = yices_val_get_mpq    <..> toQ1

    let val_get_algebraic_number_value model x =
      Alloc.(load (yices_val_get_algebraic_number model x)
             |> alloc Algebraic.t
             |> check1 algebraic_treat)

    let val_get_bv m t =
      let+ n = val_bitsize m t in
      t |> yices_val_get_bv m |> allocL ~n bool_t |+> (List.map Conv.bool.read <.> return)

    let val_get_scalar m t =
      let+ s, typ = yices_val_get_scalar m t |> alloc2 sint type_t in
      return(SInt.to_int s, typ)
                  
    let val_expand_tuple m t =
      let+ arity = val_tuple_arity m t in
      Alloc.(load (Array.start <.> yices_val_expand_tuple m t)
             |> allocN arity yval_t
             |> check1 (Array.to_list <.> List.map addr))
      
    let val_expand_function m t =
      let cont default modifs = default, modifs |> YValVector.to_list |> List.map addr in
      Alloc.(load (yices_val_expand_function m t)
             |> alloc yval_t
             |> allocV YValVector.make
             |> check2 cont)

    let val_expand_mapping m t =
      let+ arity = val_mapping_arity m t in
      Alloc.(load (Array.start <.> yices_val_expand_mapping m t)
             |> allocN arity yval_t
             |> alloc yval_t
             |> check2 (fun x1 x2 -> { args  = x1 |> Array.to_list |> List.map addr;
                                       value = x2 }))
    let val_get_tag t = getf !@t (yval_s#members#node_tag) |> Conv.yval_tag.read

    let reveal m t : yval EH.t =
      match val_get_tag t with
      | `YVAL_FINITEFIELD -> raise KN_UNIMPLEMENTED
        | `YVAL_BOOL -> let+ b  = val_get_bool m t in return(`Bool b)
        | `YVAL_RATIONAL -> let+ q = val_get_mpq m t in return(`Rational q)
        | `YVAL_BV       ->
           let+ bv = val_get_bv m t in
           let+ w  = val_bitsize m t in
           return(`BV(w, bv))

        | `YVAL_SCALAR   -> let+ s, typ = val_get_scalar m t in return(`Scalar(typ, s))

        | `YVAL_FUNCTION ->
           let+ typ   = val_function_type m t in
           let+ arity = val_function_arity m t in
           let+ default, mappings = val_expand_function m t in
           let+ mappings = map (val_expand_mapping m) mappings in 
           return(`Fun { mappings; default; typ; arity })
           
        | `YVAL_TUPLE ->
           let+ arity = val_tuple_arity m t in
           let+ components = val_expand_tuple m t in
           return(`Tuple(arity, components))
           
        | `YVAL_ALGEBRAIC ->
           let+ x = val_get_algebraic_number_value m t in
           return(`Algebraic x)
           
        | `YVAL_MAPPING -> raise_bindings_error "reveal does not apply to mapping values"
        | `YVAL_UNKNOWN -> raise_bindings_error "value is unknown"

    let rec yval_as_term m : yval -> Term.t EH.t = Term.yval_as_term (val_as_term m)
    and val_as_term m v = reveal m v |+> yval_as_term m

    let get_value_as_term m t =
      let c = yices_get_value_as_term m t in
      if sintcheck c then return_sint c
      else get_value m t |+> val_as_term m

    let formula_true_in_model  = yices_formula_true_in_model <..> toBool
    let formulas_true_in_model = yices_formulas_true_in_model <.> ofList1 term_t <..> toBool
    let terms_value m l =
      let n = List.length l in 
      Alloc.(load (Array.start <.> (yices_term_array_value m |> ofList1 term_t) l)
             |> allocN n term_t
             |> check1 Array.to_list)

    let model_term_support  = yices_model_term_support <..> TermVector.toList
    let model_terms_support =
      yices_model_term_array_support <.> ofList1 term_t <..> TermVector.toList
    
    let implicant_for_formula  = yices_implicant_for_formula <..> TermVector.toList
    let implicant_for_formulas = yices_implicant_for_formulas <.> ofList1 term_t <..> TermVector.toList
    let generalize_model m t l gen =
      (yices_generalize_model m t |> ofList1 term_t) l (Conv.yices_gen_mode.write gen)
      |> TermVector.toList
    let generalize_model_list m l1 l2 gen =
      ((yices_generalize_model_array m |> ofList1 term_t) l1
       |> ofList1 term_t) l2 (Conv.yices_gen_mode.write gen)
      |> TermVector.toList

  end

  let opt_ptr t f = function
    | Some logic -> f logic
    | None -> null t

  let (??>) = opt_ptr char (?>)
    
  let check_formula ?logic ?(model=false) ?delegate term =
    let model_ptr =
      if model then allocate (ptr model_t) (null model_t)
      else null (ptr model_t)
    in
    let status = yices_check_formula term ??>logic model_ptr ??>delegate in
    (Conv.smt_status.read status, if model then Some !@model_ptr else None)

  let check_formulas ?logic ?(model=false) ?delegate terms =
    let model_ptr =
      if model then allocate (ptr model_t) (null model_t)
      else null (ptr model_t)
    in
    let status =
      (yices_check_formulas |> swap |> ofList1 term_t) terms ??>logic model_ptr ??>delegate
    in
    (Conv.smt_status.read status, if model then Some !@model_ptr else None)

  let has_delegate = yices_has_delegate |> ofString <.> toBool 

  let export_formula_to_dimacs term ~filename ~simplify =
    Alloc.(load (yices_export_formula_to_dimacs term ?>filename (Conv.bool.write simplify))
           |> alloc smt_status_t
           |> check (fun b ((),x) -> Conv.smt_status.read !@x,Conv.bool.read b))

  let export_formulas_to_dimacs terms ~filename ~simplify =
    Alloc.(load
             ((yices_export_formulas_to_dimacs |> swap |> ofList1 term_t)
                terms ?>filename (Conv.bool.write simplify))
           |> alloc smt_status_t
           |> check (fun b ((),x) -> Conv.smt_status.read !@x,Conv.bool.read b))

  module Context = struct
    type t = context_t ptr
    let malloc ?(config=null ctx_config_t) () = config |> yices_new_context |> return_ptr
    let free   = yices_free_context
    let status = yices_context_status <.> Conv.smt_status.read
    let reset  = yices_reset_context
    let push   = yices_push <.> toUnit
    let pop    = yices_pop <.> toUnit
    let enable_option c ~option  = yices_context_enable_option c ?>option |> toUnit
    let disable_option c ~option = yices_context_disable_option c ?>option |> toUnit
    let assert_formula           = yices_assert_formula <..> toUnit
    let assert_formulas          = yices_assert_formulas <.> ofList1 term_t <..> toUnit
    let assert_blocking_clause   = yices_assert_blocking_clause <.> toUnit
    let check ?param ctx =
      let param = opt_ptr param_t (fun x->x) param in
      yices_check_context ctx param |> Conv.smt_status.read
    let check_with_assumptions ?param ctx =
      let param = opt_ptr param_t (fun x->x) param in
      yices_check_context_with_assumptions ctx param |> ofList1 term_t <.> Conv.smt_status.read
    let check_with_model ?param ctx model =
      let param = opt_ptr param_t (fun x->x) param in
      yices_check_context_with_model ctx param model |> ofList1 term_t <.> Conv.smt_status.read
    let check_with_model_and_hint ?param ctx ~hard ~soft model  =
      let terms = List.append hard soft in
      let m = List.length hard in
      let param = opt_ptr param_t (fun x->x) param in
      (yices_check_context_with_model_and_hint ctx param model |> ofList1 term_t)
        terms !>m |> Conv.smt_status.read
    let set_fixed_var_order ctx =
      yices_mcsat_set_fixed_var_order ctx |> ofList1 term_t <.> Conv.smt_status.read
    let set_initial_var_order ctx =
      yices_mcsat_set_initial_var_order ctx |> ofList1 term_t <.> Conv.smt_status.read

    let check_with_interpolation ?(build_model=true) ?param ctx_A ctx_B =
      let param = opt_ptr param_t (fun x->x) param in
      let extract status ((),interpolation_context) =
        let status = Conv.smt_status.read status in
        match status with
        | #smt_inconclusive_status as x -> x
        | `STATUS_UNSAT ->
           let interpolant =
             getf !@interpolation_context (interpolation_context_s#members#interpolant)
           in
           `STATUS_UNSAT interpolant
        | `STATUS_SAT ->
           if build_model
           then
             let model =
               getf !@interpolation_context (interpolation_context_s#members#model)
             in
             `STATUS_SAT(Some model)
           else
             `STATUS_SAT None
      in
      let to_load interpolation_context =
        setf !@interpolation_context (interpolation_context_s#members#ctx_A) ctx_A;
        setf !@interpolation_context (interpolation_context_s#members#ctx_B) ctx_B;
        yices_check_context_with_interpolation
          interpolation_context
          param
          (Conv.bool.write build_model)
      in
      Alloc.(load to_load
             |> alloc interpolation_context_t
             |> nocheck extract)

    let stop                     = yices_stop_search
    let get_model ?(keep_subst=true) m =
      yices_get_model m (Conv.bool.write keep_subst) |> return_ptr
    let get_unsat_core context   = yices_get_unsat_core context |> TermVector.toList
    let get_model_interpolant    = yices_get_model_interpolant <.> return_sint
  end

  module Param = struct
    type t = param_t ptr
    let malloc  = yices_new_param_record <.> return_ptr
    let free    = yices_free_param_record
    let default = yices_default_params_for_context
    let set p ~name ~value = yices_set_param p ?>name ?>value |> toUnit
  end

end

module Make(EH: ErrorHandling) =
  SafeMake(struct include Low type 'a checkable = 'a end)(EH)
