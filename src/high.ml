[%%import "gmp.mlh"]

open Containers
open Ctypes
[%%if gmp_present]
open Ctypes_zarith
[%%endif]
open Signed
open Unsigned
open Low

(* Abbreviation *)
module type API = High_types.API

module Types = struct
  include Low.Types
  include High_types.Types
end

open Types

module Array = CArray

module List = struct
  include List
  let map f l = rev(rev_map f l) (* Tail-recursive version of map to avoid stack overflows *)
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Monadic fold and map on lists *)
module MList(M : Monad) = struct

  open M
  let (let++) = bind
  
  let fold  (aux : 'a -> 'b -> 'a M.t) =
    let aux sofar c =
      let++ sofar = sofar in
      aux sofar c
    in
    List.fold_left aux

  let map f l =
    let aux sofar c =
      let++ c = f c in
      return(c::sofar)
    in
    List.rev l |> fold aux (return [])
end

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
      | A0(c,t)     -> let+ t = f t in return (A0(c,t))
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

[%%if gmp_present]
let ofZ f = MPZ.of_z <.> f 
let ofQ f = MPQ.of_q <.> f
[%%endif]

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

module type SafeErrorHandling = sig
  type 'a checkable
  type 'a t
  val raise_error : string -> _ t
  val return_sint : 'a sintbase checkable -> 'a sintbase t
  val return_uint : 'a uintbase checkable -> 'a uintbase t
  val return_ptr  : 'a ptr checkable -> 'a ptr t
  val return      : 'a -> 'a t
  val bind        : 'a t -> ('a -> 'b t) -> 'b t
end

module type ErrorHandling = SafeErrorHandling with type 'a checkable := 'a

module ExceptionsErrorHandling = struct
  type 'a t = 'a
  exception YicesException of error_code * error_report
  exception YicesBindingsException of string
  let raise_error s = raise(YicesBindingsException s)
  let aux check t =
    if check t then t
    else raise(YicesException(Error.code(),Error.report()))
  let return_sint t = aux sintcheck t
  let return_uint t = aux uintcheck t
  let return_ptr t  = aux ptrcheck t
  let return x = x
  let bind x f = f x
end

module SumErrorHandling = struct
  type error = Yices of error_code * error_report | Bindings of string [@@ show]
  open Stdlib
  type 'a t = ('a, error) Result.t
  let raise_error s = Error(Bindings s)
  let aux check t =
    if check t then Ok t
    else Error(Yices(Error.code(),Error.report()))
  let return_sint t = aux sintcheck t
  let return_uint t = aux uintcheck t
  let return_ptr t  = aux ptrcheck t
  let return = Result.ok
  let bind = Result.bind
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
  let (<+++>) f g x1 x2 x3 = let+ x = f x1 x2 x3 in g x

  (* Monadic fold and map *)
  open MList(EH)

  (* Conversions to/from bools *)
  let toBool  x     = let<= x = x in return(Conv.bool.read x)
  let toBool1 f a   = f a       |> toBool
  let toBool2 f a b = f a b     |> toBool
  let toBool3 f a b c = f a b c |> toBool

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
  let carray t l = Array.(let c = of_list t l in !>(length c), start c)

  (* Useful abbreviations *)
  type 'a vector = ('a, [`Struct]) structured
[%%if gmp_present]
  type mpz_array = MPZ.t abstract Array.t Array.t
  type mpq_array = MPQ.t abstract Array.t Array.t
[%%endif]

  (* Malloc memory cell(s) for function f to place its result; t specifies type of cell. *)
  module Alloc  : sig
    type ('a,'b) t
    val load : 'b -> (unit,'b) t
    val apply  : ('a, 'b -> 'c) t -> 'b -> ('a, 'c) t
    val alloc  :
      'b typ
      -> ?finalise: ('b ptr -> unit)
      -> ('a, 'b ptr -> 'c) t
      -> ('a * 'b ptr, 'c) t
    val allocN :
      int
      -> 'b typ
      -> ?finalise: ('b Array.t -> unit)
      -> ('a, 'b Array.t -> 'c) t
      -> ('a * 'b Array.t, 'c) t
[%%if gmp_present]
    val allocZ : ('a, MPZ.ptr -> 'c) t -> ('a * MPZ.ptr, 'c) t
    val allocQ : ('a, MPQ.ptr -> 'c) t -> ('a * MPQ.ptr, 'c) t
    val allocZn : int -> ('a, mpz_array -> 'c) t -> ('a * mpz_array, 'c) t
    val allocQn : int -> ('a, mpq_array -> 'c) t -> ('a * mpq_array, 'c) t
[%%endif]
    val allocV : (unit -> 'b vector) -> ('a, 'b vector ptr -> 'c) t -> ('a * 'b vector, 'c) t
    val nocheck: ('c -> 'a -> 'b) -> ('a, 'c) t -> 'b
    val check  : ('c sintbase -> 'a -> 'b) -> ('a, 'c sintbase checkable) t -> 'b EH.t
    val check1 : ('a -> 'b) -> (unit*'a, unit_t checkable) t -> 'b EH.t
    val check2 : ('a1 -> 'a2 -> 'b) -> ((unit*'a1)*'a2, unit_t checkable) t -> 'b EH.t
  end = struct
    
    type ('a,'b) t = 'a * 'b

    let load f = (),f
    let apply (y,f) x = y,f x

    let aux alloc preproc (y,f) =
      let x = alloc () in
      (y,x), (x |> preproc |> f)

    let id x = x

    let alloc hdl ?finalise =
      aux (fun () -> allocate_n hdl ~count:1 ?finalise) id
    let allocN n hdl ?finalise =
      aux (fun () -> Array.make hdl n ?finalise) id

[%%if gmp_present]
    let allocZ a = aux MPZ.make id a
    let allocQ a = aux MPQ.make id a

    let allocZn n a =
      let finalise = CArray.(iter (start <.> MPZ.clear)) in
      let make () = 
        let r = Array.make ~finalise (array 1 MPZ.t) n in
        Array.(iter (start <.> MPZ.init)) r;
        r
      in
      aux make id a
      
    let allocQn n a =
      let finalise = CArray.(iter (start <.> MPQ.clear)) in
      let make () = 
        let r = Array.make ~finalise (array 1 MPQ.t) n in
        Array.(iter (start <.> MPQ.init)) r;
        r
      in
      aux make id a
[%%endif]
    let allocV make  = aux make addr
    
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

[%%if gmp_present]
  (* Conversion to Z.t *)
  let toZ1 f = Alloc.(load f |> allocZ |> check1 MPZ.to_z)
  (* Conversion to Q.t *)
  let toQ1 f = Alloc.(load f |> allocQ |> check1 MPQ.to_q)
[%%endif]
  
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

  module type Names = sig
    type t
    val set     : t -> string -> unit EH.t
    val remove  : string -> unit
    val of_name : string -> t EH.t
    val clear   : t -> unit EH.t
    val to_name : t -> string EH.t
  end

  module Global = struct

    let version    = let* x = yices_version    in toStringR !@x
    let build_arch = let* x = yices_build_arch in toStringR !@x
    let build_mode = let* x = yices_build_mode in toStringR !@x
    let build_date = let* x = yices_build_date in toStringR !@x
    let has_mcsat      = toBool1 yices_has_mcsat
    let is_thread_safe = toBool1 yices_is_thread_safe

    let init  = yices_init
    let exit  = yices_exit
    let reset = yices_reset

    let set_out_of_mem_callback = yices_set_out_of_mem_callback
  end

  module ErrorPrint = struct
    let print    = yices_print_error    <.> return_sint
    let print_fd = yices_print_error_fd <.> return_sint
    let string   = yices_error_string   <.> toString
  end

  module Type = struct

    type t = type_t [@@deriving eq, ord]
    let hash = hash_sint

    module Names = struct
      let set x     = ofString(yices_set_type_name x) <.> toUnit
      let remove    = ofString yices_remove_type_name
      let clear     = yices_clear_type_name <.> toUnit
      let of_name   = ofString yices_get_type_by_name <.> return_sint
      let to_name   = yices_get_type_name <.> toString
    end

    let parse = ofString yices_parse_type <.> return_sint

    let bool = yices_bool_type <.> return_sint
    let int  = yices_int_type  <.> return_sint
    let real = yices_real_type <.> return_sint
    let bv i = yices_bv_type !>i |> return_sint
    let new_scalar ~card  = yices_new_scalar_type !>card |> return_sint
    let new_uninterpreted ?name () =
      let+ r = yices_new_uninterpreted_type() |> return_sint in
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

    let rec ifseries t = function
      | [] -> assert false
      | (f,x)::tail -> let+ b = f t in
        if b then Lazy.force x else ifseries t tail

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
                           | [] -> raise_error
                                     "functions must have at least 1 child"
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

  end

  module Term = struct

    type t = term_t [@@deriving eq,ord]
    let hash = hash_sint

    module Names = struct
      let set     = yices_set_term_name <.> ofString <..> toUnit
      let remove  = yices_remove_term_name |> ofString
      let clear   = yices_clear_term_name <.> toUnit
      let of_name = ofString yices_get_term_by_name <.> return_sint
      let to_name = yices_get_term_name <.> toString
    end

    let parse = ofString yices_parse_term <.> return_sint

    let true0  = yices_true  <.> return_sint
    let false0 = yices_false <.> return_sint
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
[%%if gmp_present]
      let mpz        = yices_mpz |> ofZ <.> return_sint
      let mpq        = yices_mpq |> ofQ <.> return_sint
[%%else]
      let mpq _      = raise_error("Term.Arith.mpq necessitates gmp; yices2_ocaml_bindings were not compiled with gmp support")
[%%endif]
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

[%%if gmp_present]
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
[%%endif]

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
[%%if gmp_present]
      let bvconst_mpz    ~width = yices_bvconst_mpz    !> width |> ofZ <.> return_sint
[%%endif]
      let bvconst_zero   ~width = yices_bvconst_zero   !> width |> return_sint
      let bvconst_one    ~width = yices_bvconst_one    !> width |> return_sint
      let bvconst_minus_one ~width = yices_bvconst_minus_one !> width |> return_sint
      let bvconst_from_list l =
        let l = List.map (fun b -> if b then Signed.SInt.one else Signed.SInt.zero) l in
        ofList1 sint yices_bvconst_from_array l |> return_sint
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
    let children      = yices_term_children <.> TermVector.toList

    let term_ptr2term_opt t = if equal t null_term then None else Some t
    
[%%if gmp_present]
    let sum_component term i =
      let cont q t = MPQ.to_q q, term_ptr2term_opt (!@ t) in
      Alloc.(load ((SInt.of_int <.> yices_sum_component term) i)
             |> allocQ
             |> alloc term_t
             |> check2 cont)
[%%endif]

    let sint2bool = List.map (fun x -> if SInt.(equal x one) then true else false)

    let bvsum_component term i =
      let cont carray t = Array.to_list carray |> sint2bool, term_ptr2term_opt (!@ t) in
      let+ bitwidth = bitsize term in
      Alloc.(load (Array.start <.> (SInt.of_int <.> yices_bvsum_component term) i)
             |> allocN bitwidth sint 
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
[%%if gmp_present]
    let sum_components     = args sum_component
[%%else]
    let sum_components _   = raise_error("Term.sum_components necessitates gmp; yices2_ocaml_bindings were not compiled with gmp support")
[%%endif]

    let proj_index = yices_proj_index <.> toInts
    let proj_arg   = yices_proj_arg   <.> return_sint

    let constructor x =
      let<= x = yices_term_constructor x in
      return(Conv.term_constructor.read x)

    let get_last l =
      let rec aux accu = function
        | []   -> raise_error "Term.get_last expects at least 1 argument, got 0"
        | [h]  -> return(accu, h)
        | h::l -> aux (h::accu) l
      in
      let+ index, last = aux [] l in
      return(List.rev index, last)

    let reveal t = let+ c = constructor t in
      match c with
      | `YICES_CONSTRUCTOR_ERROR -> raise_error "`YICES_CONSTRUCTOR_ERROR"
      | `YICES_BOOL_CONSTANT
      | `YICES_ARITH_CONSTANT
      | `YICES_BV_CONSTANT
      | `YICES_ARITH_ROOT_ATOM 
      | `YICES_SCALAR_CONSTANT
      | `YICES_VARIABLE
      | `YICES_UNINTERPRETED_TERM as c -> return(Term(A0(c,t)))
      | `YICES_SELECT_TERM
      | `YICES_BIT_TERM as c ->
        let+ index = proj_index t in
        let+ arg   = proj_arg t in
        return(Term(Projection(c, index, arg)))
      | `YICES_BV_SUM        -> let+ x = bvsum_components t in return(Term(BV_Sum x))
      | `YICES_ARITH_SUM     -> let+ x = sum_components t   in return(Term(Sum x))

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
          | l -> raise_error("Term.reveal expected 1 argument for `YICES_IS_INT_ATOM, got "^string_of_int(List.length l)^" of them instead")
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
      | `YICES_RDIV as c -> let+ children = children t in
        begin match children with
          | [a;b] -> return(Term(A2(c,a,b)))
          | l -> raise_error("Term.reveal expected 2 arguments, got "^string_of_int(List.length l)^" of them instead")
        end
      | `YICES_ITE_TERM -> let+ children = children t in
        begin match children with
          | [a;b;c] -> return(Term(ITE(a,b,c)))
          | l -> raise_error("Term.reveal expected 3 arguments for `YICES_ITE_TERM, got "^string_of_int(List.length l)^" of them instead")
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
          | [] -> raise_error("Term.reveal expected at least 1 argument for `YICES_APP_TERM, got empty list instead")
        end
      | `YICES_UPDATE_TERM -> let+ children = children t in
        begin match children with
          | array::l -> let+ index, value = get_last l in
            return(Term(Update{array; index; value}))
          | [] -> raise_error("Term.reveal expected at least 2 arguments for `YICES_UPDATE_TERM, got empty list instead")
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
      t |> yices_bv_const_value |> allocL ~n sint |+> (sint2bool <.> return)

    let scalar_const_value   = yices_scalar_const_value
                               <.> alloc1 sint
                               <+> (SInt.to_int <.> return)
[%%if gmp_present]
    let rational_const_value = yices_rational_const_value <.> toQ1
[%%endif]

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
                                       <..> (fun f b -> if b then f SInt.one else f SInt.zero)
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

    let get_bool_value  = yices_get_bool_value
                          <..> alloc1 bool_t
                          <++> (Conv.bool.read <.> return)
    let get_int32_value = yices_get_int32_value <..> alloc1 sint
    let get_int64_value = yices_get_int64_value <..> alloc1 long
    let get_rational32_value = yices_get_rational32_value <..> alloc2 sint uint
    let get_rational64_value = yices_get_rational64_value <..> alloc2 long ulong
[%%if gmp_present]
    let get_mpz_value    = yices_get_mpz_value <..> toZ1
    let get_mpq_value    = yices_get_mpq_value <..> toQ1
[%%endif]
    let get_double_value = yices_get_double_value <..> alloc1 float
    let get_bv_value     = yices_get_bv_value     <..> alloc1 sint
    let get_scalar_value = yices_get_scalar_value <..> alloc1 sint
    let get_value m t    = Alloc.(load (yices_get_value m t)
                                  |> alloc yval_t
                                  |> check1 (fun x -> x))

    let val_is_int32           = yices_val_is_int32      <..> toBool
    let val_is_int64           = yices_val_is_int64      <..> toBool
    let val_is_rational32      = yices_val_is_rational32 <..> toBool
    let val_is_rational64      = yices_val_is_rational64 <..> toBool
    let val_is_integer         = yices_val_is_integer    <..> toBool

    let val_bitsize            = yices_val_bitsize        <..> toIntu
    let val_tuple_arity        = yices_val_tuple_arity    <..> toIntu
    let val_mapping_arity      = yices_val_mapping_arity  <..> toIntu
    let val_function_arity     = yices_val_function_arity <..> toIntu
    let val_function_type      = yices_val_function_type  <..> return_sint

    let val_get_bool           = yices_val_get_bool
                                 <..> alloc1 bool_t
                                 <++> (Conv.bool.read <.> return)
    let val_get_int32          = yices_val_get_int32 <..> alloc1 sint
    let val_get_int64          = yices_val_get_int64 <..> alloc1 long
    let val_get_int            = val_get_int64 <++> (Long.to_int <.> return)
    let val_get_rational32     = yices_val_get_rational32 <..> alloc2 sint uint
    let val_get_rational64     = yices_val_get_rational64 <..> alloc2 long ulong
    let val_get_double         = yices_val_get_double <..> alloc1 float
[%%if gmp_present]
    let val_get_mpz            = yices_val_get_mpz    <..> toZ1
    let val_get_mpq            = yices_val_get_mpq    <..> toQ1
[%%endif]
    let val_get_bv             = yices_val_get_bv     <..> alloc1 sint
    let val_get_scalar         = yices_val_get_scalar <..> alloc2 sint type_t
    let val_expand_tuple m t   =
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
             |> check2 (fun x1 x2 -> x1 |> Array.to_list |> List.map addr, x2))
      
    let formula_true_in_model  = yices_formula_true_in_model <..> toBool
    let formulas_true_in_model = yices_formulas_true_in_model <.> ofList1 term_t <..> toBool
    let get_value_as_term      = yices_get_value_as_term     <..> return_sint
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
    let stop                     = yices_stop_search
    let get_model m ~keep_subst  = yices_get_model m (Conv.bool.write keep_subst) |> return_ptr
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

module Make(EH: ErrorHandling) = SafeMake(struct include Low type 'a checkable = 'a end)(EH)
