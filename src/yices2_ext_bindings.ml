[%%import "gmp.mlh"]

open Containers

open Sexplib
open Type
    
open Yices2_high
open Types

module List = struct
  include List
  let map f l = List.rev (List.fold_left (fun sofar a -> f a::sofar) [] l)
end

module StringHashtbl = CCHashtbl.Make(String)

exception PopLastLevel

let pp_string c fmt () = Format.string fmt c

let sexp f arg = List(Atom f::arg)
let rec pp_sexp fmt = function
  | Atom s -> Format.fprintf fmt "@[%s@]" s
  | List l -> Format.fprintf fmt "@[<hv 1>(%a)@]" (List.pp ~pp_sep:(pp_string " ") pp_sexp) l

include Make(ExceptionsErrorHandling)

module Type = struct
  include Type
  let pph height fmt t =
    let display = Types.{ width = 80; height; offset = 0 } in
    try
      t |> PP.type_string ~display
      |> Format.fprintf fmt "%s"
    with _ -> Format.fprintf fmt "null_type"

  let rec to_sexp typ =
    match reveal typ with
    | Bool -> Atom "Bool"
    | Int  -> Atom "Int"
    | Real -> Atom "Real"
    | BV i -> List[Atom "_"; Atom "BitVec"; Atom(string_of_int i)]
    | Scalar _
    | Uninterpreted _ -> Atom(PP.type_string ~display:Types.{width = 80; height = 10000; offset = 0} typ)
    | Tuple l -> l |> List.map to_sexp |> sexp "tuple"
    | Fun { dom; codom } ->
      let dom = List.map to_sexp dom in
      match dom with
      | [dom] -> sexp "Array" [dom; to_sexp codom]
      | _ -> sexp "Array" [ List dom; to_sexp codom]

  let pp fmt t = t |> to_sexp |> pp_sexp fmt

end

module TMP = struct
  include Term

  let pph height fmt t =
    let display = Types.{ width = 80; height; offset = 0 } in
    try
      t |> PP.term_string ~display
      |> Format.fprintf fmt "%s"
    with _ -> Format.fprintf fmt "null_term"

  let pow i t = 
    if i = 0 then t
    else
      let rec aux i accu =
        if i = 0 then accu else aux i (t::accu) 
      in
      sexp "*" (aux i [])

  (* For bitvector terms *)
  let width y = Type.bvsize(Term.type_of_term y)

  (*************************)
  (* Free Variable testing *)
  (*************************)

  open MTerm(struct
      include Option
      let bind = ( >>= )
    end)

  module HPairs = Hashtbl.Make(struct
      type t = Term.t * Term.t [@@deriving eq]
      let hash = Hash.pair Term.hash Term.hash
    end)

  let free_table = HPairs.create 500

  let rec fv_termstruct : type a. Term.t -> a termstruct -> bool = fun x -> function
    | A0(_,t) -> Term.equal t x
    | a    ->
      match map (fv_aux x) a with
      | Some _ -> false
      | None   -> true
  and fv_aux x t =
    match HPairs.find_opt free_table (x,t) with
    | Some b -> b
    | None ->
      let Term a = Term.reveal t in
      let answer = if fv_termstruct x a then None else Some t in
      HPairs.add free_table (x,t) answer;
      answer

  let fv x t = match fv_aux x t with
    | Some _ -> false
    | None -> true

[%%if gmp_present]
  let sum_aux =
    let one = Term.Arith.int 1 in
    fun to_sexp (coeff, term) ->
    let coeff = Term.Arith.mpq coeff in
    match term with
    | Some term ->
       if Term.equal coeff one then to_sexp term
       else sexp "*" [to_sexp coeff; to_sexp term]
    | None -> to_sexp coeff
[%%else]
  let sum_aux _ = ExceptionsErrorHandling.raise_error("Term.sum_aux necessitates gmp; yices2_ocaml_bindings were not compiled with gmp support")
[%%endif]

  let to_sexp_termstruct : type a. (t -> Sexp.t) -> a termstruct -> Sexp.t =
    fun to_sexp -> function
    | A0(_, t) ->
      let s = PP.term_string ~display:Types.{width = 800000; height = 10000; offset = 0} t in
      let s =
        if String.length s < 2 then s
        else
          match String.sub s 0 2 with
          | "0b" -> "#b"^String.sub s 2 (String.length s -2)
          | "0x" -> "#x"^String.sub s 2 (String.length s -2)
          | _ -> s
      in
      Atom s

    | A1(c,t) ->
      let t = [to_sexp t] in
      begin
        match c with
        | `YICES_NOT_TERM    -> sexp "not" t
        | `YICES_ABS         -> sexp "abs" t
        | `YICES_CEIL        -> sexp "ceil" t
        | `YICES_FLOOR       -> sexp "floor" t
        | `YICES_IS_INT_ATOM -> sexp "is-int" t
      end
    | A2(c,t1,t2) ->
      let args = [to_sexp t1; to_sexp t2] in
      begin
        match c with
        | `YICES_EQ_TERM       -> sexp "=" args
        | `YICES_ARITH_GE_ATOM -> sexp ">=" args
        | `YICES_BV_ASHR       -> sexp "bvashr" args
        | `YICES_BV_DIV        -> sexp "bvdiv" args
        | `YICES_BV_GE_ATOM    -> sexp "bvuge" args
        | `YICES_BV_LSHR       -> sexp "bvlshr" args
        | `YICES_BV_REM        -> sexp "bvurem" args
        | `YICES_BV_SDIV       -> sexp "bvsdiv" args
        | `YICES_BV_SGE_ATOM   -> sexp "bvsge" args
        | `YICES_BV_SHL        -> sexp "bvshl" args
        | `YICES_BV_SMOD       -> sexp "bvsmod" args
        | `YICES_BV_SREM       -> sexp "bvsrem" args
        | `YICES_DIVIDES_ATOM  -> sexp "divides_atom" args
        | `YICES_IDIV          -> sexp "div" args
        | `YICES_IMOD          -> sexp "mod" args
        | `YICES_RDIV          -> sexp "div" args
      end
    | ITE(c, tb, eb) ->
      let args = List.map to_sexp [c;tb;eb] in 
      sexp "ite" args
    | Astar(c,l) ->
      let args = List.map to_sexp l in 
      begin
        match c with
        | `YICES_TUPLE_TERM    -> sexp "tuple" args
        | `YICES_DISTINCT_TERM -> sexp "distinct" args
        | `YICES_OR_TERM       -> sexp "or" args
        | `YICES_XOR_TERM      -> sexp "xor" args
        | `YICES_BV_ARRAY      -> sexp "bool-to-bv" args (* Not official SMTLib syntax *)
      end
    | Bindings{c;vars;body} ->
      let aux v = List[to_sexp v; Type.to_sexp(type_of_term v)] in  
      let vars = List(List.map aux vars) in
      let body = to_sexp body in
      begin
        match c with
        | `YICES_FORALL_TERM -> sexp "forall" [vars; body]
        | `YICES_LAMBDA_TERM -> sexp "lambda" [vars; body]
      end
    | App(f,l) ->
      let f = to_sexp f in
      let l = List.map to_sexp l in
      begin
        match l with
        | [a] -> sexp "select" [f;a]
        | l -> sexp "select" [f;List l]
      end
    | Update { array; index; value} ->
      let array = to_sexp array in
      let indices = List.map to_sexp index in
      let value = to_sexp value in
      begin
        match indices with
        | [index] -> sexp "store" [array;index;value]
        | _ -> sexp "store" [array;List indices;value]
      end
    | Projection(c,i,t) ->
      let t = to_sexp t in
      begin
        match c with
        | `YICES_SELECT_TERM -> sexp "select" [Atom(string_of_int i);t]
        | `YICES_BIT_TERM    -> sexp "bitextract" [Atom(string_of_int i);t] (* Not official SMTLib syntax *)
      end
    | BV_Sum l ->
      let width = match l with
        | (coeff,_)::_ -> List.length coeff
        | [] -> assert false
      in
      let one  = Term.BV.bvconst_one ~width in
      let aux (coeff, term) =
        let coeff = Term.BV.bvconst_from_list coeff in
        match term with
        | Some term ->
          if Term.equal coeff one then to_sexp term
          else sexp "bvmul" [to_sexp coeff; to_sexp term]
        | None -> to_sexp coeff
      in
      sexp "bvadd" (List.map aux l) 

    | Sum l ->
      sexp "+" (List.map (sum_aux to_sexp) l) 

    | Product(isBV, l) ->
      let aux (term, exp) = pow (Unsigned.UInt.to_int exp) (to_sexp term) in
      sexp (if isBV then "bvmul" else "*") (List.map aux l) 

end

module BoolStruct = struct

  type 'a t =
    | Leaf of 'a
    | And of 'a t list
    | Or  of 'a t list
    | Not of 'a t
  [@@deriving eq, show]

  let rec compare compare_leaf t1 t2 = match t1, t2 with
    | Leaf t1, Leaf t2 -> compare_leaf t1 t2
    | And l1, And l2
    | Or l1, Or l2 -> List.compare (compare compare_leaf) l1 l2
    | Not t1, Not t2 -> compare compare_leaf t1 t2
    | Leaf _, _ -> -1
    | _, Leaf _ -> 1
    | Not _, _  -> -1
    | _, Not _  -> 1
    | Or _, _   -> -1
    | _, Or _   -> -1

  let rec map f = function
    | Leaf a -> Leaf (f a)
    | And l -> And(List.map (map f) l)
    | Or l  -> Or(List.map (map f) l)
    | Not a  -> Not(map f a)

  let rec nnf polarity = function
    | Leaf _ as l -> if polarity then l else Not l
    | And l ->
      let l = List.map (nnf polarity) l in
      if polarity then And l else Or l
    | Or l ->
      let l = List.map (nnf polarity) l in
      if polarity then Or l else And l
    | Not a -> nnf (not polarity) a

end


module SliceTMP = struct

  type t = { extractee : Term.t;
             indices   : (int * int) option }

  let build ?indices extractee =
    assert(Term.is_bitvector extractee);
    match indices with
    | Some(lo,hi) when not(Int.equal lo 0 && Int.equal hi (TMP.width extractee)) ->
      {extractee; indices}
    | _ -> { extractee; indices = None }

  let to_term {extractee; indices} =
    match indices with
    | None -> extractee
    | Some(lo,hi) -> 
      Term.BV.bvextract extractee lo (hi-1)

  let to_sexp to_sexp_term {extractee; indices} = 
    match indices with
    | Some(lo,hi) ->
      let f = sexp "_" [Atom "extract"; Atom(string_of_int(hi - 1)); Atom(string_of_int lo)] in
      List[f; to_sexp_term extractee]
    | _ -> to_sexp_term extractee

  let width { extractee; indices } = match indices with
    | None -> TMP.width extractee
    | Some(lo, hi) -> hi - lo

  let fv x {extractee; _} = TMP.fv x extractee
end

(* Terms extended with primitive constructs for
   bvand, bvor, bvnot, sign-extend, zero-extend, concat, etc *)

module ExtTerm = struct

  type 'a bs = private BS
  type 'a ts = private TS
      
  type (_,_) base =
    | TermStruct : 'a Types.termstruct -> ('a ts, [`tstruct]) base
    | T      : Term.t                  -> (_  ts, [`closed] ) base
    | Bits   : Term.t list             -> ([`bits]  bs, _) base 
    | Slice  : SliceTMP.t BoolStruct.t -> ([`slice] bs, _) base
    | Concat : [`block] closed list    -> ([`concat],   _) base
    | Block  : { block : _ bs closed;
                 sign_ext  : int; (* Length of sign extension *)
                 zero_ext  : int; (* Length of zero extension *) } -> ([`block], _) base

  and 'a closed     = ('a, [`closed])  base
  type 'a termstruct = ('a, [`tstruct]) base

  type 'a bits   = ([`bits]  bs, 'a) base
  type 'a slice  = ([`slice] bs, 'a) base
  type 'a concat = ([`concat], 'a) base
  type 'a block  = ([`block], 'a) base

  type bit_select = Term.t * int [@@deriving eq, ord]
  type bit_struct = bit_select BoolStruct.t [@@deriving eq, ord]

  let return_block block = Block{block; sign_ext = 0; zero_ext = 0}

  let rec to_term : type a b. (a, b) base -> Term.t = function
    | TermStruct a   -> Term.build a
    | T a            -> a
    | Bits  block    -> Term.BV.bvarray block
    | Slice slice ->
      let open BoolStruct in
      let rec aux = function
        | Leaf slice -> SliceTMP.to_term slice
        | And l    -> Term.BV.bvand (List.map aux l)
        | Or l     -> Term.BV.bvor  (List.map aux l)
        | Not a    -> Term.BV.bvnot (aux a)
      in
      aux slice
    | Concat l -> Term.BV.bvconcat (List.rev_map to_term l)
    | Block{block; sign_ext; zero_ext} ->
      let block = to_term block in
      let block = if sign_ext = 0 then block else Term.BV.sign_extend block sign_ext in
      if zero_ext = 0 then block else Term.BV.zero_extend block zero_ext

  let build : type a. a termstruct -> a closed = function
    | TermStruct a  -> T(Term.build a)
    | Block b       -> Block b
    | Bits  _ as t  -> t
    | Slice _ as t  -> t
    | Concat _ as t -> t

  let rec width : type a b. (a, b) base -> int = function
    | TermStruct a -> TMP.(width(build a))
    | T a          -> TMP.(width a)
    | Bits l       -> List.length l
    | Slice slice ->
      let open BoolStruct in
      let rec aux = function
        | Leaf slice -> SliceTMP.width slice
        | And(a::_) -> aux a
        | Or (a::_) -> aux a
        | Not a -> aux a
        | _ -> assert false
      in
      aux slice
    | Concat l -> List.fold_left (fun sofar block -> sofar + width block) 0 l
    | Block{block; sign_ext; zero_ext} -> width block + sign_ext + zero_ext

  let rec fv : type a b. Term.t -> (a, b) base -> bool = fun x -> function
    | TermStruct a -> TMP.fv_termstruct x a
    | T a          -> TMP.fv x a
    | Bits a       -> List.exists (TMP.fv x) a
    | Slice slice ->
      let open BoolStruct in
      let rec aux = function
        | Leaf slice -> SliceTMP.fv x slice
        | And l      -> List.exists aux l
        | Or l       -> List.exists aux l
        | Not a      -> aux a
      in
      aux slice
    | Concat l     -> List.exists (fun (Block{ block; _ }) -> fv x block) l
    | Block{block; _} -> fv x block

  let typeof : type a b. (a, b) base -> Type.t = function
    | TermStruct a -> Term.(type_of_term(build a))
    | T a          -> Term.(type_of_term a)
    | other        -> width other |> Type.bv

  let bvarray (type b) bits =
    let init_slice bstruct =
      let aux (extractee,i) = SliceTMP.build extractee ~indices:(i, i+1) in
      Slice(BoolStruct.map aux bstruct)
    in
    let init_bits bit = Bits [bit] in
    let close_slice : type a. a bs closed -> a bs closed = function
      | Slice slice ->
        Slice(BoolStruct.nnf true slice)
      | Bits l      -> Bits(List.rev l)
    in
    let open Types in
    (* Check if bit is t[i] *)
    let rec analyse bit =
      let open Option in
      let open BoolStruct in
      match Term.reveal bit with
      | Term(Projection(`YICES_BIT_TERM, i, t)) -> return (Leaf(t, i))
      | Term(A1(`YICES_NOT_TERM, bit)) ->
        let+ t = analyse bit in
        Not t 
      | Term(Astar(`YICES_OR_TERM, l)) ->
        let rec aux accu = function
          | [] -> return accu
          | head::tail ->
            let* t = analyse head in
            aux (t::accu) tail
        in
        let+ t = aux [] l in
        Or(List.fast_sort compare_bit_struct t)
      | _ -> None
    in
    let rec test slice bit =
      let open Option in
      let open BoolStruct in
      match slice, bit with
      | Leaf SliceTMP.{extractee; indices = Some(lo, hi) }, Leaf(t, i)
        when hi = i && Term.equal t extractee ->
        Option.return (Leaf(SliceTMP.build extractee ~indices:(lo, hi+1)))
      | And l_slice, And l_bit ->
        let+ slice = aux [] (l_slice, l_bit) in
        And slice
      | Or l_slice, Or l_bit ->
        let+ slice = aux [] (l_slice, l_bit) in
        Or slice
      | Not slice, Not bit ->
        let+ slice = test slice bit in
        Not slice
      | _ ->
        None
    and aux accu =
      let open Option in
      function
      | [], [] -> Option.return (List.rev accu)
      | (head_slice::tail_slice), (head_bit::tail_bit) ->
        let* slice = test head_slice head_bit in
        aux (slice::accu) (tail_slice, tail_bit)
      | [], _::_ | _::_, [] -> None
    in

    let rec prolong_block (Block{ block; sign_ext; zero_ext } as last) ?sign bits accu =
      match sign, bits with
      | _, [] ->
        last::accu |> List.rev  (* we have finished, we push the last block, reversing order *)
      | Some sign, bit::tail -> (* we have a sign bit and we have a new bit *)
        if Term.equal sign bit
        then prolong_block (Block{ block; sign_ext = sign_ext + 1; zero_ext }) ~sign tail accu
        else prolong_block last bits accu (* Same as our call, minus the optional argument *)
      | None, bit::tail ->                (* We don't have a sign bit and we have a new bit *)
        if Term.(equal bit (true0()))
        then prolong_block (Block{ block; sign_ext; zero_ext = zero_ext + 1 }) tail accu
        else
          match analyse bit with
          | Some s -> prolong_slice (init_slice s)  ~sign:bit tail (last::accu)
          | None   -> prolong_slice (init_bits bit) ~sign:bit tail (last::accu)

    and prolong_slice : type a.
      a bs closed -> sign:Term.t -> Term.t list -> b block list -> b block list
      = fun slice ~sign bits accu ->
        match bits with
        | [] ->
          let block = return_block (close_slice slice) in
          block::accu |> List.rev (* we have finished, closing last slice, and reversing order *)
        | bit::tail ->            (* we have a new bit to look at *)
          match slice, analyse bit with
          | Slice s, Some b ->
            begin
              match test s b with
              | Some s -> prolong_slice (Slice s) ~sign:bit tail accu
              | None   -> (* End of the slice; we see whether we have extensions *)
                let block = return_block(close_slice slice) in
                prolong_block block ~sign bits accu
            end
          | Slice _, None ->
            let block = return_block(close_slice slice) in
            prolong_slice (init_bits bit) ~sign:bit tail (block::accu)
          | Bits l, None   -> prolong_slice (Bits(bit::l)) ~sign:bit tail accu
          | Bits _, Some b ->
            let block = return_block(close_slice slice) in
            prolong_slice (init_slice b) ~sign:bit tail (block::accu)
    in
    match bits with
    | [] -> assert false (* There must be at least one bit! *)
    | bit::tail ->
      match analyse bit with
      | Some b -> prolong_slice (init_slice b) ~sign:bit tail []
      | None   -> prolong_slice (init_bits bit) ~sign:bit tail []

  module MTerm(M : Monad) = struct
    open MTerm(M)
    let (let*) = M.bind
    let (let+) a f = M.bind a (fun r -> M.return(f r))

    let rec mapList f = function
      | [] -> M.return []
      | head::tail ->
        let* h = f head in
        let+ t = mapList f tail in
        h::t

    type update = { apply : 'a. 'a closed -> 'a closed M.t }

    let auxTerm f t  = let+ T t     = f.apply(T t) in t
    let auxSlice f s = let+ Slice s = f.apply(Slice s) in s

    let mapSlice f = let open BoolStruct in function
      | Leaf(SliceTMP.{ extractee; indices }) ->
        let+ extractee = auxTerm f extractee in
        Leaf(SliceTMP.{ extractee; indices })
      | And l -> let+ l = mapList (auxSlice f) l in And l
      | Or l  -> let+ l = mapList (auxSlice f) l in Or l
      | Not a -> let+ a = auxSlice f a in Not a

    let map : type a. update -> (a, [`tstruct]) base -> (a, [`tstruct]) base M.t =
      fun f -> function
        | TermStruct a  -> let+ a      = map (auxTerm f) a        in TermStruct a
        | Slice slice   -> let+ slice  = mapSlice f slice         in Slice slice
        | Bits bits     -> let+ bits   = mapList (auxTerm f) bits in Bits bits
        | Concat concat -> let+ concat = mapList f.apply concat   in Concat concat
        | Block{block; sign_ext; zero_ext} ->
          let+ block = f.apply block in Block{block; sign_ext; zero_ext}
    
  end
  
  type t  = ExtTerm  : _ closed -> t
  type yt = YExtTerm : _ termstruct -> yt

  module HReveal = Hashtbl.Make(Term)

  let reveal_table = HReveal.create 500

  let of_yterm : Types.yterm -> yt = function
    | Term(Astar(`YICES_BV_ARRAY, bits)) -> (* In case of a BV_ARRAY, we preprocess *)
      begin
        match bvarray bits with
        | [Block{ block = Slice _ as slice; sign_ext=0; zero_ext=0}] -> YExtTerm slice
        | [Block{ block = Bits _  as bits ; sign_ext=0; zero_ext=0}] -> YExtTerm bits
        | l                                                          -> YExtTerm(Concat l)
      end
    | Term a -> YExtTerm(TermStruct a) (* Otherwise we let solve_aux analyse the term structure *)

  let of_term t =
    match Term.reveal t with
    | Term(Astar(`YICES_BV_ARRAY, _)) as tt -> (* In case of a BV_ARRAY, we look up the table *)
      begin
        match HReveal.find_opt reveal_table t with
        | Some a -> a
        | None ->
          let answer = of_yterm tt in
          HReveal.replace reveal_table t answer;
          answer
      end
    | tt -> of_yterm tt

  let rec to_sexp : type a b. (a, b) base -> Sexp.t = function
    | TermStruct a -> TMP.to_sexp_termstruct to_sexp_t a
    | T a          -> to_sexp_t a
    | Bits  block  -> sexp "bool-to-bv" (List.map to_sexp_t block)
    (* Not official SMTLib syntax *)
    | Slice slice ->
      let open BoolStruct in
      let rec aux = function
        | Leaf slice -> SliceTMP.to_sexp to_sexp_t slice
        | And l  -> sexp "bvand" (List.map aux l)
        | Or l   -> sexp "bvor"  (List.map aux l)
        | Not a  -> sexp "bvnot" [aux a]
      in
      aux slice
    | Concat l    -> sexp "concat" (List.rev_map to_sexp l)
    | Block{block; sign_ext; zero_ext} ->
      let block = to_sexp block in
      let f string i term = List[ sexp "_" [Atom string; Atom(string_of_int i)] ; term ] in
      let block = if sign_ext = 0 then block else f "sign_extend" sign_ext block in
      if zero_ext = 0 then block else f "sign_extend" zero_ext block

  and to_sexp_t : Term.t -> Sexp.t = fun t ->
    let YExtTerm t = of_term t in
    to_sexp t
  
  let pp fmt t = t |> to_sexp |> pp_sexp fmt
  
end

module Term = struct
  include TMP
  let to_sexp = ExtTerm.to_sexp_t
  let to_sexp_termstruct t = TMP.to_sexp_termstruct to_sexp t
  let pp fmt t = try
      to_sexp t |> pp_sexp fmt
    with ExceptionsErrorHandling.YicesException _ -> Format.fprintf fmt "NULL_TERM"
end

module Slice = struct
  include SliceTMP
  let to_sexp = to_sexp Term.to_sexp
  let pp fmt t = to_sexp t |> pp_sexp fmt
end

module Types = struct
  include Types
  let pp_error_report fmt Types.{badval; code; column; line; term1; term2; type1; type2} =
    Format.fprintf fmt
      "@[<v 1> \
       error: %s@,\
       bad val: %i@,\
       code: %a@,\
       column %i line %i@,\
       term1: %a@,\
       term2: %a@,\
       type1: %a@,\
       type2: %a@,\
       @]"
      (ErrorPrint.string ())
      badval
      Types.pp_error_code code
      column line
      Term.pp term1
      Term.pp term2
      Type.pp type1
      Type.pp type2
end

module Model = struct
  include Model
  let pph height fmt t =
    t |> PP.model_string ~display:Types.{ width = 100; height; offset=0}
    |> Format.fprintf fmt "%s"
  let pp = pph 1000
end

module Action = struct

  type t =
    | DeclareType of string
    | DeclareFun of string * Type.t
    | Status
    | Reset
    | Push
    | Pop
    | EnableOption of string
    | DisableOption of string
    | AssertFormula of Term.t
    | AssertFormulas of Term.t list
    | Check of Param.t option
    | CheckWithAssumptions of Param.t option * Term.t list
    | Stop
    | GetModel
    | GetUnsatCore
    | CheckWithModel of Param.t option * Model.t * Term.t list
    | GetModelInterpolant

  let to_sexp accu = function
    | DeclareType typ_string
      -> sexp "declare-sort" [Atom typ_string; Atom "0"] ::accu
    | DeclareFun(string, typ)
      -> sexp "declare-fun" [Atom string; List[]; Type.to_sexp typ] ::accu
    | Status -> sexp "get-status" [] ::accu
    | Reset  -> sexp "reset-assertions" [] ::accu
    | Push   -> sexp "push" [] ::accu
    | Pop    -> sexp "pop" [] ::accu
    | EnableOption s -> sexp "set-option" [Atom s; Atom "true"] ::accu 
    | DisableOption s -> sexp "set-option" [Atom s; Atom "false"] ::accu
    | AssertFormula t -> sexp "assert" [Term.to_sexp t] ::accu
    | AssertFormulas l ->
      List.fold_left (fun sofar t -> sexp "assert" [Term.to_sexp t]::sofar) accu l
    | Check _param -> sexp "check-sat" [] ::accu 
    | CheckWithAssumptions(_param,l) ->
      sexp "check-sat-assuming" (List.map Term.to_sexp l) ::accu
    | Stop     -> sexp "stop" [] ::accu
    | GetModel -> sexp "get-model" [] ::accu
    | GetUnsatCore -> sexp "get-unsat-core" [] ::accu
    | CheckWithModel(_param, model, terms) ->
      let aux (varl,vall) t =
        let v = Model.get_value_as_term model t in
        let t = Term.to_sexp t in
        let v = Term.to_sexp v in
        t::varl, v::vall
      in
      let varl,vall = terms |> List.rev |> List.fold_left aux ([],[]) in
      sexp "check-sat-assuming-model" [List varl; List vall] ::accu
    | GetModelInterpolant -> sexp "get-unsat-model-interpolant" [] ::accu

end

module Context = struct
  include Context

  type assertions = Term.t list list

  let pp_assertions fmt assertions = 
    Format.fprintf fmt "@[<v>%a@]" (Term.pp |> List.pp |> List.pp) assertions

  type options = unit StringHashtbl.t

  let pp_options fmt options =
    Format.fprintf fmt "@[<v>%a@]" (StringHashtbl.pp String.pp Format.silent) options

  type nonrec t = {
    config     : Config.t option;
    context    : t;
    assertions : assertions ref;
    options    : options;
    log        : Action.t list ref
  }

  let pp fmt {assertions; _} = pp_assertions fmt !assertions

  let to_sexp {log; _} =
    !log |> List.fold_left Action.to_sexp [] 

  let malloc ?config () =
    { config  = config;
      context = malloc ?config ();
      assertions = ref [[]];
      options = StringHashtbl.create 10;
      log = ref [] }

  let free   {context; _} = free context
  let status x =
    x.log := Status::!(x.log);
    status x.context

  let reset x =
    reset x.context;
    x.assertions := [[]];
    x.log := Reset::!(x.log)

  let push x =
    x.log := Push::!(x.log);
    x.assertions := []::!(x.assertions);
    push x.context

  let pop x =
    x.log := Pop::!(x.log);
    begin match !(x.assertions) with
      | []     -> assert false
      | [_last] -> raise PopLastLevel
      | _::tail -> x.assertions := tail
    end;
    pop x.context

  let enable_option {context; options; log; _} ~option =
    log := EnableOption option::!log;
    enable_option context ~option;
    StringHashtbl.replace options option ()

  let disable_option {context; options; log; _} ~option =
    log := DisableOption option::!log;
    disable_option context ~option;
    StringHashtbl.remove options option 

  let assert_formula {context; assertions; log; _} formula =
    log := AssertFormula formula::!log;
    begin match !assertions with
      | []     -> assert false
      | last::tail -> assertions := (formula::last)::tail
    end;
    assert_formula context formula

  let assert_formulas {context; assertions; log; _} formulas =
    log := AssertFormulas formulas::!log;
    begin match !assertions with
      | []     -> assert false
      | last::tail ->
        assertions := (List.rev_append (List.rev formulas) last)::tail
    end;
    assert_formulas context formulas

  let check ?param {context;log; _} =
    log := Check param::!log;
    check ?param context

  let check_with_assumptions ?param {context; log; _} assumptions =
    log := CheckWithAssumptions(param, assumptions)::!log;
    check_with_assumptions ?param context assumptions

  let stop {context; log; _} =
    log := Stop::!log;
    stop context
  let get_model {context; log; _} =
    log := GetModel::!log;
    get_model context
  let get_unsat_core {context; log; _} =
    log := GetUnsatCore::!log;
    get_unsat_core context

  let declare_type {log; _} s =
    log := DeclareType s::!log

  let declare_fun {log; _} s t =
    log := DeclareFun(s,t)::!log

  let check_with_model ?param {context; log; _} model terms =
    log := CheckWithModel(param, model, terms)::!log;
    check_with_model ?param context model terms
  let get_model_interpolant {context; log; _} =
    log := GetModelInterpolant::!log;
    get_model_interpolant context

end

module Param = struct
  include Param
  let default Context.{context; _} = default context 
end
