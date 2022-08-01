[%%import "gmp.mlh"]

open Containers

open Sexplib
open Type
    
open High
open Types

include Purification.HighAPI

module HStrings = CCHashtbl.Make(String)

module List = struct
  include List
  let map f l = List.rev (List.fold_left (fun sofar a -> f a::sofar) [] l)
end

exception PopLastLevel

let pp_sep fmt () = Format.fprintf fmt "@ "

let sexp f arg = List(Atom f::arg)
let rec pp_sexp fmt = function
  | Atom s -> Format.fprintf fmt "@[%s@]" s
  | List l -> Format.fprintf fmt "@[<hov1>(%a)@]" (List.pp ~pp_sep pp_sexp) l

(* let pp_sexp = Sexp.pp_hum *)

let use_type_names = ref true
let use_term_names = ref true
let use_type_notations = ref true
let use_term_notations = ref true

let match_n use_ref get t =
  if !use_ref then
    match get t with
    | s -> Some s
    | exception _ -> None
  else
    None

let match_nn use_notation get_notation use_names get_names t =
  match match_n use_notation get_notation t with
  | None -> match_n use_names get_names t
  | (Some _) as s -> s

let notation replace hooks t =
  let aux cont =
    replace hooks t (lazy (Format.sprintf "%t" cont))
  in
  Format.kdprintf aux

(* Configs but with settings available *)

module Config = struct

  type options = String.t HStrings.t

  type t = {
      config : Config.t;
      options: options;
      mcsat : bool ref
    }

  let malloc () = {
      config  = Config.malloc();
      options = HStrings.create 10;
      mcsat  = ref false
    }

  let free t = Config.free t.config
  let set t ~name ~value =
    HStrings.replace t.options name value;
    if String.equal name "solver-type"
    then t.mcsat := String.equal value "mcsat";
    Config.set t.config ~name ~value

  let mcsat_logics = HStrings.create 10

  let () =
    HStrings.replace mcsat_logics "QF_NRA" ();
    HStrings.replace mcsat_logics "QF_NIA" ();
    HStrings.replace mcsat_logics "QF_UFNRA" ();
    HStrings.replace mcsat_logics "QF_UFNIA" ()
    
  let default ?logic t =
    let () = match logic with
      | Some logic when HStrings.mem mcsat_logics logic -> t.mcsat := true
      | _ -> t.mcsat := false
    in
    Config.default ?logic t.config

  let get t     = HStrings.find t.options
  let options t = HStrings.to_list t.options

end
  
  
(* Type but with S-expression export *)

module TypeSexp = struct
  include Type
  let pph height fmt t =
    let display = Types.{ width = 80; height; offset = 0 } in
    try
      t |> PP.type_string ~display
      |> Format.fprintf fmt "%s"
    with _ -> Format.fprintf fmt "null_type"

  let hooks          = Global.hTypes_create 100
  let notation t     = notation HTypes.replace hooks t
  let get_notation t = HTypes.find hooks t |> Lazy.force

  let rec to_sexp typ =
    match match_nn use_type_notations get_notation use_type_names Type.Names.to_name typ with
    | Some s -> Atom s
    | None ->
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

module TermSet = Set.Make(Term)

module HPairs = Hashtbl.Make(struct
                    type t = Term.t * Term.t [@@deriving eq]
                    let hash = Hash.pair Term.hash Term.hash
                  end)

let is_free_table = HPairs.create 500
let () = Global.register_cleanup (fun ~after:_ -> HPairs.reset is_free_table)

let fv_table = Global.hTerms_create 500

module TermTMP = struct

  include Term

  let pph height fmt t =
    let display = Types.{ width = 80; height; offset = 0 } in
    try
      t |> PP.term_string ~display
      |> Format.fprintf fmt "%s"
    with _ -> Format.fprintf fmt "null_term"

  let pow i t = 
    if i <= 0 then ExceptionsErrorHandling.raise_bindings_error
                     "Exponent should be positive in a power product, not %i" i
    else
      if i = 1 then t
      else
        let rec aux i accu =
          if i = 0 then accu else aux (i-1) (t::accu) 
        in
        sexp "*" (aux i [])

  (* For bitvector terms *)
  let width_of_term y = Type.bvsize(Term.type_of_term y)

  (*************************)
  (* Free Variable testing *)
  (*************************)

  open MTerm(struct
           include Option
           let bind = ( >>= )
         end)

  let rec is_free_termstruct : type a. var:Term.t -> a termstruct -> bool = fun ~var a ->
    map (fun t -> if is_free ~var t then None else Some t) a |> Option.is_none
  and is_free ~var t =
    if Term.equal var t then true
    else
      match HPairs.find_opt is_free_table (var,t) with
      | Some b -> b
      | None ->
         let Term a = Term.reveal t in
         let answer = is_free_termstruct ~var a in
         HPairs.add is_free_table (var,t) answer;
         answer

  (**************************)
  (* Sets of Free Variables *)
  (**************************)

  open MTerm(struct
           type 'a t = 'a * TermSet.t
           let return a = a, TermSet.empty
           let bind (a, fv) f = let a, fv' = f a in a, TermSet.union fv fv' 
         end)

  let rec fv_termstruct : type a. a termstruct -> TermSet.t = function
    | A0(`YICES_UNINTERPRETED_TERM,t) -> TermSet.singleton t
    | A0 _ -> TermSet.empty
    | a -> map (fun a -> a, fv a) a |> snd
  and fv t =
    try
      let f k =
        let Term a = Term.reveal k in
        fv_termstruct a
      in
      HTerms.get_or_add fv_table ~f ~k:t
    with
    | ExceptionsErrorHandling.YicesException _
      ->
       print_endline (Format.sprintf "@[Yices error on term %i: @[%s@]@]@,"
                        (Term.hash t)
                        (ErrorPrint.string()));
       let bcktrace = Printexc.get_backtrace() in
       print_endline(Format.sprintf "@[Backtrace is:@,@[%s@]@]@]%!" bcktrace);
       print_endline "Attempting to print term";
       print_endline(Format.sprintf " @[%a@]" (pph 100) t);
       print_endline "Attempting to print error report";
       failwith "Giving up"
    | _ -> 
       print_endline (Format.sprintf "@[Catching giving up with term %i: @[%a@]@]@,"
                        (Term.hash t)
                        (pph 100) t);
       failwith "Giving up"
   
         
[%%if gmp_present]
  let sum_aux to_sexp (coeff, term) =
    let one = Term.Arith.int 1 in
    let coeff = Term.Arith.mpq coeff in
    match term with
    | Some term ->
       if Term.equal coeff one then to_sexp term
       else sexp "*" [to_sexp coeff; to_sexp term]
    | None -> to_sexp coeff

  let sexp_gmpz z =
    if Z.lt z Z.zero then sexp "-" [Atom(Z.(z |> abs |> to_string))]
    else Atom(Z.to_string z)
    
  let sexp_gmpq q =
    let q = Term.rational_const_value q in
    let num = Q.num q in
    let den = Q.den q in
    if Z.equal den Z.one then sexp_gmpz num
    else
      sexp "/" [sexp_gmpz num; sexp_gmpz den]

[%%else]
  let sum_aux   _ = raise_gmp "Term.sum_aux"
  let sexp_gmpq _ = raise_gmp "Term.sexp_gmpq"
[%%endif]

  let to_sexp_termstruct : type a. (t -> Sexp.t) -> a termstruct -> Sexp.t =
    fun to_sexp ->
    function
    | A0(c, t) ->
       let s () = PP.term_string ~display:Types.{width = 800000; height = 10000; offset = 0} t in
       begin match c with
       | `YICES_BV_CONSTANT ->
          let s = s() in
          let s =
            if String.length s < 2
            then ExceptionsErrorHandling.raise_bindings_error
                   "bv constant as a string should have at least 2 characters, got %s instead" s
            else
              match String.sub s 0 2 with
              | "0b" -> "#b"^String.sub s 2 (String.length s -2)
              | "0x" -> "#x"^String.sub s 2 (String.length s -2)
              | _ -> s
          in
          Atom s
       | `YICES_ARITH_CONSTANT ->
          (try sexp_gmpq t with ExceptionsErrorHandling.YicesBindingsException _ -> Atom(s()))
       | _ -> Atom(s())
       end

    | A1(c,t) ->
       begin
         match c with
         | `YICES_NOT_TERM ->
            begin
              match Term.constructor t with
              | `YICES_OR_TERM ->
                 let disjuncts = Term.children t in
                 sexp "and" (List.map (fun disjunct-> disjunct |> Term.not1 |> to_sexp) disjuncts)
              | `YICES_ARITH_GE_ATOM ->
                 let args = Term.children t in
                 sexp "<" (List.map to_sexp args)
              | _ -> sexp "not" [to_sexp t]
            end
         | `YICES_ABS         -> sexp "abs" [to_sexp t]
         | `YICES_CEIL        -> sexp "ceil" [to_sexp t]
         | `YICES_FLOOR       -> sexp "floor" [to_sexp t]
         | `YICES_IS_INT_ATOM -> sexp "is-int" [to_sexp t]
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
         | `YICES_RDIV          -> sexp "/" args
         | `YICES_ARITH_ROOT_ATOM -> sexp "ARITH_ROOT_ATOM" args
       end
    | ITE(c, tb, eb) ->
       let args = List.map to_sexp [c;tb;eb] in 
       sexp "ite" args
    | Astar(c,l) ->
       let args = List.map to_sexp l in
       begin
         match c with
         | `YICES_TUPLE_TERM    -> sexp "tuple"    args
         | `YICES_DISTINCT_TERM -> sexp "distinct" args
         | `YICES_OR_TERM       ->
            begin
              let filter (hyps, concl) = function
                | List[Atom "not"; arg] -> arg::hyps, concl
                | arg -> hyps, arg::concl
              in
              let to_sexp_hyps = function
                | [a] -> a
                | hyps -> sexp "and" hyps
              in
              let to_sexp_concl = function
                | [a] -> a
                | concl -> sexp "or" concl
              in
              match args |> List.rev |> List.fold_left filter ([],[]) with
              | [], l       -> to_sexp_concl l
              | hyps, []    -> sexp "not" [to_sexp_hyps hyps]
              | hyps, concl -> sexp "=>" [to_sexp_hyps hyps; to_sexp_concl concl]

            end

         | `YICES_XOR_TERM      -> sexp "xor" args
         | `YICES_BV_ARRAY      -> sexp "bool-to-bv" args (* Not official SMTLib syntax *)
       end
    | Bindings{c;vars;body} ->
       let aux v = List[to_sexp v; TypeSexp.to_sexp(type_of_term v)] in  
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

    | Sum [a] ->
       sum_aux to_sexp a 

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
    | Some(lo,hi) when not(Int.equal lo 0 && Int.equal hi (TermTMP.width_of_term extractee)) ->
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
    | None -> TermTMP.width_of_term extractee
    | Some(lo, hi) -> hi - lo

  let is_free ~var {extractee; _} = TermTMP.is_free ~var extractee
  let fv {extractee; _} = TermTMP.fv extractee
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
    | TermStruct a -> TermTMP.(width_of_term(build a))
    | T a          -> TermTMP.(width_of_term a)
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

  let rec is_free : type a b. var:Term.t -> (a, b) base -> bool = fun ~var ->
    function
    | TermStruct a -> TermTMP.is_free_termstruct ~var a
    | T a          -> TermTMP.is_free ~var a
    | Bits a       -> List.exists (TermTMP.is_free ~var) a
    | Slice slice ->
       let open BoolStruct in
       let rec aux = function
         | Leaf slice -> SliceTMP.is_free ~var slice
         | And l      -> List.exists aux l
         | Or l       -> List.exists aux l
         | Not a      -> aux a
       in
       aux slice
    | Concat l     -> List.exists (fun (Block{ block; _ }) -> is_free ~var block) l
    | Block{block; _} -> is_free ~var block

  let collect f =
    let aux sofar a = TermSet.union sofar (f a) in
    List.fold_left aux TermSet.empty
    
  let rec fv : type a b. (a, b) base -> TermSet.t = function
    | TermStruct a -> TermTMP.fv_termstruct a
    | T a          -> TermTMP.fv a
    | Bits a       -> collect TermTMP.fv a
    | Slice slice ->
       let open BoolStruct in
       let rec aux = function
         | Leaf slice   -> SliceTMP.fv slice
         | And l | Or l -> collect aux l
         | Not a        -> aux a
       in
       aux slice
    | Concat l        -> collect (fun (Block{ block; _ }) -> fv block) l
    | Block{block; _} -> fv block

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

    let mapSlice f =
      let open BoolStruct in
      function
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

  let reveal_table = Global.hTerms_create 500

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
         match HTerms.find_opt reveal_table t with
         | Some a -> a
         | None ->
            let answer = of_yterm tt in
            HTerms.replace reveal_table t answer;
            answer
       end
    | tt -> of_yterm tt

  let hooks          = Global.hTerms_create 100
  let notation t     = notation HTerms.replace hooks t
  let get_notation t = HTerms.find hooks t |> Lazy.force

  let rec to_sexp : type a b. (a, b) base -> Sexp.t = function
    | TermStruct a -> TermTMP.to_sexp_termstruct to_sexp_t a
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
    match match_nn use_type_notations get_notation use_type_names Term.Names.to_name t with
    | Some s -> Atom s
    | None ->
       let YExtTerm t = of_term t in
       to_sexp t
    
  let pp fmt t = t |> to_sexp |> pp_sexp fmt
               
end

(* Term but with S-expression export *)

module TermSexp = struct
  include TermTMP
  let notation = ExtTerm.notation
  let to_sexp = ExtTerm.to_sexp_t
  let to_sexp_termstruct t = TermTMP.to_sexp_termstruct to_sexp t
  let pp fmt t = try
      to_sexp t |> pp_sexp fmt
    with ExceptionsErrorHandling.YicesException _ -> Format.fprintf fmt "NULL_TERM"
end

module Slice = struct
  include SliceTMP
  let to_sexp = to_sexp TermSexp.to_sexp
  let pp fmt t = to_sexp t |> pp_sexp fmt
end

module Value = struct

  type t = yval

  let rec pp m fmt : t -> unit = function
    | `Bool _ | `Rational _ | `BV _ | `Scalar _ as s ->
       Format.fprintf fmt "%a" TermSexp.pp (Term.const_as_term s)
    | `Tuple(_,l) ->
       Format.fprintf fmt "(%a)" (List.pp (pp_val m)) l
    | `Fun { mappings = []; default; _ } ->
       Format.fprintf fmt "{ @[<v>@[_ --> %a@]@] }"
         (pp_val m) default
    | `Fun { mappings; default; _ } ->
       Format.fprintf fmt "{ @[<v>%a;@,@[_ --> %a@]@] }"
         (List.pp (pp_mapping m)) mappings (pp_val m) default
    | `Algebraic a ->
       Format.fprintf fmt "%s" (High.Algebraic.to_string a.libpoly)
  and pp_mapping m fmt { args; value } =
    match args with
    | [a] ->
       Format.fprintf fmt "@[@[%a@] --> %a@]" (pp_val m) a (pp_val m) value
    | _ ->
       Format.fprintf fmt "@[@[(%a)@] --> %a@]" (List.pp (pp_val m)) args (pp_val m) value

  and pp_val m fmt v = Model.reveal m v |> pp m fmt

end

module Model = struct
  include Model
  let pph height fmt t =
    t |> PP.model_string ~display:Types.{ width = 100; height; offset=0}
    |> Format.fprintf fmt "%s"
  let pp = pph 1000
end

(* Supported models *)
module SModel = struct

  type t = {
    support : Term.t list;
    model   : Model.t
  }

  let make ?support model =
    let support = match support with
      | None -> Model.collect_defined_terms model |> List.sort Term.compare
      | Some support -> support
    in
    { support; model }

  let to_sexp smodel =
    let aux t =
      let ty = TypeSexp.to_sexp(Term.type_of_term t) in
      let vars, body =
        try
          let v = Model.get_value_as_term smodel.model t in
          match Term.reveal v with
          | Term(Bindings { vars; body; _ }) ->
             let aux v = List[TermSexp.to_sexp v; TypeSexp.to_sexp(Term.type_of_term v)] in  
             Sexp.List(List.map aux vars), TermSexp.to_sexp body
          | _ -> Sexp.List [], TermSexp.to_sexp v
        with
          _ -> Sexp.List [],
               try
                 let v = Model.get_algebraic_number_value smodel.model t in
                 Sexp.Atom (Algebraic.to_string v.libpoly)
               with
                 _ -> Sexp.Atom "Can't print"
      in
      sexp "define-fun" [TermSexp.to_sexp t; vars; ty; body]
    in
    Sexp.List(smodel.support |> List.rev |> List.rev_map aux)


  let pp ?pp_start ?pp_stop ?pp_sep () fmt {support;model} =
    let aux fmt u =
      let v = Model.get_value model u in
      Format.fprintf fmt "@[<2>%a :=@ @[%a@]@]" TermSexp.pp u (Value.pp_val model) v
    in
    match support with
    | [] -> Format.fprintf fmt "[]"
    | support -> Format.fprintf fmt "%a" (List.pp ?pp_start ?pp_stop ?pp_sep aux) support

  let as_substitution smodel =
    let aux t = (t, Model.get_value_as_term smodel.model t) in
    List.rev_map aux smodel.support

  let as_assumptions smodel =
    let aux t =
      if Term.is_bool t
      then
        if Model.get_bool_value smodel.model t
        then t
        else Term.not1 t
      else
        Model.get_value_as_term smodel.model t |> Term.eq t
    in
    List.rev_map aux smodel.support

  let from_assumptions assumptions =
    let aux (model, support, constraints) assumption =
      let atom, fresh = Purification.Term.get_var assumption in
      let constraints =
        if fresh then 
          let constr1 = Term.(atom ==> assumption) in
          let constr2 = Term.(assumption ==> atom) in
          constr1::constr2::constraints
        else
          constraints
      in
      (atom, Term.true0())::model, atom::support, constraints
    in
    let model, support, constraints = List.fold_left aux ([],[],[]) assumptions in
    make ~support (Model.from_map model), constraints

end

module Assertions = struct

  type t = {
      list  : Term.t list option list; (* never empty *)
      level : int (* Always equal to (List.length list - 1), but gives O(1) access *)
    }

  let init = {
      list = [Some []];
      level = 0;
    }

  exception BlockingClauseUsage

  let assertions x =
    let aux sofar = function
      | None -> raise BlockingClauseUsage
      | Some list -> List.rev_append list sofar
    in
    List.fold_left aux [] x.list

  let level_to_sexp = function
    | None -> Sexp.Atom "BlockingClauseUsage"
    | Some assertions -> Sexp.List (List.map TermSexp.to_sexp assertions)

  let to_sexp { list; _ } = Sexp.List (List.map level_to_sexp list)
    
  let pp_level fmt = function
    | None ->
       Format.fprintf fmt "@[<v>??@]"
    | Some assertions ->
       Format.fprintf fmt "@[<v>%a@]" (List.pp TermSexp.pp) assertions

  let pp fmt assertions = 
    Format.fprintf fmt "@[<v>%a@]" (List.pp pp_level) assertions.list

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
    | AssertBlockingClause
    | Check of Param.t option
    | CheckWithAssumptions of { param : Param.t option; assumptions : Term.t list }
    | Stop
    | GetModel of { keep_subst : bool }
    | GetUnsatCore
    | CheckWithModel of { param : Param.t option; smodel : SModel.t }
    | CheckWithInterpolation of { param : Param.t option;
                                  build_model : bool;
                                  is_first    : bool;
                                  other_assertions : Assertions.t;
                                  other_log : t list }
    | GetModelInterpolant
    | GarbageCollect of Sexp.t list

  let to_sexp accu = function
    | DeclareType typ_string
      -> sexp "declare-sort" [Atom typ_string; Atom "0"] ::accu
    | DeclareFun(string, typ)
      -> sexp "declare-fun" [Atom string; List[]; TypeSexp.to_sexp typ] ::accu
    | Status -> sexp "get-status" [] ::accu
    | Reset  -> sexp "reset-assertions" [] ::accu
    | Push   -> sexp "push" [] ::accu
    | Pop    -> sexp "pop" [] ::accu
    | EnableOption s -> sexp "set-option" [Atom s; Atom "true"] ::accu 
    | DisableOption s -> sexp "set-option" [Atom s; Atom "false"] ::accu
    | AssertFormula t -> sexp "assert" [TermSexp.to_sexp t] ::accu
    | AssertFormulas l ->
       List.fold_left (fun sofar t -> sexp "assert" [TermSexp.to_sexp t]::sofar) accu l
    | AssertBlockingClause -> sexp "assert-blocking-clause" [] ::accu
    | Check _param -> sexp "check-sat" [] ::accu 
    | CheckWithAssumptions{ assumptions; _ } ->
       sexp "check-sat-assuming" (List.map TermSexp.to_sexp assumptions) ::accu
    | Stop         -> sexp "stop" [] ::accu
    | GetModel _   -> sexp "get-model" [] ::accu
    | GetUnsatCore -> sexp "get-unsat-core" [] ::accu
    | CheckWithModel{ smodel; _} ->
       sexp "check-sat-assuming-model" [SModel.to_sexp smodel] ::accu
    | CheckWithInterpolation{ build_model; is_first; other_assertions; _} ->
       let build_model = Atom(if build_model then "build_model" else "no_build_model") in
       let is_first    = Atom(if is_first then "is_first" else "is_second") in
       sexp "check-sat-with-interpolation"
         [build_model; is_first; Assertions.to_sexp other_assertions] ::accu
    | GetModelInterpolant -> sexp "get-unsat-model-interpolant" [] ::accu
    | GarbageCollect sexpl -> sexp "garbage-collect" sexpl ::accu

end

let global_log = ref []
let () = Global.register_cleanup (fun ~after:_ -> global_log := [])

module Context = struct

  let pp_options fmt options =
    Format.fprintf fmt "@[<v>%a@]" (HStrings.pp String.pp Format.silent) options

  let pp_config_options fmt config_options =
    Format.fprintf fmt "@[<v>%a@]" (HStrings.pp String.pp String.pp) config_options

  type nonrec t = {
      config     : Config.t option;
      context    : Context.t;
      assertions : Assertions.t ref;
      options    : unit HStrings.t;
      log        : Action.t list ref;
      is_alive   : bool ref;
      config_options : String.t HStrings.t;
      mcsat      : bool
    }

  let assertions ctx = !(ctx.assertions)
  let options ctx = HStrings.copy ctx.options
  let config_options ctx = HStrings.copy ctx.config_options
  let log ctx = !(ctx.log)
  let is_alive ctx = !(ctx.is_alive)
  let is_mcsat ctx = ctx.mcsat

  let all = ref []
  let () = Global.register_cleanup (* We don't erase contexts after GC *)
             (fun ~after ->
               match after with
               | `GC -> ()
               | _ -> all := [])

  let pp fmt {assertions; _} = Assertions.pp fmt !assertions

  let to_sexp {log; _} = !log |> List.fold_left Action.to_sexp [] 

  let pp_log fmt ctx = Format.fprintf fmt "%a" (List.pp pp_sexp) (to_sexp ctx)

  let malloc ?config () =
    let yconfig = Option.map (fun config -> Config.(config.config)) config in
    let config_options =
      match config with
      | Some config -> HStrings.copy config.options
      | None -> HStrings.create 1
    in
    let context = 
      { config  = config;
        context = Context.malloc ?config:yconfig ();
        assertions = ref Assertions.init;
        options = HStrings.create 10;
        log     = ref !global_log;
        is_alive = ref true;
        config_options;
        mcsat   = match config with
                  | Some config -> !(config.mcsat)
                  | None -> false
      }
    in
    all := context::!all;
    context

  let malloc_mcsat () =
    let cfg = Config.malloc () in
    Config.set cfg ~name:"solver-type" ~value:"mcsat";
    Config.set cfg ~name:"model-interpolation" ~value:"true";
    malloc ~config:cfg ()
    
  let all() = !all

  let free {context; is_alive; _ } = Context.free context; is_alive := false

  let action a x = x.log := a::!(x.log)

  (* GC invalidates references to terms and types in x.log *)
  let garbage_collect ?(record_log=false) x =
    let sexps = if record_log then to_sexp x else [] in
    x.log := [Action.GarbageCollect sexps]

  let status x =
    action Status x;
    Context.status x.context

  let reset x =
    action Reset x;
    Context.reset x.context;
    x.assertions := Assertions.init

  let push x =
    action Push x;
    let Assertions.{list; level} = !(x.assertions) in
    x.assertions := {
        list = Some []::list;
        level = level+1;
      };
    Context.push x.context

  let pop x =
    action Pop x;
    let Assertions.{list; level} = !(x.assertions) in
    begin match list with
    | []     -> assert false
    | [_last] -> raise PopLastLevel
    | _::tail -> x.assertions := { list = tail; level = level - 1 }
    end;
    Context.pop x.context

  let goto x level =
    let diff = level - !(x.assertions).level in
    if diff > 0
    then for _ = 1 to diff do push x done
    else for _ = 1 to -diff do pop x done
    
  let enable_option x ~option =
    action (EnableOption option) x;
    Context.enable_option x.context ~option;
    HStrings.replace x.options option ()

  let disable_option x ~option =
    action (DisableOption option) x;
    Context.disable_option x.context ~option;
    HStrings.remove x.options option 

  let assert_formula x formula =
    action (AssertFormula formula) x;
    let Assertions.{list; level} = !(x.assertions) in
    begin match list with
    | []         -> assert false
    | last::tail -> x.assertions := { list = (Option.map (List.cons formula) last)::tail; level }
    end;
    Context.assert_formula x.context formula

  let assert_formulas x formulas =
    action (AssertFormulas formulas) x;
    match formulas with
    | [] -> ()
    | _::_ ->
       let Assertions.{list; level} = !(x.assertions) in
       begin match list with
       | []         -> assert false
       | last::tail ->
          x.assertions := { list = (Option.map (List.rev_append (List.rev formulas)) last)::tail;
                            level }
       end;
       Context.assert_formulas x.context formulas

  let assert_blocking_clause x =
    action AssertBlockingClause x;
    let Assertions.{list; level} = !(x.assertions) in
    begin match list with
    | []         -> assert false
    | _::tail -> x.assertions := { list = None::tail; level }
    end;
    Context.assert_blocking_clause x.context

  let check ?param x =
    action (Check param) x;
    Context.check ?param x.context
    
  let check_with_assumptions ?param x assumptions =
    action (CheckWithAssumptions{param; assumptions}) x;
    if x.mcsat
    then
      begin
        let smodel, constraints = SModel.from_assumptions assumptions in
        Context.push x.context;
        Context.assert_formulas x.context constraints;
        Context.check_with_model ?param x.context smodel.model smodel.support
      end
    else
      Context.check_with_assumptions ?param x.context assumptions

  let stop x =
    action Stop x;
    Context.stop x.context
  let get_model ?(keep_subst=true) x =
    action (GetModel{ keep_subst }) x;
    Context.get_model ~keep_subst x.context

  let get_unsat_core x =
    action GetUnsatCore x;
    if x.mcsat
    then
      let rec subst t =
        let Term x = Term.reveal t in
        match x with
        | A0(`YICES_UNINTERPRETED_TERM,_) ->
           (try Purification.Term.get_body t with Not_found -> t)
        | _ ->
           Term.(map subst x |> build)
      in
      let interpolant = Context.get_model_interpolant x.context |> subst in
      Context.pop x.context;
      let Term termstruct = Term.reveal interpolant in
      match termstruct with
      | Types.Astar(`YICES_OR_TERM, l) -> List.map Term.not1 l
      | _ ->                              [Term.not1 interpolant]
    else
      Context.get_unsat_core x.context

  let declare_type x s =
    action (DeclareType s) x

  let declare_fun x s t =
    action (DeclareFun(s,t)) x

  let check_with_model ?param x model support =
    action (CheckWithModel{param; smodel = SModel.make ~support model}) x;
    Context.check_with_model ?param x.context model support

  let check_with_smodel ?param x smodel =
    action (CheckWithModel{param; smodel = smodel}) x;
    if x.mcsat
    then
      Context.check_with_model ?param x.context smodel.model smodel.support
    else
      smodel |> SModel.as_assumptions |> Context.check_with_assumptions ?param x.context

  let check_with_interpolation ?(build_model=true) ?param ctx_A ctx_B =
    action
      (CheckWithInterpolation{param; build_model; is_first = true;
                              other_assertions = !(ctx_B.assertions);
                              other_log        = !(ctx_B.log)})
      ctx_A;
    action
      (CheckWithInterpolation{param; build_model; is_first = false;
                              other_assertions = !(ctx_A.assertions);
                              other_log        = !(ctx_A.log)})
      ctx_B;
    Context.check_with_interpolation ~build_model ?param ctx_A.context ctx_B.context

  let get_model_interpolant x =
    action GetModelInterpolant x;
    if x.mcsat
    then
      Context.get_model_interpolant x.context
    else
      Context.get_unsat_core x.context |> Term.andN |> Term.not1

end

module Term = struct
  include TermSexp

  let count = ref 0
  let all_uninterpreted = ref (Some [])
  let () = Global.register_cleanup
             (fun ~after ->
               count := 0;
               match after with
               | `GC -> all_uninterpreted := None; 
               | _   -> all_uninterpreted := Some [])

  let new_uninterpreted ?(contexts=[]) ?name ty =
    let name = match name with
      | Some name -> name
      | None -> incr count; "x"^string_of_int !count
    in
    let t = new_uninterpreted ~name ty in
    all_uninterpreted := Option.map (fun l -> t::l) !all_uninterpreted;
    let action = Action.DeclareFun(name,ty) in
    global_log := action::!global_log;
    List.iter (fun x -> if !(x.Context.is_alive) then Context.action action x) contexts;
    t

  let all_uninterpreted() =
    Option.get_exn_or
      "Can't safely tell you all uninterpreted terms after garbage collection"
      !all_uninterpreted

end

module Type = struct
  include TypeSexp

  let count = ref 0
  let all_uninterpreted = ref (Some [])
  let () = Global.register_cleanup
             (fun ~after ->
               count := 0;
               match after with
               | `GC -> all_uninterpreted := None; 
               | _   -> all_uninterpreted := Some [])

  let new_uninterpreted ?(contexts=[]) ?name () =
    let name = match name with
      | Some name -> name
      | None -> incr count; "x"^string_of_int !count
    in
    let t = new_uninterpreted ~name () in
    all_uninterpreted := Option.map (fun l -> t::l) !all_uninterpreted;
    let action = Action.DeclareType name in
    global_log := action::!global_log;
    List.iter (fun x -> if !(x.Context.is_alive) then Context.action action x) contexts;
    t

  let all_uninterpreted() =
    Option.get_exn_or
      "Can't safely tell you all uninterpreted types after garbage collection"
      !all_uninterpreted

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

module GC = struct
  include GC
  let garbage_collect ?(record_log=false) ?(contexts=[]) term_list type_list keep_named =
    (* If the users asks, we compile the current log *)
    let sexps  = if record_log then List.fold_left Action.to_sexp [] !global_log else [] in
    let action = Action.GarbageCollect sexps in
    List.iter (fun x -> if !(x.Context.is_alive) then Context.garbage_collect x) contexts;
    garbage_collect term_list type_list keep_named;
    global_log := [action]
end

module Param = struct
  include Param
  let default Context.{context; _} = default context 
end
