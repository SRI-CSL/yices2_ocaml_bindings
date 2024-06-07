open Containers

open Sexplib
open Type

open Common

include High
open Types

open Ext_types
open Types

module type API = Ext_types.API

exception PopLastLevel

let pp_sep fmt () = Format.fprintf fmt "@ "
let pp_sep_break fmt () = Format.fprintf fmt "@,"

let sexp f arg = List(Atom f::arg)
let rec pp_sexp fmt = function
  | Atom s -> Format.fprintf fmt "@[%s@]" s
  | List l -> Format.fprintf fmt "@[<hov1>(%a)@]" (List.pp ~pp_sep pp_sexp) l

let use_type_names = ref true
let use_term_names = ref true
let use_type_notations = ref true
let use_term_notations = ref true

let match_n use_ref get t =
  if !use_ref then get t
  else None

let match_nn use_notation get_notation use_names get_names t =
  match match_n use_notation get_notation t with
  | None -> match_n use_names get_names t
  | (Some _) as s -> s

let notation replace hooks t =
  let aux cont =
    replace hooks t (lazy (Format.sprintf "%t" cont))
  in
  Format.kdprintf aux

type context = {
    config     : config option;
    context    : context_ptr;
    assertions : assertions ref;
    options    : unit HStrings.t;
    id         : int;
    is_alive   : bool ref;
    config_options : String.t HStrings.t;
    mcsat      : bool;
    last_check_model : unit HTerms.t;
    blocked    : bool ref
  }

module type ErrorHandling = ErrorHandling with type 'a t = 'a

module Make(EH: ErrorHandling with type 'a t = 'a) = struct

  module HighAPI = High.Make(EH)

  include HighAPI

  module Purification = Purification.Make(EH)(HighAPI)

  (* Configs but with settings available *)

  module Config = struct

    open Types
    type t = config

    let malloc () = Config{
        config  = Config.malloc();
        options = HStrings.create 10;
        logic   = ref None;
        mcsat   = ref false
      }

    let free (Config{ config; _}) = Config.free config
    let set (Config{ config; mcsat; options; _ }) ~name ~value =
      HStrings.replace options name value;
      if String.equal name "solver-type"
      then mcsat := String.equal value "mcsat";
      Config.set config ~name ~value

    let mcsat_logics = HStrings.create 10

    let () =
      ["QF_NRA"; "QF_NIA"; "QF_NIRA";
       "QF_UFNRA"; "QF_UFNIA";
       "QF_ANIA"; "QF_AUFNIA"; "QF_AUFBVNIA"]
      |> Seq.of_list
      |> Seq.map (fun x->(x,()))
      |> HStrings.replace_seq mcsat_logics
    
    let default ?logic (Config{ config; mcsat; logic = l; _ }) =
      let () = match logic with
        | Some logic when HStrings.mem mcsat_logics logic -> l := Some logic; mcsat := true
        | _ -> mcsat := false
      in
      Config.default ?logic config

    let get (Config{ options; _ })     = HStrings.find options
    let options (Config{ options; _ }) = HStrings.to_list options

  end
  
  
  (* Type but with S-expression export *)

  module TypeSexp = struct
    include Type
    let pph height fmt t =
      let display = { width = 80; height; offset = 0 } in
      if is_good t then t |> PP.type_string ~display |> Format.fprintf fmt "%s"
      else Format.fprintf fmt "null_type"

    let hooks          = Global.hTypes_create 100
    let notation t     = notation HTypes.replace hooks t
    let get_notation t = HTypes.get hooks t |> Option.map Lazy.force

    let to_name_opt t =
      if Type.Names.has_name t then Some(Type.Names.to_name t)
      else None
      
    let rec to_sexp ~smt2arrays typ =
      match match_nn use_type_notations get_notation use_type_names to_name_opt typ with
      | Some s -> Atom s
      | None ->
         match reveal typ with
         | Bool -> Atom "Bool"
         | Int  -> Atom "Int"
         | Real -> Atom "Real"
         | BV i -> List[Atom "_"; Atom "BitVec"; Atom(string_of_int i)]
         | Scalar _
           | Uninterpreted _ ->
            Atom(PP.type_string ~display:{width = 80; height = 10000; offset = 0} typ)
         | Tuple l -> l |> List.map (to_sexp ~smt2arrays) |> sexp "Tuple" (* Not official SMTLib syntax, but accepted by e.g. CVC5 *)
         | Fun { dom; codom } ->
            let codom = to_sexp ~smt2arrays codom in
            match smt2arrays with
            | None -> (* Not official SMTLib syntax for function types *)
               let dom = List.map (to_sexp ~smt2arrays) dom in
               sexp "Fun" (dom @ [codom])
            | Some(`Curry,_) -> (* SMT2Lib arrays, curryfying for multi-arguments *)
               let aux sofar domi = sexp "Array" [to_sexp ~smt2arrays domi; sofar] in
               dom |> List.rev |> List.fold_left aux codom
            | Some(`Tuple,_) -> (* SMT2Lib arrays, using tuples for multi-arguments,
                                   not official but accepted in e.g. CVC5 *)
               match List.map (to_sexp ~smt2arrays) dom with
               | [dom] -> sexp "Array" [ dom; codom]
               | dom   -> sexp "Array" [ sexp "Tuple" dom; codom]

    let pp fmt t =
      if is_good t then to_sexp ~smt2arrays:None t |> pp_sexp fmt
      else Format.fprintf fmt "null_type"

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
      let display = { width = 80; height; offset = 0 } in
      if is_good t then t |> PP.term_string ~display |> Format.fprintf fmt "%s"
      else Format.fprintf fmt "null_term"

    let rec pow_accu accu (t,i) =
      if i < 0 then EH.raise_bindings_error
                       "Exponent should be positive in a power product, not %i" i
      else if i = 0 then accu
      else pow_accu (t::accu) (t,i-1) 
    
    (* let pow i t =  *)
    (*   if i <= 0 then EH.raise_bindings_error *)
    (*                    "Exponent should be positive in a power product, not %i" i *)
    (*   else *)
    (*     if i = 1 then t *)
    (*     else *)
    (*       let rec aux i accu = *)
    (*         if i = 0 then accu else aux (i-1) (t::accu)  *)
    (*       in *)
    (*       sexp "*" (aux i []) *)

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
      let f k =
        let Term a = Term.reveal k in
        fv_termstruct a
      in
      HTerms.get_or_add fv_table ~f ~k:t
    
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

    let to_string = PP.term_string ~display:{width = 800000; height = 10000; offset = 0}

    let to_sexp_termstruct
        : type a. smt2arrays: ([`Tuple | `Curry ]*(t -> bool)) option -> (t -> Sexp.t) -> a termstruct -> Sexp.t =
      fun ~smt2arrays to_sexp ->
      function
      | A0(c, t) ->
         begin match c with
         | `YICES_BV_CONSTANT ->
            let s = to_string t in
            let s =
              if String.length s < 2
              then EH.raise_bindings_error
                     "bv constant as a string should have at least 2 characters, got %s instead" s
              else
                match String.sub s 0 2 with
                | "0b" -> "#b"^String.sub s 2 (String.length s -2)
                | "0x" -> "#x"^String.sub s 2 (String.length s -2)
                | _ -> s
            in
            Atom s
         | `YICES_ARITH_CONSTANT -> sexp_gmpq t
            (* (try *)
            (*    sexp_gmpq t *)
            (*  with ExceptionsErrorHandling.YicesBindingsException _ -> Atom(s())) *)
         | `YICES_SCALAR_CONSTANT ->
            let i = scalar_const_value t in
            List[Atom "_";
                 Atom "Const";
                 Atom(string_of_int i);
                 TypeSexp.to_sexp ~smt2arrays (Term.type_of_term t)] 
         | _ -> Atom(to_string t)
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
           | `YICES_TUPLE_TERM    -> sexp "tuple"    args (* Not official SMTLib syntax, but accepted by e.g. CVC5 *)
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
         let binder = 
           match c with
           | `YICES_FORALL_TERM -> "forall"
           (* Not official SMTLib syntax, which is not higher-order *)
           | `YICES_LAMBDA_TERM -> "lambda" 
         in
         let aux v = List[to_sexp v; TypeSexp.to_sexp ~smt2arrays (type_of_term v)] in
         sexp binder [List(List.map aux vars); to_sexp body]
      | App(f,l) ->
         begin
           let f_sexp = to_sexp f in
           let l_sexp = List.map to_sexp l in
           match smt2arrays with
           | None -> List (f_sexp::l_sexp)
           | Some(mode, as_function) ->
              if as_function f then List (f_sexp::l_sexp)
              else
                match mode with
                | `Curry ->
                   let aux sofar argi = sexp "select" [sofar; argi] in
                   List.fold_left aux f_sexp l_sexp
                | `Tuple -> (* Not official SMTLib syntax, but accepted by e.g. CVC5 *)
                   match l_sexp with
                   | [l_sexp] -> sexp "select" [f_sexp; l_sexp]
                   | _        -> sexp "select" [f_sexp; sexp "tuple" l_sexp]
         end
      | Update { array; index; value} ->
         let array_sexp   = to_sexp array in
         let indices_sexp = List.map to_sexp index in
         let value_sexp   = to_sexp value in
         begin
           match smt2arrays with
           | None -> sexp "store" (array_sexp::(indices_sexp @ [value_sexp]))
           | Some(mode, as_function) ->
              if as_function array
              then
                EH.raise_bindings_error
                  "Cannot update a function in SMTLib2: %s"
                  (to_string array)
              else
                match mode with
                | `Curry ->
                   let aux (app_i, cont) arg_i =
                     sexp "select" [app_i; arg_i],
                     fun c -> sexp "store" [app_i; arg_i; c]
                   in
                   let _, cont = List.fold_left aux (array_sexp, fun c -> c) indices_sexp in
                   cont value_sexp
                | `Tuple -> (* Not official SMTLib syntax, but accepted by e.g. CVC5 *)
                   match indices_sexp with
                   | [index] -> sexp "store" [array_sexp; index; value_sexp]
                   | _       -> sexp "store" [array_sexp; sexp "tuple" indices_sexp; value_sexp]
         end
      | Projection(c,i,t) -> (* Not official SMTLib syntax *)
         let name =
           match c with
           | `YICES_SELECT_TERM -> "tuple.select" (* but accepted by e.g. CVC5 *)
           | `YICES_BIT_TERM    -> "bit_term"
         in
         let head = List[Atom "_"; Atom name; Atom(string_of_int i)] in
         List[head; to_sexp t]
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
         let aux sofar (term, exp) = pow_accu sofar (to_sexp term, Unsigned.UInt.to_int exp) in
         sexp (if isBV then "bvmul" else "*") (List.fold_left aux [] l) 

  end

  module BoolStruct = struct

    type 'a t = 'a bool_struct [@@deriving eq, show]

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

    type t = slice

    let build ?indices extractee =
      assert(Term.is_bitvector extractee);
      match indices with
      | Some(lo,hi) when not(Int.equal lo 0 && Int.equal hi (TermTMP.width_of_term extractee)) ->
         Slice{extractee; indices}
      | _ -> Slice{ extractee; indices = None }

    let to_term (Slice{extractee; indices}) =
      match indices with
      | None -> extractee
      | Some(lo,hi) -> 
         Term.BV.bvextract extractee lo (hi-1)

    let to_sexp to_sexp_term (Slice{extractee; indices}) = 
      match indices with
      | Some(lo,hi) ->
         let f = sexp "_" [Atom "extract"; Atom(string_of_int(hi - 1)); Atom(string_of_int lo)] in
         List[f; to_sexp_term extractee]
      | _ -> to_sexp_term extractee

    let width (Slice{ extractee; indices }) = match indices with
      | None -> TermTMP.width_of_term extractee
      | Some(lo, hi) -> hi - lo

    let is_free ~var (Slice{extractee; _}) = TermTMP.is_free ~var extractee
    let fv (Slice{extractee; _}) = TermTMP.fv extractee
  end

  (* Terms extended with primitive constructs for
     bvand, bvor, bvnot, sign-extend, zero-extend, concat, etc *)

  module ExtTerm = struct

    type nonrec ('a,'b) base = ('a,'b) base
    type 'a closed     = ('a, [`closed])  base
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
      | SliceStruct slice ->
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
      | SliceStruct _ as t  -> t
      | Concat _ as t -> t

    let rec width : type a b. (a, b) base -> int = function
      | TermStruct a -> TermTMP.(width_of_term(build a))
      | T a          -> TermTMP.(width_of_term a)
      | Bits l       -> List.length l
      | SliceStruct slice ->
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
      | SliceStruct slice ->
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
      | SliceStruct slice ->
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
        SliceStruct(BoolStruct.map aux bstruct)
      in
      let init_bits bit = Bits [bit] in
      let close_slice : type a. a bs closed -> a bs closed = function
        | SliceStruct slice ->
           SliceStruct(BoolStruct.nnf true slice)
        | Bits l      -> Bits(List.rev l)
      in
      let open Types in
      (* Check if bit is t[i] *)
      let rec analyse bit =
        let open Option in
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
        match slice, bit with
        | Leaf(Slice{extractee; indices = Some(lo, hi) }), Leaf(t, i)
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
           | SliceStruct s, Some b ->
              begin
                match test s b with
                | Some s -> prolong_slice (SliceStruct s) ~sign:bit tail accu
                | None   -> (* End of the slice; we see whether we have extensions *)
                   let block = return_block(close_slice slice) in
                   prolong_block block ~sign bits accu
              end
           | SliceStruct _, None ->
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
      let ( let* ) = M.bind
      let (let+) a f = M.bind a (fun r -> M.return(f r))

      let rec mapList f = function
        | [] -> M.return []
        | head::tail ->
           let* h = f head in
           let+ t = mapList f tail in
           h::t

      type update = { apply : 'a. 'a closed -> 'a closed M.t }

      let auxTerm f t  = let+ T t     = f.apply(T t) in t
      let auxSlice f s = let+ SliceStruct s = f.apply(SliceStruct s) in s

      let mapSlice f = function
        | Leaf(Slice{ extractee; indices }) ->
           let+ extractee = auxTerm f extractee in
           Leaf(Slice{ extractee; indices })
        | And l -> let+ l = mapList (auxSlice f) l in And l
        | Or l  -> let+ l = mapList (auxSlice f) l in Or l
        | Not a -> let+ a = auxSlice f a in Not a

      let map : type a. update -> (a, [`tstruct]) base -> (a, [`tstruct]) base M.t =
        fun f -> function
              | TermStruct a  -> let+ a      = map (auxTerm f) a        in TermStruct a
              | SliceStruct slice -> let+ slice  = mapSlice f slice     in SliceStruct slice
              | Bits bits     -> let+ bits   = mapList (auxTerm f) bits in Bits bits
              | Concat concat -> let+ concat = mapList f.apply concat   in Concat concat
              | Block{block; sign_ext; zero_ext} ->
                 let+ block = f.apply block in Block{block; sign_ext; zero_ext}
      
    end
    
    type t  = ExtTerm  : _ closed -> t
    type yt = YExtTerm : _ termstruct -> yt

    let reveal_table = Global.hTerms_create 500

    let of_yterm : yterm -> yt = function
      | Term(Astar(`YICES_BV_ARRAY, bits)) -> (* In case of a BV_ARRAY, we preprocess *)
         begin
           match bvarray bits with
           | [Block{ block = SliceStruct _ as slice; sign_ext=0; zero_ext=0}] -> YExtTerm slice
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
    let get_notation t = HTerms.get hooks t |> Option.map Lazy.force

    let to_name_opt t =
      if Term.Names.has_name t then Some(Term.Names.to_name t)
      else None

    let rec to_sexp :
              type a b. smt2arrays:([`Tuple | `Curry ]*(Term.t -> bool)) option -> (a, b) base -> Sexp.t =
      fun ~smt2arrays ->
      function
      | TermStruct a ->
         TermTMP.to_sexp_termstruct ~smt2arrays (to_sexp_t ~smt2arrays) a
      | T a          -> to_sexp_t ~smt2arrays a
      | Bits  block  ->
         let aux t =
           let Term s = Term.reveal t in
           match s with
           | A0 _ ->
              begin
                match Term.const_value s with
                | `Bool b -> Some b
                | _ -> None
              end
           | _ -> None
         in
         if List.for_all (fun x -> Option.is_some(aux x)) block
         then
           let rec pp fmt = function
             | [] -> ()
             | hd::tl ->
                match aux hd with
                | Some true  -> Format.fprintf fmt "%a1" pp tl
                | Some false -> Format.fprintf fmt "%a0" pp tl
                | None -> ()
           in
           Atom (Format.sprintf "#b%a" pp block)
         else
           (* Not official SMTLib syntax *)
           sexp "bool-to-bv" (List.map (to_sexp_t ~smt2arrays) block)
      | SliceStruct slice ->
         let rec aux = function
           | Leaf slice -> SliceTMP.to_sexp (to_sexp_t ~smt2arrays) slice
           | And l  -> sexp "bvand" (List.map aux l)
           | Or l   -> sexp "bvor"  (List.map aux l)
           | Not a  -> sexp "bvnot" [aux a]
         in
         aux slice
      | Concat l    -> sexp "concat" (List.rev_map (to_sexp ~smt2arrays) l)
      | Block{block; sign_ext; zero_ext} ->
         let block = to_sexp ~smt2arrays block in
         let f string i term = List[ sexp "_" [Atom string; Atom(string_of_int i)] ; term ] in
         let block = if sign_ext = 0 then block else f "sign_extend" sign_ext block in
         if zero_ext = 0 then block else f "sign_extend" zero_ext block

    and to_sexp_t : smt2arrays:([`Tuple | `Curry ]*(Term.t -> bool)) option -> Term.t -> Sexp.t =
      fun ~smt2arrays t ->
      match
        match_nn use_term_notations get_notation use_term_names to_name_opt t
      with
      | Some s -> Atom s
      | None ->
         let YExtTerm t = of_term t in
         to_sexp ~smt2arrays t
    
    let pp fmt t =
      t |> to_sexp ~smt2arrays:None |> pp_sexp fmt
    
  end

  (* Term but with S-expression export *)

  module TermSexp = struct
    include TermTMP
    let notation = ExtTerm.notation
    let to_sexp = ExtTerm.to_sexp_t
    let pp fmt t =
      if is_good t then to_sexp ~smt2arrays:None t |> pp_sexp fmt
      else Format.fprintf fmt "null_term"
  end

  module Slice = struct
    include SliceTMP
    let to_sexp ~smt2arrays = to_sexp (TermSexp.to_sexp ~smt2arrays)
    let pp fmt t = to_sexp ~smt2arrays:None t |> pp_sexp fmt
  end

  module Model = struct
    include Model
    let pph height fmt t =
      t |> PP.model_string ~display:{ width = 100; height; offset=0}
      |> Format.fprintf fmt "%s"
    let pp = pph 1000

    let set output var : yval -> unit = function
      | `Bool b      -> set_bool output var b
      | `Rational r  -> set_mpq output var r
      | `BV(_,l)     -> set_bv_from_list output var l
      | `Algebraic a -> set_algebraic_number output var a.libpoly
      | `Scalar _    -> EH.raise_bindings_error
                          "MCSAT approach to building model does not support scalar values"
      | `Fun _       -> EH.raise_bindings_error
                          "MCSAT approach to building model does not support function values"
      | `Tuple _       -> EH.raise_bindings_error
                            "MCSAT approach to building model does not support tuple values"
    
  end

  module Value = struct

    let reveal = Model.reveal

    let rec pp m fmt : yval -> unit = function
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

  (* Supported models *)
  module SModel = struct

    type t = smodel

    let make ?support model =
      let support = match support with
        | None -> Model.collect_defined_terms model |> List.sort Term.compare
        | Some support -> support
      in
      SModel{ support; model }

    let free (SModel{model; _}) = Model.free model

    let from_map ?support m =
      let support = Option.get_lazy (fun () -> List.map fst m) support in
      Model.from_map m |> make ~support

    let as_map (SModel{model; support}) =
      let aux t = (t, Model.get_value_as_term model t) in
      List.(rev_map aux support |> rev)

    let copy mcsat (SModel{model; support} as smodel) =
      if mcsat
      then
        let model = Model.empty() in
        let add var =
          Model.get_value model var
          |> Value.reveal model
          |> Model.set model var
        in
        List.iter add support;
        make ~support model
      else
        smodel |> as_map |> from_map
    
    let empty() = make ~support:[] (Model.empty())
    
    let to_sexp ~smt2arrays (SModel{model; support}) =
      let aux t =
        let ty = TypeSexp.to_sexp ~smt2arrays (Term.type_of_term t) in
        let vars, body =
          let yval = Model.get_value model t in
          match Model.reveal model yval with
          | `Algebraic v ->
             Sexp.List [], Sexp.Atom (Algebraic.to_string v.libpoly)
          | _ ->
             let v = Model.val_as_term model yval in
             if Term.is_good v
             then
               match Term.reveal v with
               | Term(Bindings { vars; body; _ }) ->
                  let aux v = List[TermSexp.to_sexp ~smt2arrays v;
                                   TypeSexp.to_sexp ~smt2arrays (Term.type_of_term v)] in  
                  Sexp.List(List.map aux vars), TermSexp.to_sexp ~smt2arrays body
               | _ -> Sexp.List [], TermSexp.to_sexp ~smt2arrays v
             else
               Sexp.List [], Sexp.Atom "Can't print"
        in
        sexp "define-fun" [TermSexp.to_sexp ~smt2arrays t; vars; ty; body]
      in
      Sexp.List(support |> List.rev |> List.rev_map aux)


    let pp ?pp_start ?pp_stop ?pp_sep () fmt (SModel{support;model}) =
      let aux fmt u =
        let v = Model.get_value model u in
        Format.fprintf fmt "@[<2>%a :=@ @[%a@]@]" TermSexp.pp u (Value.pp_val model) v
      in
      match support with
      | [] -> Format.fprintf fmt "[]"
      | support -> Format.fprintf fmt "%a" (List.pp ?pp_start ?pp_stop ?pp_sep aux) support


    let as_assumptions (SModel{support;model}) =
      let aux sofar t =
        let a =
          if Term.is_bool t
          then if Model.get_bool_value model t then [t] else [Term.not1 t]
          else
            let val_as_term = Model.get_value_as_term model t in
            if Term.is_bitvector t
            then
              [Term.BV.bvle t val_as_term ; Term.BV.bvle val_as_term t]
            else if Term.is_arithmetic t
            then
              [Term.Arith.leq t val_as_term ; Term.Arith.leq val_as_term t]
            else
              [Term.eq t val_as_term]
        in
        a@sofar
      in
      List.(fold_left aux [] support |> rev)

    let from_assumptions ~mcsat ?smodel assumptions =
      match List.sort_uniq ~cmp:Term.compare assumptions with
      | [] -> Option.get_lazy empty smodel, [], []
      | assumptions ->
         let treat f (sofar, constraints) assumption =
           let atom, _     = Purification.Term.get_var assumption    in
           let constraints = Term.(atom === assumption)::constraints in
           f sofar atom, constraints
         in
         if mcsat
         then
           let SModel{ model; support } = Option.map_lazy empty (copy mcsat) smodel in
           let f pures atom = Model.set_bool model atom true; atom::pures in
           let pures, constraints = List.fold_left (treat f) ([],[]) assumptions in
           make ~support:(pures @ support) model, pures, constraints
         else
           let map = Option.map_or ~default:[] as_map smodel in
           let f map atom = (atom, Term.true0())::map in
           let pures, constraints = List.fold_left (treat f) ([],[]) assumptions in
           from_map (pures @ map), List.map fst pures, constraints

  end

  module Assertions = struct

    type t = assertions

    let init = Assertions {
        list = [Some []];
        level = 0;
      }

    exception BlockingClauseUsage

    let assertions (Assertions{list;_}) =
      let aux sofar = function
        | None -> raise BlockingClauseUsage
        | Some list -> List.rev_append list sofar
      in
      List.fold_left aux [] list

    let level_to_sexp ~smt2arrays = function
      | None -> Sexp.Atom "BlockingClauseUsage"
      | Some assertions -> Sexp.List (List.map (TermSexp.to_sexp ~smt2arrays) assertions)

    let to_sexp ?smt2arrays (Assertions{ list; _ }) =
      Sexp.List (List.map (level_to_sexp ~smt2arrays) list)
    
    let pp_level fmt = function
      | None ->
         Format.fprintf fmt "@[<v>??@]"
      | Some assertions ->
         Format.fprintf fmt "@[<v>%a@]" (List.pp TermSexp.pp) assertions

    let pp fmt (Assertions{list;_}) = 
      Format.fprintf fmt "@[<v>%a@]" (List.pp pp_level) list

  end

  module Action = struct

    type t = action

    let to_sexp_context ?smt2arrays = function
      | Status -> sexp "get-status" []
      | Reset  -> sexp "reset-assertions" []
      | Push   -> sexp "push" []
      | Pop    -> sexp "pop" []
      | EnableOption s -> sexp "set-option" [Atom s; Atom "true"] 
      | DisableOption s -> sexp "set-option" [Atom s; Atom "false"]
      | AssertFormula t -> sexp "assert" [TermSexp.to_sexp ~smt2arrays t]
      | AssertFormulas l ->
         sexp "assert" (List.map (TermSexp.to_sexp ~smt2arrays) l)
      | AssertBlockingClause -> sexp "assert-blocking-clause" []
      | Check _param -> sexp "check-sat" [] 
      | CheckWithAssumptions{ assumptions; _ } ->
         sexp "check-sat-assuming" (List.map (TermSexp.to_sexp ~smt2arrays) assumptions)
      | Stop         -> sexp "stop" []
      | GetModel _   -> sexp "get-model" []
      | GetUnsatCore -> sexp "get-unsat-core" []
      | CheckWithModel{ smodel; _} ->
         sexp "check-sat-assuming-model" [SModel.to_sexp ~smt2arrays smodel]
      | GetModelInterpolant -> sexp "get-unsat-model-interpolant" []

    let to_sexp ?smt2arrays accu = function
      | DeclareType(typ, card) ->
         let typ_string = Format.sprintf "%a" TypeSexp.pp typ in
         sexp "declare-sort"
           (Option.map_or
              ~default:[Atom typ_string; Atom "0"]
              (fun card -> [Atom typ_string; Atom "0"; Atom(string_of_int card)]) card)
         ::accu
      | DeclareFun(t, typ) ->
         let symb = Atom(TermSexp.to_string t) in
         sexp "declare-fun"
           begin
             match smt2arrays, Type.reveal typ with
             | None, Fun{ dom; codom} ->
                let dom = List.map (TypeSexp.to_sexp ~smt2arrays) dom in
                [symb; List dom; TypeSexp.to_sexp ~smt2arrays codom]
             | Some(mode, as_fun), Fun{ dom; codom} when as_fun t ->
                let dom = List.map (TypeSexp.to_sexp ~smt2arrays) dom in
                [symb; List dom; TypeSexp.to_sexp ~smt2arrays codom]
             | _ ->
                [symb; List[]; TypeSexp.to_sexp ~smt2arrays typ]
           end
         ::accu
      | DefineType(name, typ) ->
         let old = !use_type_names in
         use_type_names := false;
         let typ = TypeSexp.to_sexp ~smt2arrays typ in
         use_type_names := old;
         sexp "define-sort" [Atom name; List []; typ]
         ::accu
      | DefineFun(name, t, typ) ->
         let old = !use_term_names in
         use_term_names := false;
         let args =
           let Term termstruct = Term.reveal t in
           match smt2arrays, termstruct with
           | _, Bindings{ c = `YICES_LAMBDA_TERM; vars; body } ->
              let aux var =
                List [
                    TermSexp.to_sexp ~smt2arrays var;
                    TypeSexp.to_sexp ~smt2arrays (Term.type_of_term var)
                  ]
              in
              let vars = List.map aux vars in
              [Atom name;
               List vars;
               TypeSexp.to_sexp ~smt2arrays (Term.type_of_term body);
               TermSexp.to_sexp ~smt2arrays body]
           | _, _ -> [Atom name;
                      List [];
                      TypeSexp.to_sexp ~smt2arrays typ;
                      TermSexp.to_sexp ~smt2arrays t]
         in
         use_term_names := old;
         sexp "define-fun" args ::accu
      | CheckWithInterpolation{ build_model; context1; context2; _} ->
         let build_model = Atom(if build_model then "build_model" else "no_build_model") in
         sexp "check-sat-with-interpolation"
           [build_model; Atom(string_of_int context1); Atom(string_of_int context2)] ::accu
      | GarbageCollect sexpl -> sexp "garbage-collect" sexpl ::accu
      | NewContext { logic = Some logic } ->
         sexp "set-logic" [Atom logic] ::accu
      | NewContext { logic = None } ->
         sexp "new-context" [] ::accu
      | ContextAction { context_id ; context_action } ->
         let action = to_sexp_context ?smt2arrays context_action in
         if context_id = 0 then action::accu
         else sexp "context" [Atom(string_of_int context_id);action]::accu 

  end

  let global_log = ref []
  let () = Global.register_cleanup (fun ~after:_ -> global_log := [])

  module Global = struct
    include HighAPI.Global
    let log () = !global_log
    let to_sexp ?smt2arrays () =
      log () |> List.fold_left (Action.to_sexp ?smt2arrays) [] 
    let pp_log fmt =
      Format.fprintf fmt "%a"
        (List.pp ~pp_sep:pp_sep_break pp_sexp) (to_sexp ())
  end
  
  module Context = struct

    let pp_options fmt options =
      Format.fprintf fmt "@[<v>%a@]" (HStrings.pp String.pp Format.silent) options

    let pp_config_options fmt config_options =
      Format.fprintf fmt "@[<v>%a@]" (HStrings.pp String.pp String.pp) config_options

    type t = context

    let assertions ctx     = !(ctx.assertions)
    let options ctx        = HStrings.copy ctx.options
    let config_options ctx = HStrings.copy ctx.config_options
    let log ctx      =
      let aux (sofar,context_count) = function
        | ContextAction { context_id ; context_action } ->
           if Int.equal context_id ctx.id
           then ContextAction { context_id = 0 ; context_action }::sofar, context_count
           else sofar, context_count
        | NewContext _ as action ->
           if Int.equal context_count ctx.id then (action::sofar,context_count+1)
           else (sofar,context_count+1)
        | action -> action::sofar, context_count
      in
      List.(!global_log |> rev |> fold_left aux ([],0) |> fst) 
    let is_alive ctx = !(ctx.is_alive)
    let is_mcsat ctx = ctx.mcsat
    let id ctx       = ctx.id

    module HContext = Hashtbl.Make(Int)
    let all = HContext.create 10
    let next_id = ref 0
    let () = Global.register_cleanup (* We don't erase contexts after GC *)
               (fun ~after ->
                 match after with
                 | `GC -> ()
                 | _ -> HContext.reset all; next_id :=0)

    let of_id = HContext.find_opt all

    let pp fmt {assertions; _} = Assertions.pp fmt !assertions

    let to_sexp ?smt2arrays ctx =
      log ctx |> List.fold_left (Action.to_sexp ?smt2arrays) [] 

    let pp_log fmt ctx =
      Format.fprintf fmt "%a"
        (List.pp ~pp_sep:pp_sep_break pp_sexp) (to_sexp ctx)

    let malloc ?config () =
      let yconfig = Option.map (fun (Config{config;_}) -> config) config in
      let config_options, mcsat, logic =
        match config with
        | Some(Config{options; mcsat; logic; _}) ->
           HStrings.copy options, !mcsat, !logic
        | None -> HStrings.create 1, false, None
      in
      let action = NewContext{ logic }
      in
      global_log := action::!global_log;
      let context = 
        { config     = config;
          context    = Context.malloc ?config:yconfig ();
          assertions = ref Assertions.init;
          options    = HStrings.create 10;
          id         = !next_id;
          is_alive   = ref true;
          config_options;
          last_check_model = Global.hTerms_create 50;
          blocked    = ref false;
          mcsat
        }
      in
      HContext.add all context.id context;
      incr next_id;
      context

    let malloc_mcsat () =
      let cfg = Config.malloc () in
      Config.set cfg ~name:"solver-type" ~value:"mcsat";
      Config.set cfg ~name:"model-interpolation" ~value:"true";
      let ctx = malloc ~config:cfg () in
      Config.free cfg;
      ctx

    let malloc_logic logic =
      let cfg = Config.malloc () in
      Config.default ~logic cfg;
      let ctx = malloc ~config:cfg () in
      Config.free cfg;
      ctx

    let free {context; is_alive; id; _ } =
      if not !is_alive
      then EH.raise_bindings_error "Trying to free dead context #%i" id;
      Context.free context;
      is_alive := false

    let action a x =
      if not !(x.is_alive)
      then EH.raise_bindings_error "Trying to take action %a on dead context #%i"
             pp_sexp (Action.to_sexp_context a)
             x.id;
      global_log := ContextAction{ context_id = x.id; context_action = a}::!global_log

    let status x =
      action Status x;
      Context.status x.context

    let reset x =
      action Reset x;
      Context.reset x.context;
      x.assertions := Assertions.init;
      x.blocked := false

    let pop x =
      action Pop x;
      let Assertions{list; level} = !(x.assertions) in
      begin match list with
      | []     -> assert false
      | [_last] -> raise PopLastLevel
      | _::tail -> x.assertions := Assertions{ list = tail; level = level - 1 }
      end;
      Context.pop x.context

    let unblock x = if !(x.blocked) then (pop x; x.blocked := false)

    let pop x = 
      unblock x;
      pop x
    
    let push x =
      unblock x;
      action Push x;
      let Assertions{list; level} = !(x.assertions) in
      x.assertions := Assertions{
          list = Some []::list;
          level = level+1;
        };
      Context.push x.context

    let goto x level =
      unblock x;
      let Assertions{ level = x_level; _ } = !(x.assertions) in
      let diff = level - x_level in
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
      unblock x;
      action (AssertFormula formula) x;
      let Assertions{list; level} = !(x.assertions) in
      begin match list with
      | []         -> assert false
      | last::tail -> x.assertions :=
                        Assertions{ list = (Option.map (List.cons formula) last)::tail;
                                    level }
      end;
      Context.assert_formula x.context formula

    let assert_formulas x formulas =
      unblock x;
      action (AssertFormulas formulas) x;
      match formulas with
      | [] -> ()
      | _::_ ->
         let Assertions{list; level} = !(x.assertions) in
         begin match list with
         | []         -> assert false
         | last::tail ->
            x.assertions :=
              Assertions{ list =
                            (Option.map (List.rev_append (List.rev formulas)) last)::tail;
                          level }
         end;
         Context.assert_formulas x.context formulas

    let assert_blocking_clause x =
      unblock x;
      action AssertBlockingClause x;
      let Assertions{list; level} = !(x.assertions) in
      begin match list with
      | []         -> assert false
      | _::tail -> x.assertions := Assertions{ list = None::tail; level }
      end;
      Context.assert_blocking_clause x.context

    let check ?param ?assumptions ?smodel ?hints x =
      unblock x;
      HTerms.reset x.last_check_model;
      match assumptions, smodel, hints with
      | None, None, None -> 
         action (Check param) x;
         Context.check ?param x.context
      | _ ->
         let assumptions = Option.get_or ~default:[] assumptions in
         let aux t = HTerms.add x.last_check_model t () in
         if x.mcsat
         then
           let SModel{model; support} as smodel, pures, constraints =
             SModel.from_assumptions ~mcsat:true ?smodel assumptions
           in
           begin
             (match constraints with
              | []   -> ()
              | _::_ ->
                 push x;
                 assert_formulas x constraints;
                 x.blocked := true;
                 List.iter aux pures
             );
             action (CheckWithModel{param; smodel}) x;
             match hints with
             | None -> Context.check_with_model ?param x.context model support
             | Some hints ->
                Context.check_with_model_and_hint ?param x.context model ~hard:support ~soft:hints
           end
         else
           let assumptions, extra =
             match smodel with
             | Some smodel -> let extra = SModel.as_assumptions smodel in
                              assumptions @ extra, extra
             | None -> assumptions, []
           in
           List.iter aux extra;
           action (CheckWithAssumptions{param; assumptions}) x;
           Context.check_with_assumptions ?param x.context assumptions

    let set_fixed_var_order x vars = Context.set_fixed_var_order x.context vars

    let set_initial_var_order x vars = Context.set_initial_var_order x.context vars

    let get_model ?(keep_subst=true) ?support x =
      action (GetModel{ keep_subst }) x;
      Context.get_model ~keep_subst x.context |> SModel.make ?support 

    module Accu = StateMonad(struct type t = Term.t list end)
    module MTermAccu = MTerm(Accu)

    let get_model_interpolant_and_unsat_core x =
      if x.mcsat
      then
        let rec subst t =
          let Term r = Term.reveal t in
          let open Accu in
          match r with
          | A0(`YICES_UNINTERPRETED_TERM,_) ->
             fun accu -> (if HTerms.mem x.last_check_model t
                          then Term.true0(), Purification.Term.get_body t::accu
                          else t,accu)
          | _ ->
             bind (MTermAccu.map subst r) (fun x -> return(Term.build x))
        in
        action GetModelInterpolant x;
        let interpolant = Context.get_model_interpolant x.context in
        if HTerms.length x.last_check_model > 0
        then subst interpolant []
        else interpolant, []
      else
        begin
          action GetUnsatCore x;
          let raw_core = Context.get_unsat_core x.context in
          if HTerms.length x.last_check_model > 0
          then
            let interpolant, core = List.partition (HTerms.mem x.last_check_model) raw_core in
            interpolant |> Term.andN |> Term.not1, core
          else Term.true0(), raw_core
        end

    let get_model_interpolant x = get_model_interpolant_and_unsat_core x |> fst
    let get_unsat_core x = get_model_interpolant_and_unsat_core x |> snd

    let stop x =
      action Stop x;
      Context.stop x.context

    let check_with_interpolation ?(build_model=true) ?param ctx_A ctx_B =
      let raise =
        EH.raise_bindings_error "Trying to involve dead context %i in check with interpolation"
      in
      if not !(ctx_A.is_alive) then raise ctx_A.id;
      if not !(ctx_B.is_alive) then raise ctx_B.id;
      unblock ctx_A;
      unblock ctx_B;
      let action =
        CheckWithInterpolation{param; build_model;
                               context1 = ctx_A.id;
                               context2 = ctx_B.id}
      in
      global_log := action::!global_log;
      let status =
        Context.check_with_interpolation ~build_model ?param
          ctx_A.context ctx_B.context
      in
      match status with
      | #smt_inconclusive_status as x -> x
      | `STATUS_UNSAT interpolant -> `STATUS_UNSAT interpolant
      | `STATUS_SAT None ->
         `STATUS_SAT(fun ?support:_ () ->
             EH.raise_bindings_error
               "Asking for model after interpolation query without model construction.")
      | `STATUS_SAT(Some m) ->
         `STATUS_SAT(fun ?support () -> SModel.make ?support m)

    let all () = HContext.to_seq_values all
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

    let new_uninterpreted ?name ty =
      let name = match name with
        | Some name -> name
        | None -> incr count; "x"^string_of_int !count
      in
      let t = new_uninterpreted ~name ty in
      all_uninterpreted := Option.map (fun l -> t::l) !all_uninterpreted;
      global_log        := DeclareFun(t,ty)::!global_log;
      t

    let all_uninterpreted() =
      Option.get_exn_or
        "Can't safely tell you all uninterpreted terms after garbage collection"
        !all_uninterpreted

    let to_sexp ?smt2arrays = to_sexp ~smt2arrays

    module Names = struct
      include Names
      let set t name =
        global_log := DefineFun(name,t,Term.type_of_term t)::!global_log;
        set t name
    end
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

    let new_uninterpreted ?name ?card () =
      let name = match name with
        | Some name -> name
        | None -> incr count; "x"^string_of_int !count
      in
      let t = new_uninterpreted ~name ?card () in
      all_uninterpreted := Option.map (fun l -> t::l) !all_uninterpreted;
      global_log := DeclareType(t, card)::!global_log;
      t

    let all_uninterpreted() =
      Option.get_exn_or
        "Can't safely tell you all uninterpreted types after garbage collection"
        !all_uninterpreted

    let to_sexp ?smt2arrays = to_sexp ~smt2arrays:(Option.map (fun x -> x,()) smt2arrays)

    module Names = struct
      include Names
      let set typ name =
        global_log := DefineType(name,typ)::!global_log;
        set typ name
    end

  end

  module GC = struct
    include GC
    let garbage_collect ?(record_log=false) term_list type_list keep_named =
      (* If the users asks, we compile the current log *)
      let sexps  =
        if record_log
        then List.fold_left Action.to_sexp [] !global_log
        else []
      in
      global_log := [GarbageCollect sexps];
      garbage_collect term_list type_list keep_named
  end

  module Param = struct
    include Param
    let default {context; _} = default context 
  end

end

module WithExceptionsErrorHandling = Make(ExceptionsErrorHandling)
module WithNoErrorHandling = Make(NoErrorHandling)

module Types = struct

  include High.Types
  include Ext_types.Types

  type nonrec context = context

  open WithNoErrorHandling

  let pp_error_report fmt {badval; code; column; line; term1; term2; type1; type2} =
    Format.fprintf fmt
      "@[<v 1> \
       error: %s@,\
       code: %a@,\
       column %i line %i@,\
       term1: %a@,\
       term2: %a@,\
       type1: %a@,\
       type2: %a@,\
       bad val: %i@,\
       @]"
      (ErrorPrint.string ())
      pp_error_code code
      column line
      Term.pp term1
      Term.pp term2
      Type.pp type1
      Type.pp type2
      badval
end

