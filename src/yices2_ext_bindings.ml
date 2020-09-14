open Containers
open Ctypes

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

  let pp = pph 20

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

end

module Term = struct
  include Term
  let pph height fmt t =
    let display = Types.{ width = 80; height; offset = 0 } in
    try
      t |> PP.term_string ~display
      |> Format.fprintf fmt "%s"
    with _ -> Format.fprintf fmt "null_term"

  let pp = pph 20

  let pow i t = 
    if i = 0 then t
    else
      let rec aux i accu =
        if i = 0 then accu else aux i (t::accu) 
      in
      sexp "*" (aux i [])

  let rec to_sexp t =
    let Term x = reveal t in
    match x with
    | A0 _ ->
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
        | `YICES_BV_ARRAY      -> sexp "concat" args
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
      let f = to_sexp t in
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
        | `YICES_BIT_TERM    -> sexp "bitextract" [Atom(string_of_int i);t]
      end
    | BV_Sum l ->
      let width = t |> Term.type_of_term |> Type.bvsize in
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
      let one = Term.Arith.int 1 in
      let aux (coeff, term) =
        let coeff = Term.Arith.mpq coeff in
        match term with
        | Some term ->
          if Term.equal coeff one then to_sexp term
          else sexp "*" [to_sexp coeff; to_sexp term]
        | None -> to_sexp coeff
      in
      sexp "+" (List.map aux l) 

    | Product(isBV, l) ->
      let aux (term, exp) = pow (Unsigned.UInt.to_int exp) (to_sexp term) in
      sexp (if isBV then "bvmul" else "*") (List.map aux l) 

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
      (Term.pph 1000) term1
      (Term.pph 1000) term2
      (Type.pph 1000) type1
      (Type.pph 1000) type2
end

module Model = struct
  include Model
  let pph height fmt t =
    t |> PP.model_string ~display:Types.{ width = 100; height; offset=0}
    |> Format.fprintf fmt "%s"
  let pp = pph 1000
end

module Action = struct

  type param = Param.t
  let pp_param _ _ = ()

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
    | Check of param option
    | CheckWithAssumptions of param option * Term.t list
    | Stop
    | GetModel
    | GetUnsatCore

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

  let pp fmt {assertions} = pp_assertions fmt !assertions

  let to_sexp {log} =
    !log |> List.fold_left Action.to_sexp [] 

  let malloc ?config () =
    { config  = config;
      context = malloc ?config ();
      assertions = ref [[]];
      options = StringHashtbl.create 10;
      log = ref [] }

  let free   {context} = free context
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
      | [last] -> raise PopLastLevel
      | _::tail -> x.assertions := tail
    end;
    pop x.context

  let enable_option {context; options; log} ~option =
    log := EnableOption option::!log;
    enable_option context ~option;
    StringHashtbl.replace options option ()

  let disable_option {context; options; log} ~option =
    log := DisableOption option::!log;
    disable_option context ~option;
    StringHashtbl.remove options option 

  let assert_formula {context; assertions; log} formula =
    log := AssertFormula formula::!log;
    begin match !assertions with
      | []     -> assert false
      | last::tail -> assertions := (formula::last)::tail
    end;
    assert_formula context formula

  let assert_formulas {context; assertions; log} formulas =
    log := AssertFormulas formulas::!log;
    begin match !assertions with
      | []     -> assert false
      | last::tail ->
        assertions := (List.rev_append (List.rev formulas) last)::tail
    end;
    assert_formulas context formulas

  let check ?param {context;log} =
    log := Check param::!log;
    check ?param context

  let check_with_assumptions ?param {context; log} assumptions =
    log := CheckWithAssumptions(param, assumptions)::!log;
    check_with_assumptions ?param context assumptions

  let stop {context; log} =
    log := Stop::!log;
    stop context
  let get_model {context; log} =
    log := GetModel::!log;
    get_model context
  let get_unsat_core {context; log} =
    log := GetUnsatCore::!log;
    get_unsat_core context

  let declare_type {log} s =
    log := DeclareType s::!log

  let declare_fun {log} s t =
    log := DeclareFun(s,t)::!log

end

module Param = struct
  include Param
  let default Context.{context} = default context 
end
