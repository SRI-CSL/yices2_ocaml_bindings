[%%import "gmp.mlh"]
open Containers
open Ctypes
open Arg

open Sexplib
open Type
    
open Yices2_high
open Types

module Cont : sig
  type ('a, 'r) t
  val get : ('a, 'a) t -> 'a
  val ( let* ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t
  val return : 'a -> ('a, 'r) t
  val return1 : ('a -> 'b) -> 'a -> ('b, 'r) t
  val return2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, 'r) t
  val fold : ('a -> 'b -> ('b, 'c) t) -> 'a list -> 'b -> ('b, 'c) t
  val iter : ('a -> (unit, 'b) t) -> 'a list -> (unit, 'b) t
  val map : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
end = struct
  type ('a,'r) t = ('a -> 'r) -> 'r
  let get a = a (fun x->x)
  let (let*) (x : ('a,'r) t) (f : ('a -> ('b,'r) t)) : ('b,'r) t
    = fun cont -> x (fun xx -> f xx cont)
  let return x cont = cont x
  let return1 f a = return(f a)
  let return2 f a b = return(f a b)
  let rec fold f l sofar = match l with
    | [] -> return sofar
    | a::l ->
      let* sofar = f a sofar in
      fold f l sofar
  let iter f l = fold (fun a () -> f a) l ()
  let map f l =
    let* l = fold (fun a sofar -> let* a = f a in return(a::sofar)) l [] in
    return(List.rev l)
end

open Cont

module List = struct
  include List
  let map f l = List.rev (List.fold_left (fun sofar a -> f a::sofar) [] l)
end

module StringHashtbl = CCHashtbl.Make(String)
module VarMap = StringHashtbl


exception Yices_SMT2_exception of string

let sexp f arg = List(Atom f::arg)

module Bindings = struct
  include Make(ExceptionsErrorHandling)

  module Type = struct
    include Type
    let pp fmt t =
      try
        t |> PP.type_string (* ~display:Types.{ width = 100; height = 50; offset=0} *)
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

  end

  module Term = struct
    include Term
    let pp fmt t =
      try
        t |> PP.term_string (* ~display:Types.{ width = 100; height = 50; offset=0} *)
        |> Format.fprintf fmt "%s"
      with _ -> Format.fprintf fmt "null_term"

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
        Atom(PP.term_string ~display:Types.{width = 80; height = 10000; offset = 0} t)

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
        Term.pp term1
        Term.pp term2
        Type.pp type1
        Type.pp type2
  end

  module Model = struct
    include Model
    let pp fmt t =
      t |> PP.model_string ~display:Types.{ width = 100; height = 50; offset=0}
      |> Format.fprintf fmt "%s"
  end

  module Context = struct
    include Context

    type param = Param.t
    let pp_param _ _ = ()

    type action =
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
      | CheckWithModel of param option * Model.t * Term.t list
      | GetModelInterpolant
    [@@deriving show { with_path = false }]

    let to_sexp accu = function
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


    type assertions = Term.t list list

    let pp_assertions fmt assertions = 
      Format.fprintf fmt "@[<v>%a@]" (Term.pp |> List.pp |> List.pp) assertions

    type options = unit StringHashtbl.t

    let pp_options fmt options =
      Format.fprintf fmt "@[<v>%a@]" (StringHashtbl.pp String.pp Format.silent) options

    type nonrec t = {
      config  : Config.t option; 
      context : t;
      assertions : assertions ref;
      options : options;
      log : action list ref
    }

    let pp fmt {assertions} = pp_assertions fmt !assertions

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
        | [last] -> raise (Yices_SMT2_exception "popping last level")
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

    let check_with_model ?param {context; log} model terms =
      log := CheckWithModel(param, model, terms)::!log;
      check_with_model ?param context model terms
    let get_model_interpolant {context; log} =
      log := GetModelInterpolant::!log;
      get_model_interpolant context

  end

  module Param = struct
    include Param
    let default Context.{context} = default context 
  end
  
end

open Bindings

let print verbosity i fs = Format.((if verbosity >= i then fprintf else ifprintf) stdout) fs

module Variables : sig
  type t
  val init            : unit -> t
  val add             : t -> (string*Term.t) list -> t
  val permanently_add : t -> string -> Term.t -> unit
  val mem             : t -> string -> bool
  val find            : t -> string -> Term.t
end = struct

  module StringMap = Map.Make(String)
  type t = {
    uninterpreted : Term.t VarMap.t;
    bound         : Term.t StringMap.t
  }

  let init () = {
    uninterpreted = VarMap.create 10;
    bound = StringMap.empty
  }
  let add m l = { m with bound = StringMap.add_list m.bound l }
  let permanently_add m s t = VarMap.add m.uninterpreted s t
  let mem m s = VarMap.mem m.uninterpreted s || StringMap.mem s m.bound
  let find m s =
    if StringMap.mem s m.bound then StringMap.find s m.bound
    else VarMap.find m.uninterpreted s

end

module Session = struct

  type env = {
    logic   : string;
    context : Context.t;
    param   : Param.t;
    model   : Model.t option
  }

  type t = {
    verbosity : int;
    config    : Config.t;
    types     : type_t VarMap.t;
    variables : Variables.t;
    env       : env option ref;
    infos   : string StringHashtbl.t;
    options : string StringHashtbl.t
  }

  let create ~verbosity =
    print verbosity 1 "Now initialising Yices version %s@," Global.version;
    Global.init();
    print verbosity 1 "Init done@,";
    { verbosity;
      config    = Config.malloc ();
      types     = VarMap.create 10;
      variables = Variables.init();
      env       = ref None;
      infos     = StringHashtbl.create 10;
      options   = StringHashtbl.create 10 }

  let init_env ?configure session ~logic =
    begin match configure with
    | Some () -> ()
    | None -> Config.default session.config ~logic
    end;
    let context = Context.malloc ~config:session.config () in
    let param = Param.malloc() in
    let model = None in
    Param.default context param;
    session.env := Some { logic; context; param; model }

  let exit session =
    (match !(session.env) with
     | Some env ->
       Param.free env.param;
       Context.free env.context
     | None -> () );
    Config.free session.config;
    Global.exit()

end

module ParseType = struct

  type t = (type_t, type_t) Cont.t

  let atom types s = return(
      if VarMap.mem types s then VarMap.find types s
      else match s with
        | "Bool"    -> Type.bool()
        | "Int"     -> Type.int()
        | "Real"    -> Type.real()
        | _ -> raise(Yices_SMT2_exception("ParseType.atom does not understand: "^s)))

  let rec parse types : Sexp.t -> (type_t,type_t) Cont.t = function
    | Atom s -> atom types s
    | List l as sexp -> match l with
      | [Atom "Array"; a; b]         ->
        let* a = parse types a in
        let* b = parse types b in
        return(Type.func [a] b)
      | [_;Atom "BitVec"; Atom size] ->
        return(Type.bv (int_of_string size))
      | _ -> raise(Yices_SMT2_exception("ParseType.parse does not understand: "^Sexp.to_string sexp))

end

module ParseTerm = struct

  open Session

  type t = (term_t, term_t) Cont.t

  let atom session s = return
      (match s with
       | _ when Variables.mem session.variables s -> Variables.find session.variables s
       | "true"  -> Term.true0()
       | "false" -> Term.false0()
       | _ -> 
         match String.sub s 0 2 with
         | "#b" -> Term.BV.parse_bvbin (String.sub s 2 (String.length s -2))
         | "#x" -> Term.BV.parse_bvhex (String.sub s 2 (String.length s -2))
         | _ ->
           try Term.Arith.parse_rational s
           with ExceptionsErrorHandling.YicesException _
             -> Term.Arith.parse_float s)

  let rec right_assoc session op = function
    | [x; y] ->
      let* x = parse session x in
      let* y = parse session y in
      return(op x y)
    | x :: l ->
      let* x = parse session x in
      let* y = right_assoc session op l in
      return (op x y)
    | [] -> assert false

  and left_assoc_aux session accu op = function
    | x :: l -> let* x = parse session x in left_assoc_aux session (op accu x) op l
    | []     -> return accu

  and left_assoc session op = function
    | []   -> assert false
    | x::l -> let* x = parse session x in left_assoc_aux session x op l

  and chainable_aux session accu last op = function
    | x :: l -> let* x = parse session x in chainable_aux session ((op last x)::accu) x op l
    | []     -> return accu

  and chainable session op = function
    | []   -> assert false
    | x::l -> let* x = parse session x in chainable_aux session [] x op l

  and unary session f x =
    let* x = parse session x in
    return(f x)

  and binary session f x y =
    let* x = parse session x in
    let* y = parse session y in
    return(f x y)

  and ternary session f x y z =
    let* x = parse session x in
    let* y = parse session y in
    let* z = parse session z in
    return(f x y z)

  and list session f l =
    let* l = map (parse session) l in
    return(f l)

  and parse_rec session sexp = parse session sexp

  and parse session = function
    | Atom s -> atom session s
    | List l as sexp ->
      let print a (type a) b : a = print session.verbosity a b in
      print 3 "@[<v>Parsing %a@]%!@," Sexp.pp sexp;
      match l with
      | (Atom s)::l ->
        let open Term in
        begin match s, l with

          | _, l when Variables.mem session.variables s ->
            let symb = Variables.find session.variables s in
            begin match l with
              | [] -> return symb
              | _::_ ->
                let aux = Term.application symb in 
                list session aux l
            end
          | "let", [List vs; body] ->
            let reg_var sexp = match sexp with
              | List[Atom var_string; term] ->
                let* term = parse_rec session term in
                return(var_string,term)
              | _ -> raise (Yices_SMT2_exception "not a good variable binding")
            in
            let* l = Cont.map reg_var vs in
            let session = { session with variables = Variables.add session.variables l } in
            parse_rec session body

          | "forall", [List vs; body]
          | "exists", [List vs; body] ->
            let reg_var sexp = match sexp with
              | List[Atom var_string; typ] ->
                let ytyp = ParseType.parse session.types typ |> get in
                let term = Term.new_variable ytyp in
                (var_string,term)
              | _ -> raise (Yices_SMT2_exception "not a good sorted variable")
            in
            let l = List.map reg_var vs in
            let f = match s with
              | "forall" -> Term.forall
              | "exists" -> Term.exists
              | _ -> assert false
            in
            let session = { session with variables = Variables.add session.variables l } in
            unary session (f (List.map snd l)) body
            
          | "match", [_;_]  -> raise (Yices_SMT2_exception "match not supported")
          | "!", _::_       -> raise (Yices_SMT2_exception "! not supported")
            
          (* Core theory *)
          | "not", [x]      -> unary session not1 x
          | "=>", _::_::_   -> right_assoc session implies l
          | "and", l        -> list session andN l
          | "or",  l        -> list session orN l
          | "xor", l        -> list session xorN l
          | "=", _::_::_    -> let* l = chainable session eq l in return !&l
          | "distinct", _   -> list session Term.distinct l
          | "ite", [a;b;c]  -> ternary session ite a b c
          (* Arithmetic theor(ies) *)
          | "-", [a]        -> let* a = parse_rec session a in return (Arith.neg a)
          | "-", _::_::_    -> left_assoc session Arith.sub l
          | "+", _::_::_    -> left_assoc session Arith.add l
          | "*", _::_::_    -> left_assoc session Arith.mul l
          | "div", a::_::_  ->
            let* ya = parse_rec session a in
            begin
              match Term.type_of_term ya |> Type.reveal with
              | Int  -> left_assoc_aux session ya Arith.idiv l
              | Real -> left_assoc_aux session ya Arith.division l
              | _ -> raise (Yices_SMT2_exception "div should apply to Int or Real")
            end
          | "mod", [a;b]  -> binary session Arith.(%.) a b
          | "abs", [a]    -> unary session Arith.abs a
          | "<=", l   -> let* l = chainable session Arith.leq l in return !&l
          | "<",  l   -> let* l = chainable session Arith.lt l in return !&l
          | ">=", l   -> let* l = chainable session Arith.geq l in return !&l
          | ">",  l   -> let* l = chainable session Arith.gt l in return !&l
          | "to_real", [a] -> parse_rec session a
          | "to_int",  [a] -> unary session Arith.floor a
          | "is_int",  [a] -> unary session Arith.is_int_atom a
          (* ArraysEx theory *)
          | "select", [a;b] -> binary session (fun a b -> application a [b]) a b
          | "store", [a;b;c]-> ternary session (fun a b c -> update a [b] c) a b c
          (* BV theory *)
          | "concat", l -> list session BV.bvconcat  l
          | "bvand", l  -> list session BV.bvand     l
          | "bvor", l   -> list session BV.bvor      l
          | "bvadd", l  -> list session BV.bvsum     l
          | "bvmul", l  -> list session BV.bvproduct l
          | "bvudiv", [x; y] -> binary session BV.bvdiv x y
          | "bvurem", [x; y] -> binary session BV.bvrem x y
          | "bvshl",  [x; y] -> binary session BV.bvshl x y
          | "bvlshr", [x; y] -> binary session BV.bvlshr x y
          | "bvnot",  [x]    -> unary session BV.bvnot x
          | "bvneg",  [x]    -> unary session BV.bvneg x
          | "bvult",  [x;y]  -> binary session BV.bvlt  x y
          (* BV theory unofficial *)
          | "bvnand", [x; y] -> binary session BV.bvnand x y
          | "bvnor",  [x; y] -> binary session BV.bvnor  x y
          | "bvxor",  l -> list session BV.bvxor l
          | "bvxnor", [x; y] -> binary session BV.bvxnor x y
          | "bvcomp", [x; y] -> binary session (fun x y -> BV.(redand(bvxnor x y))) x y
          | "bvsub",  [x; y] -> binary session BV.bvsub x y
          | "bvsdiv", [x; y] -> binary session BV.bvsdiv x y
          | "bvsrem", [x; y] -> binary session BV.bvsrem x y
          | "bvsmod", [x; y] -> binary session BV.bvsmod x y
          | "bvashr", [x; y] -> binary session BV.bvashr x y
          | "bvule",  [x; y] -> binary session BV.bvle  x y
          | "bvugt",  [x; y] -> binary session BV.bvgt  x y
          | "bvuge",  [x; y] -> binary session BV.bvge  x y
          | "bvslt",  [x; y] -> binary session BV.bvslt x y
          | "bvsle",  [x; y] -> binary session BV.bvsle x y
          | "bvsgt",  [x; y] -> binary session BV.bvsgt x y
          | "bvsge",  [x; y] -> binary session BV.bvsge x y
          | "_", [Atom s; Atom x] when String.equal (String.sub s 0 2) "bv" ->
            let width = int_of_string x in
            let x = Unsigned.ULong.of_string(String.sub s 2 (String.length s - 2)) in
            return(BV.bvconst_uint64 ~width x)

          | _ -> raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^Sexp.to_string sexp))
        end
      (* BV theory *)
      | [List[_;Atom "extract"; Atom i; Atom j]; x] ->
        let* x = parse session x in
        return(Term.BV.bvextract x (int_of_string j) (int_of_string i))
      (* BV theory unofficial *)
      | [List[_;Atom "repeat"; Atom i]; x] ->
        let* x = parse session x in
        return(Term.BV.bvrepeat x (int_of_string i))
      | [List[_;Atom "zero_extend"; Atom i]; x] ->
        let* x = parse session x in
        return(Term.BV.zero_extend x (int_of_string i))
      | [List[_;Atom "sign_extend"; Atom i]; x] ->
        let* x = parse session x in
        return(Term.BV.sign_extend x (int_of_string i))
      | [List[_;Atom "rotate_left"; Atom i]; x] ->
        let* x = parse session x in
        return(Term.BV.rotate_left x (int_of_string i))
      | [List[_;Atom "rotate_right"; Atom i]; x] ->
        let* x = parse session x in
        return(Term.BV.rotate_right x (int_of_string i))

      | _ -> raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^Sexp.to_string sexp))

end

module ParseInstruction = struct

  open Session
      
  let get_model env = match env.model with
    | Some m -> m
    | None -> Context.get_model env.context ~keep_subst:true

  let display = { width=80; height=80; offset=0 }
  
  let parse session sexp =
    let print a (type a) b : a = print session.verbosity a b in
    match sexp with
    | List(Atom head::args) -> begin match head, args, !(session.env) with
      | "reset", _, _                              -> Global.reset()

      | "set-logic",  [Atom logic],   None         -> Session.init_env session ~logic

      | "set-logic",  [Atom logic],   Some _       ->
        raise (Yices_SMT2_exception "set_logic already used")

      | "set-option", [Atom name; Atom value], _ ->
        StringHashtbl.replace session.options name value;
        Config.set session.config ~name ~value

      | "exit",       [], _                        -> Session.exit session

      | "push",       [Atom n], Some {context}     -> Context.push context

      | "pop",        [Atom n], Some {context}     -> Context.pop context
        
      | "reset-assertions", [], Some {context}     -> Context.reset context;

      | "declare-sort", [Atom var; Atom n], _ ->
        let n = int_of_string n in
        if n <> 0
        then raise (Yices_SMT2_exception "Yices only treats uninterpreted types of arity 0");
        let ytype = Type.new_uninterpreted ~name:var () in
        VarMap.add session.types var ytype

      | "declare-fun", [Atom var; List domain; codomain], _ ->
        let domain = List.map (fun x -> ParseType.parse session.types x |> get) domain in
        let codomain = ParseType.parse session.types codomain |> get in
        let ytype = match domain with
          | []   -> codomain
          | _::_ -> Type.func domain codomain
        in
        let yvar = Term.new_uninterpreted ~name:var ytype in
        Variables.permanently_add session.variables var yvar

      | "declare-const", [Atom var; typ], _ ->
        let yvar =
          Term.new_uninterpreted ~name:var (ParseType.parse session.types typ |> get)
        in 
        Variables.permanently_add session.variables var yvar

      | "declare-datatypes", _, _          ->
        raise (Yices_SMT2_exception "Yices does not support datatypes")

      | "declare-datatype", _, _           ->
        raise (Yices_SMT2_exception "Yices does not support datatypes")

      | "define-fun", [Atom var; List domain; codomain; body], _ ->
        let parse_pair (subst,bindings,domain) pair = match pair with
          | List [Atom var_string; typ] ->
            let vartyp = ParseType.parse session.types typ |> get in
            let var = Term.new_variable vartyp in
            (var_string, var)::subst, var::bindings, vartyp::domain
          | _ -> raise (Yices_SMT2_exception "List of variables in a define-fun should be list of pairs")
        in
        let subst, bindings, domain =
          domain |> List.rev |> List.fold_left parse_pair ([],[],[])
        in
        let session_body = { session with variables = Variables.add session.variables subst } in
        let body         = ParseTerm.parse session_body body |> get in
        let body = match domain with
          | []   -> body
          | _::_ -> Term.lambda bindings body
        in
        Variables.permanently_add session.variables var body
        

      | "define-funs-rec", _, _            ->
        raise (Yices_SMT2_exception "Yices does not support recursive functions")

      | "define-fun-rec", _, _             ->
        raise (Yices_SMT2_exception "Yices does not support recursive functions")

      | "get-assertions", _, Some {context} -> print 0 "@[<v>%a@]@," Context.pp context

      | "assert", [formula], Some env ->
        let formula = ParseTerm.parse session formula |> get in
        Context.assert_formula env.context formula;
        (match env.model with
         | Some model -> Model.free model
         | None -> ());
        session.env := Some { env with model = None};

      | "check-sat", [], Some env          ->
        Context.check env.context ~param:env.param
        |> print 0 "%a@," Types.pp_smt_status

      | "check-sat-assuming", l, Some env  ->
        let assumptions = List.map (fun x -> get(ParseTerm.parse session x)) l in
        Context.check_with_assumptions env.context ~param:env.param assumptions
        |> print 0 "%a@," Types.pp_smt_status

      | "get-value", l, Some env ->
        let model = get_model env in
        let terms = List.map (fun x -> get(ParseTerm.parse session x)) l in
        print 0 "@[<v>%a@]@," (List.pp Term.pp) (Model.terms_value model terms);
        session.env := Some { env with model = Some model }

      | "get-assignment", [], Some env ->
        raise (Yices_SMT2_exception "Not sure how to treat get-assignment")

      | "get-model", [], Some env -> 
        let model = get_model env in
        print 0 "%s@," (PP.model_string model ~display);
        session.env := Some { env with model = Some model }

      | "get-unsat-assumptions", [], Some env ->
        raise (Yices_SMT2_exception "Not sure how to treat get-unsat-assumptions")

      | "get-proof", [], Some env             ->
        raise (Yices_SMT2_exception "Yices produces no proof")

      | "get-unsat-core", [], Some env ->
        let terms = Context.get_unsat_core env.context in
        List.iter (fun formula -> print_endline(PP.term_string formula ~display)) terms

      | "get-info", [ Atom key ], _                  ->
        print 0 "%s@," (StringHashtbl.find session.infos key)

      | "get-option", [ Atom key ], _                ->
        print 0 "%s@," (StringHashtbl.find session.options key)

      | "echo", [Atom s], _                   -> print_endline s

      | "set-info", [Atom key; Atom value] , _ ->
        StringHashtbl.replace session.infos key value

      | "set-info", _ , _ -> print 1 "@[Silently ignoring set-info@]@,"

      | "check-sat-assuming-model", [List vars; List vals], Some env ->
        let f (map,tlist) a b =
          let a = ParseTerm.parse session a |> get in
          let b = ParseTerm.parse session b |> get in
          (a,b)::map , a::tlist
        in
        let map,terms = List.fold_left2 f ([],[]) vars vals in
        let model = Model.from_map map in
        Context.check_with_model env.context ~param:env.param model terms
        |> print 0 "%a@," Types.pp_smt_status
      
      | "get-unsat-model-interpolant", [], Some env ->
        let interpolant = Context.get_model_interpolant env.context in
        print 0 "%a@," Term.pp interpolant

      | _ -> raise (Yices_SMT2_exception("Not part of SMT2 "^head));
      end
    | Atom s ->
      raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^Sexp.to_string sexp))
    | List l as sexp ->
      raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^Sexp.to_string sexp))

end

module SMT2 = struct

  let load_file filename = 
    let ic = open_in filename in
    let l = Sexp.input_sexps ic in
    close_in ic;
    l

  let process_all session l =
    let open Session in
    let aux sexp =
      print session.verbosity 3 "%s" (Sexp.to_string sexp);
      ParseInstruction.parse session sexp
    in
    List.iter aux l

  let process_file ?(verbosity=0) filename =
    let l = load_file filename in
    let session = Session.create ~verbosity in
    print session.verbosity 0 "@[<v>";
    print verbosity 1 "Loading sexps done: %i of them were found." (List.length l);
    process_all session l;
    print session.verbosity 0 "@]"


end
