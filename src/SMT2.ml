open Containers

open Sexplib
open Type
    
open Ext

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

module StringHashtbl = Common.HStrings
module VarMap = Common.HStrings
module HTerms = High.Types.HTerms

module type Variables = sig
  type term
  type t
  val init            : unit -> t
  val add             : t -> (string * term) list -> t
  val permanently_add : t -> string -> term -> unit
  val mem             : t -> string -> bool
  val find            : t -> string -> term
end

module type PARSER_API = sig
  module Type : sig
    type t = Ext.WithExceptionsErrorHandling.Type.t
    val bool : unit -> t
    val int : unit -> t
    val real : unit -> t
    val bv : int -> t
    val tuple : t list -> t
    val func : t list -> t -> t
    val new_uninterpreted : ?name:string -> ?card:int -> unit -> t
    module Names : sig
      val set : t -> string -> unit
    end
  end

  module Term : sig
    type t = Ext.WithExceptionsErrorHandling.Term.t
    val pp : Format.formatter -> t -> unit
    val is_good : t -> bool
    val true0 : unit -> t
    val false0 : unit -> t
    val not1 : t -> t
    val implies : t -> t -> t
    val andN : t list -> t
    val ( !& ) : t list -> t
    val orN : t list -> t
    val xorN : t list -> t
    val eq : t -> t -> t
    val distinct : t list -> t
    val ite : t -> t -> t -> t
    val tuple : t list -> t
    val select : int -> t -> t
    val update : t -> t list -> t -> t
    val application : t -> t list -> t
    val constant : Type.t -> id:int -> t
    val new_uninterpreted : ?name:string -> Type.t -> t
    val new_variable : Type.t -> t
    val forall : t list -> t -> t
    val exists : t list -> t -> t
    val lambda : t list -> t -> t
    module Names : sig
      val set : t -> string -> unit
    end
    module Arith : sig
      val parse_float : string -> t
      val parse_rational : string -> t
      val neg : t -> t
      val sub : t -> t -> t
      val add : t -> t -> t
      val mul : t -> t -> t
      val idiv : t -> t -> t
      val division : t -> t -> t
      val ( %. ) : t -> t -> t
      val abs : t -> t
      val leq : t -> t -> t
      val lt : t -> t -> t
      val geq : t -> t -> t
      val gt : t -> t -> t
      val floor : t -> t
      val is_int_atom : t -> t
    end
    module BV : sig
      val parse_bvbin : string -> t
      val parse_bvhex : string -> t
      val bvconcat : t list -> t
      val bvand : t list -> t
      val bvor : t list -> t
      val bvsum : t list -> t
      val bvproduct : t list -> t
      val bvdiv : t -> t -> t
      val bvrem : t -> t -> t
      val bvshl : t -> t -> t
      val bvlshr : t -> t -> t
      val bvnot : t -> t
      val bvneg : t -> t
      val bvlt : t -> t -> t
      val bvnand : t -> t -> t
      val bvnor : t -> t -> t
      val bvxor : t list -> t
      val bvxnor : t -> t -> t
      val redand : t -> t
      val bvsub : t -> t -> t
      val bvsdiv : t -> t -> t
      val bvsrem : t -> t -> t
      val bvsmod : t -> t -> t
      val bvashr : t -> t -> t
      val bvle : t -> t -> t
      val bvgt : t -> t -> t
      val bvge : t -> t -> t
      val bvslt : t -> t -> t
      val bvsle : t -> t -> t
      val bvsgt : t -> t -> t
      val bvsge : t -> t -> t
      val bvextract : t -> int -> int -> t
      val bvrepeat : t -> int -> t
      val zero_extend : t -> int -> t
      val sign_extend : t -> int -> t
      val rotate_left : t -> int -> t
      val rotate_right : t -> int -> t
      val bvconst_uint64 : width:int -> Unsigned.ULong.t -> t
    end
  end
  module Config : module type of Ext.WithExceptionsErrorHandling.Config
  module Param : module type of Ext.WithExceptionsErrorHandling.Param
  module ModelValue : sig
    type t
    val pp : Format.formatter -> t -> unit
  end

  module PP : sig
    val term_string : ?display:Types.display -> Term.t -> string
  end

  module Global : sig
    val version : string
    val init : unit -> unit
    val exit : unit -> unit
    val reset : unit -> unit
  end

  module SModel : sig
    type t
    val from_map : ?support:Term.t list -> (Term.t * Term.t) list -> t
    val get_value : t -> Term.t -> ModelValue.t
    val get_value_as_term : t -> Term.t -> Term.t option
    val to_sexp :
      smt2arrays:([ `Curry | `Tuple ] * (Term.t -> bool)) option -> t -> Sexp.t
  end

  module Context : sig
    type t
    val of_id : int -> t option
    val malloc : ?config:Config.t -> unit -> t
    val default_param : t -> Param.t -> unit
    val push : t -> unit
    val pop : t -> unit
    val reset : t -> unit
    val pp : Format.formatter -> t -> unit
    val assert_formula : t -> Term.t -> unit
    val assert_formulas : t -> Term.t list -> unit
    val check :
      ?param:Param.t ->
      ?assumptions:Term.t list ->
      ?smodel:SModel.t ->
      ?as_inequalities:bool ->
      ?hints:Term.t list ->
      t -> Ext.Types.smt_status
    val get_model : ?keep_subst:bool -> ?support:Term.t list -> t -> SModel.t
    val get_unsat_core : t -> Term.t list
    val get_model_interpolant : t -> Term.t
  end
end

module type API = sig

  module Ext : PARSER_API
  open Ext
  module StringHashtbl : CCHashtbl.S with type key = string
  module VarMap : CCHashtbl.S with type key = string
  module HTerms : CCHashtbl.S with type key = Term.t

  module Variables : Variables with type term := Term.t

  module Session : sig

    (** Turns the log within context into SMT2 string. *)
    (* val to_SMT2 : ?smt2arrays:[ `Curry | `Tuple ] -> env -> string *)

    (** Mutable session state for parsing and evaluation. *)
    type syntax_hooks = {
        parse_type : Type.t VarMap.t -> Sexp.t -> Type.t option;
        parse_term :
          'a. (t -> Sexp.t -> (Term.t, 'a) Cont.t) ->
          t -> Sexp.t -> ((Term.t, 'a) Cont.t) option;
        set_logic : string -> Config.t -> bool;
      }
    and t = {
        verbosity : int;
        param     : Param.t;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        types     : Type.t VarMap.t;
        variables : Variables.t;
        model     : SModel.t option ref;
        smt2functions : unit HTerms.t;
        syntax_hooks : syntax_hooks;
      }

    val set_logic: (?logic:string -> Config.t -> unit)
    val no_syntax_hooks : syntax_hooks
    val create   : ?syntax_hooks:syntax_hooks -> int -> t
    val exit : t -> unit

  end

  module ParseType : sig
    type t = (Type.t, Type.t) Cont.t
    val atom  : Type.t VarMap.t -> string -> t
    val parse : ?syntax_hooks:Session.syntax_hooks -> Type.t VarMap.t -> Sexp.t -> t
  end

  module ParseTerm : sig
    type t = (Term.t, Term.t) Cont.t

    val atom        : Session.t -> string -> t
    val right_assoc : Session.t -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> t
    val left_assoc  : Session.t -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> t
    val chainable   : Session.t -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> (Term.t list, Term.t) Cont.t
    val unary       : Session.t -> (Term.t -> Term.t) -> Sexp.t -> t
    val binary      : Session.t -> (Term.t -> Term.t -> Term.t) -> Sexp.t -> Sexp.t -> t
    val ternary     : Session.t -> (Term.t -> Term.t -> Term.t -> Term.t) -> Sexp.t -> Sexp.t -> Sexp.t -> t
    val list        : Session.t -> (Term.t list -> Term.t) -> Sexp.t list -> t
    val parse       : Session.t -> Sexp.t -> t
  end

  module ParseInstruction : sig
    val parse        : Session.t -> Sexp.t -> unit
  end

  module SMT2 : sig
    val load_file    : string -> Sexp.t list
    val process_all  : Session.t -> Sexp.t list -> unit
    val process_file : ?syntax_hooks:Session.syntax_hooks -> ?verbosity:int -> string -> unit
  end

end

exception Yices_SMT2_exception of string

let raise_smt2 a =
  Format.ksprintf ~f:(fun s -> raise(Yices_SMT2_exception s)) a

module Make_parser(Ext : PARSER_API) = struct

  open Ext
  module StringHashtbl = StringHashtbl
  module VarMap = VarMap
  module HTerms = HTerms
  
  module Variables : Variables with type term := Term.t = struct

    module StringMap = Map.Make(String)
    type t = {
        uninterpreted : Term.t VarMap.t;    (* Mutable *)
        bound         : Term.t StringMap.t  (* Immutable *)
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

  let print verbosity i fs = Format.((if verbosity >= i then fprintf else ifprintf) stdout) fs

  module Session = struct

    type syntax_hooks = {
        parse_type : Type.t VarMap.t -> Sexp.t -> Type.t option;
        parse_term :
          'a. (t -> Sexp.t -> (Term.t, 'a) Cont.t) ->
          t -> Sexp.t -> ((Term.t, 'a) Cont.t) option;
        set_logic : string -> Config.t -> bool;
      }
    and t = {
        verbosity : int;
        param     : Param.t;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        types     : Type.t VarMap.t;
        variables : Variables.t;
        model     : SModel.t option ref;
        smt2functions : unit HTerms.t;
        syntax_hooks : syntax_hooks;
      }

    let set_logic ?logic config =
      match logic with
      | None
        | Some "all"  -> ()
      | Some "none" -> Config.default config
      | Some "mcsat" ->
         Config.set config ~name:"solver-type" ~value:"mcsat";
         Config.set config ~name:"model-interpolation" ~value:"true"
      | Some logic  -> Config.default config ~logic

    let no_syntax_hooks = {
        parse_type = (fun _ _ -> None);
        parse_term = (fun _ _ _ -> None);
        set_logic = (fun _ _ -> false);
      }

    let create ?(syntax_hooks=no_syntax_hooks) verbosity =
      print verbosity 1 "Now initialising Yices version %s@," Global.version;
      Global.init();
      print verbosity 1 "Init done@,";
      {
        verbosity;
        param     = Param.malloc();
        infos     = StringHashtbl.create 10;
        options   = StringHashtbl.create 10;
        types     = VarMap.create 10;
        variables = Variables.init();
        model     = ref None;
        smt2functions = HTerms.create 10;
        syntax_hooks;
      }

    let exit session =
      Global.exit()

  end

  module ParseType = struct

    type t = (Type.t, Type.t) Cont.t

    let atom types s =
      return(
          if VarMap.mem types s then VarMap.find types s
          else match s with
               | "Bool"    -> Type.bool()
               | "Int"     -> Type.int()
               | "Real"    -> Type.real()
               | _ -> raise(Yices_SMT2_exception("ParseType.atom does not understand: "^s)))

    let rec parse ?syntax_hooks types sexp =
      match
        match syntax_hooks with
        | None -> None
        | Some hooks -> hooks.Session.parse_type types sexp
      with
      | Some typ -> return typ
      | None ->
      match sexp with
      | Atom s -> atom types s
      | List l as sexp ->
         match l with
         | (Atom "Fun"::(_::_ as l))
         | (Atom "Array"::(_::_ as l)) ->
            let* l = parse_list ?syntax_hooks types l in
            let codom, dom = List.(l |> rev |> hd_tl) in
            return(Type.func (List.rev dom) codom)
         | [_;Atom "BitVec"; Atom size] ->
            return(Type.bv (int_of_string size))
         | (Atom "Tuple")::l
           | (Atom "tuple")::l ->
            let* l = parse_list ?syntax_hooks types l in
            return(Type.tuple l)
         | _ ->
            raise(Yices_SMT2_exception("ParseType.parse does not understand: "^Sexp.to_string sexp))
    and parse_list ?syntax_hooks types = function
      | [] -> return []
      | hd::tl ->
         let* hd = parse ?syntax_hooks types hd in
         let* tl = parse_list ?syntax_hooks types tl in
         return(hd::tl)

  end

  module ParseTerm = struct

    open Session

    type t = (Term.t, Term.t) Cont.t

    let atom env s =
      return
        (match s with
         | _ when Variables.mem env.variables s -> Variables.find env.variables s
         | "true"  -> Term.true0()
         | "false" -> Term.false0()
         | _ ->
            let aux s =
              try
                let t =
                  if Str.(string_match (regexp {|.*[\.eE].*|}) s 0)
                  then Term.Arith.parse_float s
                  else Term.Arith.parse_rational s
                in
                if Term.is_good t then t
                else 
                  raise_smt2
                    "%s is not a declared symbol, nor a bitvector/rational/float constant" s
              with _ ->
                raise_smt2
                  "%s is not a declared symbol, nor a bitvector/rational/float constant" s
            in
            if String.length s < 2 then aux s
            else
              match String.sub s 0 2 with
              | "#b" -> Term.BV.parse_bvbin (String.sub s 2 (String.length s -2))
              | "#x" -> Term.BV.parse_bvhex (String.sub s 2 (String.length s -2))
              | _ -> aux s
        )

    let rec right_assoc env op = function
      | [x; y] ->
         let* x = parse env x in
         let* y = parse env y in
         return(op x y)
      | x :: l ->
         let* x = parse env x in
         let* y = right_assoc env op l in
         return (op x y)
      | [] -> assert false

    and left_assoc_aux env accu op = function
      | x :: l -> let* x = parse env x in left_assoc_aux env (op accu x) op l
      | []     -> return accu

    and left_assoc env op = function
      | []   -> assert false
      | x::l -> let* x = parse env x in left_assoc_aux env x op l

    and chainable_aux env accu last op = function
      | x :: l -> let* x = parse env x in chainable_aux env ((op last x)::accu) x op l
      | []     -> return accu

    and chainable env op = function
      | []   -> assert false
      | x::l -> let* x = parse env x in chainable_aux env [] x op l

    and unary env f x =
      let* x = parse env x in
      return(f x)

    and binary env f x y =
      let* x = parse env x in
      let* y = parse env y in
      return(f x y)

    and ternary env f x y z =
      let* x = parse env x in
      let* y = parse env y in
      let* z = parse env z in
      return(f x y z)

    and list env f l =
      let* l = map (parse env) l in
      return(f l)

    and parse_rec env sexp = parse env sexp

    and parse env sexp =
      try
      match env.syntax_hooks.parse_term parse_rec env sexp with
      | Some term -> term
      | None ->
      match sexp with
      | Atom s -> atom env s
      | List l as sexp ->
         let print a (type a) b : a = print env.verbosity a b in
         print 3 "@[<v>Parsing %a@]%!@," pp_sexp sexp;
         match l with
         | (Atom s)::l ->
            let open Term in
            begin match s, l with

            | _, l when Variables.mem env.variables s ->
               let symb = Variables.find env.variables s in
               begin match l with
               | [] -> return symb
               | _::_ ->
                  let aux = Term.application symb in 
                  list env aux l
               end
            | "let", [List vs; body] ->
               let reg_var sexp = match sexp with
                 | List[Atom var_string; term] ->
                    let* term = parse_rec env term in
                    return(var_string,term)
                 | _ -> raise (Yices_SMT2_exception "not a good variable binding")
               in
               let* l = Cont.map reg_var vs in
               let env = { env with variables = Variables.add env.variables l } in
               parse_rec env body

            | "forall", [List vs; body]
              | "exists", [List vs; body] ->
               let reg_var sexp = match sexp with
                 | List[Atom var_string; typ] ->
                    let ytyp = ParseType.parse ~syntax_hooks:env.syntax_hooks env.types typ |> get in
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
               let env = { env with variables = Variables.add env.variables l } in
               unary env (f (List.map snd l)) body
                  
            | "match", [_;_]  -> raise (Yices_SMT2_exception "match not supported")
            | "!", _::_       -> raise (Yices_SMT2_exception "! not supported")
                  
            (* Core theory *)
            | "not", [x]      -> unary env not1 x
            | "=>", _::_::_   -> right_assoc env implies l
            | "and", l        -> list env andN l
            | "or",  l        -> list env orN l
            | "xor", l        -> list env xorN l
            | "=", _::_::_    -> let* l = chainable env eq l in return !&l
            | "distinct", _   -> list env Term.distinct l
            | "ite", [a;b;c]  -> ternary env ite a b c
            | "tuple", l      -> list env Term.tuple l
            (* Arithmetic theor(ies) *)
            | "-", [a]        -> let* a = parse_rec env a in return (Arith.neg a)
            | "-", _::_::_    -> left_assoc env Arith.sub l
            | "+", _::_::_    -> left_assoc env Arith.add l
            | "*", _::_::_    -> left_assoc env Arith.mul l
            | "div", _::_::_  -> left_assoc env Arith.idiv l
            | "/",   _::_::_  -> left_assoc env Arith.division l
            | "mod", [a;b]  -> binary env Arith.(%.) a b
            | "abs", [a]    -> unary env Arith.abs a
            | "<=", l   -> let* l = chainable env Arith.leq l in return !&l
            | "<",  l   -> let* l = chainable env Arith.lt l in return !&l
            | ">=", l   -> let* l = chainable env Arith.geq l in return !&l
            | ">",  l   -> let* l = chainable env Arith.gt l in return !&l
            | "to_real", [a] -> parse_rec env a
            | "to_int",  [a] -> unary env Arith.floor a
            | "is_int",  [a] -> unary env Arith.is_int_atom a
            (* ArraysEx theory *)
            | "select", a::b ->
               let default() =
                 let* a = parse_rec env a in
                 list env (application a) b
               in
               begin
                 match a, b with
                 | Atom s, [b] ->
                    int_of_string_opt s
                    |> Option.map_lazy default (fun i -> unary env (Term.select i) b)
                 | _ -> default()
               end
            | "store", a::l ->
               let* a = parse_rec env a in
               let aux l =
                 let open List in
                 let hd, tl = hd_tl(rev l) in
                 update a (rev tl) hd
               in
               list env aux l
            (* BV theory *)
            | "concat", l -> list env BV.bvconcat  l
            | "bvand", l  -> list env BV.bvand     l
            | "bvor", l   -> list env BV.bvor      l
            | "bvadd", l  -> list env BV.bvsum     l
            | "bvmul", l  -> list env BV.bvproduct l
            | "bvudiv", [x; y] -> binary env BV.bvdiv x y
            | "bvurem", [x; y] -> binary env BV.bvrem x y
            | "bvshl",  [x; y] -> binary env BV.bvshl x y
            | "bvlshr", [x; y] -> binary env BV.bvlshr x y
            | "bvnot",  [x]    -> unary env BV.bvnot x
            | "bvneg",  [x]    -> unary env BV.bvneg x
            | "bvult",  [x;y]  -> binary env BV.bvlt  x y
            (* BV theory unofficial *)
            | "bvnand", [x; y] -> binary env BV.bvnand x y
            | "bvnor",  [x; y] -> binary env BV.bvnor  x y
            | "bvxor",  l -> list env BV.bvxor l
            | "bvxnor", [x; y] -> binary env BV.bvxnor x y
            | "bvcomp", [x; y] -> binary env (fun x y -> BV.(redand(bvxnor x y))) x y
            | "bvsub",  [x; y] -> binary env BV.bvsub x y
            | "bvsdiv", [x; y] -> binary env BV.bvsdiv x y
            | "bvsrem", [x; y] -> binary env BV.bvsrem x y
            | "bvsmod", [x; y] -> binary env BV.bvsmod x y
            | "bvashr", [x; y] -> binary env BV.bvashr x y
            | "bvule",  [x; y] -> binary env BV.bvle  x y
            | "bvugt",  [x; y] -> binary env BV.bvgt  x y
            | "bvuge",  [x; y] -> binary env BV.bvge  x y
            | "bvslt",  [x; y] -> binary env BV.bvslt x y
            | "bvsle",  [x; y] -> binary env BV.bvsle x y
            | "bvsgt",  [x; y] -> binary env BV.bvsgt x y
            | "bvsge",  [x; y] -> binary env BV.bvsge x y
            (* Constants *)
            | "_", [Atom "Const"; Atom i; typ] ->
               let typ = ParseType.parse ~syntax_hooks:env.syntax_hooks env.types typ |> get in
               return(Term.constant typ ~id:(int_of_string i))
            | "_", [Atom s; Atom x] when String.length s >= 2 && String.equal (String.sub s 0 2) "bv" ->
               let width = int_of_string x in
               let x = Unsigned.ULong.of_string(String.sub s 2 (String.length s - 2)) in
               return(BV.bvconst_uint64 ~width x)

            | _ ->
               let s = Format.to_string pp_sexp sexp in
               raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^s))
            end
         (* BV theory *)
         | [List[Atom "_"; Atom "extract"; Atom i; Atom j]; x] ->
            let* x = parse env x in
            return(Term.BV.bvextract x (int_of_string j) (int_of_string i))
         (* BV theory unofficial *)
         | [List[Atom "_";Atom "repeat"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.BV.bvrepeat x (int_of_string i))
         | [List[Atom "_";Atom "zero_extend"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.BV.zero_extend x (int_of_string i))
         | [List[Atom "_";Atom "sign_extend"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.BV.sign_extend x (int_of_string i))
         | [List[Atom "_";Atom "rotate_left"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.BV.rotate_left x (int_of_string i))
         | [List[Atom "_";Atom "rotate_right"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.BV.rotate_right x (int_of_string i))


         (* Tuple theory unofficial *)
         | [List[Atom "_";Atom "tuple.select"; Atom i]; x] ->
            let* x = parse env x in
            return(Term.select (int_of_string i) x)

         | head::tail ->
            let* head = parse env head in
            list env (Term.application head) tail
    
         | _ ->
            let s = Format.to_string pp_sexp sexp in
            raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^s))
      with exc ->
        let bt  = Printexc.get_backtrace() in
        Format.(fprintf err_formatter)
          "@[<v>@[While processing S-expression %a@]@,@[<v2>I got@,%s@]@,@[with backtrace@]@,@[%s@]@,@]"
          pp_sexp sexp
          (Printexc.to_string exc)
          bt;
        raise exc
  end

  let context_of_id ?(id=0) sexp =
    match Context.of_id id with
    | Some ctx -> ctx
    | None ->
       if Int.equal id 0
       then
       raise_smt2
         "You need an instruction (set-logic ...) or (new-context ...) before instruction %a"
         pp_sexp sexp
       else
         raise_smt2 "No live context with id %i for instruction %a" id pp_sexp sexp
  
  module ParseInstruction = struct

    open Session
    
    let get_model session = match !(session.model) with
      | Some m -> m
      | None ->
         let ctx = context_of_id (Atom "get-model") in
         Context.get_model ctx ~keep_subst:true

    let display : Types.display = { width=80; height=80; offset=0 }

    let iter f n =
      let n = int_of_string n in
      for _i = 1 to n do f() done

    let parse_context_instruction ?id session sexp head args =
      let print a (type a) b : a = print session.verbosity a b in
      let context = context_of_id ?id sexp in
      match head, args with
      | "push",       [Atom n]  -> iter (fun () -> Context.push context) n

      | "pop",        [Atom n]  -> iter (fun () -> Context.pop context) n
    
      | "reset-assertions", []  -> Context.reset context
    
      | "get-assertions", _ -> print 0 "@[<v>%a@]@," Context.pp context

      | "assert", [formula] ->
         let formula = ParseTerm.parse session formula |> get in
         Context.assert_formula context formula;
         session.model := None;

      | "assert", formulas ->
         let formulas = Cont.map (ParseTerm.parse session) formulas |> get in
         Context.assert_formulas context formulas;
         session.model := None;

      | "check-sat", []           ->
         (match Context.check context ~param:session.param with
          | `STATUS_SAT   -> print 0 "sat@,"
          | `STATUS_UNSAT -> print 0 "unsat@,"
          | `STATUS_UNKNOWN -> print 0 "unknown@,"
          | status -> print 0 "%a@," Types.pp_smt_status status)

      | "check-sat-assuming", l  ->
         let assumptions = List.map (fun x -> get(ParseTerm.parse session x)) l in
         (match Context.check ~assumptions ~param:session.param context with
          | `STATUS_SAT   -> print 0 "sat@,"
          | `STATUS_UNSAT -> print 0 "unsat@,"
          | `STATUS_UNKNOWN -> print 0 "unknown@,"
          | status -> print 0 "%a@," Types.pp_smt_status status)

      | "get-value", [List terms] ->
         let smodel = get_model session in
         let terms = List.map (fun x -> get(ParseTerm.parse session x)) terms in
         let pp_pair fmt t =
           match SModel.get_value_as_term smodel t with
           | Some v -> Format.fprintf fmt "(%a %a)" Term.pp t Term.pp v
           | None   -> Format.fprintf fmt "(%a %a)" Term.pp t ModelValue.pp (SModel.get_value smodel t)
         in
         print 0 "(@[<v>%a@])@," (List.pp ~pp_sep:Format.pp_print_cut pp_pair) terms;
         session.model := Some smodel
      | "get-value", _ ->
         raise_smt2 "get-value expects a single list of terms (SMT-LIB standard)"

      | "get-assignment", [] ->
         raise_smt2 "Not sure how to treat get-assignment"

      | "get-model", [] -> 
         let model = get_model session in
         let s = SModel.to_sexp ~smt2arrays:None model in
         print 0 "%a@," pp_sexp s;
         session.model := Some model

      | "get-unsat-assumptions", [] ->
         raise_smt2 "Not sure how to treat get-unsat-assumptions"

      | "get-proof", []      ->
         raise_smt2 "Yices produces no proof"

      | "get-unsat-core", [] ->
         let terms = Context.get_unsat_core context in
         List.iter (fun formula -> print_endline(PP.term_string formula ~display)) terms

      | "check-sat-assuming-model", [List vars; List vals] ->
         let f (map,tlist) a b =
           let a = ParseTerm.parse session a |> get in
           let b = ParseTerm.parse session b |> get in
           (a,b)::map , a::tlist
         in
         let map,support = List.fold_left2 f ([],[]) vars vals in
         let smodel = SModel.from_map ~support map in
         let status =
           Context.check ~param:session.param ~smodel context
         in
         (match status with
          | `STATUS_SAT   -> print 0 "sat@,"
          | `STATUS_UNSAT -> print 0 "unsat@,"
          | `STATUS_UNKNOWN -> print 0 "unknown@,"
          | _ -> print 0 "%a@," Types.pp_smt_status status)
    
      | "get-unsat-model-interpolant", [] ->
         let interpolant = Context.get_model_interpolant context in
         print 0 "%a@," Term.pp interpolant

      | _, args ->
         raise_smt2
           "@[<v>Not part of SMT2:@,head is@, @[%s@]@,with arguments@, @[<v>%a@]@]"
           head (List.pp Sexp.pp_hum) args

         
    let parse session sexp =
      let print a (type a) b : a = print session.verbosity a b in
      match sexp with
      | List(Atom head::args) ->
         begin match head, args with
         | "reset", _                            -> Global.reset()

         | "set-logic",  [Atom logic]            ->
            let config = Config.malloc () in
            StringHashtbl.iter (fun name value -> Config.set config ~name ~value)
              session.options;
            if not (session.syntax_hooks.set_logic logic config) then set_logic ~logic config;
            let ctx = Context.malloc ~config () in
            Context.default_param ctx session.param

         | "new-context",  []            ->
            let config = Config.malloc () in
            StringHashtbl.iter (fun name value -> Config.set config ~name ~value)
              session.options;
            set_logic config;
            let ctx = Context.malloc ~config () in
            Context.default_param ctx session.param

         | "set-option", [Atom name; Atom value] ->
            StringHashtbl.replace session.options name value

         | "exit",       []                          -> Session.exit session

         | "declare-sort", (Atom var::Atom n::card) ->
            let n = int_of_string n in
            if n <> 0
            then raise_smt2 "Yices only treats uninterpreted types of arity 0";
            let card =
              match card with
              | [] -> None
              | [Atom i] -> Some(int_of_string i)
              | _ -> raise (Yices_SMT2_exception "Wrong form of cardinality for uninterpreted type");
            in
            let ytype = Type.new_uninterpreted ~name:var ?card () in
            VarMap.add session.types var ytype

         | "declare-fun", [Atom var; List domain; codomain] ->
            let domain =
              List.map
                (fun x -> ParseType.parse ~syntax_hooks:session.syntax_hooks session.types x |> get)
                domain
            in
            let codomain =
              ParseType.parse ~syntax_hooks:session.syntax_hooks session.types codomain |> get
            in
            let ytype = match domain with
              | []   -> codomain
              | _::_ -> Type.func domain codomain
            in
            let yvar = Term.new_uninterpreted ~name:var ytype in
            if List.is_empty domain then HTerms.add session.smt2functions yvar ();
            Variables.permanently_add session.variables var yvar

         | "declare-const", [Atom var; typ] ->
            let ytype = ParseType.parse ~syntax_hooks:session.syntax_hooks session.types typ |> get in
            let yvar = Term.new_uninterpreted ~name:var ytype in 
            Variables.permanently_add session.variables var yvar

         | "declare-datatypes", _
           | "declare-datatype", _
           | "define-funs-rec", _
           | "define-fun-rec", _    ->
            raise_smt2 "Yices does not support %s" head

         | "define-sort", [Atom var; List []; body] ->
            let ytype = ParseType.parse ~syntax_hooks:session.syntax_hooks session.types body |> get in
            VarMap.add session.types var ytype;
            Type.Names.set ytype var
               
         | "define-fun", [Atom var; List domain; _codomain; body] ->
            let parse_pair (subst,bindings,domain) pair =
              match pair with
              | List [Atom var_string; typ] ->
                 let vartyp = ParseType.parse ~syntax_hooks:session.syntax_hooks session.types typ |> get in
                 let var = Term.new_variable vartyp in
                 (var_string, var)::subst, var::bindings, vartyp::domain
              | sexp ->
                 raise_smt2 "List of variables in a define-fun should be list of pairs, not %a" pp_sexp sexp
            in
            let subst, bindings, domain =
              domain |> List.rev |> List.fold_left parse_pair ([],[],[])
            in
            let session_body =
              { session with variables = Variables.add session.variables subst}
            in
            let body = ParseTerm.parse session_body body |> get in
            let body = match domain with
              | []   -> body
              | _::_ -> Term.lambda bindings body
            in
            Variables.permanently_add session.variables var body;
            Term.Names.set body var

         | "get-info", [ Atom key ] ->
            print 0 "%s@," (StringHashtbl.find session.infos key)

         | "get-option", [ Atom key ] ->
            print 0 "%s@," (StringHashtbl.find session.options key)

         | "echo", [Atom s]  -> print 0 "@[%s@]@," s

         | "set-info", [Atom key; Atom value] ->
            StringHashtbl.replace session.infos key value

         | "set-info", _ -> print 1 "@[Silently ignoring set-info@]@,"

         | "context", [Atom id; instruction] ->
            let id = int_of_string id in
            parse_context_instruction ~id session sexp head args

         | _ -> parse_context_instruction session sexp head args
  
         end
    
      | Atom _ 
        | List _ ->
         raise_smt2 "I doubt that this is an SMT2 instruction: %a" pp_sexp sexp
  end

  module SMT2 = struct

    let smt2_string_literal_tag = "__yices_smt2_string_hex"

    let hex_digit n =
      Char.chr (if n < 10 then Char.code '0' + n else Char.code 'a' + n - 10)

    let add_hex_byte buf c =
      let code = Char.code c in
      Buffer.add_char buf (hex_digit (code lsr 4));
      Buffer.add_char buf (hex_digit (code land 0xF))

    let encode_smt2_quoted_tokens bytes =
      let len = Bytes.length bytes in
      let out = Buffer.create len in
      let rec scan i =
        if i >= len then ()
        else
          match Bytes.get bytes i with
          | '|' ->
             Buffer.add_char out '"';
             scan_bar_symbol (i + 1)
          | '"' ->
             Buffer.add_char out '(';
             Buffer.add_string out smt2_string_literal_tag;
             Buffer.add_char out ' ';
             Buffer.add_char out '"';
             let next = scan_string_literal (i + 1) in
             Buffer.add_char out '"';
             Buffer.add_char out ')';
             scan next
          | c ->
             Buffer.add_char out c;
             scan (i + 1)
      and scan_bar_symbol i =
        if i >= len then raise_smt2 "unterminated quoted symbol";
        match Bytes.get bytes i with
        | '|' ->
           Buffer.add_char out '"';
           scan (i + 1)
        | '"' ->
           Buffer.add_char out '|';
           scan_bar_symbol (i + 1)
        | c ->
           Buffer.add_char out c;
           scan_bar_symbol (i + 1)
      and scan_string_literal i =
        if i >= len then raise_smt2 "unterminated string literal";
        match Bytes.get bytes i with
        | '"' when i + 1 < len && Char.equal (Bytes.get bytes (i + 1)) '"' ->
           add_hex_byte out '"';
           add_hex_byte out '"';
           scan_string_literal (i + 2)
        | '"' -> i + 1
        | c ->
           add_hex_byte out c;
           scan_string_literal (i + 1)
      in
      scan 0;
      Buffer.contents out

    let load_file filename = 
      let ic = open_in filename in
      let bytes = IO.read_all_bytes ic in
      close_in ic;
      let str = encode_smt2_quoted_tokens bytes in
      let rec parse ?parse_pos accu =
        match Sexp.parse ?parse_pos str with
        | Done(sexp, parse_pos) -> parse ~parse_pos (sexp::accu)
        | _ -> accu
      in parse [] |> List.rev
    
    let process_all session l =
      let open Session in
      let aux sexp =
        print session.verbosity 3 "%a@," pp_sexp sexp;
        ParseInstruction.parse session sexp
      in
      List.iter aux l

    let process_file ?(syntax_hooks=Session.no_syntax_hooks) ?(verbosity=0) filename =
      let l = load_file filename in
      let session = Session.create ~syntax_hooks verbosity in
      print session.verbosity 0 "@[<v>";
      print verbosity 1 "Loading sexps done: %i of them were found.@," (List.length l);
      process_all session l;
      print session.verbosity 0 "@]"

  end
end

module Make(Ext : Ext_types.API) = Make_parser(Ext)
