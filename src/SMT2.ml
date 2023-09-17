open Containers

open Sexplib
open Type
    
open Ext
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

module StringHashtbl = Common.HStrings
module VarMap = Common.HStrings

module type Variables = sig
  type term
  type t
  val init            : unit -> t
  val add             : t -> (string * term) list -> t
  val permanently_add : t -> string -> term -> unit
  val mem             : t -> string -> bool
  val find            : t -> string -> term
end

module type API = sig

  module Ext : Ext_types.API
  open Ext

  module Variables : Variables with type term := Term.t

  module Session : sig

    type env = {
        verbosity : int;
        logic     : string;
        types     : Type.t VarMap.t;
        variables : Variables.t;
        context : Context.t;
        param   : Param.t;
        model   : SModel.t option;
        smt2functions : unit HTerms.t
      }

    (** Turns the log within context into SMT2 string. *)
    val to_SMT2 : ?smt2arrays:[ `Curry | `Tuple ] -> env -> string

    type t = {
        verbosity : int;
        config    : Config.t;
        env       : env option ref;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        set_logic : string -> Config.t -> unit
      }

    val create   : ?set_logic:(string -> Config.t -> unit) -> int -> t
    val init_env : t -> logic:string -> unit
    val exit : t -> unit

  end

  module ParseType : sig
    type t = (Type.t, Type.t) Cont.t
    val atom  : Type.t VarMap.t -> string -> t
    val parse : Type.t VarMap.t -> Sexp.t -> t
  end

  module ParseTerm : sig
    type t = (Term.t, Term.t) Cont.t

    val atom        : Session.env -> string -> t
    val right_assoc : Session.env -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> t
    val left_assoc  : Session.env -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> t
    val chainable   : Session.env -> (Term.t -> Term.t -> Term.t) -> Sexp.t list -> (Term.t list, Term.t) Cont.t
    val unary       : Session.env -> (Term.t -> Term.t) -> Sexp.t -> t
    val binary      : Session.env -> (Term.t -> Term.t -> Term.t) -> Sexp.t -> Sexp.t -> t
    val ternary     : Session.env -> (Term.t -> Term.t -> Term.t -> Term.t) -> Sexp.t -> Sexp.t -> Sexp.t -> t
    val list        : Session.env -> (Term.t list -> Term.t) -> Sexp.t list -> t
    val parse       : Session.env -> Sexp.t -> t
  end

  module ParseInstruction : sig
    val parse        : Session.t -> Sexp.t -> unit
  end

  module SMT2 : sig
    val load_file    : string -> Sexp.t list
    val process_all  : Session.t -> Sexp.t list -> unit
    val process_file : ?verbosity:int -> string -> unit
  end

end

exception Yices_SMT2_exception of string

module Make(Ext : Ext_types.API) = struct

  open Ext
  
  module Variables : Variables with type term := Term.t = struct

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

  (* let get_uninterpreted {uninterpreted; _} =
   *   VarMap.keys_list uninterpreted *)
  end

  let print verbosity i fs = Format.((if verbosity >= i then fprintf else ifprintf) stdout) fs

  module Session = struct

    type env = {
        verbosity : int;
        logic     : string;
        types     : type_t VarMap.t;
        variables : Variables.t;
        context : Context.t;
        param   : Param.t;
        model   : SModel.t option;
        smt2functions : unit HTerms.t
      }

    let to_SMT2 ?smt2arrays {logic; context; smt2functions; _} =
      let smt2arrays = Option.map (fun mode -> mode, HTerms.mem smt2functions) smt2arrays in
      let log = Context.to_sexp ?smt2arrays context in
      let sl = List[Atom "set-logic"; Atom logic] in
      let pp fmt sexplist =
        Format.fprintf fmt "@[<v>%a@]" (List.pp ~pp_sep:Format.unit pp_sexp) sexplist
      in
      Format.to_string pp (sl::log)

    type t = {
        verbosity : int;
        config    : Config.t;
        env       : env option ref;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        set_logic : string -> Config.t -> unit;
      }

    let set_logic logic config =
      match logic with
      | "all" -> ()
      | "none" -> Config.default config
      | _ -> Config.default config ~logic

    let create ?(set_logic=set_logic) verbosity =
      print verbosity 1 "Now initialising Yices version %s@," Global.version;
      Global.init();
      print verbosity 1 "Init done@,";
      {
        verbosity;
        config    = Config.malloc ();
        env       = ref None;
        infos     = StringHashtbl.create 10;
        options   = StringHashtbl.create 10;
        set_logic;
      }

    let init_env session ~logic =
      let types     = VarMap.create 10 in
      let variables = Variables.init() in
      let context = Context.malloc ~config:session.config () in
      let param = Param.malloc() in
      let model = None in
      let smt2functions = Global.hTerms_create 10 in
      Param.default context param;
      session.env := Some { verbosity = session.verbosity;
                            logic;
                            types;
                            variables;
                            context;
                            param;
                            model;
                            smt2functions}

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

    let atom types s =
      return(
          if VarMap.mem types s then VarMap.find types s
          else match s with
               | "Bool"    -> Type.bool()
               | "Int"     -> Type.int()
               | "Real"    -> Type.real()
               | _ -> raise(Yices_SMT2_exception("ParseType.atom does not understand: "^s)))

    let rec parse types : Sexp.t -> (type_t,type_t) Cont.t = function
      | Atom s -> atom types s
      | List l as sexp ->
         match l with
         | (Atom "Fun"::(_::_ as l))
         | (Atom "Array"::(_::_ as l)) ->
            let* l = parse_list types l in
            let codom, dom = List.(l |> rev |> hd_tl) in
            return(Type.func (List.rev dom) codom)
         | [_;Atom "BitVec"; Atom size] ->
            return(Type.bv (int_of_string size))
         | (Atom "Tuple")::l
           | (Atom "tuple")::l ->
            let* l = parse_list types l in
            return(Type.tuple l)
         | _ ->
            raise(Yices_SMT2_exception("ParseType.parse does not understand: "^Sexp.to_string sexp))
    and parse_list types = function
      | [] -> return []
      | hd::tl ->
         let* hd = parse types hd in
         let* tl = parse_list types tl in
         return(hd::tl)

  end

  module ParseTerm = struct

    open Session

    type t = (term_t, term_t) Cont.t

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
                  then
                    ((* print_endline s; *)
                     Term.Arith.parse_float s)
                  else Term.Arith.parse_rational s
                in
                if Term.is_good t then t
                else 
                  raise (Yices_SMT2_exception
                           (s^" is not a declared symbol, nor a bitvector/rational/float constant"))
              with _ ->
                raise (Yices_SMT2_exception
                         (s^" is not a declared symbol, nor a bitvector/rational/float constant"))
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
                    let ytyp = ParseType.parse env.types typ |> get in
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
               let typ = ParseType.parse env.types typ |> get in
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

  module ParseInstruction = struct

    open Session
    
    let get_model env = match env.model with
      | Some m -> m
      | None -> Context.get_model env.context ~keep_subst:true

    let display = { width=80; height=80; offset=0 }

    let iter f n =
      let n = int_of_string n in
      for _i = 1 to n do f() done
    
    let parse session sexp =
      let print a (type a) b : a = print session.verbosity a b in
      match sexp with
      | List(Atom head::args) ->
         begin match head, args, !(session.env) with
         | "reset", _, _                              -> Global.reset()

         | "set-logic",  [Atom logic],   None         ->
            session.set_logic logic session.config;
            Session.init_env session ~logic

         | "set-logic",  [Atom _logic],   Some _       ->
            raise (Yices_SMT2_exception "set-logic already used")

         | "set-option", [Atom name; Atom value], _ ->
            StringHashtbl.replace session.options name value;
            Config.set session.config ~name ~value

         | "exit",       [], _                        -> Session.exit session

         | "push",       [Atom n], Some {context; _}  -> iter (fun () -> Context.push context) n

         | "pop",        [Atom n], Some {context; _}  -> iter (fun () -> Context.pop context) n
               
         | "reset-assertions", [], Some {context; _}  -> Context.reset context;

         | "declare-sort", (Atom _ :: Atom _ :: _), None
           | "declare-fun", [Atom _; List _; _], None
           | "declare-const", [Atom _; _],       None
           | "define-fun", [Atom _; List _; _],  None ->
            raise (Yices_SMT2_exception("Call set-logic before "^head))

         | "declare-sort", (Atom var::Atom n::card), Some env ->
            let n = int_of_string n in
            if n <> 0
            then raise (Yices_SMT2_exception "Yices only treats uninterpreted types of arity 0");
            let card =
              match card with
              | [] -> None
              | [Atom i] -> Some(int_of_string i)
              | _ -> raise (Yices_SMT2_exception "Wrong form of cardinality for uninterpreted type");
            in
            let ytype = Type.new_uninterpreted ~contexts:[env.context] ~name:var ?card () in
            VarMap.add env.types var ytype

         | "declare-fun", [Atom var; List domain; codomain], Some env ->
            let domain = List.map (fun x -> ParseType.parse env.types x |> get) domain in
            let codomain = ParseType.parse env.types codomain |> get in
            let ytype = match domain with
              | []   -> codomain
              | _::_ -> Type.func domain codomain
            in
            let yvar = Term.new_uninterpreted ~contexts:[env.context] ~name:var ytype in
            if List.is_empty domain then HTerms.add env.smt2functions yvar ();
            Variables.permanently_add env.variables var yvar

         | "declare-const", [Atom var; typ], Some env ->
            let ytype = ParseType.parse env.types typ |> get in
            let yvar = Term.new_uninterpreted ~contexts:[env.context] ~name:var ytype in 
            Variables.permanently_add env.variables var yvar

         | "declare-datatypes", _, _
           | "declare-datatype", _, _
           | "define-funs-rec", _, _
           | "define-fun-rec", _, _    ->
            raise (Yices_SMT2_exception("Yices does not support "^head))

         | "define-sort", [Atom var; List []; body], Some env ->
            let ytype = ParseType.parse env.types body |> get in
            VarMap.add env.types var ytype;
            Type.Names.set ~contexts:[env.context] ytype var
               
         | "define-fun", [Atom var; List domain; _codomain; body], Some env ->
            let parse_pair (subst,bindings,domain) pair = match pair with
              | List [Atom var_string; typ] ->
                 let vartyp = ParseType.parse env.types typ |> get in
                 let var = Term.new_variable vartyp in
                 (var_string, var)::subst, var::bindings, vartyp::domain
              | _ -> raise (Yices_SMT2_exception "List of variables in a define-fun should be list of pairs")
            in
            let subst, bindings, domain =
              domain |> List.rev |> List.fold_left parse_pair ([],[],[])
            in
            let env_body = { env with variables = Variables.add env.variables subst } in
            let body     = ParseTerm.parse env_body body |> get in
            let body = match domain with
              | []   -> body
              | _::_ -> Term.lambda bindings body
            in
            Variables.permanently_add env.variables var body;
            Term.Names.set ~contexts:[env.context] body var
               
         | "get-assertions", _, Some {context; _} -> print 0 "@[<v>%a@]@," Context.pp context

         | "assert", [formula], Some env ->
            let formula = ParseTerm.parse env formula |> get in
            Context.assert_formula env.context formula;
            Option.iter (fun (SModel{model;_}) -> Model.free model) env.model;
            session.env := Some { env with model = None};

         | "assert", formulas, Some env ->
            let formulas = Cont.map (ParseTerm.parse env) formulas |> get in
            Context.assert_formulas env.context formulas;
            Option.iter (fun (SModel{model;_}) -> Model.free model) env.model;
            session.env := Some { env with model = None};

         | "check-sat", [], Some env          ->
            (match Context.check env.context ~param:env.param with
             | `STATUS_SAT   -> print 0 "sat@,"
             | `STATUS_UNSAT -> print 0 "unsat@,"
             | status -> print 0 "%a@," Types.pp_smt_status status)

         | "check-sat-assuming", l, Some env  ->
            let assumptions = List.map (fun x -> get(ParseTerm.parse env x)) l in
            (match Context.check ~assumptions ~param:env.param env.context with
             | `STATUS_SAT   -> print 0 "sat@,"
             | `STATUS_UNSAT -> print 0 "unsat@,"
             | status -> print 0 "%a@," Types.pp_smt_status status)

         | "get-value", l, Some env ->
            let smodel = get_model env in
            let terms = List.map (fun x -> get(ParseTerm.parse env x)) l in
            print 0 "@[<v>%a@]@," (List.pp Term.pp)
              (Model.terms_value (let SModel{model;_} = smodel in model) terms);
            session.env := Some { env with model = Some smodel }

         | "get-assignment", [], Some _env ->
            raise (Yices_SMT2_exception "Not sure how to treat get-assignment")

         | "get-model", [], Some env -> 
            let model = get_model env in
            print 0 "%a@," (SModel.pp()) model;
            session.env := Some { env with model = Some model }

         | "get-unsat-assumptions", [], Some _env ->
            raise (Yices_SMT2_exception "Not sure how to treat get-unsat-assumptions")

         | "get-proof", [], Some _env             ->
            raise (Yices_SMT2_exception "Yices produces no proof")

         | "get-unsat-core", [], Some env ->
            let terms = Context.get_unsat_core env.context in
            List.iter (fun formula -> print_endline(PP.term_string formula ~display)) terms

         | "get-info", [ Atom key ], _                  ->
            print 0 "%s@," (StringHashtbl.find session.infos key)

         | "get-option", [ Atom key ], _                ->
            print 0 "%s@," (StringHashtbl.find session.options key)

         | "echo", [Atom s], _  -> print 0 "@[%s@]@," s

         | "set-info", [Atom key; Atom value] , _ ->
            StringHashtbl.replace session.infos key value

         | "set-info", _ , _ -> print 1 "@[Silently ignoring set-info@]@,"

         | "check-sat-assuming-model", [List vars; List vals], Some env ->
            let f (map,tlist) a b =
              let a = ParseTerm.parse env a |> get in
              let b = ParseTerm.parse env b |> get in
              (a,b)::map , a::tlist
            in
            let map,support = List.fold_left2 f ([],[]) vars vals in
            let model = Model.from_map map in
            let status =
              Context.check ~param:env.param ~smodel:(SModel.make ~support model) env.context
            in
            (match status with
             | `STATUS_SAT   -> print 0 "sat@,"
             | `STATUS_UNSAT -> print 0 "unsat@,"
             | _ -> print 0 "%a@," Types.pp_smt_status status)
               
         | "get-unsat-model-interpolant", [], Some env ->
            let interpolant = Context.get_model_interpolant env.context in
            print 0 "%a@," Term.pp interpolant

         | _, args, _ ->
            let msg =
              Format.sprintf
                "@[<v>Not part of SMT2:@,head is@, @[%s@]@,with arguments@, @[<v>%a@]@]"
                head (List.pp Sexp.pp_hum) args in
            raise (Yices_SMT2_exception msg);
         end
    
      | Atom _ 
        | List _ ->
         let s = Format.to_string pp_sexp sexp in
         raise(Yices_SMT2_exception("I doubt this is in the SMT2 language: "^s))

  end

  module SMT2 = struct

    let load_file filename = 
      let ic = open_in filename in
      let bytes = IO.read_all_bytes ic in
      close_in ic;
      let is_in_string = ref false in
      let is_escaped = ref false in
      let aux i = function
        | _ when !is_escaped -> is_escaped := false; ()
        | '\\' -> is_escaped := true; ()
        | '|' -> is_in_string := not !is_in_string; Bytes.set bytes i '"'
        | '"' when !is_in_string -> Bytes.set bytes i '|'
        | _ -> ()
      in
      let () = Bytes.iteri aux bytes in
      let str = bytes |> Bytes.unsafe_to_string in
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

    let process_file ?(verbosity=0) filename =
      let l = load_file filename in
      let session = Session.create verbosity in
      print session.verbosity 0 "@[<v>";
      print verbosity 1 "Loading sexps done: %i of them were found.@," (List.length l);
      process_all session l;
      print session.verbosity 0 "@]"

  end
end
