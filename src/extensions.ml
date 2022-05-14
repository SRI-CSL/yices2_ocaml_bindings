open Containers
open Sexplib

open Ext_bindings
open Extension_builder

module AddDiff = struct

  type term   = Term.t 
  type config = Config.t
  type model  = Model.t

  let config_set = Config.set

  include Trivial

  let diff_table   = Global.hTypes_create 10
  let diff_symbols = Global.hTerms_create 10

  module ExtraType = struct

    let diff typ =
      let f typ =
        match Type.reveal typ with
        | Fun {dom; _} ->
           let aux i arg_typ =
             let name = Format.sprintf "diff_%a_%i" Type.pp typ i in
             let symbol = Term.new_uninterpreted ~name (Type.(func [typ; typ] arg_typ)) in
             HTerms.add diff_symbols symbol typ;
             symbol
           in
           List.mapi aux dom
           
        | _ -> High.ExceptionsErrorHandling.raise_bindings_error
                 "get_diff only works on function types, not %a" Type.pp typ
      in
      HTypes.get_or_add diff_table ~f ~k:typ

  end

  module ExtraTerm = struct

    let diff lhs rhs =
      let diff_term_i diff_symb_i = Term.application diff_symb_i [lhs; rhs] in
      lhs |> Term.type_of_term |> ExtraType.diff |> List.map diff_term_i

    let reveal : 'a. 'a Types.termstruct -> (Term.t * Term.t) option =
      fun (type a) (tstruct : a Types.termstruct) ->
      match tstruct with
      | Types.App(f, [lhs;rhs]) when HTerms.mem diff_symbols f -> Some(lhs, rhs)
      | _ -> None

  end

  module TermPair = struct
    type t = Term.t*Term.t [@@deriving eq]
    let hash = Hash.pair Term.hash Term.hash
  end

  module HTermPairs = CCHashtbl.Make(TermPair)

  type t = unit HTerms.t

  let malloc ?config () = config, Global.hTerms_create 10
  let free = HTerms.reset

  (* Looks at an equality term t of the form a === b,
     and if it is a functional equality, generates axiom
     (a[diff(a,b)] === b[diff(a,b)]) ===> a === b
     and then possibly recursively looks at equality term (a[diff(a,b)] === b[diff(a,b)]), etc *)

  let rec generate_constraint state accu t lhs rhs =
    if HTerms.mem state t then
      begin
        (* print_endline (Format.sprintf "Already treated %a" Term.pp t); *)
        accu
      end
    else
      let () = HTerms.add state t () in
      let typ = Term.type_of_term lhs in
      if Type.is_function typ
      then
        begin
          let diff   = ExtraTerm.diff lhs rhs in
          let lhs    = Term.(application lhs diff) in
          let rhs    = Term.(application rhs diff) in
          let veq    = Term.(lhs === rhs) in
          let constr = Term.(veq ==> t) in
          (* print_endline (Format.sprintf "Constraint %a" Term.pp constr); *)
          generate_constraint state (constr::accu) veq lhs rhs
        end
      else
        begin
          (* print_endline (Format.sprintf "Not a functional equality"); *)
          accu
        end


  let assert_formula old_assert state f =
    let rec scan_struct : 'a. Term.t -> 'a Types.termstruct -> unit =
      fun t (type a) (t_struct : a Types.termstruct) ->
      match ExtraTerm.reveal t_struct with
      | Some(lhs, rhs) ->
         let constraints = generate_constraint state [] Term.(lhs === rhs) lhs rhs in
         List.iter old_assert constraints;
         scan_struct_default t_struct
      | None ->
         match t_struct with
         | Types.A2(`YICES_EQ_TERM, lhs, rhs) ->
            (* print_endline (Format.sprintf "scanning %a" Term.pp t); *)
            let constraints = generate_constraint state [] t lhs rhs in
            List.iter old_assert constraints;
            scan_struct_default t_struct   
         |  t_struct -> scan_struct_default t_struct
    and scan_struct_default : 'a. 'a Types.termstruct -> unit =
      fun t_struct -> let _ = Term.map scan_term t_struct in ()
    and scan_term t = scan t; t
    and scan t = 
      if HTerms.mem state t then ()
      else
        let Term t_struct = Term.reveal t in
        scan_struct t t_struct;
        HTerms.add state t ()

    in
    (* print_endline "---open";
     * print_endline (Format.sprintf "assert_formula %a" Term.pp f); *)
    scan f;
    (* print_endline "---close"; *)
    old_assert f

  let check _t old_model = Sat old_model

  let interpolant _t old_interpolant = old_interpolant

end

module Diff = Make(Context)(AddDiff)







module AddLength = struct

  type term   = Term.t 
  type config = Config.t
  type model  = Model.t

  let config_set = Config.set

  include Trivial

  (* These are the syntactic elements associated with a type of lengthed arrays, say lfun *)
  type lfun =
    { dom    : Type.t list; (* dom and codom are like in the type of non-lengthed array *)
      codom  : Type.t;      (* (dom -> codom) *)
      update : Term.t;      (* update      symbol as in (dom -> codom) *)
      application : Term.t; (* application symbol as in (dom -> codom) *)
      length_type : Type.t; (* type of lengths *)
      admissible  : Term.t; (* Admissibility predicate of indices w.r.t. lengths *)
      length      : Term.t; (* Length function symbol: term of type (lfun -> length_type) *)
      as_fun      : Term.t; (* Projection as non-lengthed array, forgetting the length: *)
    }                       (* term of type (lfun -> (dom -> codom)) *)

  module LFun = struct
    type t = Type.t*Type.t*Term.t [@@deriving eq]
    let hash = Hash.triple Type.hash Type.hash Term.hash
  end

  module HLFun = CCHashtbl.Make(LFun)

  (* Map from lengthed array types to lfun records *)
  let lfun_types = Global.hTypes_create 10

  (* Map from triples (dom, codom, admissible) to already constructed lengthed array types,
     to avoid reconstructing same lengthed array types several times *)
  let lfun_table = HLFun.create 10

  (* Table of all uninterpreted symbols pertaining to a lengthed array type:
     update, application, length, as_fun *)
  let lfun_symbols : ([`Update | `Application | `Length | `AsFun] * Type.t ) HTerms.t =
    Global.hTerms_create 10

  let cleanup ~after =
    match after with
    | `GC -> failwith "You cannot use the GC in this extension of arrays with lengths"
    | `Init | `Reset -> HLFun.reset lfun_table

  let () = Global.register_cleanup cleanup

  module ExtraType = struct

    let f ~dom ~codom (length_type, fun_type, admissible) =

      (* Creating uninterpreted type of lengthed arrays *)
      let sexp =
        let length_type = Type.to_sexp length_type in
        let dom = List.map Type.to_sexp dom in
        match dom with
        | [dom] -> sexp "LArray" [length_type ; dom; Type.to_sexp codom]
        | _ -> sexp "LArray" [length_type ; List dom; Type.to_sexp codom]
      in
      let name = Format.sprintf "%a" Sexp.pp sexp in
      let lfun = Type.new_uninterpreted ~name () in

      let name prefix = Format.sprintf "%s_%a" prefix Type.pp lfun in

      (* Creating the update symbol *)
      let update = Term.new_uninterpreted ~name:(name "store") Type.(func (lfun::codom::dom) lfun)
      in
      HTerms.add lfun_symbols update (`Update, lfun);

      (* Creating the application symbol *)
      let application =
        Term.new_uninterpreted ~name:(name "select") Type.(func (lfun::dom) codom)
      in
      HTerms.add lfun_symbols application (`Application, lfun);


      (* Creating the length function *)
      let length = Term.new_uninterpreted ~name:(name "length") Type.(func [lfun] length_type) in
      HTerms.add lfun_symbols length (`Length, lfun);
      
      (* Creating the as_fun function *)
      let as_fun = Term.new_uninterpreted ~name:(name "as_fun") Type.(func [lfun] fun_type) in
      HTerms.add lfun_symbols as_fun (`AsFun, lfun);

      (* Creating the length function *)
      HTypes.add lfun_types lfun
        { length_type; dom; codom; update; application; length; as_fun; admissible };
      lfun

    (* Get the type of lengthed arrays from dom to codom, with lengths of type length_type,
   and admissible predicate (if absent, an uninterpreted predicate is generated) *)
    let lfun ?admissible ~length_type ~dom ~codom () =
      let lfun = Type.func dom codom in
      let admissible = match admissible with
        | Some admissible -> admissible
        | None ->
           let name = Format.sprintf "admissible_%a_%a" Type.pp lfun Type.pp length_type in
           Term.new_uninterpreted ~name Type.(func (length_type::dom) (bool()))
      in
      HLFun.get_or_add
        ~f:(f ~dom ~codom) ~k:(length_type, lfun, admissible) lfun_table

    let is_lfun typ = HTypes.mem lfun_types typ
    let length_type typ = (HTypes.find lfun_types typ).length_type
    let dom typ    = (HTypes.find lfun_types typ).dom
    let codom typ  = (HTypes.find lfun_types typ).codom
    let length typ = (HTypes.find lfun_types typ).length
    let as_fun typ = (HTypes.find lfun_types typ).as_fun
    let update typ = (HTypes.find lfun_types typ).update
    let application typ = (HTypes.find lfun_types typ).application
    let admissible typ  = (HTypes.find lfun_types typ).admissible
  end

  module ExtraTerm = struct
    let is_lfun t     = t |> Term.type_of_term |> ExtraType.is_lfun
    let length_type t = t |> Term.type_of_term |> ExtraType.length_type
    let dom t         = t |> Term.type_of_term |> ExtraType.dom
    let codom t       = t |> Term.type_of_term |> ExtraType.codom
    let length t =
      let symbol = t |> Term.type_of_term |> ExtraType.length in
      Term.application symbol [t]
    let as_fun t =
      let symbol = t |> Term.type_of_term |> ExtraType.as_fun in
      Term.application symbol [t]
    let admissible ~lfun length index =
      let symbol = lfun |> Term.type_of_term |> ExtraType.admissible in
      Term.application symbol (length::index)
    let update lfun index value =
      let symbol = lfun |> Term.type_of_term |> ExtraType.update in
      Term.application symbol (lfun::value::index)
    let application lfun index =
      let symbol = lfun |> Term.type_of_term |> ExtraType.application in
      Term.application symbol (lfun::index)

    type reveal =
      | Update of Term.t*(Term.t list)*Term.t
      | Application of Term.t*(Term.t list)
      | Length of Term.t
      | AsFun of Term.t
      
    let reveal : 'a. 'a Types.termstruct -> reveal option =
      fun (type a) (tstruct : a Types.termstruct) ->
      match tstruct with
      | Types.App(f, args) when HTerms.mem lfun_symbols f ->
         let kind, _ = HTerms.find lfun_symbols f in
         let r =           
           match kind, args with
           | `Update, (lfun::value::index) -> Update(lfun, index, value)
           | `Application, (lfun::index)   -> Application(lfun, index)
           | `Length, [arg] -> Length arg
           | `AsFun,  [arg] -> AsFun arg
           | _ -> High.ExceptionsErrorHandling.raise_bindings_error
                    "LFun's reveal not good number of args"
         in Some r
      | _ -> None
      
  end

  type t = unit HTerms.t

  let malloc ?config () = config, Global.hTerms_create 10
  let free = HTerms.reset
                   
  let assert_formula old_assert state f =

    let rec scan_struct : 'a. Term.t -> 'a Types.termstruct -> unit =
      fun t (type a) (t_struct : a Types.termstruct) ->
      begin
        match ExtraTerm.reveal t_struct with
        | Some(Update(lfun, index, value)) ->
           Term.( (ExtraTerm.as_fun t === update (ExtraTerm.as_fun lfun) index value)
                  &&& (ExtraTerm.length t === ExtraTerm.length lfun))
           |> old_assert
           
        | Some(Application(lfun, index)) ->
           Term.(t === application (ExtraTerm.as_fun lfun) index)
           |> old_assert

        | _ -> ()
      end;
      match t_struct with

      | Types.A2(`YICES_EQ_TERM, lhs, rhs) when ExtraTerm.is_lfun lhs ->
         let diff = AddDiff.ExtraTerm.diff (ExtraTerm.as_fun lhs) (ExtraTerm.as_fun rhs) in
         let llhs = ExtraTerm.length lhs in
         let lrhs = ExtraTerm.length rhs in
         let constr = Term.((llhs === lrhs)
                            &&& not1(ExtraTerm.admissible ~lfun:lhs llhs diff)) in
         Term.(constr ==> t) |> old_assert;
         let constr = Term.((llhs === lrhs) 
                            &&& (ExtraTerm.as_fun lhs === ExtraTerm.as_fun rhs)) in
         Term.(constr ==> t) |> old_assert;
         scan_struct_default t_struct   

      |  t_struct -> scan_struct_default t_struct

    and scan_struct_default : 'a. 'a Types.termstruct -> unit =
      fun t_struct -> let _ = Term.map scan_term t_struct in ()
    and scan_term t = scan t; t
    and scan t =
      if HTerms.mem state t then ()
      else
        let () = HTerms.add state t () in
        let Term t_struct = Term.reveal t in
        scan_struct t t_struct
    in

    scan f;
    old_assert f

  let check _t old_model = Sat old_model
    
  let interpolant _t old_interpolant = old_interpolant

end

module DiffLength = Make(Diff)(AddLength)
