open Containers

open Yices2.Ext
open WithExceptionsErrorHandling
open Types
open Builder
open Types_ext

module AddDiff = struct

  type term   = Term.t
  type typ    = Type.t
  type old_context = Context.t
  type config = Config.t
  type param  = Param.t
  type smodel = SModel.t
  type model  = Model.t

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
           
        | _ -> Yices2.High.ExceptionsErrorHandling.raise_bindings_error
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
  let reset = HTerms.reset
  let push _ = ()
  let pop _ = ()
  let goto _ _ = ()

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


  let translate_assertion (_ctx : old_context) state f =
    let constraints = ref [] in
    let add_constraints l = constraints := List.rev_append l !constraints in

    let rec scan_struct : 'a. Term.t -> 'a Types.termstruct -> unit =
      fun t (type a) (t_struct : a Types.termstruct) ->
      match ExtraTerm.reveal t_struct with
      | Some (lhs, rhs) ->
         add_constraints (generate_constraint state [] Term.(lhs === rhs) lhs rhs);
         scan_struct_default t_struct
      | None ->
         match t_struct with
         | Types.A2 (`YICES_EQ_TERM, lhs, rhs) ->
            add_constraints (generate_constraint state [] t lhs rhs);
            scan_struct_default t_struct
         | t_struct -> scan_struct_default t_struct
    and scan_struct_default : 'a. 'a Types.termstruct -> unit =
      fun t_struct ->
      let _ = Term.map scan_term t_struct in
      ()
    and scan_term t =
      scan t;
      t
    and scan t =
      if HTerms.mem state t then ()
      else (
        let Term t_struct = Term.reveal t in
        scan_struct t t_struct;
        HTerms.add state t ()
      )
    in
    scan f;
    List.rev !constraints @ [f]

  let translate_assumption (_ctx : old_context) _state t = t

  let term_of_old _ t = t
  let typ_of_old _ ty = ty
  let param_to_old _ p = p
  let smodel_to_old _ m = m
  let smodel_of_old _ m = m
  let smodel_of_model _ ?support model = SModel.make ?support model

  let check _t (SModel{model; _}) = Sat model

  let interpolant _t old_interpolant = old_interpolant

  let pp_term = Term.pp
  let pp_type = Type.pp
  let term_to_sexp ?smt2arrays t = Term.to_sexp ?smt2arrays t
  let type_to_sexp ?smt2arrays t = Type.to_sexp ?smt2arrays t
  let smodel_to_sexp ?smt2arrays smodel =
    let bindings =
      SModel.as_map smodel
      |> List.map (fun (lhs, rhs) ->
             let lhs = Term.to_sexp ?smt2arrays lhs in
             let rhs = Term.to_sexp ?smt2arrays rhs in
             Sexplib.Sexp.List [Sexplib.Sexp.Atom ":="; lhs; rhs])
    in
    Sexplib.Sexp.List (Sexplib.Sexp.Atom "model" :: bindings)

end

include Make(Context)(AddDiff)
