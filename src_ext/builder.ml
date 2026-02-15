open Containers
open Sexplib

open Yices2.Common
open Yices2.Ext
open WithExceptionsErrorHandling
open Types_ext
open Types_ext.Log

let pp_sep_break fmt () = Format.fprintf fmt "@,"

module BaseContext = Context

module Context : StandardYicesContext with type t = BaseContext.t = struct
  (* The wrapped context uses the base Ext API but exposes the parametric
     log/trace types defined in Types_ext. *)
  type typ    = Type.t
  type term   = Term.t
  type config = Config.t
  type param  = Param.t
  type smodel = SModel.t
  type assertions = term Log.assertions
  type action = (term, typ, param, smodel) Log.action
  type t = BaseContext.t

  let pp_options        = BaseContext.pp_options
  let pp_config_options = BaseContext.pp_config_options

  let assertions ctx =
    let Types.Assertions{list; level} = BaseContext.assertions ctx in
    Assertions{ list; level }

  let map_context_action = function
    | Types.Status -> Status
    | Types.Reset -> Reset
    | Types.Push -> Push
    | Types.Pop -> Pop
    | Types.EnableOption s -> EnableOption s
    | Types.DisableOption s -> DisableOption s
    | Types.AssertFormula t -> AssertFormula t
    | Types.AssertFormulas ts -> AssertFormulas ts
    | Types.AssertBlockingClause -> AssertBlockingClause
    | Types.Check param -> Check param
    | Types.CheckWithAssumptions{ param; assumptions } ->
       CheckWithAssumptions{ param; assumptions }
    | Types.Stop -> Stop
    | Types.GetModel{ keep_subst } -> GetModel{ keep_subst }
    | Types.GetUnsatCore -> GetUnsatCore
    | Types.CheckWithModel{ param; smodel } ->
       CheckWithModel{ param; smodel }
    | Types.GetModelInterpolant -> GetModelInterpolant

  let map_action = function
    | Types.DeclareType(typ, card) -> DeclareType(typ, card)
    | Types.DeclareFun(t, typ) -> DeclareFun(t, typ)
    | Types.DefineType(name, typ) -> DefineType(name, typ)
    | Types.DefineFun(name, t, typ) -> DefineFun(name, t, typ)
    | Types.CheckWithInterpolation{ param; build_model; context1; context2 } ->
       CheckWithInterpolation{ param; build_model; context1; context2 }
    | Types.GarbageCollect sexpl -> GarbageCollect sexpl
    | Types.NewContext { logic } -> NewContext { logic }
    | Types.ContextAction { context_id; context_action } ->
       ContextAction{ context_id; context_action = map_context_action context_action }

  let options        = BaseContext.options
  let config         = BaseContext.config
  let config_options = BaseContext.config_options
  let log ctx        = BaseContext.log ctx |> List.map map_action
  let is_mcsat       = BaseContext.is_mcsat
  let id             = BaseContext.id
  let of_id          = BaseContext.of_id
  let all            = BaseContext.all
  let to_sexp        = BaseContext.to_sexp
  let pp             = BaseContext.pp
  let pp_log         = BaseContext.pp_log

  let malloc         = BaseContext.malloc
  let malloc_mcsat   = BaseContext.malloc_mcsat
  let malloc_logic   = BaseContext.malloc_logic
  let default_param  = BaseContext.default_param
  let status         = BaseContext.status
  let reset          = BaseContext.reset
  let push           = BaseContext.push
  let pop            = BaseContext.pop
  let goto           = BaseContext.goto
  let enable_option  = BaseContext.enable_option
  let disable_option = BaseContext.disable_option
  let assert_formula = BaseContext.assert_formula
  let assert_formulas = BaseContext.assert_formulas
  let assert_blocking_clause = BaseContext.assert_blocking_clause
  let check          = BaseContext.check
  let set_fixed_var_order   = BaseContext.set_fixed_var_order
  let set_initial_var_order = BaseContext.set_initial_var_order
  let stop           = BaseContext.stop
  let get_model      = BaseContext.get_model
  let get_unsat_core = BaseContext.get_unsat_core
  let get_model_interpolant = BaseContext.get_model_interpolant
  let check_with_interpolation = BaseContext.check_with_interpolation
end

module Make
         (Context : Context)
         (C : Ext with type old_term   := Context.term
                   and type old_typ    := Context.typ
                   and type old_context := Context.t
                   and type old_config := Context.config
                   and type old_param  := Context.param
                   and type old_smodel := Context.smodel) :
Context with type term   = C.term
        and type typ    = C.typ
        and type config = C.config
        and type param  = C.param
        and type smodel = C.smodel
        and type assertions = C.term Log.assertions
        and type action = (C.term, C.typ, C.param, C.smodel) Log.action
  = struct

  type typ    = C.typ
  type term   = C.term
  type config = C.config
  type param  = C.param
  type smodel = C.smodel
  type assertions = term Log.assertions
  type action = (term, typ, param, smodel) Log.action

  module Assertions = struct
    let init = Log.Assertions {
        list = [Some []];
        level = 0;
      }

    let add_formula (Log.Assertions{list; level}) formula =
      match list with
      | [] -> assert false
      | last::tail ->
         Log.Assertions{ list = (Option.map (List.cons formula) last)::tail; level }

    let add_formulas (Log.Assertions{list; level}) formulas =
      match list, formulas with
      | _, [] -> Log.Assertions{ list; level }
      | [], _ -> assert false
      | last::tail, _ ->
         let last = Option.map (List.rev_append (List.rev formulas)) last in
         Log.Assertions{ list = last::tail; level }

    let mark_blocking_clause (Log.Assertions{list; level}) =
      match list with
      | [] -> assert false
      | _::tail -> Log.Assertions{ list = None::tail; level }

    let push (Log.Assertions{list; level}) =
      Log.Assertions{ list = Some []::list; level = level + 1 }

    let pop (Log.Assertions{list; level}) =
      match list with
      | [] | [_] ->
         Yices2.High.ExceptionsErrorHandling.raise_bindings_error
           "pop on empty assertion stack"
      | _::tail -> Log.Assertions{ list = tail; level = level - 1 }

    let pp_level fmt = function
      | None ->
         Format.fprintf fmt "@[<v>??@]"
      | Some assertions ->
         Format.fprintf fmt "@[<v>%a@]" (List.pp C.pp_term) assertions

    let pp fmt (Log.Assertions{list;_}) =
      Format.fprintf fmt "@[<v>%a@]" (List.pp pp_level) list
  end

  module Action = struct
    let term_name t = Format.asprintf "%a" C.pp_term t
    let type_name t = Format.asprintf "%a" C.pp_type t

    let term_sexp ?smt2arrays t = C.term_to_sexp ?smt2arrays t
    let smodel_sexp ?smt2arrays t = C.smodel_to_sexp ?smt2arrays t

    let type_sexp ?smt2arrays typ =
      let mode = Option.map fst smt2arrays in
      C.type_to_sexp ?smt2arrays:mode typ

    let to_sexp_context ?smt2arrays = function
      | Status -> sexp "get-status" []
      | Reset  -> sexp "reset-assertions" []
      | Push   -> sexp "push" []
      | Pop    -> sexp "pop" []
      | EnableOption s -> sexp "set-option" [Sexp.Atom s; Sexp.Atom "true"]
      | DisableOption s -> sexp "set-option" [Sexp.Atom s; Sexp.Atom "false"]
      | AssertFormula t -> sexp "assert" [term_sexp ?smt2arrays t]
      | AssertFormulas l ->
         sexp "assert" (List.map (term_sexp ?smt2arrays) l)
      | AssertBlockingClause -> sexp "assert-blocking-clause" []
      | Check _param -> sexp "check-sat" []
      | CheckWithAssumptions{ assumptions; _ } ->
         sexp "check-sat-assuming" (List.map (term_sexp ?smt2arrays) assumptions)
      | Stop -> sexp "stop" []
      | GetModel _ -> sexp "get-model" []
      | GetUnsatCore -> sexp "get-unsat-core" []
      | CheckWithModel{ smodel; _} ->
         sexp "check-sat-assuming-model" [smodel_sexp ?smt2arrays smodel]
      | GetModelInterpolant -> sexp "get-unsat-model-interpolant" []

    let to_sexp ?smt2arrays accu = function
      | DeclareType(typ, card) ->
         let typ_string = type_name typ in
         sexp "declare-sort"
           (Option.map_or
              ~default:[Sexp.Atom typ_string; Sexp.Atom "0"]
              (fun card ->
                [Sexp.Atom typ_string; Sexp.Atom "0"; Sexp.Atom(string_of_int card)])
              card)
         ::accu
      | DeclareFun(t, typ) ->
         (* We do not try to recover a function's domain/codomain here: the
            extension may not expose a full type introspection API. *)
         let symb = Sexp.Atom(term_name t) in
         sexp "declare-fun" [symb; Sexp.List []; type_sexp ?smt2arrays typ] ::accu
      | DefineType(name, typ) ->
         sexp "define-sort"
           [Sexp.Atom name; Sexp.List []; type_sexp ?smt2arrays typ]
         ::accu
      | DefineFun(name, t, typ) ->
         sexp "define-fun"
           [Sexp.Atom name;
            Sexp.List [];
            type_sexp ?smt2arrays typ;
            term_sexp ?smt2arrays t]
         ::accu
      | CheckWithInterpolation{ build_model; context1; context2; _} ->
         let build_model =
           Sexp.Atom(if build_model then "build_model" else "no_build_model")
         in
         sexp "check-sat-with-interpolation"
           [build_model;
            Sexp.Atom(string_of_int context1);
            Sexp.Atom(string_of_int context2)]
         ::accu
      | GarbageCollect sexpl -> sexp "garbage-collect" sexpl ::accu
      | NewContext { logic = Some logic } ->
         sexp "set-logic" [Sexp.Atom logic] ::accu
      | NewContext { logic = None } ->
         sexp "new-context" [] ::accu
      | ContextAction { context_id ; context_action } ->
         let action = to_sexp_context ?smt2arrays context_action in
         if context_id = 0 then action::accu
         else sexp "context" [Sexp.Atom(string_of_int context_id); action]::accu
  end

  type t = {
      old_context : Context.t;
      config      : config option;
      state       : C.t;
      status      : Types.smt_status ref;
      model       : Context.smodel option ref;
      assertions  : assertions ref;
      log         : action list ref;
      id          : int;
    }

  let pp_options        = Context.pp_options
  let pp_config_options = Context.pp_config_options

  module HContext = Hashtbl.Make(Int)
  let all_contexts = HContext.create 10
  let () =
    Global.register_cleanup (fun ~after ->
      match after with
      | `GC -> ()
      | _ -> HContext.reset all_contexts)

  let log_context_action t context_action =
    t.log := ContextAction { context_id = 0; context_action } :: !(t.log)

  let add_context t = HContext.replace all_contexts t.id t
  let _remove_context t = HContext.remove all_contexts t.id

  let assertions t = !(t.assertions)
  let options t = Context.options t.old_context
  let config t = t.config
  let config_options t = Context.config_options t.old_context
  (* The log is kept newest-first, like Ext.Context.log.
     to_sexp folds over it to restore chronological order. *)
  let log t = !(t.log)
  let is_mcsat t = Context.is_mcsat t.old_context
  let id t = t.id
  let of_id id = HContext.find_opt all_contexts id
  let all () = HContext.to_seq_values all_contexts

  let to_sexp ?smt2arrays t =
    log t |> List.fold_left (Action.to_sexp ?smt2arrays) []

  let pp fmt t = Assertions.pp fmt !(t.assertions)

  let pp_log fmt t =
    Format.fprintf fmt "%a"
      (List.pp ~pp_sep:pp_sep_break pp_sexp) (to_sexp t)

  let malloc ?config () =
    let old_config, state = C.malloc ?config () in
    let old_context = Context.malloc ?config:old_config () in
    let id = Context.id old_context in
    let t =
      { old_context;
        config;
        state;
        status = ref (Context.status old_context);
        model  = ref None;
        assertions = ref Assertions.init;
        log = ref [NewContext { logic = None }];
        id;
      }
    in
    add_context t;
    t

  let malloc_mcsat ?interpol () =
    let _, state = C.malloc () in
    let old_context = Context.malloc_mcsat ?interpol () in
    let id = Context.id old_context in
    let t =
      { old_context;
        config = None;
        state;
        status = ref (Context.status old_context);
        model  = ref None;
        assertions = ref Assertions.init;
        log = ref [NewContext { logic = None }];
        id;
      }
    in
    add_context t;
    t

  let malloc_logic logic =
    let _, state = C.malloc () in
    let old_context = Context.malloc_logic logic in
    let id = Context.id old_context in
    let t =
      { old_context;
        config = None;
        state;
        status = ref (Context.status old_context);
        model  = ref None;
        assertions = ref Assertions.init;
        log = ref [NewContext { logic = Some logic }];
        id;
      }
    in
    add_context t;
    t

  let status t = !(t.status)

  let default_param t param =
    Context.default_param t.old_context (C.param_to_old t.state param)

  let reset t =
    log_context_action t Reset;
    Context.reset t.old_context;
    C.reset t.state;
    t.assertions := Assertions.init;
    t.model := None;
    t.status := Context.status t.old_context

  let push t =
    log_context_action t Push;
    t.assertions := Assertions.push !(t.assertions);
    Context.push t.old_context;
    C.push t.state

  let pop t =
    log_context_action t Pop;
    t.assertions := Assertions.pop !(t.assertions);
    Context.pop t.old_context;
    C.pop t.state

  let goto t level =
    let Assertions{ level = current; _ } = !(t.assertions) in
    let diff = level - current in
    if diff > 0
    then for _ = 1 to diff do push t done
    else for _ = 1 to -diff do pop t done

  let enable_option t ~option =
    log_context_action t (EnableOption option);
    Context.enable_option t.old_context ~option

  let disable_option t ~option =
    log_context_action t (DisableOption option);
    Context.disable_option t.old_context ~option

  let assert_formula t formula =
    log_context_action t (AssertFormula formula);
    t.assertions := Assertions.add_formula !(t.assertions) formula;
    (* translate_assertion may return multiple constraints (e.g., purification).
       These are asserted at the old level but do not appear in the extension log. *)
    C.translate_assertion t.old_context t.state formula
    |> List.iter (Context.assert_formula t.old_context)

  let assert_formulas t formulas =
    log_context_action t (AssertFormulas formulas);
    t.assertions := Assertions.add_formulas !(t.assertions) formulas;
    let assert_one formula =
      (* Each input formula can expand to multiple old-level constraints. *)
      C.translate_assertion t.old_context t.state formula
      |> List.iter (Context.assert_formula t.old_context)
    in
    List.iter assert_one formulas

  let assert_blocking_clause t =
    log_context_action t AssertBlockingClause;
    t.assertions := Assertions.mark_blocking_clause !(t.assertions);
    Context.assert_blocking_clause t.old_context

  let translate_param = Option.map
  let translate_list f = Option.map (List.map f)

  let rec check ?param ?assumptions ?smodel ?as_inequalities ?hints t =
    let old_param = translate_param (C.param_to_old t.state) param in
    let old_smodel = translate_param (C.smodel_to_old t.state) smodel in
    (* translate_assumption is used for assumptions, hints, and var ordering.
       It should be side-effect free: we do not add extra constraints here. *)
    let old_assumptions = translate_list (C.translate_assumption t.old_context t.state) assumptions in
    let old_hints = translate_list (C.translate_assumption t.old_context t.state) hints in
    begin match assumptions, smodel, hints with
    | None, None, None ->
       log_context_action t (Check param)
    | _ ->
       (match smodel with
        | Some smodel ->
           log_context_action t (CheckWithModel{ param; smodel })
        | None ->
           let assumptions = Option.get_or ~default:[] assumptions in
           log_context_action t (CheckWithAssumptions{ param; assumptions }))
    end;
    match Context.check ?param:old_param ?assumptions:old_assumptions ?smodel:old_smodel
            ?as_inequalities ?hints:old_hints t.old_context with
    | `STATUS_SAT ->
       begin
         match Context.get_model t.old_context |> C.check t.state with
         | Sat smodel ->
            t.model  := Some smodel;
            t.status := `STATUS_SAT;
            `STATUS_SAT
         | Unsat interpolant ->
            (* The interpolant is an internal refinement term; it should not be
               recorded in the extension-level assertion log. *)
            Context.assert_formula t.old_context interpolant;
            check ?param ?assumptions ?smodel ?as_inequalities ?hints t
       end
    | status ->
       t.model := None;
       t.status := status;
       status

  let set_fixed_var_order t vars =
    (* Variable-ordering hints are translated with translate_assumption. *)
    let vars = List.map (C.translate_assumption t.old_context t.state) vars in
    Context.set_fixed_var_order t.old_context vars

  let set_initial_var_order t vars =
    (* Variable-ordering hints are translated with translate_assumption. *)
    let vars = List.map (C.translate_assumption t.old_context t.state) vars in
    Context.set_initial_var_order t.old_context vars

  let stop t =
    log_context_action t Stop;
    Context.stop t.old_context

  let get_model ?(keep_subst=true) ?support t =
    log_context_action t (GetModel{ keep_subst });
    (* keep_subst is recorded for log parity, but the extension model is
       derived at check time from the old model. *)
    match !(t.model) with
    | Some base_smodel ->
       C.enrich_smodel t.state ?support base_smodel
    | None ->
       Yices2.High.ExceptionsErrorHandling.raise_bindings_error
         "No model: last status was %a" Types.pp_smt_status !(t.status)

  let get_unsat_core t =
    log_context_action t GetUnsatCore;
    Context.get_unsat_core t.old_context |> List.map (C.term_of_old t.state)

  let get_model_interpolant t =
    log_context_action t GetModelInterpolant;
    Context.get_model_interpolant t.old_context |> C.interpolant t.state

  let check_with_interpolation ?(build_model=true) ?param t1 t2 =
    let action =
      CheckWithInterpolation{ param; build_model; context1 = t1.id; context2 = t2.id }
    in
    t1.log := action::!(t1.log);
    t2.log := action::!(t2.log);
    let old_param = Option.map (C.param_to_old t1.state) param in
    let status =
      Context.check_with_interpolation ~build_model ?param:old_param
        t1.old_context t2.old_context
    in
    match status with
    | #Types.smt_inconclusive_status as x -> x
    | `STATUS_UNSAT interpolant ->
       `STATUS_UNSAT (C.interpolant t1.state interpolant)
    | `STATUS_SAT build_smodel ->
       let build ?support () =
         (* support is translated with translate_assumption; no extra constraints
            are introduced in this path. *)
         let support = Option.map (List.map (C.translate_assumption t1.old_context t1.state)) support in
         build_smodel ?support () |> C.smodel_of_old t1.state
       in
       `STATUS_SAT build
end

module Trivial = struct

  type t = unit

  let malloc ?config () = config, ()
  let reset _ = ()
  let push _ = ()
  let pop _ = ()
  let goto _ _ = ()

end

module SyntaxExtensions = struct

  (* type 'a formatted =
   *   | F : ('a , Format.formatter, unit) format -> 'a formatted
   *   | FormatApply : ('a -> 'b) formatted * Type.t -> 'b formatted
   * 
   * type 'a compiled = {
   *     
   *   }
   *               
   * let rec compile : type a. a formatted -> Format.formatter -> a = function
   *   | F s    -> fun fmt -> Format.fprintf fmt s
   *   | FormatApply(a,b) -> fun fmt -> print a fmt b
   * 
   * let (//) a b = FormatApply(a,b) *)

  exception StringAlreadyUsed of string

  type ('a, 'b) info = {
      add : ('a -> 'b -> unit);
      get : ('a -> 'b option);
      name: string;
      pp  : 'a Format.printer        
    }

  let buildTbl (type a) (module H : TypeIndex with type t = a) =
    let module R = struct
        module HT = CCHashtbl.Make(H)
        let tbl = HT.create 10
        let add = HT.add tbl
        let get = HT.get tbl
      end
    in
    { add = R.add;
      get = R.get;
      name = H.name;
      pp = H.pp }

  module DeclareTypes(R : NewTypes) : sig
    type reveal = Reveal : 'a R.t * 'a -> reveal
    val reveal : Type.t -> reveal option
    val build  : reveal -> Type.t
  end = struct

    type reveal = Reveal : 'a R.t * 'a -> reveal

    module Value = struct
      type 'a t = ('a, Type.t) info
    end
               
    module M = Dmap.MakeWithValue(R)(Value)
    let hdlmap = ref M.empty
    let used   = Yices2.Common.HStrings.create 10
    let ()     = Global.register_cleanup (fun ~after:_ -> HStrings.reset used) 
    let htypes = Global.hTypes_create 10

    let reveal = Types.HTypes.get htypes

    let build (Reveal(hdl, index)) =
      let {get; add; name; pp} =
        match M.find_opt hdl !hdlmap with
        | Some tbl -> tbl
        | None ->
           let (module Index) = R.index hdl in
           if HStrings.mem used Index.name then raise(StringAlreadyUsed Index.name);
           HStrings.add used Index.name ();
           let tbl = buildTbl (module Index) in
           hdlmap := M.add hdl tbl !hdlmap;
           tbl
      in
      match get index with
      | Some typ -> typ
      | None ->
         let name = Format.sprintf "%s_%a" name pp index in
         let typ  = Type.new_uninterpreted ~name () in
         add index typ;
         Types.HTypes.add htypes typ (Reveal(hdl, index));
         typ
                                         
  end

  module DeclareTerms(R : NewTerms) : sig
    type reveal = Reveal : 'a R.t * 'a -> reveal
    val reveal : Term.t -> (reveal * Term.t list) option
    val build  : reveal -> Term.t list -> Term.t
  end = struct

    type reveal = Reveal : 'a R.t * 'a -> reveal

    module Value = struct
      type 'a t = ('a, Term.t) info * ('a -> Type.t list * Type.t)
    end
               
    module M = Dmap.MakeWithValue(R)(Value)
    let hdlmap = ref M.empty
    let used   = HStrings.create 10
    let ()     = Global.register_cleanup (fun ~after:_ -> HStrings.reset used) 
    let hterms = Global.hTerms_create 10

    let reveal term =
      match Types.HTerms.get hterms term with
      | Some a -> Some(a, [])
      | None ->
         match Term.reveal term with
         | Term App(f,l) ->
            begin
              match Types.HTerms.get hterms f with
              | Some a -> Some(a, l)
              | None -> None
            end
         | _ -> None

    let build (Reveal(hdl, index)) args =
      let {get; add; name; pp}, get_type =
        match M.find_opt hdl !hdlmap with
        | Some tbl -> tbl
        | None ->
           let (module Index) = R.index hdl in
           if HStrings.mem used Index.name then raise(StringAlreadyUsed Index.name);
           HStrings.add used Index.name ();
           let tbl = buildTbl (module Index), Index.get_type in
           hdlmap := M.add hdl tbl !hdlmap;
           tbl
      in
      match get index, args with
      | Some term, []   -> term
      | Some term, _::_ -> Term.application term args
      | None, _ ->
         let name = Format.sprintf "%s_%a" name pp index in
         let typ  =
           match get_type index with
           | [], codom -> codom
           | (_::_) as dom, codom -> Type.func dom codom
         in
         let term = Term.new_uninterpreted ~name typ in
         add index term;
         Types.HTerms.add hterms term (Reveal(hdl, index));
         match args with
         | []   -> term
         | _::_ -> Term.application term args 
                                         
  end

end
