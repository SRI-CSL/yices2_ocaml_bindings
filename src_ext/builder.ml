open Containers

open Yices2.Common
open Yices2.Ext
open WithExceptionsErrorHandling
open Types
open Types_ext

module Context : StandardYicesContext with type t = Context.t = struct
  include Context
  let config_set = Config.set
  let check_with_smodel ?param t smodel = check ?param ~smodel t
  let check ?param t = check ?param t
  let get_model t = let SModel.{model;_} = get_model t in model
  type model  = Model.t
  type config = Config.t
  type term   = Term.t
end

module Make
         (Context : YicesContext)
         (C : Ext with type old_term   := Context.term
                   and type old_config := Context.config
                   and type old_model  := Context.model) :
YicesContext with type term = C.term
              and type config = C.config
              and type model  = C.model
  = struct

  type term   = C.term
  type config = C.config
  type model  = C.model

  type t = {
      old_context : Context.t;
      model  : C.model option ref;
      status : Types.smt_status ref;
      state  : C.t
    }

  let config_set = C.config_set

  let malloc ?config () =
    let old_config, state = C.malloc ?config () in
    let old_context = Context.malloc ?config:old_config () in
    { old_context;
      model  = ref None;
      status = ref (Context.status old_context);
      state }

  let malloc_mcsat () =
    let _, state = C.malloc () in
    let old_context = Context.malloc_mcsat () in
    { old_context;
      model  = ref None;
      status = ref (Context.status old_context);
      state }

  let free t = Context.free t.old_context; C.free t.state

  let assert_formula t  = C.assert_formula (Context.assert_formula t.old_context) t.state
  let assert_formulas t = List.iter (assert_formula t)

  let rec check ?param t =
    match Context.check ?param t.old_context with
    | `STATUS_SAT ->
       begin
         match Context.get_model t.old_context |> C.check t.state with
         | Sat model ->
            t.model  := Some model;
            t.status := `STATUS_SAT;
            `STATUS_SAT
         | Unsat interpolant ->
            Context.assert_formula t.old_context interpolant;
            check ?param t
       end

    | status ->
       t.model := None;
       t.status := status;
       status

  let rec check_with_smodel ?param t smodel =
    print_endline (Format.sprintf "@[<v>Check with model =@, @[<v>%a@]@]" (SModel.pp()) smodel);
    match Context.check_with_smodel ?param t.old_context smodel with
    | `STATUS_SAT ->
       begin
         match Context.get_model t.old_context |> C.check t.state with
         | Sat model ->
            t.model  := Some model;
            t.status := `STATUS_SAT;
            `STATUS_SAT
         | Unsat interpolant ->
            Context.assert_formula t.old_context interpolant;
            check_with_smodel ?param t smodel
       end
  
    | status ->
       t.model := None;
       t.status := status;
       status

  let push t = Context.push t.old_context; C.push t.state
  let pop t  = Context.pop t.old_context; C.pop t.state

  let enable_option t = Context.enable_option t.old_context
  let disable_option t = Context.disable_option t.old_context
  let get_model t =
    match !(t.model) with
    | Some model -> model
    | None -> Yices2.High.ExceptionsErrorHandling.raise_bindings_error
                "No model: last status was %a" Types.pp_smt_status !(t.status)

  let get_model_interpolant t =
    Context.get_model_interpolant t.old_context |> C.interpolant t.state
    
  let status t = !(t.status)

  let pp_log fmt t = Context.pp_log fmt t.old_context
end

module Trivial = struct

  type t = unit

  let malloc ?config () = config, ()
  let free _ = ()
  let push _ = ()
  let pop _ = ()

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

    let reveal = HTypes.get htypes

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
         HTypes.add htypes typ (Reveal(hdl, index));
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
      match HTerms.get hterms term with
      | Some a -> Some(a, [])
      | None ->
         match Term.reveal term with
         | Term App(f,l) ->
            begin
              match HTerms.get hterms f with
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
         HTerms.add hterms term (Reveal(hdl, index));
         match args with
         | []   -> term
         | _::_ -> Term.application term args 
                                         
  end

end
