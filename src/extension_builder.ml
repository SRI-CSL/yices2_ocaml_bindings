open Containers
open Ext_bindings

module type YicesContext = sig

  type term     
  type config
  type model

  val config_set : config -> name:string -> value:string -> unit

  type t
  val malloc : ?config:config -> unit -> t
  val malloc_mcsat : unit -> t
  val free : t -> unit
  val status : t -> Types.smt_status
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val check             : ?param:Param.t -> t -> Types.smt_status
  val check_with_smodel : ?param:Param.t -> t -> SModel.t -> Types.smt_status
  val get_model : ?keep_subst:bool -> t -> model
  val get_model_interpolant : t -> term
  
  val pp_log : t Format.printer
end

module type StandardYicesContext =
  YicesContext with type term   = Term.t
                and type config = Config.t
                and type model  = Model.t

module Context : StandardYicesContext with type t = Context.t = struct
  include Context
  let config_set = Config.set
  type model  = Model.t
  type config = Config.t
  type term   = Term.t
end

type ('model, 'interpolant) answer =
  | Sat   of 'model
  | Unsat of 'interpolant

module type Ext = sig

  type old_term
  type old_config
  type old_model

  type term
  type config
  type model

  val config_set : config -> name:string -> value:string -> unit

  type t
  val malloc : ?config:config -> unit -> old_config option * t
  val free   : t -> unit
  val push   : t -> unit
  val pop    : t -> unit

  val assert_formula : (old_term -> unit) -> t -> term -> unit

  val check : t -> old_model -> (model, old_term) answer

  val interpolant : t -> old_term -> term

end

module type StandardExt =
  Ext with type old_term   := Term.t
       and type old_config := Config.t
       and type old_model  := Model.t
       and type term   := Term.t
       and type config := Config.t
       and type model  := Model.t

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
      model : C.model option ref;
      status : Types.smt_status ref;
      state : C.t
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
  let get_model ?keep_subst:_ t =
    match !(t.model) with
    | Some model -> model
    | None -> High.ExceptionsErrorHandling.raise_bindings_error
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

