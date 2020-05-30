[%%import "gmp.mlh"]
open Containers
open Sexplib
open Type

open Ctypes

open Yices2_high
open Types

module Bindings : sig
  include API with type 'a eh := 'a

  module Types := Types
  module Types : sig
    include module type of Types
    val pp_error_report : error_report Format.printer
  end

  module Type := Type
  module Type : sig
    include module type of Type
    val pp : t Format.printer
  end

  module Term := Term
  module Term : sig
    include module type of Term
    val pp : t Format.printer
  end

  module Context : sig

    val assert_blocking_clause : Context.t -> unit
      
    type assertions = Term.t list list
    val pp_assertions : assertions Format.printer

    type options
    val pp_options : options Format.printer

    type nonrec t = {
      config     : Config.t option;
      context    : Context.t;
      assertions : assertions ref;
      options    : options;
    }
    val pp : Containers.Format.t -> t -> unit
    val malloc : ?config:Config.t -> unit -> t
    val free : t -> unit
    val status : t -> Types.smt_status
    val reset : t -> unit
    val push : t -> unit
    val pop : t -> unit
    val enable_option  : t -> option:string -> unit
    val disable_option : t -> option:string -> unit
    val assert_formula : t -> Term.t -> unit
    val assert_formulas : t -> Term.t list -> unit
    val check                  : ?param:Param.t -> t -> Types.smt_status
    val check_with_assumptions : ?param:Param.t -> t -> Term.t list -> Types.smt_status
    [%%if gmp_present]
    val check_with_model : ?param:Param.t -> t -> Model.t -> Term.t list -> Types.smt_status
    val get_model_interpolant : t -> Term.t
    [%%endif]
    val stop : t -> unit
    val get_model : t -> keep_subst:bool -> Model.t
    val get_unsat_core : t -> Term.t list
  end

  module Param := Param
  module Param : sig
    include module type of Param
    val default : Context.t -> t -> unit
  end

end

open Bindings

module StringHashtbl : CCHashtbl.S with type key = string
module VarMap : CCHashtbl.S with type key = string

module Cont : sig
  type ('a, 'r) t
  val get      : ('a, 'a) t -> 'a
  val ( let* ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t
  val return   : 'a -> ('a, 'r) t
  val return1  : ('a -> 'b) -> 'a -> ('b, 'r) t
  val return2  : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, 'r) t
  val fold     : ('a -> 'b -> ('b, 'c) t) -> 'a list -> 'b -> ('b, 'c) t
  val iter     : ('a -> (unit, 'b) t) -> 'a list -> (unit, 'b) t
  val map      : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
end

exception Yices_SMT2_exception of string

module Variables : sig
  type t
  val init            : unit -> t
  val add             : t -> (string*Term.t) list -> t
  val permanently_add : t -> string -> Term.t -> unit
  val mem             : t -> string -> bool
  val find            : t -> string -> Term.t
end

module Session : sig

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

  val create   : verbosity:int -> t
  val init_env : ?configure:unit -> t -> logic:string -> unit
  val exit : t -> unit

end

module ParseType : sig
  type t = (Type.t, Type.t) Cont.t
  val atom  : Type.t VarMap.t -> string -> t
  val parse : Type.t VarMap.t -> Sexp.t -> t
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
  val process_file : ?verbosity:int -> string -> unit
end
