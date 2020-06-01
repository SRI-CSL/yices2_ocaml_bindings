open Containers
open Sexplib

open Ctypes

open Yices2_high
open Types

open Yices2_ext_bindings

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
    verbosity : int;
    logic     : string;
    types     : type_t VarMap.t;
    variables : Variables.t;
    context : Context.t;
    param   : Param.t;
    model   : Model.t option
  }

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
