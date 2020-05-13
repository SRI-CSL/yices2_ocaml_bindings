open Sexplib
open Type

open Ctypes

open Yices2_high
open Types

module Bindings : API with type 'a eh := 'a
  
module VarMap : Hashtbl.S with type key = string

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
  val add             : t -> (string*term_t) list -> t
  val permanently_add : t -> string -> term_t -> unit
  val mem             : t -> string -> bool
  val find            : t -> string -> term_t
end

module Session : sig

  type env = {
    logic   : string;
    context : context_t ptr;
    assertions : term_t list;
    param   : param_t ptr;
    model   : model_t ptr option
  }

  type t = {
    verbosity : int;
    config    : ctx_config_t ptr;
    types     : type_t VarMap.t;
    variables : Variables.t;
    env       : env option ref
  }

  val create   : verbosity:int -> t
  val init_env : t -> logic:string -> unit
  val exit : t -> unit

end

module ParseType : sig
  type t = (type_t, type_t) Cont.t
  val atom  : type_t VarMap.t -> string -> t
  val parse : type_t VarMap.t -> Sexp.t -> t
end

module ParseTerm : sig
  type t = (term_t, term_t) Cont.t
  val atom        : Session.t -> string -> t
  val right_assoc : Session.t -> (term_t -> term_t -> term_t) -> Sexp.t list -> t
  val left_assoc  : Session.t -> (term_t -> term_t -> term_t) -> Sexp.t list -> t
  val chainable   : Session.t -> (term_t -> term_t -> term_t) -> Sexp.t list -> (term_t list, term_t) Cont.t
  val unary       : Session.t -> (term_t -> term_t) -> Sexp.t -> t
  val binary      : Session.t -> (term_t -> term_t -> term_t) -> Sexp.t -> Sexp.t -> t
  val ternary     : Session.t -> (term_t -> term_t -> term_t -> term_t) -> Sexp.t -> Sexp.t -> Sexp.t -> t
  val list        : Session.t -> (term_t list -> term_t) -> Sexp.t list -> t
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
