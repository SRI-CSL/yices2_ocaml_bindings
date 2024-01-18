open Sexplib

open Ext

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

module type API = sig

  module Ext : Ext_types.API
  open Ext

  module Variables : sig
    type t
    val init            : unit -> t
    val add             : t -> (string*Term.t) list -> t
    val permanently_add : t -> string -> Term.t -> unit
    val mem             : t -> string -> bool
    val find            : t -> string -> Term.t
  end

  module Session : sig

    type t = {
        verbosity : int;
        param     : Param.t;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        types     : Type.t VarMap.t;
        variables : Variables.t;
        model     : SModel.t option ref;
        smt2functions : unit Types.HTerms.t;
      }

    val create   : ?set_logic:(?logic:string -> Config.t -> unit) -> int -> t
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

end

module Make(Ext : Ext_types.API) : API with module Ext := Ext
