(** SMT-LIB v2 parser and driver built on top of the Ext API. *)
open Sexplib

module StringHashtbl : CCHashtbl.S with type key = string
module VarMap : CCHashtbl.S with type key = string

(** Continuation-style helpers used by the SMT2 parser module. *)
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

(** Minimal solver API required by the SMT2 parser and driver. *)
module type PARSER_API = sig
  module Type : sig
    type t = Ext.WithExceptionsErrorHandling.Type.t
    val bool : unit -> t
    val int : unit -> t
    val real : unit -> t
    val bv : int -> t
    val tuple : t list -> t
    val func : t list -> t -> t
    val new_uninterpreted : ?name:string -> ?card:int -> unit -> t
    module Names : sig
      val set : t -> string -> unit
    end
  end

  module Term : sig
    type t = Ext.WithExceptionsErrorHandling.Term.t
    val pp : Format.formatter -> t -> unit
    val is_good : t -> bool
    val true0 : unit -> t
    val false0 : unit -> t
    val not1 : t -> t
    val implies : t -> t -> t
    val andN : t list -> t
    val ( !& ) : t list -> t
    val orN : t list -> t
    val xorN : t list -> t
    val eq : t -> t -> t
    val distinct : t list -> t
    val ite : t -> t -> t -> t
    val tuple : t list -> t
    val select : int -> t -> t
    val update : t -> t list -> t -> t
    val application : t -> t list -> t
    val constant : Type.t -> id:int -> t
    val new_uninterpreted : ?name:string -> Type.t -> t
    val new_variable : Type.t -> t
    val forall : t list -> t -> t
    val exists : t list -> t -> t
    val lambda : t list -> t -> t
    module Names : sig
      val set : t -> string -> unit
    end
    module Arith : sig
      val parse_float : string -> t
      val parse_rational : string -> t
      val neg : t -> t
      val sub : t -> t -> t
      val add : t -> t -> t
      val mul : t -> t -> t
      val idiv : t -> t -> t
      val division : t -> t -> t
      val ( %. ) : t -> t -> t
      val abs : t -> t
      val leq : t -> t -> t
      val lt : t -> t -> t
      val geq : t -> t -> t
      val gt : t -> t -> t
      val floor : t -> t
      val is_int_atom : t -> t
    end
    module BV : sig
      val parse_bvbin : string -> t
      val parse_bvhex : string -> t
      val bvconcat : t list -> t
      val bvand : t list -> t
      val bvor : t list -> t
      val bvsum : t list -> t
      val bvproduct : t list -> t
      val bvdiv : t -> t -> t
      val bvrem : t -> t -> t
      val bvshl : t -> t -> t
      val bvlshr : t -> t -> t
      val bvnot : t -> t
      val bvneg : t -> t
      val bvlt : t -> t -> t
      val bvnand : t -> t -> t
      val bvnor : t -> t -> t
      val bvxor : t list -> t
      val bvxnor : t -> t -> t
      val redand : t -> t
      val bvsub : t -> t -> t
      val bvsdiv : t -> t -> t
      val bvsrem : t -> t -> t
      val bvsmod : t -> t -> t
      val bvashr : t -> t -> t
      val bvle : t -> t -> t
      val bvgt : t -> t -> t
      val bvge : t -> t -> t
      val bvslt : t -> t -> t
      val bvsle : t -> t -> t
      val bvsgt : t -> t -> t
      val bvsge : t -> t -> t
      val bvextract : t -> int -> int -> t
      val bvrepeat : t -> int -> t
      val zero_extend : t -> int -> t
      val sign_extend : t -> int -> t
      val rotate_left : t -> int -> t
      val rotate_right : t -> int -> t
      val bvconst_uint64 : width:int -> Unsigned.ULong.t -> t
    end
  end
  module Config : module type of Ext.WithExceptionsErrorHandling.Config
  module Param : module type of Ext.WithExceptionsErrorHandling.Param
  module ModelValue : sig
    type t
    val pp : Format.formatter -> t -> unit
  end

  module PP : sig
    val term_string : ?display:Ext.Types.display -> Term.t -> string
  end

  module Global : sig
    val version : string
    val init : unit -> unit
    val exit : unit -> unit
    val reset : unit -> unit
  end

  module SModel : sig
    type t
    val from_map : ?support:Term.t list -> (Term.t * Term.t) list -> t
    val get_value : t -> Term.t -> ModelValue.t
    val get_value_as_term : t -> Term.t -> Term.t option
    val to_sexp :
      smt2arrays:([ `Curry | `Tuple ] * (Term.t -> bool)) option -> t -> Sexp.t
  end

  module Context : sig
    type t
    val of_id : int -> t option
    val malloc : ?config:Config.t -> unit -> t
    val default_param : t -> Param.t -> unit
    val push : t -> unit
    val pop : t -> unit
    val reset : t -> unit
    val pp : Format.formatter -> t -> unit
    val assert_formula : t -> Term.t -> unit
    val assert_formulas : t -> Term.t list -> unit
    val check :
      ?param:Param.t ->
      ?assumptions:Term.t list ->
      ?smodel:SModel.t ->
      ?as_inequalities:bool ->
      ?hints:Term.t list ->
      t -> Ext.Types.smt_status
    val get_model : ?keep_subst:bool -> ?support:Term.t list -> t -> SModel.t
    val get_unsat_core : t -> Term.t list
    val get_model_interpolant : t -> Term.t
  end
end

(** SMT-LIB v2 front-end API. *)
module type API = sig

  module Ext : PARSER_API
  open Ext
  module StringHashtbl : CCHashtbl.S with type key = string
  module VarMap : CCHashtbl.S with type key = string
  module HTerms : CCHashtbl.S with type key = Term.t

  module Variables : sig
    type t
    val init            : unit -> t
    val add             : t -> (string*Term.t) list -> t
    val permanently_add : t -> string -> Term.t -> unit
    val mem             : t -> string -> bool
    val find            : t -> string -> Term.t
  end

  module Session : sig

    (** Mutable session state for parsing and evaluation. *)
    type syntax_hooks = {
        parse_type : Type.t VarMap.t -> Sexp.t -> Type.t option;
        parse_term :
          'a. (t -> Sexp.t -> (Term.t, 'a) Cont.t) ->
          t -> Sexp.t -> ((Term.t, 'a) Cont.t) option;
        set_logic : string -> Config.t -> bool;
      }
    and t = {
        verbosity : int;
        param     : Param.t;
        infos     : string StringHashtbl.t;
        options   : string StringHashtbl.t;
        types     : Type.t VarMap.t;
        variables : Variables.t;
        model     : SModel.t option ref;
        smt2functions : unit HTerms.t;
        syntax_hooks : syntax_hooks;
      }

    val set_logic: (?logic:string -> Config.t -> unit)
    val no_syntax_hooks : syntax_hooks
    val create   : ?syntax_hooks:syntax_hooks -> int -> t
    val exit : t -> unit

  end

  module ParseType : sig
    type t = (Type.t, Type.t) Cont.t
    val atom  : Type.t VarMap.t -> string -> t
    val parse : ?syntax_hooks:Session.syntax_hooks -> Type.t VarMap.t -> Sexp.t -> t
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
    val process_file : ?syntax_hooks:Session.syntax_hooks -> ?verbosity:int -> string -> unit
  end

end

(** Instantiate the SMT2 front-end over a parser-level implementation. *)
module Make_parser(Ext : PARSER_API) : API with module Ext := Ext

(** Instantiate the SMT2 front-end over a specific Ext implementation. *)
module Make(Ext : Ext_types.API) : API with module Ext := Ext
