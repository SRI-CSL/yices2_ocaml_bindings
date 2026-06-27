open Yices2
open Ext.WithExceptionsErrorHandling

(** String extension for the Yices OCaml bindings.

    The current Stage 3 implementation supports string literals, variables,
    concatenation, length, equality, disequality, one-unknown concat/literal
    equations, selected extended functions, and a small regex membership subset.
    The extension solves through a UF/LIA abstraction, then accepts [sat] only
    after validating a concrete string overlay model.

    Unsupported word equations, multi-unknown concat splits, and regex forms
    outside the initial subset return [unknown] unless the concrete model
    validator can prove the current assignment satisfies the active formulas.

    Diagnostics use [YICES_STRING_LOG_LEVEL]. The refinement loop is bounded by
    [YICES_STRING_REFINEMENT_LIMIT], defaulting to 100 extension iterations.
    Shared witness creation for positive [contains] and symbolic
    [substr]/[indexof]/[replace] reductions is bounded by
    [YICES_STRING_WITNESS_LIMIT] and [YICES_STRING_WITNESS_ROUND_LIMIT],
    defaulting to 100 total witnesses and 10 fresh witnesses per refinement
    round. *)

type string_view =
  | Lit of string
  | Concat of Term.t list
  | Len of Term.t
  | Substr of Term.t * Term.t * Term.t
  | Contains of Term.t * Term.t
  | Indexof of Term.t * Term.t * Term.t
  | Replace of Term.t * Term.t * Term.t
  | Prefixof of Term.t * Term.t
  | Suffixof of Term.t * Term.t
  | At of Term.t * Term.t
  | InRe of Term.t * regex

and regex =
  | ReEmpty
  | ReAll
  | ReAllChar
  | ReLit of string
  | ReRange of int * int
  | ReConcat of regex list
  | ReUnion of regex list
  | ReStar of regex

module Regex : sig
  type t = regex

  val empty : t
  val all : t
  val all_char : t
  val str : string -> t
  val range : string -> string -> t
  val concat : t list -> t
  val union : t list -> t
  val star : t -> t
end

module StringModel : sig
  type t = {
    base : SModel.t;
    strings : (Term.t * string) list;
  }

  (** [find_string model term] returns the concrete string assigned to [term],
      if the string extension model construction knows one. *)
  val find_string : t -> Term.t -> string option
end

module Type : sig
  include module type of Type

  (** The SMT string sort, represented as one uninterpreted Yices type. *)
  val string : unit -> Type.t

  (** [is_string ty] tests whether [ty] is the extension string sort. *)
  val is_string : Type.t -> bool
end

module Term : sig
  include module type of Term

  (** [str s] creates a string literal.
      [s] must be valid UTF-8. Length is counted in Unicode scalar values. *)
  val str : string -> Term.t

  (** [string_var ?name ()] creates a fresh uninterpreted constant of string
      type. *)
  val string_var : ?name:string -> unit -> Term.t

  (** [concat terms] creates a canonical string concatenation. The empty list is
      the empty string, a singleton returns that term, and nested concatenations
      are flattened. *)
  val concat : Term.t list -> Term.t

  (** [len term] creates the integer length term for [term]. *)
  val len : Term.t -> Term.t

  val substr : Term.t -> Term.t -> Term.t -> Term.t
  val contains : Term.t -> Term.t -> Term.t
  val indexof : Term.t -> Term.t -> Term.t -> Term.t
  val replace : Term.t -> Term.t -> Term.t -> Term.t
  val prefixof : Term.t -> Term.t -> Term.t
  val suffixof : Term.t -> Term.t -> Term.t
  val at : Term.t -> Term.t -> Term.t
  val in_re : Term.t -> regex -> Term.t

  (** [string_reveal term] returns the extension metadata for [term], if [term]
      was built by this module. *)
  val string_reveal : Term.t -> string_view option
end

module Context :
  Types_ext.Context
    with type typ = Type.t
     and type term = Term.t
     and type config = Config.t
     and type param = Param.t
     and type smodel = StringModel.t
