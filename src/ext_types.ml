open Containers
open Sexplib

open Common
open High
   
module type Config = sig
  type t
  val malloc : unit -> t
  val free : t -> unit
  val set : t -> name:string -> value:string -> unit
  val default : ?logic:string -> t -> unit
  val get     : t -> String.t -> String.t
  val options : t -> (String.t * String.t) list
end

module type Types = sig
  include module type of Types
  val pp_error_report : error_report Format.printer
end

module type Context = sig

  type typ
  type term
  type assertions
  type action
  type config
  type param
  type smodel

  val pp_options        : unit HStrings.t Format.printer
  val pp_config_options : string HStrings.t Format.printer

  type t

  val assertions     : t -> assertions (* Structure of assertions and level *)
  val options        : t -> unit HStrings.t   (* Set options (hashtbl copy) *)
  val config_options : t -> string HStrings.t (* Set config options (hashtbl copy) *)
  val log            : t -> action list   (* Everything that happened to that context *)
  val is_alive       : t -> bool            (* Whether the raw context wasn't freed *)
  val is_mcsat       : t -> bool            (* Whether the context uses mcsat *)

  (** Turns the log into list of S-expressions.
      The first executed action ends up as the head of the list. *)
  val to_sexp : t -> Sexplib.Type.t list

  (** Prints the assertions. *)
  val pp : t Format.printer

  (** Prints the log. *)
  val pp_log : t Format.printer

  val malloc : ?config:config -> unit -> t
  val malloc_mcsat : unit -> t

  (** All contexts ever created *)
  val all  : unit -> t list

  (** Free does not free the config field (which could be shared with other contexts) *)
  val free : t -> unit

  val status : t -> Types.smt_status
  val reset  : t -> unit
  val push   : t -> unit        (* Open new level and go there *)
  val pop    : t -> unit        (* Go back 1 level *)
  val goto   : t -> int -> unit (* Go to level *)
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> term -> unit
  val assert_formulas : t -> term list -> unit
  val assert_blocking_clause : t -> unit

  val check : ?param:param -> ?assumptions:term list -> ?smodel:smodel -> t
              -> Types.smt_status
  val stop             : t -> unit
  val get_model        : ?keep_subst:bool -> ?support:term list -> t -> smodel
  val get_unsat_core   : t -> term list
  val get_model_interpolant : t -> term
    
  val check_with_interpolation  :
    ?build_model:bool -> ?param:param -> t -> t
    -> (term, ?support:term list -> unit -> smodel) Types.smt_status_with_answers

  (* The next two functions are just adding declarations to the log,
     they do not actually introduce new Yices types and terms *)
  val declare_type   : t -> string -> unit    
  val declare_fun    : t -> string -> typ -> unit

end

module type Type = sig

  include High_types.Type with type 'a eh := 'a

  type context

  (** Introduce a notation for pretty-printing a type (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  val new_uninterpreted  : ?contexts:context list -> ?name:string -> unit -> t

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp : t -> Sexplib.Type.t

  (** All uninterpreted types; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

end

module type Term = sig
  include High_types.Term with type 'a eh := 'a

  type context
  type termset

  val new_uninterpreted  : ?contexts:context list -> ?name:string -> typ -> t

  (** Introduce a notation for pretty-printing a term (Notation computed lazily) *)
  val notation : t -> ('a, Format.formatter, unit) format -> 'a

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print through SMTLib expressions *)
  val pp : t Format.printer

  val to_sexp_termstruct : _ Types.termstruct -> Sexp.t
  val to_sexp            : t -> Sexp.t

  val is_free_termstruct : var:t -> _ Types.termstruct -> bool
  val is_free            : var:t -> t -> bool
  val fv_termstruct      : _ Types.termstruct -> termset
  val fv                 : t -> termset

  (** For bitvector terms *)
  val width_of_term : t -> int

  (** All uninterpreted terms; raises exception after GC pass *)
  val all_uninterpreted  : unit -> t list

end

module type Model = sig
  include High_types.Model with type 'a eh := 'a

  (** Print with specific height *)
  val pph : int -> t Format.printer

  (** Print with height 1000 *)
  val pp : t Format.printer
end

(* Supported models *)
module type SModel = sig

  type term
  type model
     
  type t = { support : term list;
             model   : model }
  val make : ?support:term list -> model -> t
  val pp :
    ?pp_start:unit Format.printer ->
    ?pp_stop:unit Format.printer ->
    ?pp_sep:unit Format.printer -> unit -> t Format.printer

  val from_map  : (term * term) list -> t

  val as_map  : t -> (term * term) list

  (* turns every assignment (x |-> v) into (x === v);
     unless x is Boolean in which case it produces x or (not x) depending on v;
     crashes if v cannot be expressed as a term, like algebraic numbers or functional values *)
  val as_assumptions : t -> term list

  (* converts assumptions into supported model,
     turning every assumption f into a Boolean assignment (x |-> true),
     for a Boolean uninterpreted term x purifying f, i.e. with constraint (x <==> f).
     It returns a triple (smodel, pures, constraints) where
     smodel is the constructed supported model,
     pures are the list of purification variables
     constraints are their definition constraints *)
  val from_assumptions : mcsat:bool -> ?smodel:t -> term list -> t * term list * term list 
end
