module HighAPI : High.API with type 'a eh := 'a

open HighAPI

module HTypes : CCHashtbl.S with type key = Type.t
module HTerms : CCHashtbl.S with type key = Term.t

module Type : sig
  (* Get type as an uninterpreted type.
     If already uninterpreted, this is the identity.
     Otherwise, create a fresh uninterpreted type on demand:
     Several calls on the same argument give the same output.
     Extra boolean indicates whether a fresh uninterpreted type has been created.
   *)
  val get_var  : Type.t -> Type.t * bool
  val get_body : Type.t -> Type.t
  val reset : unit -> unit
  type accu := (Type.t * Type.t) list
  (* Traverses the type and purify sub-types that do not satisfy the predicate;
     It does not traverse the body of the abstracted sub-types. *)
  val purify : (Type.t -> bool) -> Type.t -> accu -> Type.t * accu
end

module Term : sig
  (* Get term as an uninterpreted term.
     If already uninterpreted, this is the identity.
     Otherwise, create a fresh uninterpreted term on demand:
     Several calls on the same argument give the same output.
     Extra boolean indicates whether a fresh uninterpreted type has been created.
   *)
  val get_var  : Term.t -> Term.t * bool
  val get_body : Term.t -> Term.t
  val reset : unit -> unit
  type accu := (Term.t * Term.t) list
  (* Traverses the term and purify sub-term that do not satisfy the predicate;
     It does not traverse the body of the abstracted sub-terms. *)
  val purify : (Term.t -> bool) -> Term.t -> accu -> Term.t * accu
end

(* resets both Type and Term *)
val reset : unit -> unit
