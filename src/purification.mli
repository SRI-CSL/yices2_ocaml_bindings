open High
   
module HighAPI : High.API with type 'a eh := 'a

open HighAPI

module type StateMonad = sig
  type state
  include Monad with type 'a t = state -> 'a * state
end

module StateMonad(State : sig type t end) : StateMonad with type state := State.t

type 'a purified = { proxy : 'a; body : 'a }
                 
module type PurificationMonad = sig
  type accu
  include StateMonad with type state := accu purified list
end

module PurificationMonad(Accu : sig type t end) : PurificationMonad with type accu := Accu.t

module Type : sig
  (* Get type as an uninterpreted type.
     If already uninterpreted, this is the identity.
     Otherwise, create a fresh uninterpreted type on demand:
     Several calls on the same argument give the same output.
     Extra boolean indicates whether a fresh uninterpreted type has been created.
   *)
  val get_var  : Type.t -> Type.t * bool
  val get_body : Type.t -> Type.t
  module Accu : PurificationMonad with type accu := Type.t
  (* Traverses the type and purify sub-types that do not satisfy the predicate;
     It does not traverse the body of the abstracted sub-types. *)
  val purify : (Type.t -> bool) -> Type.t -> Type.t Accu.t
end

module Term : sig
  (* Get term as an uninterpreted term.
     If already uninterpreted, this is the identity.
     Otherwise, create a fresh uninterpreted term on demand:
     Several calls on the same argument give the same output.
     Extra boolean indicates whether a fresh uninterpreted type has been created.
   *)
  val get_var  : ?get_typ:(HighAPI.Type.t -> HighAPI.Type.t) -> Term.t -> Term.t * bool
  val get_body : Term.t -> Term.t
  module Accu : PurificationMonad with type accu := Term.t
  (* Traverses the term and purify sub-term that do not satisfy the predicate;
     It does not traverse the body of the abstracted sub-terms. *)
  val purify : (Term.t -> bool) -> Term.t -> Term.t Accu.t
end
