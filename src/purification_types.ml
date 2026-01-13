(** Types for the purification subsystem used by the Ext layer. *)
open Common
   
type 'a purified = { proxy : 'a; body : 'a }
                 
module type PurificationMonad = sig
  type accu
  include StateMonad with type state := accu purified list
end

module type API = sig

  type typ
  type term
  
  module Type : sig
    (* Get type as an uninterpreted type.
       If already uninterpreted, this is the identity.
       Otherwise, create a fresh uninterpreted type on demand:
       Several calls on the same argument give the same output.
       Extra boolean indicates whether a fresh uninterpreted type has been created.
     *)
    val get_var  : typ -> typ * bool
    val get_body : typ -> typ
    module Accu : PurificationMonad with type accu := typ
    (* Traverses the type and purify sub-types that do not satisfy the predicate;
       It does not traverse the body of the abstracted sub-types. *)
    val purify : (typ -> bool) -> typ -> typ Accu.t
  end

  module Term : sig
    (* Get term as an uninterpreted term.
       If already uninterpreted, this is the identity.
       Otherwise, create a fresh uninterpreted term on demand:
       Several calls on the same argument give the same output.
       Extra boolean indicates whether a fresh uninterpreted type has been created.
     *)
    val get_var  : ?get_typ:(typ -> typ) -> term -> term * bool
    val get_body : term -> term
    module Accu : PurificationMonad with type accu := term
    (* Traverses the term and purify sub-term that do not satisfy the predicate;
       It does not traverse the body of the abstracted sub-terms. *)
    val purify : (term -> bool) -> term -> term Accu.t
  end
end
