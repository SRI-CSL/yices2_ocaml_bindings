(** Shared utilities and small monad helpers used across the bindings. *)
open Containers
   
module HStrings : CCHashtbl.S with type key = String.t

module List : module type of List with type 'a t = 'a list

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MList(M : Monad) : sig
  val fold : ('a -> 'b -> 'a M.t) -> 'a M.t -> 'b list -> 'a M.t
  val map : ('a -> 'b M.t) -> 'a list -> 'b list M.t
end

module type StateMonad = sig
  type state
  include Monad with type 'a t = state -> 'a * state
end

module StateMonad(State : sig type t end) : StateMonad with type state := State.t
