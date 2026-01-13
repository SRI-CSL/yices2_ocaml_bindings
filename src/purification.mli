(** Term purification helpers used by the Ext layer. *)
open Purification_types

(** Purification monad specialized by an accumulator type. *)
module PurificationMonad(Accu : sig type t end) : PurificationMonad with type accu := Accu.t

(** Purification helpers over the high-level API. *)
module Make
         (EH: High.ErrorHandling with type 'a t = 'a)
         (H : High.API with type 'a eh := 'a EH.t) : API with type typ  := H.Type.t
                                                          and type term := H.Term.t
