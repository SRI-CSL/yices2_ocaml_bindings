open Yices2.Ext
open WithExceptionsErrorHandling
open Types_ext

(**********************************************************)
(* Diff symbol for extensionality in the theory of arrays *)
(**********************************************************)

module AddDiff : sig

  include StandardExt

  module ExtraType : sig
    val diff : Types.type_t -> Term.t list
  end

  module ExtraTerm : sig
    val diff : Term.t -> Term.t -> Term.t list
    val reveal : 'a Types.termstruct -> (Term.t * Term.t) option
  end

end

include StandardYicesContext
