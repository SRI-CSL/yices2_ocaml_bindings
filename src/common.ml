open Containers

module HStrings = CCHashtbl.Make(String)

module List = struct
  include List
  let map f l = rev(rev_map f l) (* Tail-recursive version of map to avoid stack overflows *)
  (* let map f l = List.rev (List.fold_left (fun sofar a -> f a::sofar) [] l) *)
end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Monadic fold and map on lists *)
module MList(M : Monad) = struct

  open M
  let (let++) = bind
              
  let fold  (aux : 'a -> 'b -> 'a M.t) =
    let aux sofar c =
      let++ sofar = sofar in
      aux sofar c
    in
    List.fold_left aux

  let map f l =
    let aux sofar c =
      let++ c = f c in
      return(c::sofar)
    in
    List.rev l |> fold aux (return [])
end

module type StateMonad = sig
  type state
  include Monad with type 'a t = state -> 'a * state
end

module StateMonad(State : sig type t end) : StateMonad with type state := State.t = struct
  type 'a t = State.t -> 'a * State.t
  let return a accu = a, accu
  let bind a f accu = let a, accu = a accu in f a accu
end

