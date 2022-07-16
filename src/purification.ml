open Containers

open High
open Types

module HighAPI = Make(ExceptionsErrorHandling)

open HighAPI

module type StateMonad = sig
  type state
  include Monad with type 'a t = state -> 'a * state
end

module StateMonad(State : sig type t end) : StateMonad with type state := State.t = struct
  type 'a t = State.t -> 'a * State.t
  let return a accu = a, accu
  let bind a f accu = let a, accu = a accu in f a accu
end

type 'a purified = { proxy : 'a; body : 'a }
                  
module type PurificationMonad = sig
  type accu
  include StateMonad with type state := accu purified list
end

module PurificationMonad(Accu : sig type t end)
       : PurificationMonad with type accu := Accu.t =
  StateMonad(struct type t = Accu.t purified list end)
  

module Type = struct

  let to_body = Global.hTypes_create 10
  let to_var  = Global.hTypes_create 10

  let get_body k = HTypes.get_or to_body ~default:k k

  let get_var k =
    match Type.reveal k with
    | Types.Uninterpreted _ -> k, false
    | _ ->
       if HTypes.mem to_var k then HTypes.find to_var k, false
       else
         let name = Format.sprintf "tpure_%i" (Type.hash k) in
         let var = Type.new_uninterpreted ~name () in
         HTypes.replace to_body var k;
         HTypes.replace to_var k var;
         var, true

  module Accu = PurificationMonad(Type)

  let rec purify filter ty =
    let open MType(Accu) in
    let (let+) = Accu.bind in
    if filter ty then
      let+ tstruct = Type.reveal ty |> map (purify filter) in
      Accu.return(Type.build tstruct)
    else
      fun accu ->
      let proxy, fresh = get_var ty in
      proxy, if fresh then { proxy; body = ty }::accu else accu
end

module Term = struct
                
  let to_body = Global.hTerms_create 1000
  let to_var  = Global.hTerms_create 1000

  let get_body k = HTerms.get_or to_body ~default:k k

  let get_var ?(get_typ=(fun x -> x)) k =
    let typ    = Term.type_of_term k in
    let newtyp = get_typ typ in
    let pure ?name () =
      let name =
        match name with
        | None      -> Format.sprintf "pure_%i" (Term.hash k)
        | Some name -> Format.sprintf "pure_%s" name
      in
      let var  = Term.new_uninterpreted ~name newtyp in
      HTerms.replace to_body var k;
      HTerms.add to_var k var;
      var, true
    in
    match Term.constructor k with
    | `YICES_VARIABLE ->
       ExceptionsErrorHandling.raise_bindings_error
         "Cannot purify possibly bound variable %s" (PP.term_string k)
    | `YICES_UNINTERPRETED_TERM ->
       if HighAPI.Type.equal typ newtyp then k, false
       else
         if HTerms.mem to_var k then HTerms.find to_var k, false
         else pure ~name:(PP.term_string k) ()
    | _ ->
       if HTerms.mem to_var k then HTerms.find to_var k, false
       else pure ()
    
  module Accu = PurificationMonad(Term)

  let rec purify filter t =
    let open MTerm(Accu) in
    let (let+) = Accu.bind in
    if filter t then
      let Term x = Term.reveal t in
      let+ tstruct = map (purify filter) x in
      Accu.return(Term.build tstruct)
    else
      fun accu ->
      let proxy, fresh = get_var t in
      proxy, if fresh then { proxy; body = t }::accu else accu

end
