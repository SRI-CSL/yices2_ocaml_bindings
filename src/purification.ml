open Containers

open High
open Types
open Ext_bindings

module Type = struct

  module HTypes = CCHashtbl.Make(Type)
  let to_body = HTypes.create 10
  let to_var  = HTypes.create 10

  let reset () =
    HTypes.reset to_body; 
    HTypes.reset to_var

  let get_body = HTypes.find to_body
  let get_var  = HTypes.find to_var

  let var t = Type.new_uninterpreted ~name:(Format.sprintf "tpure_%i" (Type.hash t)) ()
  let add k =
    let f k =
      let var = var k in
      HTypes.replace to_body var k;
      var
    in
    HTypes.get_or_add to_var ~f ~k

  module Accu = struct
    type accu = (Type.t * Type.t) list
    type 'a t = accu -> 'a * accu
    let return a accu = a, accu
    let bind a f accu = let a, accu = a accu in f a accu
  end

  let rec purify filter ty =
    let open MType(Accu) in
    let (let+) = Accu.bind in
    if filter ty then
      let+ tstruct = Type.reveal ty |> map (purify filter) in
      Accu.return(Type.build tstruct)
    else
      fun accu ->
      let r = add ty in
      r, (r, ty)::accu
end

module Term = struct
                
  let to_body = HTerms.create 1000
  let to_var  = HTerms.create 1000

  let reset () =
    HTerms.reset to_body; 
    HTerms.reset to_var

  let get_body = HTerms.find to_body
  let get_var  = HTerms.find to_var

  let var t =
    Term.new_uninterpreted ~name:(Format.sprintf "pure_%i" (Term.hash t)) (Term.type_of_term t)

  let add k =
    let f k =
      let var = var k in
      HTerms.replace to_body var k;
      var
    in
    HTerms.get_or_add to_var ~f ~k
    
  module Accu = struct
    type accu = (Term.t * Term.t) list
    type 'a t = accu -> 'a * accu
    let return a accu = a, accu
    let bind a f accu = let a, accu = a accu in f a accu
  end

  let rec purify filter t =
    let open MTerm(Accu) in
    let (let+) = Accu.bind in
    if filter t then
      let Term x = Term.reveal t in
      let+ tstruct = map (purify filter) x in
      Accu.return(Term.build tstruct)
    else
      fun accu ->
      let r = add t in
      r, (r, t)::accu

end

let reset() = Type.reset(); Term.reset()
