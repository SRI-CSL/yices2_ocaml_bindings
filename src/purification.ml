open Containers

open High
open Types

module HighAPI = Make(ExceptionsErrorHandling)

open HighAPI

module HTypes = CCHashtbl.Make(Type)
module HTerms = CCHashtbl.Make(Term)

module Type = struct

  module HTypes = CCHashtbl.Make(Type)
  let to_body = HTypes.create 10
  let to_var  = HTypes.create 10

  let reset () =
    HTypes.reset to_body; 
    HTypes.reset to_var

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
      let r, fresh = get_var ty in
      r, if fresh then (r, ty)::accu else accu
end

module Term = struct
                
  let to_body = HTerms.create 1000
  let to_var  = HTerms.create 1000

  let reset () =
    HTerms.reset to_body; 
    HTerms.reset to_var

  let get_body k = HTerms.get_or to_body ~default:k k

  let get_var k =
    match Term.constructor k with
    | `YICES_VARIABLE ->
       ExceptionsErrorHandling.raise_bindings_error
         "Cannot purify possibly bound variable %s" (PP.term_string k)
    | `YICES_UNINTERPRETED_TERM -> k, false
    | _ ->
       if HTerms.mem to_var k then HTerms.find to_var k, false
       else
         let typ  = Term.type_of_term k in
         let name = Format.sprintf "pure_%i" (Term.hash k) in
         let var  = Term.new_uninterpreted ~name typ in
         HTerms.replace to_body var k;
         HTerms.add to_var k var;
         var, true
    
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
      let r, fresh = get_var t in
      r, if fresh then (r, t)::accu else accu

end

let reset() = Type.reset(); Term.reset()

