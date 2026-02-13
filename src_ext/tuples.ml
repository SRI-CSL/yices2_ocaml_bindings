open Containers

open Yices2
open Ext
open Types
open Types_ext

open Ext.WithExceptionsErrorHandling
open Builder

module TopTuple = struct

  exception Map2Fail

  type 'a t =
    | Single of 'a
    | Multiple of 'a t list

  let return a = Single a

  let rec map f = function
    | Single a   -> Single (f a)
    | Multiple a -> Multiple (List.map (map f) a)

  let rec map2 f a b =
    match a, b with
    | Single a, Single b -> Single (f a b)
    | Multiple a, Multiple b -> Multiple (List.map2 (map2 f) a b)
    | _ -> raise Map2Fail

  let bind al f =
    match al with
    | Single a -> f a
    | Multiple _ -> failwith "Only supposed to produce a single output"

  let rec flatten_rev accu = function
    | Single a -> a :: accu
    | Multiple l -> List.fold_left flatten_rev accu l

  let flatten a = flatten_rev [] a |> List.rev

  let map_flatten f x =
    List.fold_left (fun sofar ty -> f ty |> flatten_rev sofar) [] x |> List.rev
end

type state = {
  htbl : Term.t TopTuple.t HTerms.t;
}

let malloc_state () = { htbl = Global.hTerms_create 100 }

let reset_state st = HTerms.reset st.htbl

let rec type_blast t =
  let open Types in
  match Type.reveal t with
  | Bool | Int | Real | BV _ | Scalar _ | Uninterpreted _ -> TopTuple.Single t
  | Tuple l -> Multiple (List.map type_blast l)
  | Fun { dom; codom } ->
     let dom = TopTuple.map_flatten type_blast dom in
     let codom = type_blast codom in
     let aux codom = Type.build (Fun { dom; codom }) in
     TopTuple.map aux codom

let rec type_check t =
  let open Types in
  match Type.reveal t with
  | Bool | Int | Real | BV _ | Scalar _ | Uninterpreted _ -> true
  | Tuple _ -> false
  | Fun { dom; codom } -> type_check codom && List.for_all type_check dom

module Blast = Yices2.High.MTerm (TopTuple)

let atom_blast f t =
  match Term.type_of_term t |> type_blast with
  | Multiple l -> TopTuple.Multiple (List.map (TopTuple.map f) l)
  | Single ty ->
     match Type.reveal ty with
     | Types.Fun _ -> TopTuple.Single (f ty)
     | _ -> TopTuple.Single t

let tuple_blast st t =
  let rec tuple_blast t =
    let open Types in
    let Term t_struct = Term.reveal t in
    match t_struct with
    | A0 (`YICES_VARIABLE, t) ->
       HTerms.get_or_add st.htbl ~f:(atom_blast Term.new_variable) ~k:t
    | A0 (`YICES_UNINTERPRETED_TERM, t) ->
       HTerms.get_or_add st.htbl ~f:(atom_blast Term.new_uninterpreted) ~k:t
    | A2 (`YICES_EQ_TERM, t1, t2) ->
       TopTuple.Single
         (TopTuple.map2 Term.eq (tuple_blast t1) (tuple_blast t2)
          |> TopTuple.flatten
          |> Term.andN)
    | ITE (c, tb, eb) ->
       TopTuple.bind (tuple_blast c) (fun c ->
           TopTuple.map2 (Term.ite c) (tuple_blast tb) (tuple_blast eb))
    | Astar (`YICES_TUPLE_TERM, l) -> TopTuple.Multiple (List.map tuple_blast l)
    | Astar (`YICES_DISTINCT_TERM, l) ->
       let distinct a accu b =
         (TopTuple.map2 Term.neq a b |> TopTuple.flatten |> Term.orN) :: accu
       in
       let rec aux accu = function
         | [] -> Term.andN accu
         | hd :: tail -> aux (List.fold_left (distinct hd) accu tail) tail
       in
       TopTuple.Single (aux [] (List.map tuple_blast l))
    | Bindings { c; vars; body } ->
       let vars = TopTuple.map_flatten tuple_blast vars in
       begin
         match c, tuple_blast body with
         | `YICES_FORALL_TERM, Single body -> Single (Term.forall vars body)
         | `YICES_LAMBDA_TERM, Single body -> Single (Term.lambda vars body)
         | _ -> failwith "tuple_blast quantifiers"
       end
    | App (f, l) ->
       let l = TopTuple.map_flatten tuple_blast l in
       let f = tuple_blast f in
       TopTuple.map (fun f -> Term.application f l) f
    | Update { array; index; value } ->
       let array = tuple_blast array in
       let value = tuple_blast value in
       let index = TopTuple.map_flatten tuple_blast index in
       TopTuple.map2 (fun array value -> Term.update array index value) array value
    | Projection (`YICES_SELECT_TERM, i, t) ->
       begin
         match tuple_blast t with
         | Multiple l -> List.nth l (i - 1)
         | Single _ -> failwith "Expected a tuple for tuple.select"
       end
    | _ -> Blast.map tuple_blast t_struct |> TopTuple.map Term.build
  in
  tuple_blast t

let blast_formula st f =
  match tuple_blast st f with
  | TopTuple.Single f -> f
  | TopTuple.Multiple _ -> failwith "Tuple-blasting a formula should give a single formula"

let blast_flat st t = tuple_blast st t |> TopTuple.flatten

let rec term_has_tuple t =
  let has_tuple_type t = not (type_check (Term.type_of_term t)) in
  if has_tuple_type t then true
  else
    let Types.Term ts = Term.reveal t in
    match ts with
    | Astar (`YICES_TUPLE_TERM, _) -> true
    | Projection (`YICES_SELECT_TERM, _, _) -> true
    | A0 _ -> false
    | A1 (_, a) -> term_has_tuple a
    | A2 (_, a, b) -> term_has_tuple a || term_has_tuple b
    | ITE (c, tb, eb) -> term_has_tuple c || term_has_tuple tb || term_has_tuple eb
    | Astar (_, l) -> List.exists term_has_tuple l
    | Bindings { vars; body; _ } -> List.exists term_has_tuple vars || term_has_tuple body
    | App (f, l) -> term_has_tuple f || List.exists term_has_tuple l
    | Update { array; index; value } ->
       term_has_tuple array
       || List.exists term_has_tuple index
       || term_has_tuple value
    | Projection (`YICES_BIT_TERM, _, a) -> term_has_tuple a
    | BV_Sum l ->
       List.exists (fun (_coeff, t_opt) -> Option.exists term_has_tuple t_opt) l
    | FF_Sum l ->
       List.exists (fun (_coeff, t_opt) -> Option.exists term_has_tuple t_opt) l
    | Sum l ->
       List.exists (fun (_coeff, t_opt) -> Option.exists term_has_tuple t_opt) l
    | Product (_is_bv, l) ->
       List.exists (fun (t, _power) -> term_has_tuple t) l

module ExtAlways = struct
  type term = Term.t
  type typ = Type.t
  type old_context = Builder.Context.t
  type config = Config.t
  type param = Param.t
  type smodel = SModel.t
  type model = Model.t

  type t = state

  let malloc ?config () = config, malloc_state ()
  let reset st = reset_state st
  let push _ = ()
  let pop _ = ()
  let goto _ _ = ()

  let translate_assertion (_ctx : old_context) st f = [blast_formula st f]
  let translate_assumption (_ctx : old_context) st f = blast_formula st f

  let check _ (Types.SModel { model; _ }) = Sat model

  let term_of_old _ t = t
  let typ_of_old _ ty = ty
  let param_to_old _ p = p
  let smodel_to_old _ m = m
  let smodel_of_old _ m = m
  let smodel_of_model _ ?support model = SModel.make ?support model

  let interpolant _ old_interpolant = old_interpolant

  let pp_term = Term.pp
  let pp_type = Type.pp
  let term_to_sexp ?smt2arrays t = Term.to_sexp ?smt2arrays t
  let type_to_sexp ?smt2arrays t = Type.to_sexp ?smt2arrays t
  let smodel_to_sexp ?smt2arrays smodel =
    let bindings =
      SModel.as_map smodel
      |> List.map (fun (lhs, rhs) ->
             let lhs = Term.to_sexp ?smt2arrays lhs in
             let rhs = Term.to_sexp ?smt2arrays rhs in
             Sexplib.Sexp.List [Sexplib.Sexp.Atom ":="; lhs; rhs])
    in
    Sexplib.Sexp.List (Sexplib.Sexp.Atom "model" :: bindings)
end

module ExtOnlyMCSAT = struct
  include ExtAlways

  let translate_assertion (ctx : old_context) st f =
    if Builder.Context.is_mcsat ctx && term_has_tuple f then [blast_formula st f] else [f]

  let translate_assumption (ctx : old_context) st f =
    if Builder.Context.is_mcsat ctx && term_has_tuple f then blast_formula st f else f
end

module ContextAlways = Make (Builder.Context) (ExtAlways)

module ContextOnlyMCSAT = Make (Builder.Context) (ExtOnlyMCSAT)

module Always = struct
  include WithExceptionsErrorHandling
  module Context = ContextAlways
end

module OnlyMCSAT = struct
  include WithExceptionsErrorHandling
  module Context = ContextOnlyMCSAT
end
