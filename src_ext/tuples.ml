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

(* Reveal a single leaf term's value using the given query function. *)
let reveal_leaf query t = query t

(* Helper: build a tuple model_value from a list of children. *)
let mk_tuple children =
  let n = List.length children in
  ModelValue.build (`Tuple (n, children))

(* Helper: compare model_value arg vectors by their term representations. *)
let mv_args_equal args1 args2 =
  try List.for_all2 (fun a b ->
    match ModelValue.val_as_term a, ModelValue.val_as_term b with
    | Some t1, Some t2 -> Term.equal t1 t2
    | _ -> false
  ) args1 args2
  with Invalid_argument _ -> false

(* Helper: look up an arg vector in a mapping list. *)
let find_in_mappings mappings args =
  List.find_opt (fun { args = a; _ } -> mv_args_equal a args) mappings

(* Helper: given a codomain TopTuple.t structure and a flat list of leaf values,
   assemble a nested tuple model_value following the structure.
   Returns the assembled value and the remaining unused values. *)
let rec assemble_codom structure values =
  match structure with
  | TopTuple.Single _ ->
    (match values with
     | v :: rest -> v, rest
     | [] -> failwith "assemble_codom: not enough values")
  | TopTuple.Multiple l ->
    let children, rest = List.fold_left (fun (acc, vs) s ->
      let child, vs' = assemble_codom s vs in
      (child :: acc, vs')
    ) ([], values) l in
    mk_tuple (List.rev children), rest

(* Collect all unique arg vectors from leaf functions' mapping tables. *)
let collect_unique_args leaf_fvs =
  let all_args = List.concat_map
    (fun fv -> List.map (fun m -> m.args) fv.mappings) leaf_fvs in
  let rec dedup acc = function
    | [] -> List.rev acc
    | args :: rest ->
      if List.exists (mv_args_equal args) acc then dedup acc rest
      else dedup (args :: acc) rest
  in
  dedup [] all_args

(* Merge leaf function fun_vals into a single fun_val whose codomain is a tuple.
   codom_structure is the type_blast of the original codomain (a TopTuple.t of types).
   leaf_fvs are the revealed fun_vals of the component functions. *)
let merge_fun_vals codom_structure leaf_fvs orig_type =
  let unique_args = collect_unique_args leaf_fvs in
  let merged_mappings = List.map (fun args ->
    let values = List.map (fun fv ->
      match find_in_mappings fv.mappings args with
      | Some m -> m.value
      | None -> fv.default
    ) leaf_fvs in
    let value, _ = assemble_codom codom_structure values in
    { args; value }
  ) unique_args in
  let default_values = List.map (fun fv -> fv.default) leaf_fvs in
  let default, _ = assemble_codom codom_structure default_values in
  let arity = match leaf_fvs with fv :: _ -> fv.arity | [] -> 0 in
  { mappings = merged_mappings; default; typ = orig_type; arity }

(* Evaluate blasted leaves in the base model, reassemble as a model_value.
   query is a (term -> model_value) function for evaluating leaf terms.
   orig_type is the type of the original (pre-blast) term.
   For Single: query the leaf directly.
   For Multiple with Tuple type: recursively build children.
   For Multiple with Fun type: reveal leaf functions and merge mapping tables. *)
let rec build_model_value query orig_type = function
  | TopTuple.Single t -> reveal_leaf query t
  | TopTuple.Multiple l ->
    let open Types in
    match Type.reveal orig_type with
    | Tuple component_types ->
      let children = List.map2 (build_model_value query) component_types l in
      mk_tuple children
    | Fun { codom; _ } ->
      (* Codomain was split into components; each leaf in l is a function. *)
      let codom_structure = type_blast codom in
      (* Flatten the nested Multiple to get all leaf functions *)
      let leaf_terms = TopTuple.flatten (TopTuple.Multiple l) in
      let leaf_mvs = List.map (reveal_leaf query) leaf_terms in
      let force_mapping { args; value } =
        { args = List.map Lazy.force args;
          value = Lazy.force value }
      in
      let force_fun_val { mappings; default; typ; arity } =
        { mappings = List.map force_mapping mappings;
          default = Lazy.force default;
          typ; arity }
      in
      (* Extract fun_vals from the revealed leaf functions *)
      let leaf_fvs = List.map (fun mv ->
        match ModelValue.reveal mv with
        | `Fun fv -> force_fun_val fv
        | _ -> failwith "build_model_value: expected function leaf"
      ) leaf_mvs in
      (* Merge into a single fun_val with tuple codomain *)
      let merged_fv = merge_fun_vals codom_structure leaf_fvs orig_type in
      ModelValue.build (`Fun merged_fv)
    | _ -> failwith "build_model_value: unexpected Multiple for non-tuple/function type"

(* Build a get_value transformer that tuple-blasts terms before querying
   the C model, then reassembles tuple/function values from the leaves. *)
let make_transform st =
  fun (base : Term.t -> ModelValue.t) ->
    let rec transformed t =
      match tuple_blast st t with
      | TopTuple.Single t' -> base t'
      | blasted -> build_model_value transformed (Term.type_of_term t) blasted
    in
    transformed

(* Compute a default support list from the base model's defined terms,
   reverse-mapping blasted leaves back to their original terms. *)
let default_support st smodel =
  let leaf_to_orig : Term.t HTerms.t = HTerms.create 100 in
  HTerms.iter (fun orig blasted ->
    List.iter (fun leaf ->
      if not (Term.equal leaf orig) then
        HTerms.replace leaf_to_orig leaf orig
    ) (TopTuple.flatten blasted)
  ) st.htbl;
  let seen : unit HTerms.t = HTerms.create 100 in
  SModel.support smodel
  |> List.filter_map (fun t ->
    let orig = HTerms.find_opt leaf_to_orig t |> Option.get_or ~default:t in
    if HTerms.mem seen orig then None
    else begin HTerms.replace seen orig (); Some orig end)

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

  type t = state

  let malloc ?config () = config, malloc_state ()
  let reset st = reset_state st
  let push _ = ()
  let pop _ = ()
  let goto _ _ = ()

  let translate_assertion (_ctx : old_context) st f = [blast_formula st f]
  let translate_assumption (_ctx : old_context) st f = blast_formula st f

  let check _ smodel = Sat smodel

  let term_of_old _ t = t
  let typ_of_old _ ty = ty
  let param_to_old _ p = p
  let smodel_to_old _ m = m
  let smodel_of_old _ m = m
  let enrich_smodel st ?support smodel =
    let support = match support with
      | Some s -> s
      | None -> default_support st smodel
    in
    let transform = make_transform st in
    SModel.with_transform transform (SModel.with_support support smodel)

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
