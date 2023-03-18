open Containers

open Yices2
open Ext
open Builder
open Types_ext

module Arg = struct

  type term   = Term.t 
  type config = Config.t
  type model  = Model.t

  let config_set = Config.set

  include Trivial

  module TopTuple = struct

    exception Map2Fail
    
    type 'a t =
      | Single of 'a
      | Multiple of 'a t list

    let return a = Single a

    let rec map f = function
      | Single a   -> Single(f a)
      | Multiple a -> Multiple(List.map (map f) a)

    let rec map2 f a b =
      match a, b with
      | Single a, Single b -> Single(f a b)
      | Multiple a, Multiple b -> Multiple(List.map2 (map2 f) a b)
      | _ -> raise Map2Fail
        
    let bind al f =
      match al with
      | Single a -> f a
      | Multiple _ -> failwith "Only supposed t produce a single output"

    let rec flatten accu = function
      | Single a -> a::accu
      | Multiple l -> List.fold_left flatten accu l

    let map_flatten f x =
      List.fold_left (fun sofar ty -> f ty |> flatten sofar) [] x

  end

  let rec type_blast t =
    let open Types in
    match Type.reveal t with
    | Bool
      | Int
      | Real
      | BV _ 
      | Scalar _
      | Uninterpreted _  -> TopTuple.Single t
    | Tuple l ->
       Multiple(List.map type_blast l)
    | Fun { dom; codom } ->
       let dom = TopTuple.map_flatten type_blast dom in
       let codom = type_blast codom in
       let aux codom = Type.build (Fun { dom; codom }) in
       TopTuple.map aux codom

  module Blast = Yices2.High.MTerm(TopTuple)

  let htbl = Global.hTerms_create 100
  let atom_blast f t =
    match Term.type_of_term t |> type_blast with
    | Single _   -> TopTuple.Single t
    | Multiple l -> TopTuple.Multiple(List.map (TopTuple.map f) l)
  
  let rec tuple_blast t =
    let open Types in
    let Term t = Term.reveal t in
    match t with
    | A0(`YICES_VARIABLE,t) ->
       HTerms.get_or_add htbl ~f:(atom_blast Term.new_variable) ~k:t
    | A0(`YICES_UNINTERPRETED_TERM,t) ->
       HTerms.get_or_add htbl ~f:(atom_blast Term.new_uninterpreted) ~k:t
    | A2(`YICES_EQ_TERM,t1,t2) ->
       TopTuple.Single(TopTuple.map2 Term.eq (tuple_blast t1) (tuple_blast t2)
                       |> TopTuple.flatten []
                       |> Term.andN)         
    | ITE(c, tb, eb) ->
       TopTuple.map2 (Term.ite c) (tuple_blast tb) (tuple_blast eb)
    | Astar(`YICES_TUPLE_TERM,l) ->
       TopTuple.Multiple (List.map tuple_blast l)
    | Astar(`YICES_DISTINCT_TERM,l) ->
       let distinct a accu b =
         TopTuple.(map2 Term.neq a b |> flatten [] |> Term.orN) :: accu
       in
       let rec aux accu = function
         | [] -> Term.andN accu
         | hd::tail -> aux (List.fold_left (distinct hd) accu tail) tail 
       in
       Single(aux [] (List.map tuple_blast l))
    | Bindings{c;vars;body} ->
       let vars = TopTuple.map_flatten tuple_blast vars in
       begin
         match c, tuple_blast body with
         | `YICES_FORALL_TERM, Single body -> Single(Term.forall vars body)
         | `YICES_LAMBDA_TERM, Single body -> Single(Term.lambda vars body)
         | _ -> failwith "tuple_blast quantifiers"
       end
    | App(f,l) ->
       let l = TopTuple.map_flatten tuple_blast l in
       let f = tuple_blast f in
       TopTuple.map (fun f -> Term.application f l) f
    | Update { array; index; value} ->
       let array = tuple_blast array in
       let value = tuple_blast value in
       let index = TopTuple.map_flatten tuple_blast index in
       TopTuple.map2 (fun array value -> Term.update array index value) array value
    | Projection(`YICES_SELECT_TERM,i,t) ->
       begin
         match tuple_blast t with
         | Multiple l -> List.nth l (i-1)
         | Single _ -> failwith "We should find a Multiple list"
       end
    | _ -> Blast.map tuple_blast t |> TopTuple.map Term.build   

  
  (* Given how the solver you're extending asserts formulas,
     how do you want to assert a formula? *)
  (* val assert_formula : (old_term -> unit) -> t -> term -> unit *)

  let assert_formula old_assert () f =
    match tuple_blast f with
    | Single f ->
       Format.(fprintf stdout "%a" Term.pp f);
       old_assert f
    | Multiple _ -> failwith "Tuple-blasting a formula should give formula."

  (* Whenever the solver you're extending produces a model,
     if you are happy with is, please convert it to your own notion of model;
     if you are unhappy with it,
     please explain why by giving the solver you're extending a model interpolant. *)
  (* val check : t -> old_model -> (model, old_term) answer *)

  let check _ old_model = Sat old_model

  (* Whenever the solver you're extending returns UNSAT, with old_term interpolant,
     you should convert that interpolant into a term interpolant. *)
  (* val interpolant : t -> old_term -> term *)

  let interpolant _t old_interpolant = old_interpolant

end

module Context = Make(Context)(Arg)
