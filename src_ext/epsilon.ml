open Containers

open Yices2
open Ext
open Builder
open Types_ext
   
module NewTerms = struct
  type _ t    = Epsilon : Type.t t
  let compare (type a b) (Epsilon : a t) (Epsilon : b t) = (Dmap.Eq : (a,b) Dmap.cmp)
  let index (type a) (Epsilon : a t) =
    (module struct
       include Type
       let name = "ε"
       let get_type typ = Type.[func [typ] (bool())], typ
     end : TermIndex with type t = a)
end

module Arg = struct

  type term   = Term.t 
  type config = Config.t
  type model  = Model.t

  let config_set = Config.set

  include Trivial

  include SyntaxExtensions.DeclareTerms(NewTerms)

  let reveal t =
    match reveal t with
    | Some(Reveal(Epsilon,typ),[pred]) ->
       begin
         let open Types in
         match Term.reveal pred with
         | Term Bindings{ c = `YICES_LAMBDA_TERM; vars = [var]; body }
           -> (Some(var, typ, body) : (Term.t * Type.t * Term.t) option)
         | _ -> failwith "Not epsilon term!"
       end
    | _ -> None

  let assert_formula old_assert () f =
    let is_epsilon t = match reveal t with Some _ -> true | None -> false in
    let f, l = Purification.Term.purify is_epsilon f [] in
    let generate Purification.{ proxy; body } =
      match reveal body with
      | Some(var, _, body) ->
         Term.subst_term [var, proxy] body |> old_assert
      | None -> failwith "Should be epsilon term"
    in
    List.iter generate l;
    old_assert f

  let check _ old_model = Sat old_model

  let interpolant _t old_interpolant = old_interpolant

end

module Term = struct
  include Term

  let epsilon var formula =
    let typ     = Term.type_of_term var in
    let vvar    = Term.new_variable typ in
    let formula = Term.(lambda [var] (subst_term [var, vvar] formula)) in
    Arg.(build (Reveal(Epsilon, typ)) [formula])

  let epsilon_reveal = Arg.reveal
end

module Model = struct
  include Model

  let yval_as_term m = function
    | `Algebraic algebraic ->
       let var = Type.real() |> Term.new_variable in
       let aux (powered, sofar) coeff =
         let monomial = coeff, powered in
         let powered = Term.Arith.(var ** powered) in
         (powered, monomial::sofar)
       in
       let zero = Term.Arith.zero() in
       let one  = Term.Arith.int 1 in
       let open High.Types in
       let _,poly = List.fold_left aux (one,[]) algebraic.coeffs in
       let poly_is0 = Term.(Arith.poly_mpz poly |> eq zero) in
       let lb = Term.Arith.mpq algebraic.a in
       let ub = Term.Arith.mpq algebraic.b in
       let lb = Term.Arith.(if algebraic.a_open then lt else leq) lb var in
       let ub = Term.Arith.(if algebraic.a_open then lt else leq) var ub in
       let predicate_body = Term.andN [poly_is0; lb; ub] in
       Term.epsilon var predicate_body
    | v -> yval_as_term m v
        
  let val_as_term m v = reveal m v |> yval_as_term m

  let get_value_as_term m t = get_value m t |> val_as_term m
     
end

module Context = Make(Context)(Arg)
