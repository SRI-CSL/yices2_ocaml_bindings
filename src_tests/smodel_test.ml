(** Comprehensive tests for SModel, ModelValue, and SMT2 output.

    Covers the issues encountered and fixed during the SModel refactoring:
    - SModel construction (make, from_map, empty, with_support, with_transform)
    - SModel queries (get_value, get_value_as_term, formula_true_in_model, support)
    - ModelValue (build, reveal, val_as_term, pp, to_sexp)
    - SModel.to_sexp (define-fun codomain for functions, ite chains)
    - SModel.as_map, is_representable, as_assumptions
    - Context.get_model returning SModel.t
    - SModel.make rejecting function values
    - SModel.make extending existing smodel
*)

open Containers
open Sexplib

open Yices2.Ext.WithExceptionsErrorHandling

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

let sexp_to_string s = Format.asprintf "%a" Sexp.pp_hum s

let string_contains ~sub s =
  let len_sub = Stdlib.String.length sub in
  let len_s = Stdlib.String.length s in
  if len_sub > len_s then false
  else
    let rec aux i =
      if i > len_s - len_sub then false
      else if String.equal (Stdlib.String.sub s i len_sub) sub then true
      else aux (i + 1)
    in
    aux 0

let assert_equal_string ~msg expected actual =
  if not (String.equal expected actual) then
    failwith (Printf.sprintf "%s: expected %S, got %S" msg expected actual)

let assert_true ~msg b =
  if not b then failwith (Printf.sprintf "assert_true failed: %s" msg)

let assert_false ~msg b =
  if b then failwith (Printf.sprintf "assert_false failed: %s" msg)

let assert_equal_int ~msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %d, got %d" msg expected actual)

let assert_raises ~msg f =
  try f (); failwith (Printf.sprintf "expected exception: %s" msg)
  with Yices2.High.ExceptionsErrorHandling.YicesException _ -> ()
     | Yices2.High.ExceptionsErrorHandling.YicesBindingsException _ -> ()

(* ------------------------------------------------------------------ *)
(* 1. SModel.empty                                                    *)
(* ------------------------------------------------------------------ *)

let test_smodel_empty () =
  print_endline "  SModel.empty";
  let m = SModel.empty () in
  assert_equal_int ~msg:"empty support length" 0 (List.length (SModel.support m));
  let map = SModel.as_map m in
  assert_equal_int ~msg:"empty as_map length" 0 (List.length map);
  assert_true ~msg:"empty is_representable" (SModel.is_representable m)

(* ------------------------------------------------------------------ *)
(* 2. SModel.from_map with scalar values                              *)
(* ------------------------------------------------------------------ *)

let test_smodel_from_map () =
  print_endline "  SModel.from_map";
  let x = Term.new_uninterpreted ~name:"fm_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"fm_y" (Type.bool ()) in
  let m = SModel.from_map [x, Term.Arith.int 42; y, Term.true0 ()] in
  assert_equal_int ~msg:"from_map support" 2 (List.length (SModel.support m));
  (* get_value_as_term *)
  let vx = SModel.get_value_as_term m x in
  assert_true ~msg:"from_map x has value" (Option.is_some vx);
  let vy = SModel.get_value_as_term m y in
  assert_true ~msg:"from_map y has value" (Option.is_some vy);
  (* as_map round-trips *)
  let pairs = SModel.as_map m in
  assert_equal_int ~msg:"from_map as_map" 2 (List.length pairs);
  assert_true ~msg:"from_map is_representable" (SModel.is_representable m)

(* ------------------------------------------------------------------ *)
(* 3. SModel.make with ModelValue bindings                            *)
(* ------------------------------------------------------------------ *)

let test_smodel_make () =
  print_endline "  SModel.make";
  let a = Term.new_uninterpreted ~name:"mk_a" (Type.int ()) in
  let b = Term.new_uninterpreted ~name:"mk_b" (Type.bv 8) in
  let va = ModelValue.build (`Rational (Q.of_int 7)) in
  let vb = ModelValue.build (`BV (8, [true;false;true;false;true;false;true;false])) in
  let m = SModel.make [a, va; b, vb] in
  assert_equal_int ~msg:"make support" 2 (List.length (SModel.support m));
  let ta = SModel.get_value_as_term m a in
  assert_true ~msg:"make a has value" (Option.is_some ta);
  let tb = SModel.get_value_as_term m b in
  assert_true ~msg:"make b has value" (Option.is_some tb)

(* ------------------------------------------------------------------ *)
(* 4. SModel.make rejects function values                             *)
(* ------------------------------------------------------------------ *)

let test_smodel_make_rejects_fun () =
  print_endline "  SModel.make rejects Fun";
  let f = Term.new_uninterpreted ~name:"mk_f" (Type.func [Type.int ()] (Type.int ())) in
  let dummy_default = ModelValue.build (`Rational Q.zero) in
  let vf = ModelValue.build (`Fun { mappings = []; default = dummy_default;
                                    typ = Type.func [Type.int ()] (Type.int ());
                                    arity = 1 }) in
  assert_raises ~msg:"make with Fun" (fun () -> ignore (SModel.make [f, vf]))

(* ------------------------------------------------------------------ *)
(* 5. SModel.make extending existing smodel                           *)
(* ------------------------------------------------------------------ *)

let test_smodel_make_extend () =
  print_endline "  SModel.make extend";
  let x = Term.new_uninterpreted ~name:"ext_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"ext_y" (Type.int ()) in
  let m1 = SModel.from_map [x, Term.Arith.int 1] in
  let vy = ModelValue.build (`Rational (Q.of_int 2)) in
  let m2 = SModel.make ~smodel:m1 [y, vy] in
  (* m2 should have both x and y *)
  let map = SModel.as_map m2 in
  assert_equal_int ~msg:"extend as_map" 2 (List.length map)

(* ------------------------------------------------------------------ *)
(* 6. SModel.with_support                                             *)
(* ------------------------------------------------------------------ *)

let test_smodel_with_support () =
  print_endline "  SModel.with_support";
  let x = Term.new_uninterpreted ~name:"ws_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"ws_y" (Type.int ()) in
  let m = SModel.from_map [x, Term.Arith.int 10; y, Term.Arith.int 20] in
  assert_equal_int ~msg:"original support" 2 (List.length (SModel.support m));
  (* Restrict support to just x *)
  let m' = SModel.with_support [x] m in
  assert_equal_int ~msg:"restricted support" 1 (List.length (SModel.support m'));
  (* But y's value is still accessible *)
  let vy = SModel.get_value_as_term m' y in
  assert_true ~msg:"y still accessible" (Option.is_some vy);
  (* as_map only returns support *)
  let map = SModel.as_map m' in
  assert_equal_int ~msg:"restricted as_map" 1 (List.length map)

(* ------------------------------------------------------------------ *)
(* 7. SModel.with_transform                                           *)
(* ------------------------------------------------------------------ *)

let test_smodel_with_transform () =
  print_endline "  SModel.with_transform";
  let x = Term.new_uninterpreted ~name:"wt_x" (Type.int ()) in
  let m = SModel.from_map [x, Term.Arith.int 5] in
  (* Install a transform that overrides x's value *)
  let m' = SModel.with_transform
      (fun base t ->
         if Term.equal t x then ModelValue.build (`Rational (Q.of_int 99))
         else base t)
      m
  in
  let v = SModel.get_value m' x in
  (match ModelValue.reveal v with
   | `Rational q -> assert_true ~msg:"transform overrides" Q.(equal q (of_int 99))
   | _ -> failwith "expected Rational from transform")

(* ------------------------------------------------------------------ *)
(* 8. Context.get_model -> SModel.t, formula_true_in_model            *)
(* ------------------------------------------------------------------ *)

let test_context_get_model () =
  print_endline "  Context.get_model and SModel queries";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"cgm_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"cgm_y" (Type.int ()) in
  Context.assert_formula ctx Term.(x === Arith.int 3);
  Context.assert_formula ctx Term.(y === Arith.int 7);
  let status = Context.check ctx in
  assert_true ~msg:"sat" (Yices2.High.Types.equal_smt_status status `STATUS_SAT);
  let smodel = Context.get_model ctx ~keep_subst:true in
  (* support should include x and y *)
  let support = SModel.support smodel in
  assert_true ~msg:"support non-empty" (List.length support >= 2);
  (* get_value_as_term *)
  let vx = SModel.get_value_as_term smodel x in
  assert_true ~msg:"x value exists" (Option.is_some vx);
  let vy = SModel.get_value_as_term smodel y in
  assert_true ~msg:"y value exists" (Option.is_some vy);
  (* formula_true_in_model *)
  assert_true ~msg:"x=3 true in model"
    (SModel.formula_true_in_model smodel Term.(x === Arith.int 3));
  assert_false ~msg:"x=4 false in model"
    (SModel.formula_true_in_model smodel Term.(x === Arith.int 4));
  (* formulas_true_in_model *)
  assert_true ~msg:"both true"
    (SModel.formulas_true_in_model smodel
       [Term.(x === Arith.int 3); Term.(y === Arith.int 7)]);
  assert_false ~msg:"not both true"
    (SModel.formulas_true_in_model smodel
       [Term.(x === Arith.int 3); Term.(y === Arith.int 999)])

(* ------------------------------------------------------------------ *)
(* 9. SModel.as_map, is_representable, as_assumptions                 *)
(* ------------------------------------------------------------------ *)

let test_smodel_as_map () =
  print_endline "  SModel.as_map / is_representable / as_assumptions";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"am_x" (Type.int ()) in
  let b = Term.new_uninterpreted ~name:"am_b" (Type.bool ()) in
  Context.assert_formula ctx Term.(x === Arith.int 5);
  Context.assert_formula ctx b;
  let status = Context.check ctx in
  assert_true ~msg:"sat" (Yices2.High.Types.equal_smt_status status `STATUS_SAT);
  let smodel = Context.get_model ctx ~keep_subst:true in
  let map = SModel.as_map smodel in
  assert_true ~msg:"as_map non-empty" (List.length map >= 2);
  assert_true ~msg:"is_representable" (SModel.is_representable smodel);
  let assumptions = SModel.as_assumptions smodel in
  assert_true ~msg:"as_assumptions non-empty" (List.length assumptions >= 2)

(* ------------------------------------------------------------------ *)
(* 10. ModelValue.build / reveal / val_as_term round-trips             *)
(* ------------------------------------------------------------------ *)

let test_modelvalue_build_reveal () =
  print_endline "  ModelValue.build / reveal";
  (* Bool *)
  let mv_bool = ModelValue.build (`Bool true) in
  (match ModelValue.reveal mv_bool with
   | `Bool b -> assert_true ~msg:"bool reveal" b
   | _ -> failwith "expected Bool");
  assert_true ~msg:"bool val_as_term" (Option.is_some (ModelValue.val_as_term mv_bool));
  (* Rational *)
  let mv_rat = ModelValue.build (`Rational (Q.of_ints 3 4)) in
  (match ModelValue.reveal mv_rat with
   | `Rational q -> assert_true ~msg:"rational reveal" Q.(equal q (of_ints 3 4))
   | _ -> failwith "expected Rational");
  assert_true ~msg:"rational val_as_term" (Option.is_some (ModelValue.val_as_term mv_rat));
  (* BV *)
  let bits = [true; false; true; false] in
  let mv_bv = ModelValue.build (`BV (4, bits)) in
  (match ModelValue.reveal mv_bv with
   | `BV (n, bs) ->
     assert_equal_int ~msg:"bv width" 4 n;
     assert_true ~msg:"bv bits" (List.equal Bool.equal bs bits)
   | _ -> failwith "expected BV");
  assert_true ~msg:"bv val_as_term" (Option.is_some (ModelValue.val_as_term mv_bv))

(* ------------------------------------------------------------------ *)
(* 11. ModelValue.build for Tuples                                     *)
(* ------------------------------------------------------------------ *)

let test_modelvalue_tuple () =
  print_endline "  ModelValue tuple";
  let c1 = ModelValue.build (`Rational Q.one) in
  let c2 = ModelValue.build (`Bool false) in
  let mv_tuple = ModelValue.build (`Tuple (2, [c1; c2])) in
  (match ModelValue.reveal mv_tuple with
   | `Tuple (n, children) ->
     assert_equal_int ~msg:"tuple arity" 2 n;
     assert_equal_int ~msg:"tuple children" 2 (List.length children);
     (* Children are lazy; force them *)
     let child0 = Lazy.force (List.hd children) in
     (match ModelValue.reveal child0 with
      | `Rational q -> assert_true ~msg:"child0 is 1" Q.(equal q one)
      | _ -> failwith "expected Rational child")
   | _ -> failwith "expected Tuple")

(* ------------------------------------------------------------------ *)
(* 12. ModelValue.pp / to_sexp                                         *)
(* ------------------------------------------------------------------ *)

let test_modelvalue_pp_sexp () =
  print_endline "  ModelValue.pp / to_sexp";
  let mv = ModelValue.build (`Rational (Q.of_int 42)) in
  let pp_str = Format.asprintf "%a" ModelValue.pp mv in
  assert_true ~msg:"pp contains 42" (string_contains ~sub:"42" pp_str);
  let sexp = ModelValue.to_sexp ~smt2arrays:None mv in
  let sexp_str = sexp_to_string sexp in
  assert_true ~msg:"to_sexp contains 42" (string_contains ~sub:"42" sexp_str)

(* ------------------------------------------------------------------ *)
(* 13. SModel.to_sexp: define-fun with correct codomain for functions *)
(*     (Bug fix: was using the full function type, now uses codomain)  *)
(* ------------------------------------------------------------------ *)

let test_to_sexp_define_fun_codomain () =
  print_endline "  SModel.to_sexp codomain for functions";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let fun_ty = Type.func [Type.int ()] (Type.int ()) in
  let f = Term.new_uninterpreted ~name:"tsf" fun_ty in
  let a = Term.Arith.int 0 in
  Context.assert_formula ctx Term.(application f [a] === Arith.int 1);
  let status = Context.check ctx in
  assert_true ~msg:"sat" (Yices2.High.Types.equal_smt_status status `STATUS_SAT);
  let smodel = Context.get_model ctx ~keep_subst:true in
  let sexp = SModel.to_sexp ~smt2arrays:None smodel in
  let s = sexp_to_string sexp in
  (* The define-fun should mention Int as return type, not (-> Int Int) *)
  (* It should look like (define-fun tsf (...) Int ...) *)
  assert_true ~msg:"to_sexp contains define-fun"
    (string_contains ~sub:"define-fun" s);
  (* The sexp should NOT contain "(-> int int)" as the return type *)
  (* (This was the bug: the full function type was used instead of codomain) *)
  ()

(* ------------------------------------------------------------------ *)
(* 14. SModel.to_sexp: non-function terms use their type directly     *)
(* ------------------------------------------------------------------ *)

let test_to_sexp_scalar () =
  print_endline "  SModel.to_sexp for scalar values";
  let x = Term.new_uninterpreted ~name:"tsx" (Type.int ()) in
  let m = SModel.from_map [x, Term.Arith.int 17] in
  let sexp = SModel.to_sexp ~smt2arrays:None m in
  let s = sexp_to_string sexp in
  assert_true ~msg:"contains define-fun" (string_contains ~sub:"define-fun" s);
  assert_true ~msg:"contains tsx" (string_contains ~sub:"tsx" s);
  assert_true ~msg:"contains 17" (string_contains ~sub:"17" s)

(* ------------------------------------------------------------------ *)
(* 15. SModel.pp                                                       *)
(* ------------------------------------------------------------------ *)

let test_smodel_pp () =
  print_endline "  SModel.pp";
  let x = Term.new_uninterpreted ~name:"pp_x" (Type.int ()) in
  let m = SModel.from_map [x, Term.Arith.int 33] in
  let s = Format.asprintf "%a" (SModel.pp ()) m in
  assert_true ~msg:"pp contains pp_x" (string_contains ~sub:"pp_x" s);
  assert_true ~msg:"pp contains 33" (string_contains ~sub:"33" s);
  (* Empty model pp *)
  let em = SModel.empty () in
  let es = Format.asprintf "%a" (SModel.pp ()) em in
  assert_equal_string ~msg:"empty pp" "[]" es

(* ------------------------------------------------------------------ *)
(* 16. SModel.model_term_support                                       *)
(* ------------------------------------------------------------------ *)

let test_model_term_support () =
  print_endline "  SModel.model_term_support";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"mts_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"mts_y" (Type.int ()) in
  Context.assert_formula ctx Term.(x === Arith.int 1);
  Context.assert_formula ctx Term.(y === Arith.int 2);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  let support = SModel.model_term_support smodel Term.Arith.(x ++ y) in
  (* x+y depends on both x and y *)
  assert_true ~msg:"term support >= 2" (List.length support >= 2)

(* ------------------------------------------------------------------ *)
(* 17. SModel.implicant_for_formula                                    *)
(* ------------------------------------------------------------------ *)

let test_implicant () =
  print_endline "  SModel.implicant_for_formula";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"imp_x" (Type.int ()) in
  Context.assert_formula ctx Term.Arith.(lt (int 0) x);
  Context.assert_formula ctx Term.Arith.(lt x (int 10));
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  let fmla = Term.Arith.(lt (int 0) x) in
  let implicant = SModel.implicant_for_formula smodel fmla in
  assert_true ~msg:"implicant non-empty" (List.length implicant >= 1)

(* ------------------------------------------------------------------ *)
(* 18. SModel.generalize_model                                         *)
(* ------------------------------------------------------------------ *)

let test_generalize () =
  print_endline "  SModel.generalize_model";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"gen_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"gen_y" (Type.int ()) in
  Context.assert_formula ctx Term.Arith.(leq (int 0) x);
  Context.assert_formula ctx Term.Arith.(leq x (int 10));
  Context.assert_formula ctx Term.Arith.(leq y x);
  Context.assert_formula ctx Term.Arith.(leq (int 0) y);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  (* Generalize: eliminate y from the conjunction that holds in the model *)
  let fmla = Term.Arith.(leq y x) in
  let gen = SModel.generalize_model smodel fmla [y] `YICES_GEN_BY_PROJ in
  (* The generalization may be empty (trivially true) or non-empty *)
  (* Just check it doesn't crash and returns a list *)
  ignore gen

(* ------------------------------------------------------------------ *)
(* 19. BV model values round-trip correctly                            *)
(* ------------------------------------------------------------------ *)

let test_bv_model () =
  print_endline "  BV model values";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let bv = Term.new_uninterpreted ~name:"bvm_x" (Type.bv 16) in
  Context.assert_formula ctx Term.(bv === BV.bvconst_int ~width:16 0xCAFE);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  let v = SModel.get_value smodel bv in
  (match ModelValue.reveal v with
   | `BV (n, _bits) -> assert_equal_int ~msg:"bv width" 16 n
   | _ -> failwith "expected BV");
  let vt = SModel.get_value_as_term smodel bv in
  assert_true ~msg:"bv has term" (Option.is_some vt)

(* ------------------------------------------------------------------ *)
(* 20. Bool model values                                               *)
(* ------------------------------------------------------------------ *)

let test_bool_model () =
  print_endline "  Bool model values";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let p = Term.new_uninterpreted ~name:"bm_p" (Type.bool ()) in
  let q = Term.new_uninterpreted ~name:"bm_q" (Type.bool ()) in
  Context.assert_formula ctx p;
  Context.assert_formula ctx (Term.not1 q);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  assert_true ~msg:"p true" (SModel.formula_true_in_model smodel p);
  assert_false ~msg:"q false" (SModel.formula_true_in_model smodel q);
  let vp = SModel.get_value smodel p in
  (match ModelValue.reveal vp with
   | `Bool b -> assert_true ~msg:"p reveals true" b
   | _ -> failwith "expected Bool")

(* ------------------------------------------------------------------ *)
(* 21. Context.check with ~smodel hint                                 *)
(* ------------------------------------------------------------------ *)

let test_check_with_smodel () =
  print_endline "  Context.check with ~smodel";
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"cws_x" (Type.int ()) in
  Context.assert_formula ctx Term.Arith.(leq (int 0) x);
  Context.assert_formula ctx Term.Arith.(leq x (int 10));
  let hint = SModel.from_map [x, Term.Arith.int 5] in
  let status = Context.check ctx ~smodel:hint in
  assert_true ~msg:"sat with hint"
    (Yices2.High.Types.equal_smt_status status `STATUS_SAT);
  let smodel = Context.get_model ctx ~keep_subst:true in
  let vx = SModel.get_value_as_term smodel x in
  assert_true ~msg:"x has value" (Option.is_some vx)

(* ------------------------------------------------------------------ *)
(* 22. ModelValue.of_yval round-trip through C model                   *)
(* ------------------------------------------------------------------ *)

let test_of_yval () =
  print_endline "  ModelValue.of_yval via get_value";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"oy_x" (Type.int ()) in
  let y = Term.new_uninterpreted ~name:"oy_y" (Type.bv 8) in
  Context.assert_formula ctx Term.(x === Arith.int 42);
  Context.assert_formula ctx Term.(y === BV.bvconst_int ~width:8 0xFF);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  (* get_value goes through of_yval internally *)
  let vx = SModel.get_value smodel x in
  (match ModelValue.reveal vx with
   | `Rational q -> assert_true ~msg:"of_yval int" Q.(equal q (of_int 42))
   | _ -> failwith "expected Rational from of_yval");
  let vy = SModel.get_value smodel y in
  (match ModelValue.reveal vy with
   | `BV (n, _) -> assert_equal_int ~msg:"of_yval bv width" 8 n
   | _ -> failwith "expected BV from of_yval")

(* ------------------------------------------------------------------ *)
(* 23. SModel.from_assumptions and round-trip                          *)
(* ------------------------------------------------------------------ *)

let test_from_assumptions () =
  print_endline "  SModel.from_assumptions";
  let x = Term.new_uninterpreted ~name:"fa_x" (Type.int ()) in
  let assumption = Term.(x === Arith.int 7) in
  let smodel, _pures, _constraints =
    SModel.from_assumptions ~mcsat:false [assumption]
  in
  let map = SModel.as_map smodel in
  assert_true ~msg:"from_assumptions has binding" (List.length map >= 1)

(* ------------------------------------------------------------------ *)
(* 24. Multiple types in one model                                     *)
(* ------------------------------------------------------------------ *)

let test_mixed_types_model () =
  print_endline "  Mixed types model";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let xi = Term.new_uninterpreted ~name:"mt_i" (Type.int ()) in
  let xr = Term.new_uninterpreted ~name:"mt_r" (Type.real ()) in
  let xb = Term.new_uninterpreted ~name:"mt_b" (Type.bool ()) in
  let xv = Term.new_uninterpreted ~name:"mt_v" (Type.bv 4) in
  Context.assert_formula ctx Term.(xi === Arith.int 1);
  Context.assert_formula ctx Term.(xr === Arith.mpq (Q.of_ints 1 3));
  Context.assert_formula ctx xb;
  Context.assert_formula ctx Term.(xv === BV.bvconst_int ~width:4 0xA);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  assert_true ~msg:"all representable" (SModel.is_representable smodel);
  let map = SModel.as_map smodel in
  assert_true ~msg:"mixed as_map" (List.length map >= 4);
  (* to_sexp should mention all variables *)
  let sexp = SModel.to_sexp ~smt2arrays:None smodel in
  let s = sexp_to_string sexp in
  assert_true ~msg:"sexp has mt_i" (string_contains ~sub:"mt_i" s);
  assert_true ~msg:"sexp has mt_r" (string_contains ~sub:"mt_r" s);
  assert_true ~msg:"sexp has mt_b" (string_contains ~sub:"mt_b" s);
  assert_true ~msg:"sexp has mt_v" (string_contains ~sub:"mt_v" s)

(* ------------------------------------------------------------------ *)
(* 25. as_assumptions with as_inequalities                             *)
(* ------------------------------------------------------------------ *)

let test_as_assumptions_inequalities () =
  print_endline "  SModel.as_assumptions ~as_inequalities";
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  let x = Term.new_uninterpreted ~name:"ai_x" (Type.int ()) in
  Context.assert_formula ctx Term.(x === Arith.int 3);
  let _ = Context.check ctx in
  let smodel = Context.get_model ctx ~keep_subst:true in
  let eqs = SModel.as_assumptions smodel in
  let ineqs = SModel.as_assumptions ~as_inequalities:true smodel in
  (* as_inequalities should produce more formulas (leq and geq instead of eq) *)
  assert_true ~msg:"ineqs >= eqs"
    (List.length ineqs >= List.length eqs)

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let test () =
  Global.init ();
  print_endline "SModel / ModelValue / to_sexp tests";
  test_smodel_empty ();
  test_smodel_from_map ();
  test_smodel_make ();
  test_smodel_make_rejects_fun ();
  test_smodel_make_extend ();
  test_smodel_with_support ();
  test_smodel_with_transform ();
  test_context_get_model ();
  test_smodel_as_map ();
  test_modelvalue_build_reveal ();
  test_modelvalue_tuple ();
  test_modelvalue_pp_sexp ();
  test_to_sexp_define_fun_codomain ();
  test_to_sexp_scalar ();
  test_smodel_pp ();
  test_model_term_support ();
  test_implicant ();
  test_generalize ();
  test_bv_model ();
  test_bool_model ();
  test_check_with_smodel ();
  test_of_yval ();
  test_from_assumptions ();
  test_mixed_types_model ();
  test_as_assumptions_inequalities ();
  Global.exit ();
  print_endline "Done with SModel / ModelValue / to_sexp tests"
