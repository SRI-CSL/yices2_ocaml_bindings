open Signed
open Unsigned
open Containers
open Yices2_high

module EH1 = Make(ExceptionsErrorHandling)

let test () =
  print_endline "Terms tests";
  let open EH1 in
  let open Global in
  init();

  let open Term in
  let open Arith in

  let check t =
    let Term tr = reveal t in
    assert(Term.equal t (build tr));
    let t' = build(map (fun x -> x) tr) in
    assert(Term.equal t t')
  in
  let check = List.iter check in
  let true_  = true0() in
  let false_ = false0() in
  let bool_t = Type.bool() in
  let int_t  = Type.int() in
  let unint_t = Type.new_uninterpreted() in
  assert(not (equal true_ false_));
  let const1 = constant unint_t 0 in
  let const2 = new_uninterpreted_term unint_t in
  let bconst1 = new_uninterpreted_term bool_t in
  let iconst1 = new_uninterpreted_term int_t in
  let var1 = new_variable unint_t in
  let bvar1 = new_variable bool_t in
  let ivar1 = new_variable int_t in
  let ivar2 = new_variable int_t in
  let ivar3 = new_variable int_t in
  let ivar4 = new_variable int_t in
  let zero = Arith.zero() in
  let int1 = Arith.int 13 in
  let int2 = Arith.int 17 in
  assert(equal zero (Arith.int 0));
  let fun1_t = Type.func [int_t] bool_t in
  let fun1 = new_variable fun1_t in
  let app1 = application fun1 [int1] in
  let fun2_t = Type.func [int_t; int_t] bool_t in
  let fun2 = new_variable fun2_t in
  let app2 = application fun2 [int1; int1] in
  let fun3_t = Type.func [int_t; int_t; int_t] bool_t in
  let fun3 = new_variable fun3_t in
  let app3 = application fun3 [int1; int1; int1] in
  let tup3_t = Type.tuple [bool_t; int_t; unint_t] in
  let tupconst1 = new_variable(tup3_t) in
  let ta4  = [int_t; int_t; int_t; int_t] in
  let int4 = [int1; int2; iconst1; ivar1] in
  let int4_2 = [ivar1; ivar2; ivar3; ivar4] in
  let fun4_t = Type.func ta4 bool_t in
  let fun4 = new_variable fun4_t in
  let app4 = application fun4 int4 in
  let ite1 = ite bconst1 int1 int2 in
  let eq1  = int1 === int1 in
  let neq1 = int1 =/= int1 in
  let not1 = !!false_ in
  let bool5 = [false_; eq1; neq1; app4; false_] in
  let or1  = !| bool5 in
  let and1 = !& bool5 in
  let xor1 = !^ bool5 in
  let or2_  = or1 ||| and1 in
  let and2_ = or1 &&& and1 in
  let xor2_ = or1 *** and1 in
  let or3  = !|[or1; and1; or2_] in
  let and3 = !&[or1; and1; and2_] in
  let xor3 = !^[or1; and1; xor2_] in
  let iff1 = and1 <=> or1 in
  let implies1 = and1 ==> or1 in
  let tup1  = tuple int4 in
  let pair1 = tuple[eq1; xor2_] in
  let triple1 = tuple[ite1; fun4; or3] in
  let select1 = select 2 tup1 in
  let select2 = select 2 tupconst1 in
  let tupup1  = tuple_update tup1 2 int2 in
  let update1 = update fun1 [int1] false_ in
  let update2 = update fun2 [int1; int1] false_ in
  let update3 = update fun3 [int1; int1; int1] false_ in
  let update4 = update fun4 int4 false_ in
  let distinct1 = distinct int4 in
  let var2  = new_variable(unint_t) in
  let vareq = var1 === var2 in
  let vars2 = [var1; var2] in
  let forall1 = forall vars2 vareq in
  let exists1 = exists vars2 vareq in
  let lambda1 = lambda vars2 vareq in

  let int64_1 = int64(Long.of_int 42) in
  let rat_1   = rational 13 7 in
  let rat32_1 = rational32 (SInt.of_int 13) (UInt.of_int 7) in
  let rat64_1 = rational64 (Long.of_int (-47)) (ULong.of_int 111) in
  let gmpz = Z.of_int 42 in
  let mpz1 = mpz gmpz in
  let gmpq = Q.of_ints 42 77 in
  let mpq1 = mpq gmpq in
  let rat1 = parse_rational "-3/117" in
  let float1 = parse_float "-3.117e-2" in
  let add1 = int1 ++ int1 in
  let sub1 = int1 -- zero in
  let neg1 = !- int1 in
  assert(equal !-zero zero);
  assert(not (equal neg1 int1));
  let mul1 = int1 ** int1 in
  let square1 = square int1 in
  assert(equal mul1 square1);
  let power1 = int1 ^^ 4 in
  let sum1  = !+ int4 in
  let product1 = !* int4 in
  let product2 = !* int4_2 in
  let div1  = int1 // int1 in
  let idiv1 = int1 /. int1 in
  let imod1 = int1 %. int1 in
  let divatom1 = int1 ||. int1 in
  let intatom1 = is_int_atom(int1) in
  let abs1 = abs(neg1) in
  assert(equal abs1 int1);
  let floor1 = floor rat1 in
  let ceil1  = ceil rat1 in
  let coeff4 = [2; 3; 4; 5] in
  let coeff4_32 = List.map SInt.of_int coeff4 in
  let coeff4_64 = List.map Long.of_int coeff4 in
  let coeff4_mpz = List.map Z.of_int coeff4 in
  let combine2 a b = a,b in
  let poly   = poly_int(List.map2 combine2 coeff4 int4) in
  let poly32 = poly_int32 (List.map2 combine2 coeff4_32 int4) in
  let poly64 = poly_int64 (List.map2 combine2 coeff4_64 int4) in
  let polympz = poly_mpz (List.map2 combine2 coeff4_mpz int4) in
  let denum4 = [12; 13; 14; 15] in
  let denum4_32 = List.map UInt.of_int denum4 in
  let denum4_64 = List.map ULong.of_int denum4 in
  let denum4_mpz = List.map Z.of_int denum4 in
  let combine3 (a,b) c = a,b,c in
  let polyrat = poly_rational (List.map2 combine3 (List.map2 combine2 coeff4 denum4) int4) in
  let polyrat32 = poly_rational32
      (List.map2 combine3 (List.map2 combine2 coeff4_32 denum4_32) int4)
  in
  let polyrat64 = poly_rational64
      (List.map2 combine3 (List.map2 combine2 coeff4_64 denum4_64) int4)
  in
  let polyratmpq = poly_mpq
      (List.map2 combine2 (List.map2 Q.make coeff4_mpz denum4_mpz) int4)
  in
  let areqatom1   = arith_eq int1 zero in
  let arneqatom1  = arith_neq int1 zero in
  let argeqatom1  = geq int1 zero in
  let arleqatom1  = leq int1 zero in
  let argtatom1   = gt int1 zero in
  let arltatom1   = lt int1 zero in
  let areq0atom1  = eq0 int1 in
  let arneq0atom1 = neq0 int1 in
  let argeq0atom1 = geq0 int1 in
  let arleq0atom1 = leq0 int1 in
  let argt0atom1  = gt0 int1 in
  let arlt0atom1  = lt0 int1 in


  
  let bv_t = Type.bv 8 in
  let open BV in
  let bvconstu_1   = bvconst_int ~width:8 42 in
  let bvconstu32_1 = bvconst_uint32 ~width:8 (UInt.of_int 42) in
  let bvconstu64_1 = bvconst_uint64 ~width:8 (ULong.of_int 42) in
  let bvconst32_1 = bvconst_int32 ~width:8 (SInt.of_int 42) in
  let bvconst64_1 = bvconst_int64 ~width:8 (Long.of_int 42) in
  let bvconstzero_1 = bvconst_zero 16 in
  let bvconstone_1  = bvconst_one 16 in
  let bvconstminusone_1 = bvconst_minus_one 32 in
  let bvconstarray1 = bvconst_from_list [true; false; true; false] in
  let bvvar1 = new_variable bv_t in
  let bvvar2 = new_variable bv_t in
  let bvvar3 = new_variable bv_t in
  let bvvar4 = new_variable bv_t in
  let bvbin1 = parse_bvbin "100101" in
  let bvhex1 = parse_bvhex "f0a1b3" in
  let bvadd1 = bvadd bvbin1 bvbin1 in
  let bvsub1 = bvsub bvbin1 bvbin1 in
  let bvneg1 = bvneg bvbin1 in
  let bvmul1 = bvmul bvbin1 bvbin1 in
  let bvsquare1 = bvsquare(bvbin1) in
  let bvpower1 = bvpower bvbin1 3 in
  let bvdiv1  = bvdiv bvbin1 bvbin1 in
  let bvrem1  = bvrem bvbin1 bvbin1 in
  let bvsdiv1 = bvsdiv bvbin1 bvbin1 in
  let bvsrem1 = bvsrem bvbin1 bvbin1 in
  let bvsmod1 = bvsmod bvbin1 bvbin1 in
  let bvnot1  = bvnot bvbin1 in
  let bvnand1 = bvnand bvbin1 bvbin1 in
  let bvnor1  = bvnor bvbin1 bvbin1 in
  let bvxnor1 = bvxnor bvbin1 bvbin1 in
  let bvshl1  = bvshl bvbin1 bvbin1 in
  let bvlshr1 = bvlshr bvbin1 bvbin1 in
  let bvashr1 = bvashr bvbin1 bvbin1 in
  let bvand1  = bvand [bvbin1; bvbin1; bvbin1; bvbin1] in
  let bvor1   = bvor  [bvbin1; bvbin1; bvbin1; bvbin1] in
  let bvand2_1 = bvand [bvbin1; bvbin1] in
  let bvor2_1  = bvor  [bvbin1; bvbin1] in
  let bvxor2_1 = bvxor [bvbin1; bvbin1] in
  let bvand3_1 = bvand [bvbin1; bvbin1; bvbin1] in
  let bvor3_1  = bvor  [bvbin1; bvbin1; bvbin1] in
  let bvxor3_1 = bvxor [bvbin1; bvbin1; bvbin1] in
  let bvsum1 = bvsum   [bvbin1; bvbin1; bvbin1; bvbin1] in
  let bvsum2 = bvsum   [bvvar1; bvvar2; bvvar3; bvvar4] in
  let bvproduct1 = bvproduct [bvbin1; bvbin1; bvbin1; bvbin1] in
  let shleft0_1  = shift_left0 bvbin1 5 in
  let shleft1_1  = shift_left1 bvbin1 4 in
  let shright0_1 = shift_right0 bvbin1 3 in
  let shright1_1 = shift_right1 bvbin1 2 in
  let ashright_1 = ashift_right bvbin1 1 in
  let rotleft_1  = rotate_left bvbin1 6 in
  let rotright_1 = rotate_right bvbin1 5 in
  let bvextract1 = bvextract bvbin1 2 4 in
  let bvconcat2_1 = bvconcat2 bvbin1 bvbin1 in
  let bvconcat_1 = bvconcat [bvbin1; bvbin1; bvbin1; bvbin1] in
  let bvrepeat1 = bvrepeat bvbin1 8 in
  let signext1 = sign_extend bvbin1 3 in
  let zeroext1 = zero_extend bvbin1 4 in
  let redand1  = redand bvbin1 in
  let redor1   = redor bvbin1 in
  let redcomp1 = redcomp bvbin1 bvbin1 in
  let bvarray1 = bvarray [true_; false_; true_; false_] in
  let bitextract1 = bitextract bvbin1 3 in
  let bveqatom1 = bveq bvbin1 bvbin1 in
  let bvneqatom1 = bvneq bvbin1 bvbin1 in
  let bvgeatom1 = bvge bvbin1 bvbin1 in
  let bvgtatom1 = bvgt bvbin1 bvbin1 in
  let bvleatom1 = bvle bvbin1 bvbin1 in
  let bvltatom1 = bvlt bvbin1 bvbin1 in
  let bvsgeatom1 = bvsge bvbin1 bvbin1 in
  let bvsgtatom1 = bvsgt bvbin1 bvbin1 in
  let bvsleatom1 = bvsle bvbin1 bvbin1 in
  let bvsltatom1 = bvslt bvbin1 bvbin1 in

  let ptype1 = Type.parse "int" in
  assert(Type.equal ptype1 (Type.int()));
  let pterm1 = Term.parse "42" in
  assert(Term.equal pterm1 (int 42));
  let subst1 = Term.subst_term [new_variable ptype1, int 2; new_variable ptype1, int 3] (int 42)
  in
  let _substarr1 = subst_terms
      [new_variable ptype1, int 2;
       new_variable ptype1, int 3 ]
      [int 2; int 3; int 7]
  in
  let () = Type.Names.set ptype1 "I" in
  let () = Term.Names.set pterm1 "answer" in
  let gettype1 = Type.Names.of_name "I" in
  assert(Type.equal gettype1 ptype1);
  let getterm1 = Term.Names.of_name "answer" in
  assert(Term.equal getterm1 pterm1);
  let gettypename1 = Type.Names.to_name ptype1 in
  assert(String.equal gettypename1 "I");
  let gettermname1 = Term.Names.to_name pterm1 in
  assert(String.equal gettermname1 "answer");
  let () = Type.Names.remove "I" in
  let () = Term.Names.remove "answer" in
  let () = Type.Names.clear ptype1 in
  let () = Term.Names.clear pterm1 in
  let typeofterm1 = Term.type_of_term pterm1 in
  assert(Type.equal typeofterm1 (Type.int()));
  assert(Bool.equal (Term.is_bool false_) true);
  assert(Bool.equal (Term.is_bool pterm1) false);
  assert(Bool.equal (Term.is_int false_) false);
  assert(Bool.equal (Term.is_int pterm1) true);
  assert(Bool.equal (Term.is_real false_) false);
  assert(Bool.equal (Term.is_real pterm1) false);
  assert(Bool.equal (Term.is_arithmetic false_) false);
  assert(Bool.equal (Term.is_arithmetic pterm1) true);
  assert(Bool.equal (Term.is_bitvector false_) false);
  assert(Bool.equal (Term.is_bitvector bvbin1) true);
  assert(Bool.equal (Term.is_tuple false_) false);
  assert(Bool.equal (Term.is_tuple tup1) true);
  assert(Bool.equal (Term.is_function false_) false);
  assert(Bool.equal (Term.is_function fun1) true);
  assert(Bool.equal (Term.is_scalar false_) false);
  assert(Bool.equal (Term.is_scalar fun1) false);
  assert(Int.equal (Term.bitsize bvbin1) 6);
  assert(Bool.equal (Term.is_ground false_) true);
  assert(Bool.equal (Term.is_ground var1) false);
  assert(Bool.equal (Term.is_atomic false_) true);
  (* or1 is atomic because it simplifies to true *)
  assert(Bool.equal (Term.is_atomic or1) true);
  assert(Bool.equal (Term.is_composite false_) false);
  assert(Bool.equal (Term.is_composite ite1) true);
  assert(Bool.equal (Term.is_composite tup1) true);
  assert(Bool.equal (Term.is_projection false_) false);
  (* Select1 simplifies *)
  assert(Bool.equal (Term.is_projection select1) false);
  assert(Bool.equal (Term.is_projection select2) true);
  assert(Bool.equal (Term.is_sum ite1) false);
  assert(Bool.equal (Term.is_sum sum1) true);
  assert(Bool.equal (Term.is_bvsum(select1)) false);
  (* bvsum1 simplifies since the terms are all numbers *)
  assert(Bool.equal (Term.is_bvsum bvsum1) false);
  assert(Bool.equal (Term.is_bvsum bvsum2) true);
  assert(Bool.equal (Term.is_product ite1) false);
  assert(Bool.equal (Term.is_product product1) false);
  assert(Bool.equal (Term.is_product product2) true);
  assert(Types.equal_term_constructor (Term.constructor true_) `YICES_BOOL_CONSTANT);
  assert(Types.equal_term_constructor (Term.constructor int1) `YICES_ARITH_CONSTANT);
  assert(Types.equal_term_constructor (Term.constructor bvconst32_1) `YICES_BV_CONSTANT);
  assert(Int.equal (Term.num_children bvconst32_1) 0);
  assert(Int.equal (Term.num_children select2) 1);
  assert(Int.equal (Term.num_children tup1) 4);
  assert(Term.equal (Term.child tup1 2) iconst1);

  check [true_; false_; const1; const2; bconst1; iconst1; var1; bvar1; zero; int1; int2; fun1;
         app1; fun2; app2; fun3; app3; tupconst1; fun4; app4; ite1; eq1; neq1; not1; or1; and1;
         xor1; or2_; and2_; xor2_; or3; and3; xor3; iff1; implies1; tup1; pair1; triple1;
         select1; select2; tupup1; update1; update2; update3; update4; distinct1; var2; vareq;
         forall1;  exists1; lambda1; int64_1; rat_1; rat32_1; rat64_1; mpz1; mpq1; rat1; float1;
         add1; sub1; neg1; mul1; square1; power1; sum1; product1; product2; div1; idiv1; imod1;
         divatom1; intatom1; abs1; floor1; ceil1; poly; poly32; poly64; polympz; polyrat;
         polyrat32; polyrat64; polyratmpq; areqatom1; arneqatom1; argeqatom1; arleqatom1;
         argtatom1; arltatom1;
         areq0atom1; arneq0atom1; argeq0atom1; arleq0atom1; argt0atom1; arlt0atom1;
         bvconstu_1; bvconstu32_1; bvconstu64_1; bvconst32_1; bvconst64_1; bvconstzero_1;
         bvconstone_1; bvconstminusone_1; bvconstarray1; bvvar1; bvbin1; bvhex1; bvadd1; bvsub1;
         bvneg1; bvmul1; bvsquare1; bvpower1; bvdiv1; bvrem1; bvsdiv1; bvsrem1; bvsmod1; bvnot1;
         bvnand1; bvnor1; bvxnor1; bvshl1; bvlshr1; bvashr1; bvand1; bvor1; bvand2_1; bvor2_1;
         bvxor2_1; bvand3_1; bvor3_1; bvxor3_1; bvsum1; bvsum2; bvproduct1; shleft0_1; shleft1_1;
         shright0_1; shright1_1; shleft1_1; ashright_1; rotleft_1; rotright_1; bvextract1;
         bvconcat2_1; bvconcat_1; bvrepeat1; signext1; zeroext1; redand1; redor1; redcomp1;
         bvarray1; bitextract1; bveqatom1; bvneqatom1; bvgeatom1; bvgtatom1; bvleatom1;
         bvltatom1; bvsgeatom1; bvsgtatom1; bvsleatom1; bvsltatom1; pterm1; subst1 ];

  let projarg1 = proj_arg(select2) in
  assert(Int.equal (proj_index select2) 2);
  assert(Term.equal (proj_arg select2) tupconst1);
  let val_p = bool_const_value true_ in
  assert(Bool.equal val_p true);
  let bval = bv_const_value bvconst32_1 in
  assert(List.equal Bool.equal bval [false; true; false; true; false; true; false; false]);
  let scalar_t = Type.new_scalar 20 in
  let scalar_c = Term.constant scalar_t 13 in
  assert(Int.equal (scalar_const_value scalar_c) 13);
  let _gmpq = rational_const_value rat32_1 in
  let _gmpq, _pterm = sum_component sum1 2 in
  (* value must be an array of eight integers since bvsum has type (bitvector 8) *)
  let value, pterm = bvsum_component bvsum2 1 in
  let pterm = match pterm with
    | Some x -> x
    | None -> assert false
  in
  check [projarg1; scalar_c; pterm];
  assert(Bool.equal (Term.is_bitvector pterm) true);
  assert(Bool.equal (Term.is_bitvector bvvar2) true);
  assert(List.equal Bool.equal value [ true; false; false; false; false; false; false; false]);
  assert(Term.equal pterm bvvar2);
  let pterm, exp1 = product_component product2 1 in
  check [pterm];
  assert(UInt.equal exp1 UInt.one);
  assert(Term.equal pterm ivar2);
  
  let open GC in
  let () = incref_term pterm in
  assert(Int.equal (num_posref_terms()) 1);
  let () = decref_term pterm in
  assert(Int.equal (num_posref_terms()) 0);
  let () = incref_type unint_t in
  assert(Int.equal (num_posref_types()) 1);
  let () = decref_type unint_t in
  assert(Int.equal (num_posref_types()) 0);
  let () = incref_term int1 in
  let () = incref_type int_t in
  let () = garbage_collect int4 ta4 false in
  assert(Int.equal (num_terms()) 7);
  assert(Int.equal (num_types()) 3);
  print_endline "Done with Terms tests";
  exit()
