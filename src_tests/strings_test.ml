module S = Extensions.Strings
module Y = Yices2.Ext.WithExceptionsErrorHandling

let with_context f =
  Y.Global.init();
  Fun.protect
    ~finally:(fun () -> Y.Global.exit())
    (fun () ->
       let ctx = S.Context.malloc () in
       f ctx)

let assert_status expected actual =
  assert (Yices2.Ext.Types.equal_smt_status actual expected)

let assert_check expected ctx =
  S.Context.check ctx |> assert_status expected

let assert_no_model ctx =
  let no_model =
    try
      ignore (S.Context.get_model ctx);
      false
    with
    | Yices2.High.ExceptionsErrorHandling.YicesException _
    | Yices2.High.ExceptionsErrorHandling.YicesBindingsException _ -> true
  in
  assert no_model

let test_literal_length () =
  with_context @@ fun ctx ->
  let abc = S.Term.str "abc" in
  S.Context.assert_formula ctx S.Term.(S.Term.len abc === Arith.int 3);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model abc)) "abc")

let test_literal_length_unsat () =
  with_context @@ fun ctx ->
  let abc = S.Term.str "abc" in
  S.Context.assert_formula ctx S.Term.(S.Term.len abc === Arith.int 4);
  assert_check `STATUS_UNSAT ctx

let test_unicode_scalar_length () =
  with_context @@ fun ctx ->
  let lambda_str = S.Term.str "lambda: \206\187" in
  S.Context.assert_formula ctx S.Term.(S.Term.len lambda_str === Arith.int 9);
  assert_check `STATUS_SAT ctx

let test_invalid_utf8 () =
  Y.Global.init();
  let rejected =
    try
      ignore (S.Term.str "\192\128");
      false
    with
    | Yices2.High.ExceptionsErrorHandling.YicesBindingsException _ -> true
    | Yices2.High.ExceptionsErrorHandling.YicesException _ -> true
  in
  Y.Global.exit();
  assert rejected

let test_variable_forced_to_literal () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage1_x" () in
  let abc = S.Term.str "abc" in
  S.Context.assert_formula ctx S.Term.(x === abc);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "abc")

let test_distinct_literals_unsat () =
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx S.Term.(S.Term.str "abc" === S.Term.str "def");
  assert_check `STATUS_UNSAT ctx

let test_ground_concat () =
  with_context @@ fun ctx ->
  let ab = S.Term.concat [S.Term.str "a"; S.Term.str "b"] in
  S.Context.assert_formula ctx S.Term.(ab === S.Term.str "ab");
  S.Context.assert_formula ctx S.Term.(S.Term.len ab === Arith.int 2);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model ab)) "ab")

let test_ground_concat_unsat () =
  with_context @@ fun ctx ->
  let ac = S.Term.concat [S.Term.str "a"; S.Term.str "c"] in
  S.Context.assert_formula ctx S.Term.(ac === S.Term.str "ab");
  assert_check `STATUS_UNSAT ctx

let test_concat_suffix_refinement_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_concat_x" () in
  let xb = S.Term.concat [x; S.Term.str "b"] in
  S.Context.assert_formula ctx S.Term.(xb === S.Term.str "ab");
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "a");
  assert (String.equal (Option.get (S.StringModel.find_string model xb)) "ab")

let test_concat_interior_refinement_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_concat_mid_x" () in
  let axc = S.Term.concat [S.Term.str "a"; x; S.Term.str "c"] in
  S.Context.assert_formula ctx S.Term.(axc === S.Term.str "abc");
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "b");
  assert (String.equal (Option.get (S.StringModel.find_string model axc)) "abc")

let test_model_support_filter () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_support_x" () in
  let xb = S.Term.concat [x; S.Term.str "b"] in
  S.Context.assert_formula ctx S.Term.(xb === S.Term.str "ab");
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ~support:[x] ctx in
  assert (List.length model.S.StringModel.strings = 1);
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "a")

let test_concat_incompatible_suffix_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_bad_suffix_x" () in
  let xb = S.Term.concat [x; S.Term.str "b"] in
  S.Context.assert_formula ctx S.Term.(xb === S.Term.str "ac");
  assert_check `STATUS_UNSAT ctx

let test_concat_assignment_conflict_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_conflict_x" () in
  let xb = S.Term.concat [x; S.Term.str "b"] in
  S.Context.assert_formula ctx S.Term.(xb === S.Term.str "ab");
  S.Context.assert_formula ctx S.Term.(x === S.Term.str "c");
  assert_check `STATUS_UNSAT ctx

let test_length_filler_model () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_len_x" () in
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.length (Option.get (S.StringModel.find_string model x)) = 3)

let test_satisfiable_disequality () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_distinct_x" () in
  let y = S.Term.string_var ~name:"stage2_distinct_y" () in
  S.Context.assert_formula ctx S.Term.(not1 (x === y));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  S.Context.assert_formula ctx S.Term.(S.Term.len y === Arith.int 2);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  let y_value = Option.get (S.StringModel.find_string model y) in
  assert (not (String.equal x_value y_value))

let test_negated_literal_equality_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_neg_x" () in
  S.Context.assert_formula ctx S.Term.(not1 (x === str "abc"));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (not (String.equal (Option.get (S.StringModel.find_string model x)) "abc"))

let test_multi_unknown_concat_unknown () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage2_unknown_x" () in
  let y = S.Term.string_var ~name:"stage2_unknown_y" () in
  S.Context.assert_formula ctx S.Term.(concat [x; y] === str "ab");
  assert_check `STATUS_UNKNOWN ctx;
  assert_no_model ctx

let contains_text haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    index + needle_len <= haystack_len
    && (String.equal (String.sub haystack index needle_len) needle
        || loop (index + 1))
  in
  needle_len = 0 || loop 0

let test_contains_sat () =
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx S.Term.(contains (str "abc") (str "b"));
  assert_check `STATUS_SAT ctx

let test_contains_symbolic_witness_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_contains_symbolic_x" () in
  let y = S.Term.string_var ~name:"stage3_contains_symbolic_y" () in
  S.Context.assert_formula ctx S.Term.(contains x y);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  let y_value = Option.get (S.StringModel.find_string model y) in
  assert (contains_text x_value y_value)

let test_contains_refinement_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_contains_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abc");
  S.Context.assert_formula ctx S.Term.(contains x (str "d"));
  assert_check `STATUS_UNSAT ctx

let test_not_contains_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_not_contains_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abc");
  S.Context.assert_formula ctx S.Term.(not1 (contains x (str "d")));
  assert_check `STATUS_SAT ctx

let test_substr_sat_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_substr_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abcdef");
  S.Context.assert_formula ctx S.Term.(substr x (Arith.int 1) (Arith.int 3) === str "bcd");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_substr_bad_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abcdef");
  S.Context.assert_formula ctx S.Term.(substr x (Arith.int 1) (Arith.int 3) === str "xxx");
  assert_check `STATUS_UNSAT ctx

let test_substr_symbolic_witness_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_substr_symbolic_x" () in
  S.Context.assert_formula ctx S.Term.(substr x (Arith.int 1) (Arith.int 3) === str "bcd");
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  assert (String.length x_value >= 4);
  assert (String.equal (String.sub x_value 1 3) "bcd")

let indexof_text haystack needle start =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if start < 0 || start > haystack_len then -1
  else if needle_len = 0 then start
  else
    let rec loop index =
      if index + needle_len > haystack_len then -1
      else if String.equal (String.sub haystack index needle_len) needle then index
      else loop (index + 1)
    in
    loop start

let test_indexof_sat_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_indexof_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abcabc");
  S.Context.assert_formula ctx S.Term.(indexof x (str "bc") (Arith.int 0) === Arith.int 1);
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_indexof_bad_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abcabc");
  S.Context.assert_formula ctx S.Term.(indexof x (str "bc") (Arith.int 0) === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_indexof_symbolic_witness_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_indexof_symbolic_x" () in
  S.Context.assert_formula ctx S.Term.(indexof x (str "bc") (Arith.int 0) === Arith.int 1);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  assert (indexof_text x_value "bc" 0 = 1)

let replace_text haystack needle replacement =
  if String.equal needle "" then replacement ^ haystack
  else
    let start = indexof_text haystack needle 0 in
    if start < 0 then haystack
    else
      let prefix = String.sub haystack 0 start in
      let suffix_start = start + String.length needle in
      let suffix =
        String.sub haystack suffix_start (String.length haystack - suffix_start)
      in
      prefix ^ replacement ^ suffix

let replace_all_text haystack needle replacement =
  if String.equal needle "" then haystack
  else
    let haystack_len = String.length haystack in
    let needle_len = String.length needle in
    let output = Buffer.create haystack_len in
    let rec loop start =
      let found = indexof_text haystack needle start in
      if found < 0 then
        Buffer.add_substring output haystack start (haystack_len - start)
      else begin
        Buffer.add_substring output haystack start (found - start);
        Buffer.add_string output replacement;
        loop (found + needle_len)
      end
    in
    loop 0;
    Buffer.contents output

let test_replace_prefix_suffix_at () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_replace_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abc");
  S.Context.assert_formula ctx S.Term.(replace x (str "b") (str "x") === str "axc");
  S.Context.assert_formula ctx S.Term.(prefixof (str "a") x);
  S.Context.assert_formula ctx S.Term.(suffixof (str "c") x);
  S.Context.assert_formula ctx S.Term.(at x (Arith.int 1) === str "b");
  assert_check `STATUS_SAT ctx

let test_replace_symbolic_witness_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_replace_symbolic_x" () in
  S.Context.assert_formula ctx S.Term.(replace x (str "b") (str "x") === str "axc");
  S.Context.assert_formula ctx S.Term.(not1 (x === str "axc"));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  assert (String.equal (replace_text x_value "b" "x") "axc");
  assert (not (String.equal x_value "axc"))

let test_replace_all_ground_semantics () =
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(replace_all (str "ababa") (str "a") (str "x") === str "xbxbx");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(replace_all (str "ababa") (str "a") (str "x") === str "ababa");
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(replace_all (str "abc") (str "") (str "x") === str "abc");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(replace_all (str "a") (str "a") (str "aa") === str "aa");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(replace_all (str "a") (str "a") (str "aa") === str "aaaa");
  assert_check `STATUS_UNSAT ctx

let test_replace_all_symbolic_witness_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_replace_all_symbolic_x" () in
  S.Context.assert_formula ctx S.Term.(contains x (str "a"));
  S.Context.assert_formula ctx
    S.Term.(replace_all x (str "a") (str "b") === str "bb");
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  assert (contains_text x_value "a");
  assert (String.equal (replace_all_text x_value "a" "b") "bb")

let test_replace_all_fixed_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_replace_all_fixed_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "aaa");
  S.Context.assert_formula ctx
    S.Term.(replace_all x (str "a") (str "b") === str "aba");
  assert_check `STATUS_UNSAT ctx

let test_rewrite_simplification_axioms () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_contains_empty_x" () in
  S.Context.assert_formula ctx S.Term.(not1 (contains x (str "")));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_empty_contains_x" () in
  S.Context.assert_formula ctx S.Term.(contains (str "") x);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_empty_prefix_x" () in
  S.Context.assert_formula ctx S.Term.(not1 (prefixof (str "") x));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_empty_suffix_x" () in
  S.Context.assert_formula ctx S.Term.(not1 (suffixof (str "") x));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_substr_negative_x" () in
  S.Context.assert_formula ctx
    S.Term.(not1 (substr x (Arith.int (-1)) (Arith.int 2) === str ""));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_indexof_negative_x" () in
  S.Context.assert_formula ctx
    S.Term.(not1 (indexof x (str "a") (Arith.int (-1)) === Arith.int (-1)));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_replace_empty_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abc");
  S.Context.assert_formula ctx
    S.Term.(replace x (str "") (str "p") === str "pabc");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"rewrite_at_negative_x" () in
  S.Context.assert_formula ctx
    S.Term.(not1 (at x (Arith.int (-1)) === str ""));
  assert_check `STATUS_UNSAT ctx

let test_containment_abstraction () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_prefix_x" () in
  let y = S.Term.string_var ~name:"containment_prefix_y" () in
  S.Context.assert_formula ctx S.Term.(prefixof y x);
  S.Context.assert_formula ctx S.Term.(not1 (contains x y));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_suffix_x" () in
  let y = S.Term.string_var ~name:"containment_suffix_y" () in
  S.Context.assert_formula ctx S.Term.(suffixof y x);
  S.Context.assert_formula ctx S.Term.(not1 (contains x y));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_concat_x" () in
  let y = S.Term.string_var ~name:"containment_concat_y" () in
  S.Context.assert_formula ctx S.Term.(x === concat [str "p"; y; str "q"]);
  S.Context.assert_formula ctx S.Term.(not1 (contains x y));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_regex_forces_x" () in
  let contains_z =
    S.Regex.concat [S.Regex.all; S.Regex.str "z"; S.Regex.all]
  in
  S.Context.assert_formula ctx (S.Term.in_re x contains_z);
  S.Context.assert_formula ctx S.Term.(not1 (contains x (str "z")));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_regex_excludes_x" () in
  S.Context.assert_formula ctx
    (S.Term.in_re x (S.Regex.star (S.Regex.range "a" "c")));
  S.Context.assert_formula ctx S.Term.(contains x (str "z"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"containment_regex_sat_x" () in
  S.Context.assert_formula ctx
    (S.Term.in_re x (S.Regex.star (S.Regex.range "a" "c")));
  S.Context.assert_formula ctx S.Term.(not1 (contains x (str "z")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_SAT ctx

let int_var name =
  S.Term.new_uninterpreted ~name S.Type.(int ())

let test_character_abstraction () =
  with_context @@ fun ctx ->
  let i = int_var "char_substr_i" in
  let n = int_var "char_substr_n" in
  S.Context.assert_formula ctx
    S.Term.(contains (substr (str "abc") i n) (str "z"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"char_class_x" () in
  let i = int_var "char_class_i" in
  let n = int_var "char_class_n" in
  S.Context.assert_formula ctx S.Term.(x === substr (str "abc") i n);
  S.Context.assert_formula ctx S.Term.(contains x (str "z"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let i = int_var "char_at_i" in
  S.Context.assert_formula ctx S.Term.(contains (at (str "abc") i) (str "z"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let y = S.Term.string_var ~name:"char_replace_y" () in
  S.Context.assert_formula ctx
    S.Term.(contains (replace (str "aaaa") y (str "bb")) (str "c"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let y = S.Term.string_var ~name:"char_replace_all_y" () in
  S.Context.assert_formula ctx
    S.Term.(contains (replace_all (str "aaaa") y (str "bb")) (str "c"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let i = int_var "char_concat_i" in
  let n = int_var "char_concat_n" in
  let j = int_var "char_concat_j" in
  let text =
    S.Term.concat
      [S.Term.substr (S.Term.str "ab") i n; S.Term.at (S.Term.str "cd") j]
  in
  S.Context.assert_formula ctx S.Term.(contains text (str "z"));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let i = int_var "char_sat_i" in
  let n = int_var "char_sat_n" in
  S.Context.assert_formula ctx S.Term.(i === Arith.int 1);
  S.Context.assert_formula ctx S.Term.(n === Arith.int 1);
  S.Context.assert_formula ctx
    S.Term.(contains (substr (str "abc") i n) (str "b"));
  assert_check `STATUS_SAT ctx

let test_code_operators () =
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx S.Term.(to_code (str "A") === Arith.int 65);
  S.Context.assert_formula ctx S.Term.(to_code (str "") === Arith.int (-1));
  S.Context.assert_formula ctx S.Term.(to_code (str "ab") === Arith.int (-1));
  S.Context.assert_formula ctx S.Term.(from_code (Arith.int 65) === str "A");
  S.Context.assert_formula ctx S.Term.(from_code (Arith.int (-1)) === str "");
  S.Context.assert_formula ctx S.Term.(from_code (Arith.int 0xD800) === str "");
  S.Context.assert_formula ctx S.Term.(from_code (Arith.int 0x110000) === str "");
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"to_code_len_x" () in
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  S.Context.assert_formula ctx S.Term.(not1 (to_code x === Arith.int (-1)));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"to_code_range_x" () in
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  S.Context.assert_formula ctx S.Term.(Arith.lt (to_code x) (Arith.int 0));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"to_code_surrogate_x" () in
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  S.Context.assert_formula ctx
    S.Term.(
      Arith.geq (to_code x) (Arith.int 0xD800)
      &&& Arith.leq (to_code x) (Arith.int 0xDFFF));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let c = int_var "from_code_valid_c" in
  S.Context.assert_formula ctx S.Term.(c === Arith.int 65);
  S.Context.assert_formula ctx S.Term.(S.Term.len (from_code c) === Arith.int 0);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let c = int_var "from_code_invalid_c" in
  S.Context.assert_formula ctx S.Term.(c === Arith.int (-1));
  S.Context.assert_formula ctx S.Term.(not1 (from_code c === str ""));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  S.Context.assert_formula ctx
    S.Term.(not1 (to_code (from_code (Arith.int 65)) === Arith.int 65));
  assert_check `STATUS_UNSAT ctx

let test_reduction_prioritization () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"priority_replace_all_x" () in
  let needle = S.Term.string_var ~name:"priority_replace_all_needle" () in
  let replacement = S.Term.string_var ~name:"priority_replace_all_replacement" () in
  let result = S.Term.string_var ~name:"priority_replace_all_result" () in
  S.Context.assert_formula ctx S.Term.(x === str "aa");
  S.Context.assert_formula ctx S.Term.(needle === str "a");
  S.Context.assert_formula ctx S.Term.(replacement === str "b");
  S.Context.assert_formula ctx S.Term.(result === str "aa");
  S.Context.assert_formula ctx
    S.Term.(result === replace_all x needle replacement);
  assert_check `STATUS_UNSAT ctx

let test_witness_sharing_aliases () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"witness_alias_x" () in
  let p = S.Term.string_var ~name:"witness_alias_p" () in
  let s = S.Term.string_var ~name:"witness_alias_s" () in
  let needle = S.Term.concat [S.Term.str "b"; S.Term.str "c"] in
  let split = S.Term.concat [p; S.Term.concat [S.Term.str "b"; S.Term.str "c"]; s] in
  S.Context.assert_formula ctx S.Term.(x === split);
  S.Context.assert_formula ctx S.Term.(p === str "a");
  S.Context.assert_formula ctx S.Term.(s === str "d");
  S.Context.assert_formula ctx S.Term.(contains x needle);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "abcd");
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"literal_seed_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abcd");
  S.Context.assert_formula ctx S.Term.(substr x (Arith.int 1) (Arith.int 2) === str "bc");
  S.Context.assert_formula ctx S.Term.(indexof x (str "bc") (Arith.int 0) === Arith.int 1);
  S.Context.assert_formula ctx S.Term.(replace x (str "bc") (str "XY") === str "aXYd");
  S.Context.assert_formula ctx
    S.Term.(replace_all x (str "b") (str "B") === str "aBcd");
  assert_check `STATUS_SAT ctx

let test_regex_range_sat_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_regex_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "b");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.range "a" "c"));
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"stage3_regex_bad_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "b");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.range "a" "a"));
  assert_check `STATUS_UNSAT ctx

let test_regex_literal_length_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_len_literal_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "abc"));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_len_literal_sat_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "abc"));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "abc")

let test_regex_union_length_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_len_union_x" () in
  let regex = S.Regex.union [S.Regex.str "a"; S.Regex.str "bbb"] in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_regex_lower_bound_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_len_lower_bound_x" () in
  let regex = S.Regex.concat [S.Regex.str "abc"; S.Regex.all] in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_len_lower_bound_sat_x" () in
  let regex = S.Regex.concat [S.Regex.str "abc"; S.Regex.all] in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 5);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 5);
  assert (String.starts_with ~prefix:"abc" value)

let test_regex_intersection_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_intersection_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "a"));
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "b"));
  assert_check `STATUS_UNSAT ctx

let test_regex_equality_class_intersection () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_eq_x" () in
  let y = S.Term.string_var ~name:"regex_eq_y" () in
  S.Context.assert_formula ctx S.Term.(x === y);
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "a"));
  S.Context.assert_formula ctx (S.Term.in_re y (S.Regex.str "b"));
  assert_check `STATUS_UNSAT ctx

let test_regex_literal_equality_conflict () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_lit_conflict_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "abc");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "abd"));
  assert_check `STATUS_UNSAT ctx

let test_regex_equality_class_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_eq_witness_x" () in
  let y = S.Term.string_var ~name:"regex_eq_witness_y" () in
  let regex = S.Regex.star (S.Regex.str "aa") in
  S.Context.assert_formula ctx S.Term.(x === y);
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len y === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "aaaa");
  assert (String.equal (Option.get (S.StringModel.find_string model y)) "aaaa")

let test_regex_constraints_not_grouped_without_equality () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_no_eq_x" () in
  let y = S.Term.string_var ~name:"regex_no_eq_y" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "a"));
  S.Context.assert_formula ctx (S.Term.in_re y (S.Regex.str "b"));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "a");
  assert (String.equal (Option.get (S.StringModel.find_string model y)) "b")

let test_regex_direct_negative_deferred () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_direct_neg_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.str "a"));
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "b")));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "a")

let test_regex_fixed_length_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_star_even_x" () in
  let regex = S.Regex.star (S.Regex.str "aa") in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "aaaa");
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_star_odd_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_UNSAT ctx

let test_regex_failed_length_enumerates_to_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_star_ge_x" () in
  let regex = S.Regex.star (S.Regex.str "aa") in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(Arith.geq (S.Term.len x) (Arith.int 3));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value >= 3);
  assert (String.length value mod 2 = 0);
  assert (String.for_all (Char.equal 'a') value)

let test_regex_periodic_length_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_periodic_even_unsat_x" () in
  let regex = S.Regex.star (S.Regex.str "aa") in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_periodic_even_sat_x" () in
  let regex = S.Regex.star (S.Regex.str "aa") in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "aaaa")

let test_regex_semilinear_length_refinement () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_semilinear_gap_x" () in
  let regex =
    S.Regex.star (S.Regex.union [S.Regex.str "aa"; S.Regex.str "bbb"])
  in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_semilinear_sat_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 5);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 5);
  assert (String.equal value "aabbb" || String.equal value "bbbaa");
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_semilinear_gcd_gap_x" () in
  let regex =
    S.Regex.star (S.Regex.union [S.Regex.str "aaaa"; S.Regex.str "bbbbbb"])
  in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 5);
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_semilinear_gcd_sat_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 10);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 10);
  assert (String.equal value "aaaabbbbbb" || String.equal value "bbbbbbaaaa")

let abc_star = S.Regex.star (S.Regex.range "a" "c")

let assert_abc_string value =
  assert (String.for_all (fun ch -> 'a' <= ch && ch <= 'c') value)

let test_regex_prefix_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_prefix_shape_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(prefixof (str "bc") x);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 4);
  assert (String.starts_with ~prefix:"bc" value);
  assert_abc_string value

let test_regex_suffix_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_suffix_shape_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(suffixof (str "bc") x);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 4);
  assert (String.ends_with ~suffix:"bc" value);
  assert_abc_string value

let test_regex_contains_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_contains_shape_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(contains x (str "bc"));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 4);
  assert (contains_text value "bc");
  assert_abc_string value

let test_regex_concat_prefix_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_concat_prefix_shape_x" () in
  let y = S.Term.string_var ~name:"regex_concat_prefix_shape_y" () in
  S.Context.assert_formula ctx S.Term.(x === concat [str "bc"; y]);
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  let y_value = Option.get (S.StringModel.find_string model y) in
  assert (String.length x_value = 4);
  assert (String.starts_with ~prefix:"bc" x_value);
  assert (String.equal x_value ("bc" ^ y_value));
  assert_abc_string x_value

let test_regex_concat_suffix_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_concat_suffix_shape_x" () in
  let y = S.Term.string_var ~name:"regex_concat_suffix_shape_y" () in
  S.Context.assert_formula ctx S.Term.(x === concat [y; str "bc"]);
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 4);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let x_value = Option.get (S.StringModel.find_string model x) in
  let y_value = Option.get (S.StringModel.find_string model y) in
  assert (String.length x_value = 4);
  assert (String.ends_with ~suffix:"bc" x_value);
  assert (String.equal x_value (y_value ^ "bc"));
  assert_abc_string x_value

let test_regex_at_shape_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_at_shape_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x abc_star);
  S.Context.assert_formula ctx S.Term.(at x (Arith.int 1) === str "c");
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 3);
  assert (Char.equal value.[1] 'c');
  assert_abc_string value

let test_regex_negative_all_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_all_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x S.Regex.all);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x S.Regex.all));
  assert_check `STATUS_UNSAT ctx

let test_regex_negative_difference_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_diff_x" () in
  let regex = S.Regex.union [S.Regex.str "a"; S.Regex.str "b"] in
  S.Context.assert_formula ctx (S.Term.in_re x regex);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "a")));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "b")

let test_regex_negative_only_witness () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_only_x" () in
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "a")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.length value = 1);
  assert (not (String.equal value "a"))

let test_regex_negative_range_difference () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_range_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.range "a" "d"));
  S.Context.assert_formula ctx
    S.Term.(not1 (S.Term.in_re x (S.Regex.range "b" "c")));
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  let value = Option.get (S.StringModel.find_string model x) in
  assert (String.equal value "a" || String.equal value "d")

let test_regex_negative_shape_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_shape_x" () in
  let a_prefix = S.Regex.concat [S.Regex.str "a"; S.Regex.all] in
  S.Context.assert_formula ctx S.Term.(prefixof (str "a") x);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x a_prefix));
  assert_check `STATUS_UNSAT ctx

let test_regex_negative_literal_equality_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_neg_lit_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "a");
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "a")));
  assert_check `STATUS_UNSAT ctx

let test_regex_combined_negative_length_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_neg_len_x" () in
  let finite = S.Regex.union [S.Regex.str "a"; S.Regex.str "bb"; S.Regex.str "ccc"] in
  S.Context.assert_formula ctx (S.Term.in_re x finite);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "bb")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_regex_combined_negative_length_sat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_neg_len_sat_x" () in
  let finite = S.Regex.union [S.Regex.str "a"; S.Regex.str "bb"; S.Regex.str "ccc"] in
  S.Context.assert_formula ctx (S.Term.in_re x finite);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "bb")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 3);
  assert_check `STATUS_SAT ctx;
  let model = S.Context.get_model ctx in
  assert (String.equal (Option.get (S.StringModel.find_string model x)) "ccc")

let test_regex_combined_negative_range_length_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_neg_range_len_x" () in
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.range "a" "d"));
  S.Context.assert_formula ctx
    S.Term.(not1 (S.Term.in_re x (S.Regex.range "b" "c")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_regex_combined_shape_negative_length_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_shape_neg_len_x" () in
  let finite = S.Regex.union [S.Regex.str "a"; S.Regex.str "ab"; S.Regex.str "ba"] in
  S.Context.assert_formula ctx (S.Term.in_re x finite);
  S.Context.assert_formula ctx S.Term.(prefixof (str "a") x);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "ab")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_regex_combined_literal_negative_length_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_lit_neg_len_x" () in
  let finite = S.Regex.union [S.Regex.str "a"; S.Regex.str "bb"] in
  S.Context.assert_formula ctx (S.Term.in_re x finite);
  S.Context.assert_formula ctx S.Term.(x === str "a");
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x (S.Regex.str "bb")));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 2);
  assert_check `STATUS_UNSAT ctx

let test_regex_combined_negative_all_removed_unsat () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_combined_neg_empty_x" () in
  let finite = S.Regex.union [S.Regex.str "a"; S.Regex.str "bb"] in
  S.Context.assert_formula ctx (S.Term.in_re x finite);
  S.Context.assert_formula ctx S.Term.(not1 (S.Term.in_re x finite));
  S.Context.assert_formula ctx S.Term.(S.Term.len x === Arith.int 1);
  assert_check `STATUS_UNSAT ctx

let test_regex_stage_g_constructors () =
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_stage_g_inter_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "b");
  S.Context.assert_formula ctx
    (S.Term.in_re x
       (S.Regex.inter [S.Regex.range "a" "c"; S.Regex.range "b" "d"]));
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_stage_g_comp_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "a");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.comp (S.Regex.str "a")));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_stage_g_plus_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "aaa");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.plus (S.Regex.str "a")));
  assert_check `STATUS_SAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_stage_g_opt_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "aa");
  S.Context.assert_formula ctx (S.Term.in_re x (S.Regex.opt (S.Regex.str "a")));
  assert_check `STATUS_UNSAT ctx;
  with_context @@ fun ctx ->
  let x = S.Term.string_var ~name:"regex_stage_g_loop_x" () in
  S.Context.assert_formula ctx S.Term.(x === str "aaa");
  S.Context.assert_formula ctx
    (S.Term.in_re x (S.Regex.loop ~lo:2 ~hi:4 (S.Regex.str "a")));
  assert_check `STATUS_SAT ctx

let test () =
  print_endline "Stage 3 string extension tests";
  test_literal_length ();
  test_literal_length_unsat ();
  test_unicode_scalar_length ();
  test_invalid_utf8 ();
  test_variable_forced_to_literal ();
  test_distinct_literals_unsat ();
  test_ground_concat ();
  test_ground_concat_unsat ();
  test_concat_suffix_refinement_sat ();
  test_concat_interior_refinement_sat ();
  test_model_support_filter ();
  test_concat_incompatible_suffix_unsat ();
  test_concat_assignment_conflict_unsat ();
  test_length_filler_model ();
  test_satisfiable_disequality ();
  test_negated_literal_equality_sat ();
  test_multi_unknown_concat_unknown ();
  test_contains_sat ();
  test_contains_symbolic_witness_sat ();
  test_contains_refinement_unsat ();
  test_not_contains_sat ();
  test_substr_sat_unsat ();
  test_substr_symbolic_witness_sat ();
  test_indexof_sat_unsat ();
  test_indexof_symbolic_witness_sat ();
  test_replace_prefix_suffix_at ();
  test_replace_symbolic_witness_sat ();
  test_replace_all_ground_semantics ();
  test_replace_all_symbolic_witness_sat ();
  test_replace_all_fixed_unsat ();
  test_rewrite_simplification_axioms ();
  test_containment_abstraction ();
  test_character_abstraction ();
  test_code_operators ();
  test_reduction_prioritization ();
  test_witness_sharing_aliases ();
  test_regex_range_sat_unsat ();
  test_regex_literal_length_refinement ();
  test_regex_union_length_refinement ();
  test_regex_lower_bound_refinement ();
  test_regex_intersection_refinement ();
  test_regex_equality_class_intersection ();
  test_regex_literal_equality_conflict ();
  test_regex_equality_class_witness ();
  test_regex_constraints_not_grouped_without_equality ();
  test_regex_direct_negative_deferred ();
  test_regex_fixed_length_witness ();
  test_regex_failed_length_enumerates_to_sat ();
  test_regex_periodic_length_refinement ();
  test_regex_semilinear_length_refinement ();
  test_regex_prefix_shape_witness ();
  test_regex_suffix_shape_witness ();
  test_regex_contains_shape_witness ();
  test_regex_concat_prefix_shape_witness ();
  test_regex_concat_suffix_shape_witness ();
  test_regex_at_shape_witness ();
  test_regex_negative_all_unsat ();
  test_regex_negative_difference_witness ();
  test_regex_negative_only_witness ();
  test_regex_negative_range_difference ();
  test_regex_negative_shape_unsat ();
  test_regex_negative_literal_equality_unsat ();
  test_regex_combined_negative_length_unsat ();
  test_regex_combined_negative_length_sat ();
  test_regex_combined_negative_range_length_unsat ();
  test_regex_combined_shape_negative_length_unsat ();
  test_regex_combined_literal_negative_length_unsat ();
  test_regex_combined_negative_all_removed_unsat ();
  test_regex_stage_g_constructors ();
  print_endline "Done with Stage 3 string extension tests"
