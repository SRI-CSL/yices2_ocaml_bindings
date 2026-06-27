module A = Extensions.Regex_automata

let expect_some = function
  | Some value -> value
  | None -> assert false

let compile regex =
  match A.compile regex with
  | Ok automaton -> automaton
  | Error msg -> failwith msg

let assert_accepts automaton text =
  assert (A.accepts automaton text)

let assert_rejects automaton text =
  assert (not (A.accepts automaton text))

let assert_witness_length automaton length =
  let witness = expect_some (A.witness_of_length automaton length) in
  assert_accepts automaton witness;
  assert (A.scalar_length witness = Ok length);
  witness

let assert_compile_error regex =
  match A.compile regex with
  | Ok _ -> assert false
  | Error _ -> ()

let test_literal_and_range () =
  let abc = compile (A.Lit "abc") in
  assert_accepts abc "abc";
  assert_rejects abc "ab";
  assert_rejects abc "abcd";
  assert (String.equal (assert_witness_length abc 3) "abc");
  assert (A.witness_of_length abc 2 = None);
  let upper = compile (A.Range (Char.code 'A', Char.code 'Z')) in
  assert_accepts upper "A";
  assert_accepts upper "Z";
  assert_rejects upper "a"

let test_star_fixed_length_witnesses () =
  let aa_star = compile (A.Star (A.Lit "aa")) in
  ignore (assert_witness_length aa_star 0);
  ignore (assert_witness_length aa_star 2);
  ignore (assert_witness_length aa_star 4);
  assert (A.witness_of_length aa_star 1 = None);
  assert (A.witness_of_length aa_star 3 = None)

let test_all_and_allchar () =
  let allchar = compile A.AllChar in
  assert_accepts allchar "A";
  assert_accepts allchar "\206\187";
  assert_rejects allchar "";
  assert_rejects allchar "ab";
  let all = compile A.All in
  assert_accepts all "";
  assert_accepts all "abc";
  ignore (assert_witness_length all 8)

let test_combinators () =
  let abcd = compile (A.Concat [A.Lit "ab"; A.Lit "cd"]) in
  assert_accepts abcd "abcd";
  assert_rejects abcd "ab";
  assert_rejects abcd "cd";
  let a_or_b = compile (A.Union [A.Lit "a"; A.Lit "b"]) in
  assert_accepts a_or_b "a";
  assert_accepts a_or_b "b";
  assert_rejects a_or_b "";
  assert_rejects a_or_b "c";
  let one_two_three =
    compile (A.Union [A.Lit "a"; A.Union [A.Lit "bb"; A.Lit "ccc"]])
  in
  assert_accepts one_two_three "ccc";
  assert (String.equal (assert_witness_length one_two_three 3) "ccc")

let test_epsilon_heavy () =
  let eps_star = compile (A.Star (A.Lit "")) in
  assert_accepts eps_star "";
  assert_rejects eps_star "a";
  assert (A.witness_of_length eps_star 0 = Some "");
  assert (A.witness_of_length eps_star 1 = None);
  let a_or_eps = compile (A.Union [A.Lit "a"; A.Lit ""]) in
  assert_accepts a_or_eps "";
  assert_accepts a_or_eps "a";
  assert_rejects a_or_eps "aa";
  assert (A.has_length a_or_eps 0);
  assert (A.has_length a_or_eps 1);
  assert (not (A.has_length a_or_eps 2));
  let a_or_eps_star = compile (A.Star (A.Union [A.Lit "a"; A.Lit ""])) in
  let aa = compile (A.Lit "aa") in
  let intersection =
    match A.intersect a_or_eps_star aa with
    | Ok automaton -> automaton
    | Error msg -> failwith msg
  in
  assert_accepts intersection "aa";
  assert_rejects intersection "a";
  assert (String.equal (assert_witness_length intersection 2) "aa")

let test_intersection () =
  let aa_star = compile (A.Star (A.Lit "aa")) in
  let lower =
    compile (A.Concat [
        A.Range (Char.code 'a', Char.code 'z');
        A.Range (Char.code 'a', Char.code 'z');
        A.Range (Char.code 'a', Char.code 'z');
        A.Range (Char.code 'a', Char.code 'z');
      ])
  in
  let intersection =
    match A.intersect aa_star lower with
    | Ok automaton -> automaton
    | Error msg -> failwith msg
  in
  assert (not (A.is_empty intersection));
  let witness = assert_witness_length intersection 4 in
  assert (String.equal witness "aaaa");
  assert (A.witness_of_length intersection 2 = None)

let test_empty_intersection () =
  let a = compile (A.Lit "a") in
  let b = compile (A.Lit "b") in
  let intersection =
    match A.intersect a b with
    | Ok automaton -> automaton
    | Error msg -> failwith msg
  in
  assert (A.is_empty intersection);
  assert (A.witness intersection = None)

let test_length_domain () =
  let abc = compile (A.Lit "abc") in
  assert (A.length_domain abc = A.Length_finite [3]);
  let union = compile (A.Union [A.Lit "a"; A.Lit "bbb"]) in
  assert (A.length_domain union = A.Length_finite [1; 3]);
  let aa_star = compile (A.Star (A.Lit "aa")) in
  assert (A.length_domain aa_star = A.Length_periodic { base = []; threshold = 0; period = 2 });
  let aa_or_bbb_star =
    compile (A.Star (A.Union [A.Lit "aa"; A.Lit "bbb"]))
  in
  assert
    (A.length_domain aa_or_bbb_star
     = A.Length_periodic { base = [0]; threshold = 2; period = 1 });
  let four_six_star =
    compile (A.Star (A.Union [A.Lit "aaaa"; A.Lit "bbbbbb"]))
  in
  assert
    (A.length_domain four_six_star
     = A.Length_periodic { base = [0]; threshold = 4; period = 2 })

let shape name result =
  match result with
  | Ok automaton -> automaton
  | Error msg -> failwith (name ^ ": " ^ msg)

let test_shape_constructors () =
  let exact = shape "exact" (A.exact "abc") in
  assert_accepts exact "abc";
  assert_rejects exact "ab";
  assert_rejects exact "abcd";
  let prefix = shape "prefix" (A.prefix "ab") in
  assert_accepts prefix "ab";
  assert_accepts prefix "abcd";
  assert_rejects prefix "a";
  assert_rejects prefix "ba";
  let suffix = shape "suffix" (A.suffix "cd") in
  assert_accepts suffix "cd";
  assert_accepts suffix "abcd";
  assert_rejects suffix "c";
  assert_rejects suffix "dc";
  let contains = shape "contains" (A.contains "bc") in
  assert_accepts contains "bc";
  assert_accepts contains "abcd";
  assert_rejects contains "acbd";
  let fixed = shape "fixed_position" (A.fixed_position ~index:1 ~scalar:(Char.code 'z')) in
  assert_accepts fixed "az";
  assert_accepts fixed "azzz";
  assert_rejects fixed "za";
  assert_rejects fixed "a";
  assert (A.fixed_position ~index:(-1) ~scalar:(Char.code 'a') = Error "fixed-position index is negative")

let test_complement_and_difference () =
  let abc = compile (A.Range (Char.code 'a', Char.code 'c')) in
  let not_abc = shape "complement" (A.complement abc) in
  assert_accepts not_abc "";
  assert_accepts not_abc "d";
  assert_accepts not_abc "aa";
  assert_rejects not_abc "a";
  assert_rejects not_abc "b";
  assert_rejects not_abc "c";
  let ad = compile (A.Range (Char.code 'a', Char.code 'd')) in
  let bc = compile (A.Range (Char.code 'b', Char.code 'c')) in
  let diff = shape "difference" (A.difference ad bc) in
  assert_accepts diff "a";
  assert_accepts diff "d";
  assert_rejects diff "";
  assert_rejects diff "b";
  assert_rejects diff "c";
  assert_rejects diff "e";
  let overlap_left = compile (A.Union [A.Range (Char.code 'a', Char.code 'c'); A.Range (Char.code 'f', Char.code 'h')]) in
  let overlap_right = compile (A.Range (Char.code 'b', Char.code 'g')) in
  let overlap_diff = shape "overlap difference" (A.difference overlap_left overlap_right) in
  assert_accepts overlap_diff "a";
  assert_accepts overlap_diff "h";
  assert_rejects overlap_diff "b";
  assert_rejects overlap_diff "c";
  assert_rejects overlap_diff "f";
  assert_rejects overlap_diff "g";
  let all = compile A.All in
  let none = shape "all minus all" (A.difference all all) in
  assert (A.is_empty none)

let test_combined_length_domain () =
  let one_two_three =
    compile (A.Union [A.Lit "a"; A.Lit "bb"; A.Lit "ccc"])
  in
  let without_two = shape "finite difference domain" (A.difference one_two_three (compile (A.Lit "bb"))) in
  assert (A.length_domain without_two = A.Length_finite [1; 3]);
  let range_diff =
    shape
      "range difference domain"
      (A.difference
         (compile (A.Range (Char.code 'a', Char.code 'd')))
         (compile (A.Range (Char.code 'b', Char.code 'c'))))
  in
  assert (A.length_domain range_diff = A.Length_finite [1]);
  let empty = shape "empty difference domain" (A.difference one_two_three one_two_three) in
  assert (A.length_domain empty = A.Length_empty);
  let infinite = shape "infinite difference domain" (A.difference (compile A.All) (compile (A.Lit "a"))) in
  assert (A.length_domain infinite = A.Length_periodic { base = []; threshold = 0; period = 1 });
  let even_intersection =
    shape
      "cyclic intersection domain"
      (A.intersect (compile (A.Star (A.Lit "aa"))) (compile A.All))
  in
  assert
    (A.length_domain even_intersection
     = A.Length_periodic { base = []; threshold = 0; period = 2 })

let test_stage_g_regex_constructs () =
  let inter = compile (A.Inter [A.Range (Char.code 'a', Char.code 'c'); A.Range (Char.code 'b', Char.code 'd')]) in
  assert_accepts inter "b";
  assert_accepts inter "c";
  assert_rejects inter "a";
  assert_rejects inter "d";
  let comp = compile (A.Comp (A.Lit "a")) in
  assert_accepts comp "";
  assert_accepts comp "b";
  assert_rejects comp "a";
  let plus = compile (A.Plus (A.Lit "a")) in
  assert_rejects plus "";
  assert_accepts plus "a";
  assert_accepts plus "aaa";
  let opt = compile (A.Opt (A.Lit "a")) in
  assert_accepts opt "";
  assert_accepts opt "a";
  assert_rejects opt "aa";
  let loop = compile (A.Loop (A.Lit "a", 2, 4)) in
  assert_rejects loop "a";
  assert_accepts loop "aa";
  assert_accepts loop "aaaa";
  assert_rejects loop "aaaaa"

let test_errors () =
  let abc = compile (A.Lit "abc") in
  assert_rejects abc "\255\254";
  assert_compile_error (A.Lit "\255");
  assert_compile_error (A.Range (0xD800, 0xDFFF));
  assert_compile_error (A.Range (10, 5))

let test_expected_results_corpus () =
  let cases = [
    A.Lit "abc", "", false;
    A.Lit "abc", "abc", true;
    A.Lit "abc", "abcd", false;
    A.Range (Char.code 'a', Char.code 'c'), "a", true;
    A.Range (Char.code 'a', Char.code 'c'), "b", true;
    A.Range (Char.code 'a', Char.code 'c'), "d", false;
    A.Concat [A.Lit "a"; A.Star (A.Lit "b")], "a", true;
    A.Concat [A.Lit "a"; A.Star (A.Lit "b")], "abbb", true;
    A.Concat [A.Lit "a"; A.Star (A.Lit "b")], "ba", false;
    A.Union [A.Lit "a"; A.Lit "bbb"], "a", true;
    A.Union [A.Lit "a"; A.Lit "bbb"], "bbb", true;
    A.Union [A.Lit "a"; A.Lit "bbb"], "bb", false;
    A.Star (A.Lit "aa"), "", true;
    A.Star (A.Lit "aa"), "aa", true;
    A.Star (A.Lit "aa"), "aaa", false;
    A.AllChar, "\206\187", true;
    A.AllChar, "\206\187x", false;
    A.All, "", true;
    A.All, "abc\206\187", true;
  ] in
  List.iter
    (fun (regex, text, expected) ->
       let automaton = compile regex in
       assert (Bool.equal (A.accepts automaton text) expected))
    cases

let test () =
  print_endline "Regex automata test";
  test_literal_and_range ();
  test_star_fixed_length_witnesses ();
  test_all_and_allchar ();
  test_combinators ();
  test_epsilon_heavy ();
  test_intersection ();
  test_empty_intersection ();
  test_length_domain ();
  test_shape_constructors ();
  test_complement_and_difference ();
  test_combined_length_domain ();
  test_stage_g_regex_constructs ();
  test_errors ();
  test_expected_results_corpus ();
  print_endline "Regex automata test completed"
