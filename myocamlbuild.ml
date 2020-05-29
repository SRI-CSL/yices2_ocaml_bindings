open Ocamlbuild_plugin

let gmp_true path env _build =
  let arg = env (Pathname.concat path "gmp.mlh") in
  tag_any ["gmp"];
  Echo(["[%%define gmp_present true]"], arg)

let gmp_false path env _build =
  let arg = env (Pathname.concat path "gmp.mlh") in
  print_endline "Warning: gmp not detected";
  Echo(["[%%define gmp_present false]"], arg)

let gmp_test env _build =
  let arg = env "gmp.c" in
  Echo(["#include <gmp.h>"], arg)

let meta_gmp _env _build =
  Cmd(S[A "cp"; P "src/META_gmp"; Px "src/META"])

let meta_nogmp _env _build =
  Cmd(S[A "cp"; P "src/META_nogmp"; Px "src/META"])

let () = dispatch begin function
    | After_rules ->
      rule "gmp test"  ~prod:"gmp.c" ~insert:(`top) gmp_test;
      rule "src/gmp false" ~prod:"src/gmp.mlh" ~insert:(`top) (gmp_false "src");
      rule "src/gmp true"  ~prod:"src/gmp.mlh" ~dep:"gmp.o" ~insert:(`top) (gmp_true "src");
      rule "src_tests/gmp false" ~prod:"src_tests/gmp.mlh" ~insert:(`top) (gmp_false "src_tests");
      rule "src_tests/gmp true"  ~prod:"src_tests/gmp.mlh" ~dep:"gmp.o" ~insert:(`top) (gmp_true "src_tests");
      rule "meta nogmp"  ~prod:"src/META" ~dep:"src/META_nogmp" ~insert:(`top) meta_nogmp;
      rule "meta gmp"    ~prod:"src/META" ~deps:["src/gmp.o";"src/META_gmp"] ~insert:(`top) meta_gmp;
      dep ["ocaml"; "ocamldep"; "main"] ["src/gmp.mlh"];
      dep ["ocaml"; "compile"; "main"] ["src/gmp.mlh"];
      dep ["ocaml"; "ocamldep"; "tests"] ["src_tests/gmp.mlh"];
      dep ["ocaml"; "compile"; "tests"] ["src_tests/gmp.mlh"];
      flag ["ocaml"; "compile"; "gmp"] (S [A "-package"; A "zarith"; A "-package"; A "ctypes-zarith"]);
      flag ["ocaml"; "link"; "gmp"] (S [A "-package"; A "zarith"; A "-package"; A "ctypes-zarith"])
    | _ -> ()
  end
