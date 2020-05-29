open Ocamlbuild_plugin

let gmp_test env _build =
  let arg = env "gmp.c" in
  Echo(["#include <gmp.h>"], arg)

let gmp_false env _build =
  let arg = env "%gmp.mlh" in
  Echo(["[%%define gmp_present false]"], arg)

let gmp_true env _build =
  let arg = env "%/gmp.mlh" in
  Echo(["[%%define gmp_present true]"], arg)

let meta_nogmp _env _build =
  Cmd(S[A "cp"; P "src/META_nogmp"; Px "src/META"])

let meta_gmp _env _build =
  Cmd(S[A "cp"; P "src/META_gmp"; Px "src/META"])

let test_nogmp env _build =
  print_endline "Warning: gmp not detected";
  Nop

let test_gmp env _build =
  tag_any ["gmp"];
  Nop

let () = dispatch begin function
    | After_rules ->
      rule "gmp test"      ~prod:"gmp.c"                  ~insert:(`top) gmp_test;
      rule "gmp.mlh false" ~prod:"%/gmp.mlh"              ~insert:(`top) gmp_false;
      rule "gmp.mlh true"  ~prod:"%/gmp.mlh" ~dep:"gmp.o" ~insert:(`top) gmp_true;
      rule "meta nogmp"  ~prod:"src/META" ~dep:"src/META_nogmp"          ~insert:(`top) meta_nogmp;
      rule "meta gmp"    ~prod:"src/META" ~deps:["gmp.o";"src/META_gmp"] ~insert:(`top) meta_gmp;
      rule "test_gmp false" ~prod:"test_gmp"              ~insert:(`top) test_nogmp;
      rule "test_gmp true"  ~prod:"test_gmp" ~dep:"gmp.o" ~insert:(`top) test_gmp;
      dep ["ocaml"; "compile"; "main"]   ["src/gmp.mlh"];
      dep ["ocaml"; "ocamldep"; "main"]  ["src/gmp.mlh"];
      dep ["ocaml"; "compile"; "tests"]  ["src_tests/gmp.mlh"];
      dep ["ocaml"; "ocamldep"; "tests"] ["src_tests/gmp.mlh"];
      let gmp_options = (S [A "-package"; A "zarith"; A "-package"; A "ctypes-zarith"]) in
      flag ["ocaml"; "compile"; "gmp"] gmp_options;
      flag ["ocaml"; "link"; "gmp"] gmp_options;
    | _ -> ()
  end
