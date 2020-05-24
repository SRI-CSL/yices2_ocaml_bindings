open Ocamlbuild_plugin

let gmp_true env _build =
  let arg = env "src/gmp.mlh" in
  tag_any ["gmp"];
  Echo(["[%%define gmp_present true]"], arg)

let gmp_false env _build =
  let arg = env "src/gmp.mlh" in
  print_endline "Warning: gmp not detected";
  Echo(["[%%define gmp_present false]"], arg)

let gmp_test env _build =
  let arg = env "src/gmp.c" in
  Echo(["#include <gmp.h>"], arg)

let meta_gmp _env _build =
  Cmd(S[A "cp"; P "src/META_gmp"; Px "src/META"])

let meta_nogmp _env _build =
  Cmd(S[A "cp"; P "src/META_nogmp"; Px "src/META"])

let () = dispatch begin function
    | After_rules ->
      rule "gmp test"  ~prod:"src/gmp.c" ~insert:(`top) gmp_test;
      rule "gmp false" ~prod:"src/gmp.mlh" ~insert:(`top) gmp_false;
      rule "gmp true"  ~prod:"src/gmp.mlh" ~dep:"src/gmp.o" ~insert:(`top) gmp_true;
      rule "meta nogmp"  ~prod:"src/META" ~dep:"src/META_nogmp" ~insert:(`top) meta_nogmp;
      rule "meta gmp"    ~prod:"src/META" ~deps:["src/gmp.o";"src/META_gmp"] ~insert:(`top) meta_gmp;
      dep ["ocaml"; "ocamldep"] ["src/gmp.mlh"];
      dep ["ocaml"; "compile"] ["src/gmp.mlh"];
      flag ["ocaml"; "compile"; "gmp"] (S [A "-package"; A "zarith"; A "-package"; A "ctypes-zarith"])
    | _ -> ()
  end
