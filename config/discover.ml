module C = Configurator.V1

let test t =
  let b = C.c_test t ~c_flags:["-c"] "#include <gmp.h>" in
  if b && not Gmp.present
  then
    print_endline "Warning: you have gmp but you do not have the package ctypes-zarith; we strongly recommend you install ctypes-zarith to benefit from all of Yices' features, for instance by running 'opam install ctypes-zarith'"
         
let () = C.main ~name:"gmp_test" test
