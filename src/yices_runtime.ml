open Ctypes
open Foreign

let () =
  let open High in
  let open Global in
  init();
  exit();
  print_endline version
