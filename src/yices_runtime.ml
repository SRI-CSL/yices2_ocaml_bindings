open Ctypes
open Foreign

let () =
  let open High in
  let open Global in
  init();
  let config = Config.new_config () in
  let _ = Config.default_config_for_logic config ~logic:"QF_BV" in
  let context = new_context config in
  let param = new_param_record() in
  default_params_for_context context param;
  begin
    match check_context context param with
    | `STATUS_SAT   -> print_endline "SAT"
    | `STATUS_UNSAT -> print_endline "UNSAT"
    | _ -> print_endline "other"
  end;
  free_param_record param;
  free_context context;
  Config.free_config config;
  exit();
  print_endline version
