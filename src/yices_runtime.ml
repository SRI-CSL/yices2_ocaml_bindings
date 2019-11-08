open Ctypes
open Foreign

open High

let () =
  let open Global in
  print_endline("Initialising Yices version "^version);
  init();
  print_endline "Init done";
  let config = Config.new_config () in
  print_endline "New config done";
  let _ = Config.default_config_for_logic config ~logic:"QF_BV" in
  print_endline "Set config done";
  let context = new_context config in
  print_endline "New context done";
  let param = new_param_record() in
  print_endline "New param done";
  default_params_for_context context param;
  print_endline "Set param done";
  begin
    match check_context context param with
    | `STATUS_SAT   -> print_endline "SAT"
    | `STATUS_UNSAT -> print_endline "UNSAT"
    | _ -> print_endline "other"
  end;
  print_endline "Adding assertion \"false\"";
  let _ = assert_formula context (Term.yices_false()) in
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
  print_endline "Exited gracefully"
