open High

module EH = Make(ExceptionsErrorHandling)

let () =
  let open EH in
  let open Global in
  print_endline("Initialising Yices version "^version);
  init();
  print_endline "Init done";
  let config = Config.malloc () in
  print_endline "New config done";
  let _ = Config.default config ~logic:"QF_BV" in
  print_endline "Set config done";
  let context = Context.malloc config in
  print_endline "New context done";
  let param = Param.malloc() in
  print_endline "New param done";
  Param.default context param;
  print_endline "Set param done";
  begin
    match Context.check context param with
    | `STATUS_SAT   -> print_endline "SAT"
    | `STATUS_UNSAT -> print_endline "UNSAT"
    | _ -> print_endline "other"
  end;
  print_endline "Adding assertion \"false\"";
  let _ = Context.assert_formula context (Term.yfalse()) in
  begin
    match Context.check context param with
    | `STATUS_SAT   -> print_endline "SAT"
    | `STATUS_UNSAT -> print_endline "UNSAT"
    | _ -> print_endline "other"
  end;
  Param.free param;
  Context.free context;
  Config.free config;
  exit();
  print_endline "Exited gracefully"
