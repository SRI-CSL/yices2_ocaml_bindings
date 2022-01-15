[%%import "gmp.mlh"]

open Yices2.High

let () = Printexc.record_backtrace true
    
module EH1 = Make(ExceptionsErrorHandling)

let () =
  print_endline "First test, using exceptions for error handling";
  let open EH1 in
  let open Global in
  print_endline("Initialising Yices version "^version);
  init();
  print_endline "Init done";
  let config = Config.malloc () in
  print_endline "New config done";
  let _ = Config.default config ~logic:"QF_BV" in
  print_endline "Set config done";
  let context = Context.malloc ~config () in
  print_endline "New context done";
  let param = Param.malloc() in
  print_endline "New param done";
  Param.default context param;
  print_endline "Set param done";
  let status = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  print_endline "Adding assertion \"false\"";
  let () = Context.assert_formula context (Term.false0()) in
  let status = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  Param.free param;
  Context.free context;
  Config.free config;
  exit();
  print_endline "Exited gracefully\n"

module EH2 = Make(SumErrorHandling)

let _ =
  print_endline "Second test, using Result monad for error handling";
  let (let+) = SumErrorHandling.bind in
  let open EH2 in
  let open Global in
  let+ s = version in
  print_endline("Initialising Yices version "^s);
  init();
  print_endline "Init done";
  let+ config  = Config.malloc () in
  print_endline "New config done";
  let+ ()      = Config.default config ~logic:"QF_BV" in
  print_endline "Set config done";
  let+ context = Context.malloc ~config () in
  print_endline "New context done";
  let+ param   = Param.malloc() in
  print_endline "New param done";
  Param.default context param;
  print_endline "Set param done";
  let status   = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  print_endline "Adding assertion \"false\"";
  let+ formula = Term.false0() in
  let+ ()      = Context.assert_formula context formula in
  let status   = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  Param.free param;
  Context.free context;
  Config.free config;
  exit();
  print_endline "Exited gracefully";
  SumErrorHandling.return()

let () = print_endline ""
let () = Context_test.test_context()
let () = print_endline ""
let () = Context_test.test_ext_context()
let () = print_endline ""
let () = Error_test.test()
let () = print_endline ""
let () = Types_test.test()
let () = print_endline ""
let () =
  try
    Terms_test.test()
  with
    Yices2.High.ExceptionsErrorHandling.YicesException _ ->
    print_endline (EH1.ErrorPrint.string())
[%%if gmp_present]
let () =
  try
    Terms_test.test_gmp()
  with
    Yices2.High.ExceptionsErrorHandling.YicesException _ ->
    print_endline (EH1.ErrorPrint.string())
[%%else]
let () = print_endline ""
let () = print_endline "Warning: gmp not present; so gmp-dependent tests were skipped."
[%%endif]

let () = print_endline ""
let () = Experiments.test()
