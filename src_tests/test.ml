open Yices2.High

let () = Printexc.record_backtrace true

(* Diagnostic instrumentation for the intermittent
   "Failed to reset signal stack (err 22)" exit crash. See sigalt_probe.c. *)
external sigalt_install_probe : unit -> unit = "caml_sigalt_install_probe"
external sigalt_query         : string -> unit = "caml_sigalt_query"
external sigalt_segv_count    : unit -> int = "caml_sigalt_segv_count"
external sigalt_scan          : string -> unit = "caml_sigalt_scan_onstack"

let cp label = sigalt_query label; sigalt_scan label

let () = sigalt_scan "pre-install"
let () = if Sys.getenv_opt "YICES_SIGALT_NOPROBE" = None then sigalt_install_probe ()
let () = cp "startup"
let () = at_exit (fun () ->
  Printf.eprintf "[sigalt] total faults during run = %d\n%!" (sigalt_segv_count ());
  cp "atexit")
    
module EH1 = Make(ExceptionsErrorHandling)

let () =
  match Sys.getenv_opt "YICES_FREEVAR_STRESS" with
  | Some _ -> (try Context_test.stress_freevar () with Exit -> ()); cp "after-stress"; exit 0
  | None -> ()

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
  Context.default_param context param;
  print_endline "Set param done";
  let status = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  print_endline "Adding assertion \"false\"";
  let () = Context.assert_formula context (Term.false0()) in
  let status = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  exit();
  print_endline "Exited gracefully\n"

let () = cp "after-test1(exit-called)"

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
  Context.default_param context param;
  print_endline "Set param done";
  let status   = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  print_endline "Adding assertion \"false\"";
  let+ formula = Term.false0() in
  let+ ()      = Context.assert_formula context formula in
  let status   = Context.check context ~param in
  print_endline(Types.show_smt_status status);
  exit();
  print_endline "Exited gracefully";
  SumErrorHandling.return()

let () = cp "after-test2(exit-called)"
let () = print_endline ""
let () = Context_test.test_context()
let () = cp "after-test_context"
let () = print_endline ""
let () = Context_test.test_ext_context()
let () = cp "after-test_ext_context"
let () = print_endline ""
let () =
  (* Minimal sanity check for funptr-based callback registration *)
  print_endline "Funptr test: registering out-of-memory callback";
  let callback = Funptr_test.test_out_of_mem_callback () in
  Yices2.Low.yices_set_out_of_mem_callback callback;
  print_endline "Funptr test completed";
  print_endline ""
let () = cp "after-funptr"

(* let () = Context_test.test_lfun() *)
(* let () = print_endline "" *)

(* let () = Context_test.test_mcsat_arrays() *)
(* let () = print_endline "" *)

let () =
  try
    Context_test.test_tupleblast()
  with
  | Yices2.High.ExceptionsErrorHandling.YicesException _ as ex ->
      Printf.printf "tupleblast error: %s\n%!"
        (EH1.ErrorPrint.string ());
      raise ex
let () = cp "after-tupleblast"
let () = print_endline ""

let () = Error_test.test()
let () = cp "after-error"
let () = print_endline ""
let () = Types_test.test()
let () = cp "after-types"
let () = print_endline ""
let () = Terms_test.test()
let () = cp "after-terms"
let () =
  try
    Terms_test.test_gmp()
  with
    Yices2.High.ExceptionsErrorHandling.YicesException _ ->
    print_endline (EH1.ErrorPrint.string())
let () = cp "after-test_gmp"

let () = print_endline ""
let () = Experiments.test()
let () = cp "after-experiments"

let () = cp "before-smodel"
let () = print_endline ""
let () = Smodel_test.test()

let () = cp "before-lifecycle"
let () = print_endline ""
let () = Lifecycle_test.test()
let () = cp "after-all-tests"
