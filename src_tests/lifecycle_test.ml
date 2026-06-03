open Yices2.High

module EH1 = Make(ExceptionsErrorHandling)
module EH2 = Make(SumErrorHandling)

(* Regression tests for the Runtime_lifecycle finalizer guard.

   Each test forces finalizers to run after a lifecycle transition
   (exit / reset / re-init). Without the generation guard these would
   double-free Yices-owned pointers and corrupt the runtime. With the
   guard the finalizers must be inert, so the tests should complete and
   the process should exit cleanly.

   Allocation happens inside [@inline never] helpers so the allocated
   values are not lexical bindings kept live across the forced GC. *)

(* Test 1: allocate config/context/param/model, exit, then force GC.
   The finalizers fire after exit and must be inert. *)
let[@inline never] allocate_and_exit () =
  let open EH1 in
  let cfg   = Config.malloc () in
  let ctx   = Context.malloc ~config:cfg () in
  let param = Param.malloc () in
  let model = Model.empty () in
  ignore (cfg, ctx, param, model);
  Global.exit ()

let test_exit () =
  print_endline "Lifecycle test 1: finalizers after exit";
  EH1.Global.init ();
  allocate_and_exit ();
  Gc.full_major ();
  Gc.compact ();
  print_endline "  ok"

(* Test 2: generation-mismatch case. Keep the stale values reachable across
   the second init (via [stash]), then drop them so they are finalized while
   a *new* generation is alive. This exercises the gen <> current_generation
   branch of the guard, not just the alive=false branch. *)
let stash : (EH1.Config.t * EH1.Context.t) option ref = ref None

let[@inline never] allocate_stale () =
  let open EH1 in
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  stash := Some (cfg, ctx)

let test_reinit () =
  print_endline "Lifecycle test 2: stale finalizers after re-init (generation mismatch)";
  EH1.Global.init ();
  allocate_stale ();
  EH1.Global.exit ();

  EH1.Global.init ();
  stash := None;
  Gc.full_major ();
  Gc.compact ();
  EH1.Global.exit ();
  print_endline "  ok"

(* Test 3: same as test 1 but with reset (yices_exit + yices_init) instead
   of exit. Pre-reset objects must be finalized inertly. *)
let[@inline never] allocate_and_reset () =
  let open EH1 in
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  ignore (cfg, ctx);
  Global.reset ()

let test_reset () =
  print_endline "Lifecycle test 3: finalizers after reset";
  EH1.Global.init ();
  allocate_and_reset ();
  Gc.full_major ();
  Gc.compact ();
  EH1.Global.exit ();
  print_endline "  ok"

(* Test 4: cross-functor. Allocate through the exceptions functor, exit
   through the Result functor. This verifies the lifecycle guard is shared
   outside the High.Make functor. *)
let[@inline never] allocate_via_eh1 () =
  let open EH1 in
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  ignore (cfg, ctx)

let test_cross_functor () =
  print_endline "Lifecycle test 4: cross-functor shared lifecycle";
  EH1.Global.init ();
  allocate_via_eh1 ();
  (match EH2.Global.exit () with _ -> ());
  Gc.full_major ();
  Gc.compact ();
  print_endline "  ok"

let test () =
  print_endline "Runtime lifecycle / finalizer guard tests";
  test_exit ();
  test_reinit ();
  test_reset ();
  test_cross_functor ();
  print_endline "Done with lifecycle tests"
