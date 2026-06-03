open Yices2.High

external sigalt_onstack : unit -> int = "caml_yices_sigalt_onstack"
external raw_exit : int -> 'a = "caml_yices_raw_exit"

module Yices = Make(NoErrorHandling)

let stress_iters () =
  match Sys.getenv_opt "YICES_SIGALT_STRESS_ITERS" with
  | Some s -> (try int_of_string s with _ -> 20000)
  | None -> 20000

let gc_interval () =
  match Sys.getenv_opt "YICES_SIGALT_GC_INTERVAL" with
  | Some s -> max 1 (try int_of_string s with _ -> 1000)
  | None -> 1000

let gc_interval () =
  match Sys.getenv_opt "YICES_SIGALT_GC_INTERVAL" with
  | Some s -> max 1 (try int_of_string s with _ -> 1000)
  | None -> 1000

let () =
  Yices.Global.init ();
  let n = stress_iters () in
  let gc_interval = gc_interval () in
  let bool_ty = Yices.Type.bool () in
  let bvar = Yices.Term.new_variable bool_ty in
  let cfg = Yices.Config.malloc () in
  Yices.Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Yices.Config.set cfg ~name:"model-interpolation" ~value:"true";
  Yices.Config.set cfg ~name:"mode" ~value:"push-pop";
  Printf.eprintf "[sigalt-freevar] stress iterations: %d; gc interval: %d\n%!"
    n gc_interval;
  for i = 1 to n do
    let ctx = Yices.Context.malloc ~config:cfg () in
    let _ = Yices.Context.assert_formula ctx bvar in
    if sigalt_onstack () = 1 then begin
      Printf.eprintf "[sigalt-freevar] stuck SS_ONSTACK after iteration %d\n%!" i;
      raw_exit 2
    end;
    if i mod gc_interval = 0 then Gc.full_major ()
  done;
  Gc.full_major ();
  Printf.eprintf "[sigalt-freevar] no stuck SS_ONSTACK after %d iterations\n%!" n
