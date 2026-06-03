open Yices2.High

external sigalt_onstack : unit -> int = "caml_yices_sigalt_onstack"
external raw_exit : int -> 'a = "caml_yices_raw_exit"

module Yices = Make(NoErrorHandling)

let stress_iters () =
  match Sys.getenv_opt "YICES_SIGALT_STRESS_ITERS" with
  | Some s -> (try int_of_string s with _ -> 200000)
  | None -> 200000

let () =
  Yices.Global.init ();
  let n = stress_iters () in
  let bool_ty = Yices.Type.bool () in
  Printf.eprintf "[sigalt-freevar] stress iterations: %d\n%!" n;
  for i = 1 to n do
    let cfg = Yices.Config.malloc () in
    Yices.Config.set cfg ~name:"solver-type" ~value:"mcsat";
    Yices.Config.set cfg ~name:"model-interpolation" ~value:"true";
    Yices.Config.set cfg ~name:"mode" ~value:"push-pop";
    let ctx = Yices.Context.malloc ~config:cfg () in
    let bvar = Yices.Term.new_variable bool_ty in
    let _ = Yices.Context.assert_formula ctx bvar in
    if sigalt_onstack () = 1 then begin
      Printf.eprintf "[sigalt-freevar] stuck SS_ONSTACK after iteration %d\n%!" i;
      raw_exit 2
    end
  done;
  Printf.eprintf "[sigalt-freevar] no stuck SS_ONSTACK after %d iterations\n%!" n
