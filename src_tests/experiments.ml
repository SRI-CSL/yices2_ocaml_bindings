open Yices2.High

module EH1 = Make(ExceptionsErrorHandling)

let test() =
  print_endline "Experiments";
  let open EH1 in
  Global.init();

  let config = Config.malloc() in
  (* MCSAT does not support tuples *)
  (* Config.set config ~name:"solver-type" ~value:"mcsat"; *)
  Config.set config ~name:"model-interpolation" ~value:"true";
  Config.set config ~name:"mode" ~value:"multi-checks";
  let context = Context.malloc ~config () in
  let x  = Term.new_uninterpreted (Type.bv 4) in
  let y  = Term.new_uninterpreted (Type.real()) in
  let ty = Type.tuple [Type.bv 4; Type.real()] in
  let z = Term.new_uninterpreted ty in
  Context.assert_formula context Term.(z =/= tuple [x;y]) ;
  let _ = Context.check context in
  Global.exit();
  print_endline "Done with experiments";
  ()
