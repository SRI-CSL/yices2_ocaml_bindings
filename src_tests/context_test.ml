open Yices2.High

module EH1 = Make(ExceptionsErrorHandling)

let test_config () =
  print_endline "Config tests";
  let open EH1 in
  let open Global in
  init();
  let cfg = Config.malloc () in
  Config.set cfg ~name:"mode" ~value:"push-pop";
  begin
    try
      Config.set cfg ~name:"baz" ~value:"bar";
      assert false;
    with
      _ ->
      let error_string = ErrorPrint.string () in
      assert(String.equal error_string "invalid parameter")
  end;
  begin
    try
      Config.set cfg ~name:"mode" ~value:"bar";
      assert false;
    with
      _ ->
      let error_string = ErrorPrint.string () in
      assert(String.equal error_string "value not valid for parameter")
  end;
  Config.default cfg ~logic:"QF_UFNIRA";
  Config.free cfg;
  print_endline "Done with Config tests";
  exit()

module type Context = sig
  open EH1
  type t
  val malloc : ?config:Config.t -> unit -> t
  val free : t -> unit
  val status : t -> Types.smt_status
  val reset  : t -> unit
  val push   : t -> unit
  val pop    : t -> unit
  val enable_option   : t -> option:string -> unit
  val disable_option  : t -> option:string -> unit
  val assert_formula  : t -> Term.t -> unit
  val assert_formulas : t -> Term.t list -> unit
  val assert_blocking_clause : t -> unit
  val check : ?param:Param.t -> t -> Types.smt_status
  val check_with_assumptions : ?param:Param.t -> t -> Term.t list -> Types.smt_status
  val stop      : t -> unit
  val get_model : t -> keep_subst:bool -> Model.t
  val get_unsat_core   : t -> Term.t list
  val check_with_model : ?param:Param.t -> t -> Model.t -> Term.t list -> Types.smt_status
  val get_model_interpolant : t -> Term.t
  val get    : t -> EH1.Context.t
end

let test_context (type a) (module Context : Context with type t = a) (ctx : a) =
  let module Type = EH1.Type in
  let module Term = EH1.Term in
  let module Param = EH1.Param in
  let module ErrorPrint = EH1.ErrorPrint in
  let _stat = Context.status ctx in
  let () = Context.push ctx in
  Context.assert_formula ctx (Term.false0());
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  let () = Context.pop ctx in
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  Context.reset ctx;
  let () = Context.enable_option ctx ~option:"arith-elim" in
  let () = Context.disable_option ctx ~option:"arith-elim" in
  let stat = Context.status ctx in
  assert(Types.equal_smt_status stat `STATUS_IDLE);
  Context.reset ctx;
  let boolt = Type.bool () in
  let bvar1 = Term.new_variable boolt in
  begin
    try Context.assert_formula ctx bvar1;
      assert false;
    with _ -> 
      let _error_string = ErrorPrint.string () in ()
      (* assert(String.equal error_string "assertion contains a free variable") *)
      (* MCSAT sends another error message *)
  end;
  let bv_t  = Type.bv 3 in
  let bvar1 = Term.new_uninterpreted bv_t in
  let () = Term.Names.set bvar1 "x" in
  let bvar2 = Term.new_uninterpreted bv_t in
  let () = Term.Names.set bvar2 "y" in
  let bvar3 = Term.new_uninterpreted bv_t in
  let () = Term.Names.set bvar3 "z" in
  let fmla1 = Term.parse "(= x (bv-add y z))" in
  let fmla2 = Term.parse "(bv-gt y 0b000)" in
  let fmla3 = Term.parse "(bv-gt z 0b000)" in
  let () = Context.assert_formula ctx fmla1 in
  let () = Context.assert_formulas ctx [fmla1; fmla2; fmla3] in
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  Context.stop ctx;
  let param = Param.malloc () in
  Param.default (Context.get ctx) param;
  let () = Param.set param ~name:"dyn-ack" ~value:"true" in
  begin
    try Param.set param ~name:"foo" ~value:"bar";
      assert false;
    with _ -> 
      let error_string = ErrorPrint.string () in
      assert(String.equal error_string "invalid parameter")
  end;
  begin
    try Param.set param ~name:"dyn-ack" ~value:"bar";
      assert false;
    with _ -> 
      let error_string = ErrorPrint.string () in
      assert(String.equal error_string "value not valid for parameter")
  end;
  Param.free param;
  ()

let test_native_context cfg =
  let open EH1 in
  let ctx = Context.malloc ~config:cfg () in
  let module Context = struct
      include Context
      let get x = x
    end
  in
  test_context (module Context) ctx;
  Context.free ctx;
  Global.exit()
  
let test_ext_context cfg =
  let open Yices2.Ext_bindings in
  let ctx = Context.malloc ~config:cfg () in
  let module Context = struct
      include Context
      let get x = x.context
    end
  in
  Context.goto ctx 5;
  Context.assert_formula ctx (Term.false0());
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  Context.goto ctx 0;
  let smt_stat = Context.check ctx in
  print_endline(Types.show_smt_status smt_stat);
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  test_context (module Context) ctx;
  Context.free ctx;
  Global.exit()

  
let test_regular_context () =
  print_endline "Regular context tests";
  let open EH1 in
  let open Global in
  init();
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  Context.assert_formula ctx Term.(new_uninterpreted (Type.real()) === (Arith.zero()));
  let _status = Context.check ctx in
  let () = Context.assert_blocking_clause ctx in
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  Context.free ctx;
  test_native_context cfg;
  print_endline "Done with Context tests"
         
let test_mcsat_context () =
  print_endline "MCSAT contexts tests";
  let open EH1 in
  let open Global in
  init();
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  test_native_context cfg;
  print_endline "Done with Context tests"

let test_regular_ext_context () =
  print_endline "Regular extended context tests";
  let open EH1 in
  let open Global in
  init();
  let cfg = Config.malloc () in
  let ctx = Context.malloc ~config:cfg () in
  Context.assert_formula ctx Term.(new_uninterpreted (Type.real()) === (Arith.zero()));
  let _status = Context.check ctx in
  let () = Context.assert_blocking_clause ctx in
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  Context.free ctx;
  test_ext_context cfg;
  print_endline "Done with Context tests"
         
let test_mcsat_ext_context () =
  print_endline "MCSAT extended contexts tests";
  let open EH1 in
  let open Global in
  init();
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  test_ext_context cfg;
  print_endline "Done with Context tests"

