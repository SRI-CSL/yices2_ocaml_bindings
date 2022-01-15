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
  type config
  val malloc : ?config:config -> unit -> t
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
  val get_model : ?keep_subst:bool -> t -> Model.t
  val get_unsat_core   : t -> Term.t list
  val check_with_model : ?param:Param.t -> t -> Model.t -> Term.t list -> Types.smt_status
  val get_model_interpolant : t -> Term.t

  module Param : sig
    type context := t
    type t = EH1.Param.t
    val malloc : unit -> t
    val free : t -> unit
    val set : t -> name:string -> value:string -> unit
    val default : context -> t -> unit
  end
end


let test_context (type a) (type c)
      (module Context : Context with type t = a and type config = c)
      (ctx   : a)
      (mcsat : bool)
  =

  let module Type = EH1.Type in
  let module Term = EH1.Term in
  let module Param = EH1.Param in
  let module ErrorPrint = EH1.ErrorPrint in

  (* Basic tests, asserts, checks, push, pops, options, reset *)
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

  (* No variables in assertions *)
  let boolt = Type.bool () in
  let bvar1 = Term.new_variable boolt in
  begin
    try Context.assert_formula ctx bvar1;
      assert false;
    with _ -> 
      let error_string = ErrorPrint.string () in ();
      (* Next line is commented out because MCSAT sends another error message *)
      if not mcsat then assert(String.equal error_string "assertion contains a free variable")
  end;
  
  (* Parsing and naming *)
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
  Context.reset ctx;

  (* Getting a functional value as term (currently not supported) *)
  let f = Term.new_uninterpreted ~name:"f" Type.(func [bv 18] (bv 18)) in 
  let a = Term.BV.bvconst_int ~width:18 3 in
  Context.assert_formula ctx Term.(application f [a] === a);
  Context.assert_formula ctx Term.(application f [BV.bvnot a] === BV.bvnot a);
  let _status = Context.check ctx in
  let _model  = Context.get_model ctx in
  (* print_endline (CCFormat.sprintf "%a" Yices2.Ext_bindings.Model.pp model); *)
  (* Next line is commented out because it is not supported yet *)
  (* let _ = EH1.Model.get_value_as_term model f in *)
  Context.reset ctx;


  (* Testing parameters *)
  let module Param = Context.Param in
  let param = Param.malloc () in
  Param.default ctx param;
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

  (* Testing blocking clause *)
  if not mcsat
  then
    begin
      Context.assert_formula ctx Term.(new_uninterpreted (Type.real()) === (Arith.zero()));
      let _status = Context.check ctx in
      let () = Context.assert_blocking_clause ctx in
      let smt_stat = Context.check ctx in
      assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
    end;  
  ()

let test_native_context cfg mcsat =
  let open EH1 in
  let ctx = Context.malloc ~config:cfg () in
  let module Context = struct
      include Context
      type config = Config.t
      module Param = Param
    end
  in
  test_context (module Context) ctx mcsat;
  Context.free ctx;
  Global.exit()
  
let test_ext_context cfg =
  let open Yices2.Ext_bindings in
  let ctx = Context.malloc ~config:cfg () in
  let module Context = struct
      include Context
      type config = Config.t
      module Param = Param
    end
  in
  Context.goto ctx 5;
  Context.assert_formula ctx (Term.false0());
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  Context.goto ctx 0;
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  test_context (module Context) ctx (Context.is_mcsat ctx);
  Context.free ctx;
  Global.exit()


let test_regular_context () =
  print_endline "Regular context tests";
  let open EH1 in
  Global.init();
  let cfg = Config.malloc () in
  test_native_context cfg false;
  print_endline "Done with Context tests"
         
let test_mcsat_context () =
  print_endline "MCSAT contexts tests";
  let open EH1 in
  Global.init();
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  test_native_context cfg true;
  print_endline "Done with Context tests"

let test_regular_ext_context () =
  print_endline "Regular extended context tests";
  let open Yices2.Ext_bindings in
  Global.init();
  let cfg = Config.malloc () in
  test_ext_context cfg;
  print_endline "Done with Context tests"
         
let test_mcsat_ext_context () =
  print_endline "MCSAT extended contexts tests";
  let open Yices2.Ext_bindings in
  Global.init();
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  test_ext_context cfg;
  print_endline "Done with Context tests"

