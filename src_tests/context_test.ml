open Yices2.High

module EH1 = Make(ExceptionsErrorHandling)

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
  val check_with_interpolation : ?build_model:bool ->
                                 ?param:Param.t ->
                                 t -> t -> Types.smt_status * Term.t option * Model.t option

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
      (mcsat : bool)
      (cfg : c)
  =

  let module Type = EH1.Type in
  let module Term = EH1.Term in
  let module Param = EH1.Param in
  let module ErrorPrint = EH1.ErrorPrint in

  let ctx = Context.malloc ~config:cfg () in

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

  Context.free ctx

(* Testing interpolation *)
  
let test_interpolation (type a) (type c)
      (module Context : Context with type t = a and type config = c)
      (cfg : c)
      assertA
      assertB
  =
  let ctxA = Context.malloc ~config:cfg () in
  let ctxB = Context.malloc ~config:cfg () in

  Context.assert_formulas ctxA assertA;
  Context.assert_formulas ctxB assertB;

  let param = Context.Param.malloc() in
  Context.Param.default ctxA param;
  let r =
    Context.check_with_interpolation ~build_model:true ~param ctxA ctxB
  in
  Context.free ctxA;
  Context.free ctxB;
  r

let test_interpolation (type a) (type c)
      (module Context : Context with type t = a and type config = c)
      (mcsat : bool)
      (cfg : c)
  =

  let module Type = EH1.Type in
  let module Term = EH1.Term in

  let realT = Type.real() in
  let r1 = Term.new_uninterpreted ~name:"r1" realT in
  let r2 = Term.new_uninterpreted ~name:"r2" realT in

  let fmla1 = Term.parse "(> r1 3)" in
  let fmla2 = Term.parse "(< r1 4)" in
  let fmla3 = Term.parse "(< (- r1 r2) 0)" in

  let () =
    try
      match test_interpolation (module Context) cfg [fmla1; fmla2; fmla3] [] with
  
      | `STATUS_SAT, None, Some model ->
         let v1 = EH1.Model.get_rational64_value model r1 in
         assert(CCEqual.pair Signed.Long.equal Unsigned.ULong.equal v1
                  (Signed.Long.of_int 7, Unsigned.ULong.of_int 2));
         let v2 = EH1.Model.get_rational64_value model r2 in
         assert(CCEqual.pair Signed.Long.equal Unsigned.ULong.equal v2
                  (Signed.Long.of_int 5, Unsigned.ULong.of_int 1));
  
      | status, _, _ ->
         let _ = ExceptionsErrorHandling.check_status status in
         assert false
    with ExceptionsErrorHandling.YicesException _ when not mcsat -> () 
  in
  
  let fmla4 = Term.parse "(< r2 3)" in
  
  let () = 
    try
      match test_interpolation (module Context) cfg [fmla1; fmla2; fmla3] [fmla4] with

      | `STATUS_UNSAT, Some interpolant, None ->
         let string = CCFormat.sprintf "%s" (EH1.PP.term_string interpolant) in
         assert(String.equal string "(>= (+ -3 r2) 0)")
         (* print_endline (CCFormat.sprintf "%a" Yices2.Ext_bindings.Term.pp interpolant) *)
        
      | status, _, _ ->
         let _ = ExceptionsErrorHandling.check_status status in
         assert false
    with ExceptionsErrorHandling.YicesException _ when not mcsat -> ()
       | _ ->
         print_endline(CCFormat.sprintf "%a"
                         Yices2.Ext_bindings.Types.pp_error_report
                         (Error.report()));
          
  in
  
  ()

(* Running a function for testing a configuration *)

module type Config = sig
  type t   
  val malloc : unit -> t
  val free : t -> unit
  val set : t -> name:string -> value:string -> unit
  val default : ?logic:string -> t -> unit
end

let cfg_makeNtest (type a) (module Config : Config with type t = a) test_cfg =

  EH1.Global.init();

  print_endline "Config tests";
  let cfg = Config.malloc () in
  Config.set cfg ~name:"mode" ~value:"push-pop";
  begin
    try
      Config.set cfg ~name:"baz" ~value:"bar";
      assert false;
    with
      _ ->
      let error_string = EH1.ErrorPrint.string () in
      assert(String.equal error_string "invalid parameter")
  end;
  begin
    try
      Config.set cfg ~name:"mode" ~value:"bar";
      assert false;
    with
      _ ->
      let error_string = EH1.ErrorPrint.string () in
      assert(String.equal error_string "value not valid for parameter")
  end;
  Config.default cfg ~logic:"QF_UFNIRA";
  Config.free cfg;
  print_endline "Done with Config tests";
  
  (* Now preparing the call to test_cfg *)
  let cfg = Config.malloc () in

  print_endline "Regular context tests";
  test_cfg false cfg;

  print_endline "MCSAT contexts tests";
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  test_cfg true cfg;

  Config.free cfg;
  EH1.Global.exit();
  print_endline "Done with Regular and MCSAT Context tests"

(* We've pushed as far as we could the common code for native and extended contexts.
   Now we do the two separately *)

module NativeContext = struct
  open EH1
  include Context
  type config = Config.t
  module Param = Param
end

module ExtContext = struct
  open Yices2.Ext_bindings
  include Context
  type config = Config.t
  module Param = Param
end

let test_native_context mcsat cfg =
  test_context       (module NativeContext) mcsat cfg;
  test_interpolation (module NativeContext) mcsat cfg
  
let test_ext_context mcsat cfg =
  let open Yices2.Ext_bindings in

  let ctx = Context.malloc ~config:cfg () in
  assert(Bool.equal mcsat (Context.is_mcsat ctx));
  Context.goto ctx 5;
  Context.assert_formula ctx (Term.false0());
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  Context.goto ctx 0;
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  Context.free ctx;

    
  test_context       (module ExtContext) mcsat cfg;
  test_interpolation (module ExtContext) mcsat cfg


let test_context () =
  print_endline "High bindings tests";
  cfg_makeNtest (module EH1.Config) test_native_context

let test_ext_context () =
  print_endline "Extended bindings tests";
  cfg_makeNtest (module Yices2.Ext_bindings.Config) test_ext_context
