open Yices2.High

(* module EH1 = Make(ExceptionsErrorHandling) *)
module EH1 = Make(NoErrorHandling)

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
                                 t -> t -> (Term.t, Model.t option) Types.smt_status_with_answers

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
  (* print_endline (CCFormat.sprintf "%a" Yices2.Ext.Model.pp model); *)
  (* Next line is commented out because it is not supported yet *)
  (* let _ = EH1.Model.get_value_as_term model f in *)
  Context.reset ctx;

  (* Testing algebraic numbers *)
  if mcsat
  then
    begin
      let x = Term.new_uninterpreted (Type.real()) in
      Context.assert_formula ctx Term.(Arith.(((neg x) ** (neg x)) === Arith.int 2));
      let status = Context.check ctx in
      assert(Types.equal_smt_status status `STATUS_SAT);
      let model = Context.get_model ctx in
      let sq2 = EH1.Model.get_algebraic_number_value model x in
      (* The following line does not work without an extension for epsilon-terms *)
      (* let _sq2_term = EH1.Model.get_value_as_term model x in *)
      (* print_endline(EH1.PP.term_string sq2_term); *)
      assert Q.(equal sq2.a (of_ints (-23) 16));
      assert Q.(equal sq2.b (of_ints (-45) 32));
      assert CCList.(equal Z.equal sq2.coeffs (List.map Z.of_int [-2;0;1]));
      EH1.Model.free model;
      Context.reset ctx;
    end;

  
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
    match test_interpolation (module Context) cfg [fmla1; fmla2; fmla3] [] with
      
    | `STATUS_SAT(Some model) ->
       let v1 = EH1.Model.get_rational64_value model r1 in
       assert(CCEqual.pair Signed.Long.equal Unsigned.ULong.equal v1
                (Signed.Long.of_int 7, Unsigned.ULong.of_int 2));
       let v2 = EH1.Model.get_rational64_value model r2 in
       assert(CCEqual.pair Signed.Long.equal Unsigned.ULong.equal v2
                (Signed.Long.of_int 5, Unsigned.ULong.of_int 1))
       
    | status -> if status_is_not_error status || mcsat then assert false
  in
  
  let fmla4 = Term.parse "(< r2 3)" in

  let () = 
      match test_interpolation (module Context) cfg [fmla1; fmla2; fmla3] [fmla4] with

      | `STATUS_UNSAT interpolant ->
         let string = CCFormat.sprintf "%s" (EH1.PP.term_string interpolant) in
         (* print_endline (CCFormat.sprintf "UNSAT with interpolant %a" Yices2.Ext.Term.pp interpolant); *)
         assert(String.equal string "(>= (+ -3 r2) 0)")
        
      | status -> if status_is_not_error status || mcsat then assert false
       
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

(* module Ext = Yices2.Ext.Make(ExceptionsErrorHandling) *)

module ExtContext = struct
  open Yices2.Ext.WithExceptionsErrorHandling
  include Context
  let check_with_assumptions ?param context assumptions = check ?param ~assumptions context
  let check_with_model ?param context model support =
    check ?param ~smodel:(SModel.make ~support model) context
  let check ?param context = check ?param context
  let get_model ?keep_subst context =
    let SModel{model;_} = get_model ?keep_subst context in
    model

  let check_with_interpolation ?build_model ?param ctxa ctxb =
    match build_model, check_with_interpolation ?build_model ?param ctxa ctxb with
    | None, `STATUS_SAT _
      | Some false, `STATUS_SAT _ -> `STATUS_SAT None
    | Some true, `STATUS_SAT f -> let SModel{ model; _} = f () in `STATUS_SAT(Some model)
    | _, `STATUS_UNSAT t -> `STATUS_UNSAT t
    | _, (#Yices2.Low.Types.smt_inconclusive_status as s) -> s

  type config = Config.t
  module Param = Param
end

let test_native_context mcsat cfg =
  test_context       (module NativeContext) mcsat cfg;
  test_interpolation (module NativeContext) mcsat cfg

let test_ext_context mcsat cfg =
  let open Yices2.Ext.WithExceptionsErrorHandling in
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

  let scalar = Type.new_uninterpreted ~name:"scalar_type" ~card:3 () in
  assert(Type.Names.has_name scalar);
  let cst = Term.constant scalar ~id:1 in
  let () = Term.Names.set cst "CST" in
  assert(Term.Names.has_name cst);

  test_context       (module ExtContext) mcsat cfg;
  test_interpolation (module ExtContext) mcsat cfg


let test_context () =
  print_endline "High bindings tests";
  cfg_makeNtest (module EH1.Config) test_native_context

let test_ext_context () =
  print_endline "Extended bindings tests";
  cfg_makeNtest (module Yices2.Ext.WithExceptionsErrorHandling.Config) test_ext_context

let test_tupleblast () =
  let open Yices2.Ext.WithExceptionsErrorHandling in
  let open Extensions in
  let open Tuples in
  Global.init();
  let config = Config.malloc () in
  Config.set config ~name:"solver-type" ~value:"mcsat";
  let ctx = Context.malloc ~config () in
  let v  = Term.new_uninterpreted (Type.(tuple [real(); real()])) in 
  let v' = Term.(tuple [select 2 v; Arith.(select 1 v ++ int 1 )]) in
  Context.assert_formula ctx (Term.eq v v');
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_UNSAT);
  
(* let test_lfun () = *)
(*   let open Yices2.Ext in *)
(*   let open Extensions in *)
(*   Global.init(); *)
(*   let ctx = ArrayLength.malloc () in *)
(*   let int = Type.(int()) in *)
(*   let admissible = *)
(*     let length = Term.new_variable int in *)
(*     let index  = Term.new_variable int in *)
(*     Term.(lambda [length; index] Arith.(leq (zero()) index &&& lt index length)) *)
(*   in *)
(*   let open ArrayLength.AddLength in *)
(*   let typ = ExtraType.lfun ~admissible ~length_type:int ~dom:[int] ~codom:int () in *)
(*   (\* print_endline (CCFormat.sprintf "%a" Type.pp typ); *\) *)
(*   let a = Term.new_uninterpreted ~name:"a" typ in *)
(*   let b = Term.new_uninterpreted ~name:"b" typ in *)
(*   (\* print_endline (CCFormat.sprintf "%a" Term.pp a); *)
(*    * print_endline (CCFormat.sprintf "%a" Term.pp b); *\) *)
(*   try *)
(*     let la = Term.( ExtraTerm.length a === Arith.int 2) in *)
(*     (\* print_endline (CCFormat.sprintf "%a" Term.pp la); *\) *)
(*     ArrayLength.assert_formula ctx la; *)
(*     let lb = Term.( ExtraTerm.length b === Arith.int 2) in *)
(*     (\* print_endline (CCFormat.sprintf "%a" Term.pp lb); *\) *)
(*     ArrayLength.assert_formula ctx lb; *)
(*     let l = Term.( [ *)
(*                      ExtraTerm.application a [Arith.zero()] === Arith.zero(); *)
(*                      ExtraTerm.application b [Arith.zero()] === Arith.zero(); *)
(*                      ExtraTerm.application a [Arith.int 1] === Arith.int 1; *)
(*                      ExtraTerm.application b [Arith.int 1] === Arith.int 1 ]) *)
(*     in *)
(*     ArrayLength.assert_formulas ctx l; *)
(*     ArrayLength.assert_formula ctx Term.(a =/= b); *)
(*     match ArrayLength.check ctx with *)
(*     | `STATUS_UNSAT -> *)
(*        (\* print_endline (CCFormat.sprintf "@[Log is:@,@[<v>%a@]@]" ArrayLength.pp_log ctx); *\)        *)
(*        (\* print_endline (CCFormat.sprintf "@[UNSAT@]"); *\) *)
(*        print_endline "Done with Extension \"Arrays with Length\"" *)
(*     | `STATUS_SAT -> *)
(*        CCFormat.(fprintf stdout) "@[Model is:@,@[%a@]@]" Model.pp (ArrayLength.get_model ctx); *)
(*        CCFormat.(fprintf stdout) "@[Log is:@,@[<v>%a@]@]" ArrayLength.pp_log ctx; *)
(*        assert false *)
(*     | _ -> assert false *)
(*   with *)
(*   | ExceptionsErrorHandling.YicesException(_,report) as exc -> *)
(*      let bcktrace = Printexc.get_backtrace() in *)
(*      CCFormat.(fprintf stdout) "@[Yices error: @[%s@]@]@," (ErrorPrint.string()); *)
(*      CCFormat.(fprintf stdout) "@[Error report:@,@[<v2>  %a@]@," *)
(*        Types.pp_error_report report; *)
(*      CCFormat.(fprintf stdout) "@[Backtrace is:@,@[%s@]@]@]%!" bcktrace; *)
(*      raise exc *)

(* let test_mcsat_arrays () = *)
(*   let open Yices2.Ext in *)
(*   let open Extensions.MCSATarrays in *)
(*   Global.init(); *)
(*   let real  = Type.(real()) in *)
(*   let typ   = Type.func [ real ] real in *)
(*   let a     = Term.new_uninterpreted ~name:"a" typ in *)
(*   let b     = Term.new_uninterpreted ~name:"b" typ in *)
(*   let index = [ Term.Arith.int 1 ] in *)
(*   let a_mod = Term.update a index (Term.Arith.int 2) in *)
(*   let i     = Term.new_uninterpreted ~name:"i" real in *)
(*   let index2 = [i] in *)
(*   try *)

(*     let () = (\* Read over write: same index *\) *)
(*       let ctx = Arrays.malloc_mcsat () in *)
(*       Arrays.assert_formula ctx Term.(b === a_mod); *)
(*       Arrays.assert_formula ctx Term.(application b index2 =/= Term.Arith.int 2); *)
(*       let smodel = Model.from_map [i , Term.Arith.int 1] |> SModel.make ~support:[i] in  *)
(*       match Arrays.check_with_smodel ctx smodel with *)
(*       | `STATUS_UNSAT -> *)
(*          (\* print_endline (CCFormat.sprintf "@[Log is:@,@[<v>%a@]@]" ArrayLength.pp_log ctx); *\) *)
(*          print_endline (CCFormat.sprintf "@[UNSAT@]"); *)
(*          print_endline (CCFormat.sprintf "@[Interpolant: %a@]" Term.pp (Arrays.get_model_interpolant ctx)); *)
(*          Arrays.free ctx *)
(*       | `STATUS_SAT -> *)
(*          CCFormat.(fprintf stdout) "@[Model is:@,@[%a@]@]" Model.pp (Arrays.get_model ctx); *)
(*          CCFormat.(fprintf stdout) "@[Log is:@,@[<v>%a@]@]" Arrays.pp_log ctx; *)
(*          assert false *)
(*       | _ -> assert false *)
(*     in *)

(*     let () = (\* Read over write: different index *\) *)
(*       let ctx = Arrays.malloc () in *)
(*       Arrays.assert_formula ctx Term.(b === a_mod); *)
(*       Arrays.assert_formula ctx Term.(application b index2 =/= application a index2); *)
(*       let smodel = Model.from_map [i , Term.Arith.int 0] |> SModel.make ~support:[i] in *)
(*       match Arrays.check_with_smodel ctx smodel with *)
(*       | `STATUS_UNSAT -> *)
(*          (\* print_endline (CCFormat.sprintf "@[Log is:@,@[<v>%a@]@]" ArrayLength.pp_log ctx); *\)        *)
(*          print_endline (CCFormat.sprintf "@[UNSAT@]"); *)
(*          print_endline (CCFormat.sprintf "@[Interpolant: %a@]" Term.pp (Arrays.get_model_interpolant ctx)); *)
(*          Arrays.free ctx *)
(*       | `STATUS_SAT -> *)
(*          CCFormat.(fprintf stdout) "@[Model is:@,@[%a@]@]" Model.pp (Arrays.get_model ctx); *)
(*          CCFormat.(fprintf stdout) "@[Log is:@,@[<v>%a@]@]" Arrays.pp_log ctx; *)
(*          assert false *)
(*       | _ -> assert false *)
(*     in *)

(*     print_endline "Done with Extension \"Arrays in MCSAT\"" *)

(*   with *)
(*   | ExceptionsErrorHandling.YicesException(_,report) as exc -> *)
(*      let bcktrace = Printexc.get_backtrace() in *)
(*      CCFormat.(fprintf stdout) "@[Yices error: @[%s@]@]@," (ErrorPrint.string()); *)
(*      CCFormat.(fprintf stdout) "@[Error report:@,@[<v2>  %a@]@," *)
(*        Types.pp_error_report report; *)
(*      CCFormat.(fprintf stdout) "@[Backtrace is:@,@[%s@]@]@]%!" bcktrace; *)
(*      raise exc *)
