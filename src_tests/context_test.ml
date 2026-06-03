open Yices2.High

(* DIAG instrumentation (see sigalt_probe.c) *)
external sigalt_query : string -> unit = "caml_sigalt_query"
external sigalt_scan  : string -> unit = "caml_sigalt_scan_onstack"
external sigalt_reinstall : unit -> unit = "caml_sigalt_install_probe"
external sigalt_count : unit -> int = "caml_sigalt_segv_count"
external sigalt_onstack : unit -> int = "caml_sigalt_onstack"
external sigalt_wait : int -> unit = "caml_sigalt_wait"
external sigalt_clear : unit -> int = "caml_sigalt_clear"
external sigalt_raw_exit : int -> 'a = "caml_sigalt_raw_exit"
let cp label = sigalt_query label; sigalt_scan label

(* module EH1 = Make(ExceptionsErrorHandling) *)
module EH1 = Make(NoErrorHandling)

module type Context = sig
  open EH1
  type t
  type config
  type model
       
  val malloc : ?config:config -> unit -> t
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
  val get_model : ?keep_subst:bool -> t -> model
  val get_unsat_core   : t -> Term.t list
  val check_with_model : ?param:Param.t -> t -> model -> Term.t list -> Types.smt_status
  val get_model_interpolant : t -> Term.t
  val check_with_interpolation : ?build_model:bool ->
                                 ?param:Param.t ->
                                 t -> t -> (Term.t, model option) Types.smt_status_with_answers
  val default_param : t -> Param.t -> unit
  val get_algebraic_number_value : model -> Term.t -> Types.algebraic
  val get_rational64_value : model -> Term.t -> Signed.Long.t * Unsigned.ULong.t

  module Param : sig
    type t = EH1.Param.t
    val malloc : unit -> t
    val set : t -> name:string -> value:string -> unit
  end
end


let test_context (type a) (type c) (type m)
      (module Context : Context with type t = a and type config = c and type model = m)
      (mcsat : bool)
      (cfg : c)
  =

  let module Type = EH1.Type in
  let module Term = EH1.Term in
  let module Param = EH1.Param in
  let module ErrorPrint = EH1.ErrorPrint in

  let lbl0 s = Printf.sprintf "tc[mcsat=%b]:%s" mcsat s in
  let ctx = Context.malloc ~config:cfg () in
  cp (lbl0 "tc-start");

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
  cp (lbl0 "after-pushpop-checks");
  let () = Context.enable_option ctx ~option:"arith-elim" in
  let () = Context.disable_option ctx ~option:"arith-elim" in
  let stat = Context.status ctx in
  assert(Types.equal_smt_status stat `STATUS_IDLE);
  Context.reset ctx;

  (* No variables in assertions *)
  let boolt = Type.bool () in
  let bvar1 = Term.new_variable boolt in
  cp (lbl0 "before-freevar-assert");
  begin
    try Context.assert_formula ctx bvar1;
      cp (lbl0 "freevar-assert-returned");
      assert false;
    with e -> 
      Printf.eprintf "[freevar] caught exception: %s\n%!" (Printexc.to_string e);
      cp (lbl0 "in-freevar-handler");
      let error_string = ErrorPrint.string () in ();
      (* Next line is commented out because MCSAT sends another error message *)
      if not mcsat then assert(String.equal error_string "assertion contains a free variable")
  end;
  cp (lbl0 "after-freevar-block");
  
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
  cp (lbl0 "after-parse");
  let () = Context.assert_formula ctx fmla1 in
  let () = Context.assert_formulas ctx [fmla1; fmla2; fmla3] in
  cp (lbl0 "after-asserts");
  let smt_stat = Context.check ctx in
  assert(Types.equal_smt_status smt_stat `STATUS_SAT);
  cp (lbl0 "after-parse-check");
  Context.stop ctx;
  Context.reset ctx;

  (* Getting a functional value as term (currently not supported) *)
  let f = Term.new_uninterpreted ~name:"f" Type.(func [bv 18] (bv 18)) in 
  let a = Term.BV.bvconst_int ~width:18 3 in
  Context.assert_formula ctx Term.(application f [a] === a);
  Context.assert_formula ctx Term.(application f [BV.bvnot a] === BV.bvnot a);
  let _status = Context.check ctx in
  cp (lbl0 "after-func-check");
  let _model  = Context.get_model ctx in
  cp (lbl0 "after-func-get_model");
  (* print_endline (CCFormat.sprintf "%a" Yices2.Ext.Model.pp model); *)
  (* Next line is commented out because it is not supported yet *)
  (* let _ = EH1.Model.get_value_as_term model f in *)
  Context.reset ctx;

  let lbl s = Printf.sprintf "tc[mcsat=%b]:%s" mcsat s in
  cp (lbl "before-algebraic");
  (* Testing algebraic numbers *)
  if mcsat
  then
    begin
      let x = Term.new_uninterpreted (Type.real()) in
      Context.assert_formula ctx Term.(Arith.(((neg x) ** (neg x)) === Arith.int 2));
      cp (lbl "alg-before-check");
      let status = Context.check ctx in
      cp (lbl "alg-after-check");
      assert(Types.equal_smt_status status `STATUS_SAT);
      let model = Context.get_model ctx in
      cp (lbl "alg-after-get_model");
      let sq2 = Context.get_algebraic_number_value model x in
      cp (lbl "alg-after-get_algebraic");
      (* The following line does not work without an extension for epsilon-terms *)
      (* let _sq2_term = EH1.Model.get_value_as_term model x in *)
      (* print_endline(EH1.PP.term_string sq2_term); *)
      assert Q.(equal sq2.a (of_ints (-23) 16));
      assert Q.(equal sq2.b (of_ints (-45) 32));
      assert CCList.(equal Z.equal sq2.coeffs (List.map Z.of_int [-2;0;1]));
      Context.reset ctx;
    end;

  
  (* Testing parameters *)
  let module Param = Context.Param in
  let param = Param.malloc () in
  Context.default_param ctx param;
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

(* Testing interpolation *)
  
let test_interpolation (type a) (type c) (type m)
      (module Context : Context with type t = a and type config = c and type model = m)
      (cfg : c)
      assertA
      assertB
  =
  let ctxA = Context.malloc ~config:cfg () in
  let ctxB = Context.malloc ~config:cfg () in

  Context.assert_formulas ctxA assertA;
  Context.assert_formulas ctxB assertB;

  let param = Context.Param.malloc() in
  Context.default_param ctxA param;
  let r =
    Context.check_with_interpolation ~build_model:true ~param ctxA ctxB
  in
  r

let test_interpolation (type a) (type c) (type m)
      (module Context : Context with type t = a and type config = c and type model = m)
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
       let v1 = Context.get_rational64_value model r1 in
       assert(CCEqual.pair Signed.Long.equal Unsigned.ULong.equal v1
                (Signed.Long.of_int 7, Unsigned.ULong.of_int 2));
       let v2 = Context.get_rational64_value model r2 in
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
  print_endline "Done with Config tests";
  
  (* Now preparing the call to test_cfg *)
  let cfg = Config.malloc () in

  cp "makeNtest:before-regular";
  print_endline "Regular context tests";
  test_cfg false cfg;
  cp "makeNtest:after-regular";

  print_endline "MCSAT contexts tests";
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  sigalt_reinstall ();
  let faults_before = sigalt_count () in
  test_cfg true cfg;
  let faults_after = sigalt_count () in
  Printf.eprintf "[sigalt] MCSAT-solve fault delta = %d (before=%d after=%d)\n%!"
    (faults_after - faults_before) faults_before faults_after;
  cp "makeNtest:after-mcsat(before-exit)";

  EH1.Global.exit();
  cp "makeNtest:after-exit";
  print_endline "Done with Regular and MCSAT Context tests"

(* We've pushed as far as we could the common code for native and extended contexts.
   Now we do the two separately *)

module NativeContext = struct
  open EH1
  include Context
  type model = Model.t
  type config = Config.t
  let get_algebraic_number_value = Model.get_algebraic_number_value
  let get_rational64_value = Model.get_rational64_value
  module Param = Param
end

(* module Ext = Yices2.Ext.Make(ExceptionsErrorHandling) *)

module ExtContext = struct
  open Yices2.Ext.WithExceptionsErrorHandling
  include Context
  type model = SModel.t
  let check_with_assumptions ?param context assumptions = check ?param ~assumptions context
  let check_with_model ?param context smodel support =
    check ?param ~smodel:(SModel.with_support support smodel) context
  let check ?param context = check ?param context
  let get_model ?keep_subst context = get_model ?keep_subst context

  let check_with_interpolation ?build_model ?param ctxa ctxb =
    match build_model, check_with_interpolation ?build_model ?param ctxa ctxb with
    | None, `STATUS_SAT _
      | Some false, `STATUS_SAT _ -> `STATUS_SAT None
    | Some true, `STATUS_SAT f -> `STATUS_SAT(Some (f ()))
    | _, `STATUS_UNSAT t -> `STATUS_UNSAT t
    | _, (#Yices2.Low.Types.smt_inconclusive_status as s) -> s

  let get_algebraic_number_value smodel x =
    match ModelValue.reveal (SModel.get_value smodel x) with
    | `Algebraic a -> a
    | _ -> failwith "expected algebraic number"

  let get_rational64_value smodel x =
    match ModelValue.reveal (SModel.get_value smodel x) with
    | `Rational q ->
      (Signed.Long.of_int (Z.to_int (Q.num q)),
       Unsigned.ULong.of_int (Z.to_int (Q.den q)))
    | _ -> failwith "expected rational"

  type config = Config.t
  module Param = Param
end

let test_native_context mcsat cfg =
  test_context       (module NativeContext) mcsat cfg;
  test_interpolation (module NativeContext) mcsat cfg

let test_ext_context mcsat cfg =
  let lbl s = Printf.sprintf "ext-worker[mcsat=%b]:%s" mcsat s in
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
  cp (lbl "after-goto-checks");

  let scalar = Type.new_uninterpreted ~name:"scalar_type" ~card:3 () in
  assert(Type.Names.has_name scalar);
  let cst = Term.constant scalar ~id:1 in
  let () = Term.Names.set cst "CST" in
  assert(Term.Names.has_name cst);
  cp (lbl "after-scalar");

  test_context       (module ExtContext) mcsat cfg;
  cp (lbl "after-test_context");
  test_interpolation (module ExtContext) mcsat cfg;
  cp (lbl "after-test_interpolation")


let test_context () =
  print_endline "High bindings tests";
  cfg_makeNtest (module EH1.Config) test_native_context

let test_ext_context () =
  print_endline "Extended bindings tests";
  cfg_makeNtest (module Yices2.Ext.WithExceptionsErrorHandling.Config) test_ext_context

(* Deterministic reproducer for the SS_ONSTACK flip: repeatedly assert a free
   VARIABLE term into a fresh MCSAT context and poll the alt-stack flag after
   each iteration. Gated by env YICES_FREEVAR_STRESS (= max iterations, default
   200000). *)
let stress_freevar () =
  let open Yices2.Ext.WithExceptionsErrorHandling in
  EH1.Global.init ();
  let n = try int_of_string (Sys.getenv "YICES_FREEVAR_STRESS") with _ -> 200000 in
  let reuse = Sys.getenv_opt "YICES_FREEVAR_REUSE" <> None in
  let no_assert = Sys.getenv_opt "YICES_FREEVAR_NOASSERT" <> None in
  let native = Sys.getenv_opt "YICES_FREEVAR_NATIVE" <> None in
  let no_yices_exit = Sys.getenv_opt "YICES_FREEVAR_NO_YICES_EXIT" <> None in
  let fail_on_flip = Sys.getenv_opt "YICES_FREEVAR_FAIL_ON_FLIP" <> None in
  let gc_interval =
    match Sys.getenv_opt "YICES_FREEVAR_GC_INTERVAL" with
    | Some s -> max 1 (try int_of_string s with _ -> 1000)
    | None -> 1000
  in
  (match Sys.getenv_opt "YICES_FREEVAR_WAIT" with
   | Some s -> sigalt_wait (try int_of_string s with _ -> 6)
   | None -> ());
  if Sys.getenv_opt "YICES_FREEVAR_OCAMLONLY" <> None then begin
    let flipped = ref false in
    let i = ref 0 in
    let sink = ref [] in
    Printf.eprintf "[stress] start onstack=%d ocaml-only\n%!" (sigalt_onstack ());
    while (not !flipped) && !i < n do
      incr i;
      sink := (Array.make 64 !i) :: (if !i land 1023 = 0 then [] else !sink);
      ignore (Sys.opaque_identity !sink);
      if sigalt_onstack () = 1 then begin
        flipped := true;
        Printf.eprintf "[stress] (ocaml-only) SS_ONSTACK flipped at iteration %d\n%!" !i;
        if fail_on_flip then sigalt_raw_exit 2
      end
    done;
    if not !flipped then Printf.eprintf "[stress] (ocaml-only) no flip after %d iters\n%!" !i;
    raise Exit
  end;
  if native then begin
    let boolt = EH1.Type.bool () in
    let bvar = EH1.Term.new_variable boolt in
    let cfg = EH1.Config.malloc () in
    let keep = Sys.getenv_opt "YICES_FREEVAR_KEEP" <> None in
    let kept = ref [] in
    let flipped = ref false in
    let i = ref 0 in
    EH1.Config.set cfg ~name:"solver-type" ~value:"mcsat";
    EH1.Config.set cfg ~name:"model-interpolation" ~value:"true";
    EH1.Config.set cfg ~name:"mode" ~value:"push-pop";
    Printf.eprintf "[stress] start onstack=%d native keep=%b gc_interval=%d\n%!"
      (sigalt_onstack ()) keep gc_interval;
    while (not !flipped) && !i < n do
      incr i;
      let ctx = EH1.Context.malloc ~config:cfg () in
      let _ = EH1.Context.assert_formula ctx bvar in
      if keep then kept := Obj.repr (ctx, cfg) :: !kept;
      if sigalt_onstack () = 1 then begin
        flipped := true;
        Printf.eprintf "[stress] (native) SS_ONSTACK flipped at iteration %d\n%!" !i;
        let after = sigalt_clear () in
        Printf.eprintf "[stress] (native) after sigalt_clear: onstack=%d\n%!" after;
        if fail_on_flip then sigalt_raw_exit 2
      end;
      if !i mod gc_interval = 0 then Gc.full_major ()
    done;
    ignore (Sys.opaque_identity !kept);
    Gc.full_major ();
    if not !flipped then Printf.eprintf "[stress] (native) no flip after %d iters\n%!" !i;
    if not no_yices_exit then EH1.Global.exit ();
    raise Exit
  end;
  let boolt = EH1.Type.bool () in
  let bvar = EH1.Term.new_variable boolt in
  let flipped = ref false in
  let i = ref 0 in
  let last_exn = ref "<none>" in
  let cfg = Config.malloc () in
  Config.set cfg ~name:"solver-type" ~value:"mcsat";
  Config.set cfg ~name:"model-interpolation" ~value:"true";
  Config.set cfg ~name:"mode" ~value:"push-pop";
  let mkctx () =
    Context.malloc ~config:cfg ()
  in
  let shared = if reuse then Some (mkctx ()) else None in
  Printf.eprintf "[stress] start onstack=%d faults=%d reuse=%b no_assert=%b gc_interval=%d\n%!"
    (sigalt_onstack ()) (sigalt_count ()) reuse no_assert gc_interval;
  while (not !flipped) && !i < n do
    incr i;
    let ctx = match shared with Some c -> c | None -> mkctx () in
    if not no_assert then
      (try Context.assert_formula ctx bvar with e -> last_exn := Printexc.to_string e);
    if sigalt_onstack () = 1 then begin
      flipped := true;
      Printf.eprintf "[stress] SS_ONSTACK flipped at iteration %d, faults=%d\n%!"
        !i (sigalt_count ());
      Printf.eprintf "[stress] last exception: %s\n%!" !last_exn;
      sigalt_scan "stress-flip-owner";
      if fail_on_flip then sigalt_raw_exit 2
    end;
    if !i mod gc_interval = 0 then Gc.full_major ()
  done;
  Gc.full_major ();
  if not !flipped then
    Printf.eprintf "[stress] no flip after %d iters, faults=%d\n%!" !i (sigalt_count ());
  if not no_yices_exit then EH1.Global.exit ()

let test_tupleblast () =
  let open Yices2.Ext.WithExceptionsErrorHandling in
  let module TupleCtx = Extensions.Tuples.ContextOnlyMCSAT in
  Global.init();
  let ctx = TupleCtx.malloc_mcsat () in
  let v  = Term.new_uninterpreted (Type.(tuple [real(); real()])) in 
  let v' = Term.(tuple [select 2 v; Arith.(select 1 v ++ int 1 )]) in
  TupleCtx.assert_formula ctx (Term.eq v v');
  let smt_stat = TupleCtx.check ctx in
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
