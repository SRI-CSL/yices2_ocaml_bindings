module C = Configurator.V1

let is_system op s =
  let re = Str.regexp_string_case_fold op in
  try
    ignore (Str.search_forward re s 0 : int);
    true
  with Not_found -> false

let () =
  let sys = ref "" in
  let pkg = ref [] in
  let first = ref "" in
  let optcomp_cookie_file = ref None in
  let args =
    Arg.(
    [ ("-system", Set_string sys, "set system");
      ("-pkg", Tuple[Set_string first; String(fun s -> pkg := (!first,s)::!pkg)], "package dependency");
      ("-optcomp-cookie", String (fun s -> optcomp_cookie_file := Some s),
       "emit a lines file with ppx_optcomp cookie based on Yices version")
    ])
  in
  C.main ~args ~name:"gmp" @@
    fun c ->
    let open C.Pkg_config in
    let sys = !sys in
    let base =
      if is_system "freebsd" sys || is_system "openbsd" sys then
        { libs   = [ "-L/usr/local/lib";    ];
          cflags = [ "-I/usr/local/include"; "-fPIC"; "-w" ] }
      else if is_system "macosx" sys then
        { libs   = [ "-L/opt/local/lib";     "-L/usr/local/lib";    ];
          cflags = [ "-I/opt/local/include"; "-I/usr/local/include"; "-w" ] }
      else
        { libs   = []; cflags = ["-fPIC"; "-w"] }
    in
    let aux sofar (linux_name, macos_name) =
      let package = 
        if is_system "macosx" sys then macos_name else linux_name
      in
      let default () =
        { libs   = sofar.libs @ ["-l"^package];
          cflags = sofar.cflags }
      in
      match C.Pkg_config.get c with
      | None -> default()
      | Some pc ->
         match C.Pkg_config.query pc ~package with
         | None -> default()
         | Some deps ->
            { libs   = sofar.libs @ deps.libs ;
              cflags = sofar.cflags @ deps.cflags }
    in
    let conf = List.fold_left aux base !pkg in

    C.Flags.write_sexp "c_flags.sexp" conf.cflags;

    C.Flags.write_sexp "c_library_flags.sexp" conf.libs;

    C.Flags.write_lines "c_flags.lines" conf.cflags;

    begin
      match !optcomp_cookie_file with
      | None -> ()
      | Some fname ->
          let open C.C_define in
          let defs =
            import c ~c_flags:conf.cflags ~includes:["yices.h"]
              [ ("__YICES_VERSION", Int);
                ("__YICES_VERSION_MAJOR", Int);
                ("__YICES_VERSION_PATCHLEVEL", Int)
              ]
          in
          let find_int name =
            match List.assoc_opt name defs with
            | Some (Value.Int v) -> v
            | _ -> C.die "Missing or non-int %s from yices.h" name
          in
          let v_major = find_int "__YICES_VERSION" in
          let v_minor = find_int "__YICES_VERSION_MAJOR" in
          let ge_2_7 =
            v_major > 2 || (v_major = 2 && v_minor >= 7)
          in
          let env_expr =
            if ge_2_7 then
              "env~yices_ge_2_7:(Defined(true))"
            else
              "env~yices_ge_2_7:Undefined"
          in
          C.Flags.write_lines fname
            [ "-cookie"; "ppx_optcomp.env=" ^ env_expr ]
    end
