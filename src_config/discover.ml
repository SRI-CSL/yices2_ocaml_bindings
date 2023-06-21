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
  let args =
    Arg.(
    [ ("-system", Set_string sys, "set system");
      ("-pkg", Tuple[Set_string first; String(fun s -> pkg := (!first,s)::!pkg)], "package dependency")
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

    C.Flags.write_lines "c_flags.lines" conf.cflags
