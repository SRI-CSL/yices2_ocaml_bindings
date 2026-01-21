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
  let vendor_root = ref "" in
  let vendor_prefix_arg = ref "" in
  let args =
    Arg.(
    [ ("-system", Set_string sys, "set system");
      ("-vendor-root", Set_string vendor_root, "path to project root for vendored deps");
      ("-vendor-prefix", Set_string vendor_prefix_arg, "prefix for vendored deps");
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
    let has_mcsat conf =
      let c_file = Filename.temp_file "yices_mcsat" ".c" in
      let exe_file = Filename.temp_file "yices_mcsat" ".exe" in
      let code =
        "#include <yices.h>\n\
         int main(void) { return yices_has_mcsat() ? 0 : 1; }\n"
      in
      let oc = open_out c_file in
      output_string oc code;
      close_out oc;
      let cc = C.ocaml_config_var_exn c "c_compiler" in
      let compile =
        C.Process.run c cc
          (conf.cflags @ [ c_file ] @ conf.libs @ [ "-o"; exe_file ])
      in
      let cleanup () =
        (try if Sys.file_exists c_file then Sys.remove c_file with _ -> ());
        (try if Sys.file_exists exe_file then Sys.remove exe_file with _ -> ())
      in
      if compile.exit_code <> 0 then (cleanup (); false) else
        let libpaths =
          let add_path acc flag =
            if String.length flag >= 2 && String.sub flag 0 2 = "-L" then
              let path = String.sub flag 2 (String.length flag - 2) in
              path :: acc
            else
              acc
          in
          List.rev (List.fold_left add_path [] conf.libs)
        in
        let env =
          if libpaths = [] then None
          else
            let joined = String.concat ":" libpaths in
            let append name =
              match Sys.getenv_opt name with
              | None -> joined
              | Some v -> joined ^ ":" ^ v
            in
            Some
              [ "LD_LIBRARY_PATH=" ^ append "LD_LIBRARY_PATH";
                "DYLD_LIBRARY_PATH=" ^ append "DYLD_LIBRARY_PATH" ]
        in
        let run =
          match env with
          | None -> C.Process.run c exe_file []
          | Some env -> C.Process.run c ~env exe_file []
        in
        let ok = run.exit_code = 0 in
        cleanup ();
        ok
    in
    let opam_prefix =
      match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
      | Some p when p <> "" -> Some p
      | _ ->
          begin match C.which c "opam" with
          | None -> None
          | Some opam ->
              let res = C.Process.run c opam [ "var"; "prefix" ] in
              if res.exit_code = 0 then
                let p = String.trim res.stdout in
                if p = "" then None else Some p
              else
                None
          end
    in
    begin
      match opam_prefix with
      | None -> ()
      | Some p ->
          let pc_dir = Filename.concat p "lib/pkgconfig" in
          let new_value =
            match Sys.getenv_opt "PKG_CONFIG_PATH" with
            | None | Some "" -> pc_dir
            | Some v -> pc_dir ^ ":" ^ v
          in
          Unix.putenv "PKG_CONFIG_PATH" new_value
    end;
    let vendor_prefix =
      let provided = !vendor_prefix_arg in
      if provided <> "" then
        Some provided
      else
        match opam_prefix with
        | Some p -> Some p
        | None ->
            let root = !vendor_root in
            if root = "" then None else Some (Filename.concat root "vendor/_install")
    in
    let vendor_prefix =
      match vendor_prefix with
      | None -> None
      | Some p ->
          if Filename.is_relative p then
            Some (Filename.concat (Sys.getcwd ()) p)
          else
            Some p
    in
    let vendor_yices_flags sofar =
      match vendor_prefix with
      | None -> None
      | Some prefix ->
          let libdir = Filename.concat prefix "lib" in
          let incdir = Filename.concat prefix "include" in
          let candidates =
            [ Filename.concat libdir "libyices.a";
              Filename.concat libdir "libyices.so";
              Filename.concat libdir "libyices.dylib" ]
          in
          let has_yices = List.exists Sys.file_exists candidates in
          if not has_yices then None
          else
            Some
              { libs = sofar.libs @ [ "-L" ^ libdir; "-lyices"; "-lcudd" ];
                cflags = sofar.cflags @ [ "-I" ^ incdir ] }
    in
    let aux sofar (linux_name, macos_name) =
      let pkg_name = linux_name in
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
         | None ->
            if pkg_name = "yices" then
              match vendor_yices_flags sofar with
              | Some conf -> conf
              | None ->
                  C.die "Could not find yices via pkg-config or vendored install under %s"
                    (match vendor_prefix with
                     | None -> "<unset>"
                     | Some p -> p)
            else
              default()
         | Some deps ->
            let conf =
              { libs = sofar.libs @ deps.libs;
                cflags = sofar.cflags @ deps.cflags }
            in
            if pkg_name <> "yices" then
              conf
            else if has_mcsat conf then
              conf
            else
              match vendor_yices_flags sofar with
              | Some conf -> conf
              | None ->
                  C.die "Yices found but MCSAT is disabled; no vendored build under %s"
                    (match vendor_prefix with
                     | None -> "<unset>"
                     | Some p -> p)
    in
    let conf = List.fold_left aux base !pkg in
    let conf =
      let has_libpoly = List.exists ((=) "-lpoly") conf.libs in
      let has_libdir libdir =
        List.exists (fun flag -> flag = "-L" ^ libdir) conf.libs
      in
      match opam_prefix with
      | Some prefix ->
          let libdir = Filename.concat prefix "lib" in
          if has_libpoly && not (has_libdir libdir) then
            (* Yices' pkg-config adds -lpoly but not libpoly's opam libdir. *)
            { conf with libs = ("-L" ^ libdir) :: conf.libs }
          else
            conf
      | None -> conf
    in

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
