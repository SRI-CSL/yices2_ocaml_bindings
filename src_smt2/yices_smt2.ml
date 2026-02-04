open Containers

open Yices2.Ext

module SMT2_plain = Yices2.SMT2.Make(WithExceptionsErrorHandling)

let rec sexp_has_tuples : Sexplib.Sexp.t -> bool = function
  | Sexplib.Sexp.Atom _ -> false
  | Sexplib.Sexp.List (Sexplib.Sexp.Atom "Tuple" :: _tl) -> true
  | Sexplib.Sexp.List (Sexplib.Sexp.Atom "tuple" :: _tl) -> true
  | Sexplib.Sexp.List
      [ Sexplib.Sexp.List
          [ Sexplib.Sexp.Atom "_"; Sexplib.Sexp.Atom "tuple.select"; _i ];
        _x
      ] ->
     true
  | Sexplib.Sexp.List l -> List.exists sexp_has_tuples l

module MakeTupleBlastExt (Enabled : sig val enabled : bool end) = struct
  include WithExceptionsErrorHandling

  module Context = struct
    module Base = WithExceptionsErrorHandling.Context
    include Base

    let should_blast ctx = Enabled.enabled && Base.is_mcsat ctx

    let blast_formula f =
      match Extensions.Tuples.Arg.tuple_blast f with
      | Extensions.Tuples.Arg.TopTuple.Single f -> f
      | Extensions.Tuples.Arg.TopTuple.Multiple _ ->
         failwith "Tuple blasting a Boolean formula must return a single formula"

    let blast_flat t =
      Extensions.Tuples.Arg.tuple_blast t |> Extensions.Tuples.Arg.TopTuple.flatten

    let blast_formulas l = List.map blast_formula l
    let blast_terms_flat l = List.concat_map blast_flat l

    let assert_formula ctx f =
      if should_blast ctx then Base.assert_formula ctx (blast_formula f)
      else Base.assert_formula ctx f

    let assert_formulas ctx l =
      if should_blast ctx then Base.assert_formulas ctx (blast_formulas l)
      else Base.assert_formulas ctx l

    let check ?param ?assumptions ?smodel ?as_inequalities ?hints ctx =
      if should_blast ctx then
        let assumptions = Option.map blast_formulas assumptions in
        let hints = Option.map blast_terms_flat hints in
        Base.check ?param ?assumptions ?smodel ?as_inequalities ?hints ctx
      else
        Base.check ?param ?assumptions ?smodel ?as_inequalities ?hints ctx

    let set_fixed_var_order ctx l =
      if should_blast ctx then Base.set_fixed_var_order ctx (blast_terms_flat l)
      else Base.set_fixed_var_order ctx l

    let set_initial_var_order ctx l =
      if should_blast ctx then Base.set_initial_var_order ctx (blast_terms_flat l)
      else Base.set_initial_var_order ctx l
  end
end

let () =
  let args = ref [] in
  let description = "Executable for solving smt2 files. One filename as argument." in
  Arg.parse [] (fun a -> args := a :: !args) description;
  match !args with
  | [filename] ->
     (try
        let sexps = SMT2_plain.SMT2.load_file filename in
        let has_tuples = List.exists sexp_has_tuples sexps in
        let module Enabled = struct let enabled = has_tuples end in
        let module Ext = MakeTupleBlastExt (Enabled) in
        let module SMT2 = Yices2.SMT2.Make (Ext) in
        let session = SMT2.Session.create 0 in
        Format.printf "@[<v>";
        SMT2.SMT2.process_all session sexps;
        Format.printf "@]@."
      with
        ExceptionsErrorHandling.YicesException (_, report) as exc ->
        let bt = Printexc.get_backtrace () in
        Format.(fprintf stderr) "@[<v>%a@]%!" Types.pp_error_report report;
        Format.(fprintf stderr) "@[<v>%s@]%!" bt;
        raise exc)
  | [] -> failwith "Too few arguments in the command"
  | _ -> failwith "Too many arguments in the command"
