open Sexplib

module S = Extensions.Strings
module Escaping = Extensions.String_smt2_escaping
module Base = Yices2.Ext.WithExceptionsErrorHandling

module StringAPI = struct
  include Base

  module Type = S.Type
  module Term = S.Term
  module Context = S.Context

  module SModel = struct
    type t = S.StringModel.t

    let from_map ?support bindings =
      { S.StringModel.base = Base.SModel.from_map ?support bindings; strings = [] }

    let get_value model term =
      Base.SModel.get_value model.S.StringModel.base term

    let get_value_as_term model term =
      match S.StringModel.find_string model term with
      | Some text -> Some (S.Term.str text)
      | None -> Base.SModel.get_value_as_term model.S.StringModel.base term

    let to_sexp ~smt2arrays model =
      let string_bindings =
        List.map
          (fun (term, text) ->
             Sexp.List
               [
                 Sexp.Atom ":=";
                 S.Term.to_sexp ?smt2arrays term;
                 Sexp.Atom (Format.sprintf "%S" text);
               ])
          model.S.StringModel.strings
      in
      let base_bindings =
        match Base.SModel.to_sexp ~smt2arrays model.S.StringModel.base with
        | Sexp.List (Sexp.Atom "model" :: bindings) -> bindings
        | sexp -> [sexp]
      in
      Sexp.List (Sexp.Atom "model" :: string_bindings @ base_bindings)
  end
end

module SMT2 = Yices2.SMT2.Make_parser(StringAPI)
module Cont = Yices2.SMT2.Cont

let raise_smt2 fmt =
  Format.ksprintf (fun msg -> raise (Yices2.SMT2.Yices_SMT2_exception msg)) fmt

let sexp_to_string = Sexp.to_string_hum

let parse_nonnegative_int_atom s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> n
  | Some _ | None -> raise_smt2 "expected non-negative integer constant, got %s" s

let decode_string_literal sexp =
  match sexp with
  | Sexp.List [Sexp.Atom tag; Sexp.Atom hex] when String.equal tag Escaping.literal_tag ->
     begin
       match Escaping.decode_hex_literal_content hex with
       | Ok text -> Some text
       | Error msg -> raise_smt2 "%s" msg
     end
  | _ -> None

let required_string_literal sexp =
  match decode_string_literal sexp with
  | Some text -> text
  | None -> raise_smt2 "expected string literal, got %s" (sexp_to_string sexp)

let parse_type types sexp =
  match sexp with
  | Sexp.Atom "String" -> Some (S.Type.string ())
  | _ ->
     ignore types;
     None

type yices_logic = {
  logic : string;
  force_mcsat : bool;
}

let apply_yices_logic config { logic; force_mcsat } =
  Base.Config.default ~logic config;
  if force_mcsat then
    Base.Config.set config ~name:"solver-type" ~value:"mcsat"

let yices_logic_for_string_logic = function
  (* Pure strings and strings with linear integer arithmetic still need UF+LIA:
     string operators are abstracted with uninterpreted symbols and lengths. *)
  | "QF_S"
  | "QF_SLIA" ->
     Some { logic = "QF_UFLIA"; force_mcsat = false }

  (* Nonlinear integer arithmetic requires MCSAT in the underlying context. *)
  | "QF_SNIA" ->
     Some { logic = "QF_UFNIA"; force_mcsat = true }

  (* Strings always introduce integer length constraints, so real fragments use
     mixed integer/real arithmetic rather than a pure LRA/NRA logic. *)
  | "QF_SLRA"
  | "QF_SLIRA"
  | "QF_SLRIA" ->
     Some { logic = "QF_UFLIRA"; force_mcsat = false }
  | "QF_SNRA"
  | "QF_SNIRA"
  | "QF_SNRIA" ->
     Some { logic = "QF_UFNIRA"; force_mcsat = true }

  (* Arrays plus strings plus LIA fits a Yices-supported array/UF/LIA logic.
     Other array/BV/string combinations fall back to ALL below. *)
  | "QF_SALIA" ->
     Some { logic = "QF_AUFLIA"; force_mcsat = false }

  (* Yices has no precise SMT-LIB logic for UF+BV+LIA, which is what strings
     plus bitvectors require after length abstraction. *)
  | "QF_SBV"
  | "QF_SUFBV"
  | "QF_SABV"
  | "QF_SAUFBV" ->
     Some { logic = "ALL"; force_mcsat = false }

  | _ -> None

let string_logic logic config =
  match yices_logic_for_string_logic logic with
  | Some yices_logic ->
     apply_yices_logic config yices_logic;
     true
  | None -> false

let rec parse_regex : type a.
  (SMT2.Session.t -> Sexp.t -> (S.Term.t, a) Cont.t) ->
  SMT2.Session.t -> Sexp.t -> (S.regex, a) Cont.t =
  fun parse env sexp ->
  let open Cont in
  match sexp with
  | Sexp.Atom "re.none" -> return S.Regex.empty
  | Sexp.Atom "re.all" -> return S.Regex.all
  | Sexp.Atom "re.allchar" -> return S.Regex.all_char
  | Sexp.List [Sexp.Atom "str.to_re"; term] ->
     begin
       match decode_string_literal term with
       | Some text -> return (S.Regex.str text)
       | None ->
          let* term = parse env term in
          return (S.Regex.to_re term)
     end
  | Sexp.List (Sexp.Atom "re.++" :: regexes) ->
     let* regexes = Cont.map (parse_regex parse env) regexes in
     return (S.Regex.concat regexes)
  | Sexp.List (Sexp.Atom "re.union" :: regexes) ->
     let* regexes = Cont.map (parse_regex parse env) regexes in
     return (S.Regex.union regexes)
  | Sexp.List [Sexp.Atom "re.*"; regex] ->
     let* regex = parse_regex parse env regex in
     return (S.Regex.star regex)
  | Sexp.List (Sexp.Atom "re.inter" :: regexes) ->
     let* regexes = Cont.map (parse_regex parse env) regexes in
     return (S.Regex.inter regexes)
  | Sexp.List [Sexp.Atom "re.comp"; regex] ->
     let* regex = parse_regex parse env regex in
     return (S.Regex.comp regex)
  | Sexp.List [Sexp.Atom "re.+"; regex] ->
     let* regex = parse_regex parse env regex in
     return (S.Regex.plus regex)
  | Sexp.List [Sexp.Atom "re.opt"; regex] ->
     let* regex = parse_regex parse env regex in
     return (S.Regex.opt regex)
  | Sexp.List [Sexp.List [Sexp.Atom "_"; Sexp.Atom "re.loop"; Sexp.Atom lo; Sexp.Atom hi]; regex] ->
     let* regex = parse_regex parse env regex in
     return
       (S.Regex.loop
          ~lo:(parse_nonnegative_int_atom lo)
          ~hi:(parse_nonnegative_int_atom hi)
          regex)
  | Sexp.List [Sexp.Atom "re.range"; lo; hi] ->
     return (S.Regex.range (required_string_literal lo) (required_string_literal hi))
  | _ ->
     raise_smt2 "unsupported regex %s" (sexp_to_string sexp)

let parse_term : type a.
  (SMT2.Session.t -> Sexp.t -> (S.Term.t, a) Cont.t) ->
  SMT2.Session.t -> Sexp.t -> ((S.Term.t, a) Cont.t) option =
  fun parse env sexp ->
  let open Cont in
  match decode_string_literal sexp with
  | Some text -> Some (return (S.Term.str text))
  | None ->
     match sexp with
     | Sexp.List (Sexp.Atom head :: args) ->
        begin
          match head, args with
          | "str.++", args ->
             Some
               (let* args = Cont.map (parse env) args in
                return (S.Term.concat args))
          | "str.len", [arg] ->
             Some
               (let* arg = parse env arg in
                return (S.Term.len arg))
          | "str.substr", [string; start; length] ->
             Some
               (let* string = parse env string in
                let* start = parse env start in
                let* length = parse env length in
                return (S.Term.substr string start length))
          | "str.contains", [haystack; needle] ->
             Some
               (let* haystack = parse env haystack in
                let* needle = parse env needle in
                return (S.Term.contains haystack needle))
          | "str.indexof", [haystack; needle; start] ->
             Some
               (let* haystack = parse env haystack in
                let* needle = parse env needle in
                let* start = parse env start in
                return (S.Term.indexof haystack needle start))
          | "str.replace", [haystack; needle; replacement] ->
             Some
               (let* haystack = parse env haystack in
                let* needle = parse env needle in
                let* replacement = parse env replacement in
                return (S.Term.replace haystack needle replacement))
          | "str.replace_all", [haystack; needle; replacement] ->
             Some
               (let* haystack = parse env haystack in
                let* needle = parse env needle in
                let* replacement = parse env replacement in
                return (S.Term.replace_all haystack needle replacement))
          | "str.to_code", [string] ->
             Some
               (let* string = parse env string in
                return (S.Term.to_code string))
          | "str.from_code", [code] ->
             Some
               (let* code = parse env code in
                return (S.Term.from_code code))
          | "str.prefixof", [prefix; string] ->
             Some
               (let* prefix = parse env prefix in
                let* string = parse env string in
                return (S.Term.prefixof prefix string))
          | "str.suffixof", [suffix; string] ->
             Some
               (let* suffix = parse env suffix in
                let* string = parse env string in
                return (S.Term.suffixof suffix string))
          | "str.at", [string; index] ->
             Some
               (let* string = parse env string in
                let* index = parse env index in
                return (S.Term.at string index))
          | "str.in_re", [string; regex] ->
             Some
               (let* string = parse env string in
                let* regex = parse_regex parse env regex in
                return (S.Term.in_re string regex))
          | ( "str.len"
            | "str.substr"
            | "str.contains"
            | "str.indexof"
            | "str.replace"
            | "str.replace_all"
            | "str.to_code"
            | "str.from_code"
            | "str.prefixof"
            | "str.suffixof"
            | "str.at"
            | "str.in_re" ), _ ->
             raise_smt2 "wrong arity for %s in %s" head (sexp_to_string sexp)
          | _ -> None
        end
     | _ -> None

let syntax_hooks : SMT2.Session.syntax_hooks = {
    parse_type;
    parse_term;
    set_logic = string_logic;
  }

module Session = SMT2.Session

type fmf_config = {
  enabled : bool;
  max_total_length : int;
  max_rounds : int;
  log : bool;
}

type fmf_split = {
  prefix : Sexp.t list;
  suffix : Sexp.t list;
}

let bool_from_env name =
  match Sys.getenv_opt name with
  | None -> false
  | Some raw ->
     let raw = String.lowercase_ascii raw in
     List.mem raw ["1"; "true"; "yes"; "on"]

let nonnegative_env name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> (
      match int_of_string_opt raw with
      | Some n when n >= 0 -> n
      | _ -> default)

let fmf_config_from_env () =
  let max_total_length =
    nonnegative_env "YICES_STRING_FMF_MAX_TOTAL_LENGTH" 8
  in
  {
    enabled = bool_from_env "YICES_STRING_FMF";
    max_total_length;
    max_rounds =
      nonnegative_env
        "YICES_STRING_FMF_MAX_ROUNDS"
        (max_total_length + 1);
    log = bool_from_env "YICES_STRING_FMF_LOG";
  }

let fmf_log config fmt =
  Format.kasprintf
    (fun msg ->
       if config.log then Format.eprintf "FMF: %s@." msg)
    fmt

let command_head = function
  | Sexp.List (Sexp.Atom head :: _) -> Some head
  | _ -> None

let is_check_sat = function
  | Sexp.List [Sexp.Atom "check-sat"] -> true
  | _ -> false

let is_quiet_prefix_command sexp =
  match command_head sexp with
  | Some
      ( "set-logic"
      | "set-option"
      | "set-info"
      | "declare-sort"
      | "declare-fun"
      | "declare-const"
      | "declare-datatypes"
      | "declare-datatype"
      | "define-sort"
      | "define-fun"
      | "define-funs-rec"
      | "define-fun-rec"
      | "assert"
      | "push"
      | "pop"
      | "reset-assertions" ) ->
     true
  | _ -> false

let is_quiet_suffix_command sexp =
  match command_head sexp with
  | Some "exit" -> true
  | _ -> false

let split_single_check sexps =
  let rec scan prefix = function
    | [] -> None
    | sexp :: suffix when is_check_sat sexp ->
       if List.exists is_check_sat suffix then None
       else Some { prefix = List.rev prefix; suffix }
    | sexp :: rest -> scan (sexp :: prefix) rest
  in
  scan [] sexps

let string_root_name = function
  | Sexp.List [Sexp.Atom "declare-const"; Sexp.Atom name; Sexp.Atom "String"] ->
     Some name
  | Sexp.List
      [
        Sexp.Atom "declare-fun";
        Sexp.Atom name;
        Sexp.List [];
        Sexp.Atom "String";
      ] ->
     Some name
  | Sexp.List
      [
        Sexp.Atom "define-fun";
        Sexp.Atom name;
        Sexp.List [];
        Sexp.Atom "String";
        _;
      ] ->
     Some name
  | _ -> None

let unique_strings strings =
  List.sort_uniq String.compare strings

let string_roots sexps =
  List.filter_map string_root_name sexps |> unique_strings

let length_bound_assertion roots bound =
  let length_term name =
    Sexp.List [Sexp.Atom "str.len"; Sexp.Atom name]
  in
  let total =
    match List.map length_term roots with
    | [] -> Sexp.Atom "0"
    | [term] -> term
    | terms -> Sexp.List (Sexp.Atom "+" :: terms)
  in
  Sexp.List
    [
      Sexp.Atom "assert";
      Sexp.List [Sexp.Atom "<="; total; Sexp.Atom (string_of_int bound)];
    ]

let cleanup_after_bounded_round () =
  try StringAPI.Global.reset () with _ -> ()

let bounded_status sexps =
  let session = Session.create ~syntax_hooks 0 in
  try
    SMT2.SMT2.process_all session sexps;
    let status =
      match StringAPI.Context.of_id 0 with
      | Some context -> StringAPI.Context.check ~param:session.Session.param context
      | None -> raise_smt2 "FMF round did not create context 0"
    in
    cleanup_after_bounded_round ();
    status
  with exn ->
    cleanup_after_bounded_round ();
    raise exn

let fmf_applicable config sexps =
  match split_single_check sexps with
  | None -> Error "expected exactly one plain check-sat command"
  | Some split ->
     if not (List.for_all is_quiet_prefix_command split.prefix) then
       Error "prefix contains output-producing or unsupported commands"
     else if not (List.for_all is_quiet_suffix_command split.suffix) then
       Error "commands after check-sat require ordinary processing"
     else
       let roots = string_roots split.prefix in
       if List.is_empty roots then Error "no root string variables to bound"
       else if config.max_rounds = 0 then Error "YICES_STRING_FMF_MAX_ROUNDS is 0"
       else Ok (split, roots)

let process_file_with_fmf config filename =
  let sexps = SMT2.SMT2.load_file filename in
  match fmf_applicable config sexps with
  | Error reason ->
     fmf_log config "disabled: %s" reason;
     SMT2.SMT2.process_file ~syntax_hooks filename
  | Ok ({ prefix; suffix = _ }, roots) ->
     let last_bound =
       min config.max_total_length (config.max_rounds - 1)
     in
     let rec loop bound =
       if bound > last_bound then begin
         fmf_log config
           "falling back after all bounded rounds through total length %d"
           last_bound;
         SMT2.SMT2.process_file ~syntax_hooks filename
       end else
         let bounded_sexps =
           prefix @ [length_bound_assertion roots bound]
         in
         match (try Ok (bounded_status bounded_sexps) with exn -> Error exn) with
         | Error exn ->
            fmf_log config
              "bounded round at total length %d raised %s; falling back"
              bound
              (Printexc.to_string exn);
            SMT2.SMT2.process_file ~syntax_hooks filename
         | Ok `STATUS_SAT ->
            fmf_log config "bounded sat at total length %d" bound;
            print_endline "sat"
         | Ok `STATUS_UNSAT ->
            fmf_log config "bounded unsat at total length %d" bound;
            loop (bound + 1)
         | Ok `STATUS_UNKNOWN ->
            fmf_log config
              "bounded round at total length %d returned unknown; falling back"
              bound;
            SMT2.SMT2.process_file ~syntax_hooks filename
         | Ok status ->
            fmf_log config
              "bounded round at total length %d returned %a; falling back"
              bound
              Yices2.Ext.Types.pp_smt_status
              status;
            SMT2.SMT2.process_file ~syntax_hooks filename
     in
     loop 0

let () =
  Printexc.record_backtrace true;
  let args = ref [] in
  let description =
    "Executable for SMT-LIB string benchmarks. One filename as argument."
  in
  Arg.parse [] (fun arg -> args := arg :: !args) description;
  match List.rev !args with
  | [filename] ->
     begin
       try
         let fmf_config = fmf_config_from_env () in
         if fmf_config.enabled then process_file_with_fmf fmf_config filename
         else SMT2.SMT2.process_file ~syntax_hooks filename
       with
       | Yices2.SMT2.Yices_SMT2_exception msg ->
          Format.eprintf "string SMT2 error: %s@." msg;
          exit 1
       | Yices2.High.ExceptionsErrorHandling.YicesException (_, report) as exc ->
          Format.eprintf "%a@." Yices2.Ext.Types.pp_error_report report;
          raise exc
     end
  | [] -> failwith "Too few arguments in the command"
  | _ -> failwith "Too many arguments in the command"
