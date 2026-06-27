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
       try SMT2.SMT2.process_file ~syntax_hooks filename with
       | Yices2.SMT2.Yices_SMT2_exception msg ->
          Format.eprintf "string SMT2 error: %s@." msg;
          exit 1
       | Yices2.High.ExceptionsErrorHandling.YicesException (_, report) as exc ->
          Format.eprintf "%a@." Yices2.Ext.Types.pp_error_report report;
          raise exc
     end
  | [] -> failwith "Too few arguments in the command"
  | _ -> failwith "Too many arguments in the command"
