open Containers
open Sexplib

open Yices2
open Ext.WithExceptionsErrorHandling
open Builder
open Types_ext

module YTypes = Yices2.Ext.Types
module HTerms = Yices2.Ext.Types.HTerms
module RA = Regex_automata

type string_view =
  | Lit of string
  | Concat of Term.t list
  | Len of Term.t
  | Substr of Term.t * Term.t * Term.t
  | Contains of Term.t * Term.t
  | Indexof of Term.t * Term.t * Term.t
  | Replace of Term.t * Term.t * Term.t
  | ReplaceAll of Term.t * Term.t * Term.t
  | ToCode of Term.t
  | FromCode of Term.t
  | Prefixof of Term.t * Term.t
  | Suffixof of Term.t * Term.t
  | At of Term.t * Term.t
  | InRe of Term.t * regex

and regex =
  | ReEmpty
  | ReAll
  | ReAllChar
  | ReLit of string
  | ReToRe of Term.t
  | ReRange of int * int
  | ReConcat of regex list
  | ReUnion of regex list
  | ReStar of regex
  | ReInter of regex list
  | ReComp of regex
  | RePlus of regex
  | ReOpt of regex
  | ReLoop of regex * int * int

type literal_info = {
  id : int;
  text : string;
  scalar_length : int;
}

let string_type_ref = ref None
let len_symbol_ref = ref None
let substr_symbol_ref = ref None
let contains_symbol_ref = ref None
let indexof_symbol_ref = ref None
let replace_symbol_ref = ref None
let replace_all_symbol_ref = ref None
let to_code_symbol_ref = ref None
let from_code_symbol_ref = ref None
let prefixof_symbol_ref = ref None
let suffixof_symbol_ref = ref None
let at_symbol_ref = ref None
let concat_symbols : (int, Term.t) Hashtbl.t = Hashtbl.create 17
let rec regex_equal lhs rhs =
  match lhs, rhs with
  | ReEmpty, ReEmpty | ReAll, ReAll | ReAllChar, ReAllChar -> true
  | ReLit lhs, ReLit rhs -> String.equal lhs rhs
  | ReToRe lhs, ReToRe rhs -> Term.equal lhs rhs
  | ReRange (llo, lhi), ReRange (rlo, rhi) -> llo = rlo && lhi = rhi
  | ReConcat lhs, ReConcat rhs
  | ReUnion lhs, ReUnion rhs
  | ReInter lhs, ReInter rhs ->
      List.length lhs = List.length rhs && List.for_all2 regex_equal lhs rhs
  | ReStar lhs, ReStar rhs
  | ReComp lhs, ReComp rhs
  | RePlus lhs, RePlus rhs
  | ReOpt lhs, ReOpt rhs ->
      regex_equal lhs rhs
  | ReLoop (lbody, llo, lhi), ReLoop (rbody, rlo, rhi) ->
      llo = rlo && lhi = rhi && regex_equal lbody rbody
  | _ -> false

let rec regex_hash = function
  | ReEmpty -> Hashtbl.hash 0
  | ReAll -> Hashtbl.hash 1
  | ReAllChar -> Hashtbl.hash 2
  | ReLit text -> Hashtbl.hash (3, text)
  | ReToRe term -> Hashtbl.hash (4, Term.hash term)
  | ReRange (lo, hi) -> Hashtbl.hash (5, lo, hi)
  | ReConcat regexes -> Hashtbl.hash (6, List.map regex_hash regexes)
  | ReUnion regexes -> Hashtbl.hash (7, List.map regex_hash regexes)
  | ReStar regex -> Hashtbl.hash (8, regex_hash regex)
  | ReInter regexes -> Hashtbl.hash (9, List.map regex_hash regexes)
  | ReComp regex -> Hashtbl.hash (10, regex_hash regex)
  | RePlus regex -> Hashtbl.hash (11, regex_hash regex)
  | ReOpt regex -> Hashtbl.hash (12, regex_hash regex)
  | ReLoop (regex, lo, hi) -> Hashtbl.hash (13, regex_hash regex, lo, hi)

module HRegex = Hashtbl.Make(struct
  type t = regex
  let equal = regex_equal
  let hash = regex_hash
end)

let regex_symbols : Term.t HRegex.t = HRegex.create 17
let literal_ids : (string, literal_info) Hashtbl.t = Hashtbl.create 101
let next_literal_id = ref 0

let literal_terms = Global.hTerms_create 101
let term_views = Global.hTerms_create 257

let () =
  Global.register_cleanup (fun ~after:_ ->
      string_type_ref := None;
      len_symbol_ref := None;
      substr_symbol_ref := None;
      contains_symbol_ref := None;
      indexof_symbol_ref := None;
      replace_symbol_ref := None;
      replace_all_symbol_ref := None;
      to_code_symbol_ref := None;
      from_code_symbol_ref := None;
      prefixof_symbol_ref := None;
      suffixof_symbol_ref := None;
      at_symbol_ref := None;
      Hashtbl.clear concat_symbols;
      HRegex.clear regex_symbols;
      Hashtbl.clear literal_ids;
      next_literal_id := 0)

let raise_invalid_utf8 s i =
  Yices2.High.ExceptionsErrorHandling.raise_bindings_error
    "invalid UTF-8 string literal at byte offset %d in %S" i s

let unicode_max = 0x10FFFF
let surrogate_lo = 0xD800
let surrogate_hi = 0xDFFF

let valid_scalar_code code =
  0 <= code
  && code <= unicode_max
  && not (surrogate_lo <= code && code <= surrogate_hi)

let string_of_scalar_code code =
  if not (valid_scalar_code code) then
    invalid_arg "string_of_scalar_code: invalid Unicode scalar";
  let buffer = Buffer.create 4 in
  if code <= 0x7F then
    Buffer.add_char buffer (Char.chr code)
  else if code <= 0x7FF then begin
    Buffer.add_char buffer (Char.chr (0xC0 lor (code lsr 6)));
    Buffer.add_char buffer (Char.chr (0x80 lor (code land 0x3F)))
  end else if code <= 0xFFFF then begin
    Buffer.add_char buffer (Char.chr (0xE0 lor (code lsr 12)));
    Buffer.add_char buffer (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buffer (Char.chr (0x80 lor (code land 0x3F)))
  end else begin
    Buffer.add_char buffer (Char.chr (0xF0 lor (code lsr 18)));
    Buffer.add_char buffer (Char.chr (0x80 lor ((code lsr 12) land 0x3F)));
    Buffer.add_char buffer (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buffer (Char.chr (0x80 lor (code land 0x3F)))
  end;
  Buffer.contents buffer

let utf8_scalar_length s =
  let len = String.length s in
  let byte i = Char.code s.[i] in
  let continuation i =
    i < len && byte i land 0xC0 = 0x80
  in
  let rec loop count i =
    if i = len then count
    else
      let b0 = byte i in
      if b0 <= 0x7F then
        loop (count + 1) (i + 1)
      else if b0 >= 0xC2 && b0 <= 0xDF then
        if continuation (i + 1) then loop (count + 1) (i + 2)
        else raise_invalid_utf8 s i
      else if b0 = 0xE0 then
        if i + 2 < len
           && byte (i + 1) >= 0xA0 && byte (i + 1) <= 0xBF
           && continuation (i + 2)
        then loop (count + 1) (i + 3)
        else raise_invalid_utf8 s i
      else if (b0 >= 0xE1 && b0 <= 0xEC) || (b0 >= 0xEE && b0 <= 0xEF) then
        if continuation (i + 1) && continuation (i + 2)
        then loop (count + 1) (i + 3)
        else raise_invalid_utf8 s i
      else if b0 = 0xED then
        if i + 2 < len
           && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x9F
           && continuation (i + 2)
        then loop (count + 1) (i + 3)
        else raise_invalid_utf8 s i
      else if b0 = 0xF0 then
        if i + 3 < len
           && byte (i + 1) >= 0x90 && byte (i + 1) <= 0xBF
           && continuation (i + 2)
           && continuation (i + 3)
        then loop (count + 1) (i + 4)
        else raise_invalid_utf8 s i
      else if b0 >= 0xF1 && b0 <= 0xF3 then
        if continuation (i + 1) && continuation (i + 2) && continuation (i + 3)
        then loop (count + 1) (i + 4)
        else raise_invalid_utf8 s i
      else if b0 = 0xF4 then
        if i + 3 < len
           && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x8F
           && continuation (i + 2)
           && continuation (i + 3)
        then loop (count + 1) (i + 4)
        else raise_invalid_utf8 s i
      else
        raise_invalid_utf8 s i
  in
  loop 0 0

let string_type () =
  match !string_type_ref with
  | Some ty when Type.is_good ty -> ty
  | _ ->
      let ty = Type.new_uninterpreted ~name:"String" () in
      string_type_ref := Some ty;
      ty

let is_string_type ty =
  Type.equal ty (string_type ())

let check_string_term t =
  let ty = Term.type_of_term t in
  if not (is_string_type ty) then
    Yices2.High.ExceptionsErrorHandling.raise_bindings_error
      "expected a String term, got %a of type %a" Term.pp t Type.pp ty

let check_int_term t =
  let ty = Term.type_of_term t in
  if not (Type.equal ty Type.(int ())) then
    Yices2.High.ExceptionsErrorHandling.raise_bindings_error
      "expected an Int term, got %a of type %a" Term.pp t Type.pp ty

let len_symbol () =
  match !len_symbol_ref with
  | Some f when Term.is_good f -> f
  | _ ->
      let f =
        Term.new_uninterpreted
          ~name:"__yices_string_len"
          Type.(func [string_type ()] (int ()))
      in
      len_symbol_ref := Some f;
      f

let cached_symbol slot name typ =
  match !slot with
  | Some f when Term.is_good f -> f
  | _ ->
      let f = Term.new_uninterpreted ~name typ in
      slot := Some f;
      f

let substr_symbol () =
  cached_symbol
    substr_symbol_ref
    "__yices_string_substr"
    Type.(func [string_type (); int (); int ()] (string_type ()))

let contains_symbol () =
  cached_symbol
    contains_symbol_ref
    "__yices_string_contains"
    Type.(func [string_type (); string_type ()] (bool ()))

let indexof_symbol () =
  cached_symbol
    indexof_symbol_ref
    "__yices_string_indexof"
    Type.(func [string_type (); string_type (); int ()] (int ()))

let replace_symbol () =
  cached_symbol
    replace_symbol_ref
    "__yices_string_replace"
    Type.(func [string_type (); string_type (); string_type ()] (string_type ()))

let replace_all_symbol () =
  cached_symbol
    replace_all_symbol_ref
    "__yices_string_replace_all"
    Type.(func [string_type (); string_type (); string_type ()] (string_type ()))

let to_code_symbol () =
  cached_symbol
    to_code_symbol_ref
    "__yices_string_to_code"
    Type.(func [string_type ()] (int ()))

let from_code_symbol () =
  cached_symbol
    from_code_symbol_ref
    "__yices_string_from_code"
    Type.(func [int ()] (string_type ()))

let prefixof_symbol () =
  cached_symbol
    prefixof_symbol_ref
    "__yices_string_prefixof"
    Type.(func [string_type (); string_type ()] (bool ()))

let suffixof_symbol () =
  cached_symbol
    suffixof_symbol_ref
    "__yices_string_suffixof"
    Type.(func [string_type (); string_type ()] (bool ()))

let at_symbol () =
  cached_symbol
    at_symbol_ref
    "__yices_string_at"
    Type.(func [string_type (); int ()] (string_type ()))

let concat_symbol arity =
  if arity < 2 then
    Yices2.High.ExceptionsErrorHandling.raise_bindings_error
      "internal error: concat_symbol expects arity >= 2, got %d" arity;
  match Hashtbl.find_opt concat_symbols arity with
  | Some f when Term.is_good f -> f
  | _ ->
      let dom = List.init arity (fun _ -> string_type ()) in
      let f =
        Term.new_uninterpreted
          ~name:(Format.sprintf "__yices_string_concat_%d" arity)
          Type.(func dom (string_type ()))
      in
      Hashtbl.replace concat_symbols arity f;
      f

let regex_symbol regex =
  match HRegex.find_opt regex_symbols regex with
  | Some f when Term.is_good f -> f
  | _ ->
      let name =
        Format.sprintf "__yices_string_in_re_%d" (HRegex.length regex_symbols)
      in
      let f = Term.new_uninterpreted ~name Type.(func [string_type ()] (bool ())) in
      HRegex.replace regex_symbols regex f;
      f

let record_view term view =
  HTerms.replace term_views term view;
  term

let literal s =
  match Hashtbl.find_opt literal_ids s with
  | Some info ->
      let term = Term.constant (string_type ()) ~id:info.id in
      record_view term (Lit info.text)
  | None ->
      let scalar_length = utf8_scalar_length s in
      let id = !next_literal_id in
      incr next_literal_id;
      let info = { id; text = s; scalar_length } in
      Hashtbl.add literal_ids s info;
      let term = Term.constant (string_type ()) ~id in
      HTerms.replace literal_terms term info;
      record_view term (Lit s)

let literal_info term =
  HTerms.find_opt literal_terms term

let reveal_string term =
  HTerms.find_opt term_views term

let rec flatten_concat acc = function
  | [] -> List.rev acc
  | term :: tail ->
      match reveal_string term with
      | Some (Concat terms) -> flatten_concat acc (terms @ tail)
      | _ -> flatten_concat (term :: acc) tail

let concat terms =
  let terms = flatten_concat [] terms in
  match terms with
  | [] -> literal ""
  | [term] ->
      check_string_term term;
      term
  | _ ->
      List.iter check_string_term terms;
      let term = Term.application (concat_symbol (List.length terms)) terms in
      record_view term (Concat terms)

let len term =
  check_string_term term;
  let lterm = Term.application (len_symbol ()) [term] in
  record_view lterm (Len term)

let substr string start length =
  check_string_term string;
  check_int_term start;
  check_int_term length;
  let term = Term.application (substr_symbol ()) [string; start; length] in
  record_view term (Substr (string, start, length))

let contains haystack needle =
  check_string_term haystack;
  check_string_term needle;
  let term = Term.application (contains_symbol ()) [haystack; needle] in
  record_view term (Contains (haystack, needle))

let indexof haystack needle start =
  check_string_term haystack;
  check_string_term needle;
  check_int_term start;
  let term = Term.application (indexof_symbol ()) [haystack; needle; start] in
  record_view term (Indexof (haystack, needle, start))

let replace haystack needle replacement =
  check_string_term haystack;
  check_string_term needle;
  check_string_term replacement;
  let term =
    Term.application (replace_symbol ()) [haystack; needle; replacement]
  in
  record_view term (Replace (haystack, needle, replacement))

let replace_all haystack needle replacement =
  check_string_term haystack;
  check_string_term needle;
  check_string_term replacement;
  let term =
    Term.application (replace_all_symbol ()) [haystack; needle; replacement]
  in
  record_view term (ReplaceAll (haystack, needle, replacement))

let to_code string =
  check_string_term string;
  let term = Term.application (to_code_symbol ()) [string] in
  record_view term (ToCode string)

let from_code code =
  check_int_term code;
  let term = Term.application (from_code_symbol ()) [code] in
  record_view term (FromCode code)

let prefixof prefix string =
  check_string_term prefix;
  check_string_term string;
  let term = Term.application (prefixof_symbol ()) [prefix; string] in
  record_view term (Prefixof (prefix, string))

let suffixof suffix string =
  check_string_term suffix;
  check_string_term string;
  let term = Term.application (suffixof_symbol ()) [suffix; string] in
  record_view term (Suffixof (suffix, string))

let at string index =
  check_string_term string;
  check_int_term index;
  let term = Term.application (at_symbol ()) [string; index] in
  record_view term (At (string, index))

let in_re string regex =
  check_string_term string;
  let term = Term.application (regex_symbol regex) [string] in
  record_view term (InRe (string, regex))

module StringModel = struct
  type t = {
    base : SModel.t;
    strings : (Term.t * string) list;
  }

  let find_string model term =
    List.find_map
      (fun (key, value) -> if Term.equal key term then Some value else None)
      model.strings
end

module StringTermSet = Set.Make(Term)

type eq_atom = {
  atom : Term.t;
  lhs : Term.t;
  rhs : Term.t;
}

type witness_key =
  | ContainsPrefix of Term.t * Term.t
  | ContainsSuffix of Term.t * Term.t
  | SubstrPrefix of Term.t * Term.t * Term.t
  | SubstrSuffix of Term.t * Term.t * Term.t
  | IndexofPrefix of Term.t * Term.t * Term.t
  | IndexofSuffix of Term.t * Term.t * Term.t
  | ReplacePrefix of Term.t * Term.t * Term.t
  | ReplaceSuffix of Term.t * Term.t * Term.t
  | ReplaceAllPrefix of Term.t * Term.t * Term.t
  | ReplaceAllSuffix of Term.t * Term.t * Term.t
[@@warning "-37"]

module WitnessKey = struct
  type t = witness_key

  let equal lhs rhs =
    match lhs, rhs with
    | ContainsPrefix (lh, ln), ContainsPrefix (rh, rn)
    | ContainsSuffix (lh, ln), ContainsSuffix (rh, rn) ->
        Term.equal lh rh && Term.equal ln rn
    | SubstrPrefix (ls, li, ln), SubstrPrefix (rs, ri, rn)
    | SubstrSuffix (ls, li, ln), SubstrSuffix (rs, ri, rn) ->
        Term.equal ls rs && Term.equal li ri && Term.equal ln rn
    | IndexofPrefix (lh, ln, li), IndexofPrefix (rh, rn, ri)
    | IndexofSuffix (lh, ln, li), IndexofSuffix (rh, rn, ri) ->
        Term.equal lh rh && Term.equal ln rn && Term.equal li ri
    | ReplacePrefix (lh, ln, lr), ReplacePrefix (rh, rn, rr)
    | ReplaceSuffix (lh, ln, lr), ReplaceSuffix (rh, rn, rr)
    | ReplaceAllPrefix (lh, ln, lr), ReplaceAllPrefix (rh, rn, rr)
    | ReplaceAllSuffix (lh, ln, lr), ReplaceAllSuffix (rh, rn, rr) ->
        Term.equal lh rh && Term.equal ln rn && Term.equal lr rr
    | _ -> false

  let hash = function
    | ContainsPrefix (haystack, needle) ->
        Hashtbl.hash (0, Term.hash haystack, Term.hash needle)
    | ContainsSuffix (haystack, needle) ->
        Hashtbl.hash (1, Term.hash haystack, Term.hash needle)
    | SubstrPrefix (string, start, length) ->
        Hashtbl.hash (2, Term.hash string, Term.hash start, Term.hash length)
    | SubstrSuffix (string, start, length) ->
        Hashtbl.hash (3, Term.hash string, Term.hash start, Term.hash length)
    | IndexofPrefix (haystack, needle, start) ->
        Hashtbl.hash (4, Term.hash haystack, Term.hash needle, Term.hash start)
    | IndexofSuffix (haystack, needle, start) ->
        Hashtbl.hash (5, Term.hash haystack, Term.hash needle, Term.hash start)
    | ReplacePrefix (haystack, needle, replacement) ->
        Hashtbl.hash
          (6, Term.hash haystack, Term.hash needle, Term.hash replacement)
    | ReplaceSuffix (haystack, needle, replacement) ->
        Hashtbl.hash
          (7, Term.hash haystack, Term.hash needle, Term.hash replacement)
    | ReplaceAllPrefix (haystack, needle, replacement) ->
        Hashtbl.hash
          (8, Term.hash haystack, Term.hash needle, Term.hash replacement)
    | ReplaceAllSuffix (haystack, needle, replacement) ->
        Hashtbl.hash
          (9, Term.hash haystack, Term.hash needle, Term.hash replacement)
end

module HWitness = Hashtbl.Make(WitnessKey)

let witness_key_name = function
  | ContainsPrefix _ -> "contains-prefix"
  | ContainsSuffix _ -> "contains-suffix"
  | SubstrPrefix _ -> "substr-prefix"
  | SubstrSuffix _ -> "substr-suffix"
  | IndexofPrefix _ -> "indexof-prefix"
  | IndexofSuffix _ -> "indexof-suffix"
  | ReplacePrefix _ -> "replace-prefix"
  | ReplaceSuffix _ -> "replace-suffix"
  | ReplaceAllPrefix _ -> "replace-all-prefix"
  | ReplaceAllSuffix _ -> "replace-all-suffix"

type refinement_operator =
  | Op_concat
  | Op_substr
  | Op_contains
  | Op_indexof
  | Op_replace
  | Op_replace_all
  | Op_to_code
  | Op_from_code
  | Op_prefixof
  | Op_suffixof
  | Op_at
  | Op_in_re

let refinement_operator_name = function
  | Op_concat -> "concat"
  | Op_substr -> "substr"
  | Op_contains -> "contains"
  | Op_indexof -> "indexof"
  | Op_replace -> "replace"
  | Op_replace_all -> "replace_all"
  | Op_to_code -> "to_code"
  | Op_from_code -> "from_code"
  | Op_prefixof -> "prefixof"
  | Op_suffixof -> "suffixof"
  | Op_at -> "at"
  | Op_in_re -> "in_re"

let refinement_operator_of_view = function
  | Substr _ -> Some Op_substr
  | Contains _ -> Some Op_contains
  | Indexof _ -> Some Op_indexof
  | Replace _ -> Some Op_replace
  | ReplaceAll _ -> Some Op_replace_all
  | ToCode _ -> Some Op_to_code
  | FromCode _ -> Some Op_from_code
  | Prefixof _ -> Some Op_prefixof
  | Suffixof _ -> Some Op_suffixof
  | At _ -> Some Op_at
  | InRe _ -> Some Op_in_re
  | Lit _ | Concat _ | Len _ -> None

type stats = {
  mutable refinement_iterations : int;
  mutable generated_lemmas : int;
  mutable generated_witnesses : int;
  mutable active_iterations : int;
  mutable length_finite_lemmas : int;
  mutable length_periodic_lemmas : int;
  mutable length_lower_bound_lemmas : int;
  mutable length_failed_lemmas : int;
  mutable length_combined_lemmas : int;
  operator_counts : (string, int) Hashtbl.t;
}

type t = {
  mutable frames : Term.t list list;
  mutable internal_frames : Term.t list list;
  generated : unit HTerms.t;
  generated_rewrites : unit HTerms.t;
  generated_contains_splits : unit HTerms.t;
  generated_symbolic_reductions : unit HTerms.t;
  witnesses : Term.t HWitness.t;
  mutable last_unknown : string option;
  mutable last_strings : (Term.t * string) list;
  mutable next_witness_id : int;
  refinement_limit : int;
  witness_limit : int;
  witness_round_limit : int;
  stats : stats;
}

let current_terms state =
  List.concat state.frames @ List.concat state.internal_frames

let public_terms state =
  List.concat state.frames

let reset_generated state =
  HTerms.reset state.generated;
  HTerms.reset state.generated_rewrites;
  HTerms.reset state.generated_contains_splits;
  HTerms.reset state.generated_symbolic_reductions

let default_refinement_limit = 100
let default_witness_limit = 100
let default_witness_round_limit = 10

let nonnegative_int_from_env name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> (
      match int_of_string_opt raw with
      | Some n when n >= 0 -> n
      | _ ->
          String_log.warn
            "ignoring invalid %s=%S; using %d"
            name raw default;
          default)

let refinement_limit_from_env () =
  nonnegative_int_from_env
    "YICES_STRING_REFINEMENT_LIMIT"
    default_refinement_limit

let witness_limit_from_env () =
  nonnegative_int_from_env "YICES_STRING_WITNESS_LIMIT" default_witness_limit

let witness_round_limit_from_env () =
  nonnegative_int_from_env
    "YICES_STRING_WITNESS_ROUND_LIMIT"
    default_witness_round_limit

let empty_stats () =
  {
    refinement_iterations = 0;
    generated_lemmas = 0;
    generated_witnesses = 0;
    active_iterations = 0;
    length_finite_lemmas = 0;
    length_periodic_lemmas = 0;
    length_lower_bound_lemmas = 0;
    length_failed_lemmas = 0;
    length_combined_lemmas = 0;
    operator_counts = Hashtbl.create 17;
  }

let reset_stats stats =
  stats.refinement_iterations <- 0;
  stats.generated_lemmas <- 0;
  stats.generated_witnesses <- 0;
  stats.active_iterations <- 0;
  stats.length_finite_lemmas <- 0;
  stats.length_periodic_lemmas <- 0;
  stats.length_lower_bound_lemmas <- 0;
  stats.length_failed_lemmas <- 0;
  stats.length_combined_lemmas <- 0;
  Hashtbl.clear stats.operator_counts

let reset_active_iterations state =
  state.stats.active_iterations <- 0

let increment_operator_count stats op =
  let key = refinement_operator_name op in
  let count = Option.value ~default:0 (Hashtbl.find_opt stats.operator_counts key) in
  Hashtbl.replace stats.operator_counts key (count + 1)

let record_refinement_lemma stats op =
  stats.generated_lemmas <- stats.generated_lemmas + 1;
  increment_operator_count stats op

let refinement_operator_count stats op =
  let key = refinement_operator_name op in
  Option.value ~default:0 (Hashtbl.find_opt stats.operator_counts key)

let record_length_finite_lemma stats =
  stats.length_finite_lemmas <- stats.length_finite_lemmas + 1

let record_length_periodic_lemma stats =
  stats.length_periodic_lemmas <- stats.length_periodic_lemmas + 1

let record_length_lower_bound_lemma stats =
  stats.length_lower_bound_lemmas <- stats.length_lower_bound_lemmas + 1

let record_length_failed_lemma stats =
  stats.length_failed_lemmas <- stats.length_failed_lemmas + 1

let record_length_combined_lemma stats =
  stats.length_combined_lemmas <- stats.length_combined_lemmas + 1

let operator_counts_summary stats =
  Hashtbl.fold
    (fun key count acc -> (key, count) :: acc)
    stats.operator_counts
    []
  |> List.sort (fun (lhs, _) (rhs, _) -> String.compare lhs rhs)
  |> List.map (fun (key, count) -> Format.sprintf "%s=%d" key count)
  |> String.concat ", "

let log_stats state outcome =
  let operator_counts = operator_counts_summary state.stats in
  if String.equal operator_counts "" then
    String_log.info
      "Stage 3 stats after %s: %d extension iteration(s), %d lemma(s), %d witness(es); length lemmas: finite=%d, periodic=%d, lower=%d, failed=%d, combined=%d"
      outcome
      state.stats.refinement_iterations
      state.stats.generated_lemmas
      state.stats.generated_witnesses
      state.stats.length_finite_lemmas
      state.stats.length_periodic_lemmas
      state.stats.length_lower_bound_lemmas
      state.stats.length_failed_lemmas
      state.stats.length_combined_lemmas
  else
    String_log.info
      "Stage 3 stats after %s: %d extension iteration(s), %d lemma(s), %d witness(es); length lemmas: finite=%d, periodic=%d, lower=%d, failed=%d, combined=%d; operators: %s"
      outcome
      state.stats.refinement_iterations
      state.stats.generated_lemmas
      state.stats.generated_witnesses
      state.stats.length_finite_lemmas
      state.stats.length_periodic_lemmas
      state.stats.length_lower_bound_lemmas
      state.stats.length_failed_lemmas
      state.stats.length_combined_lemmas
      operator_counts

let malloc ?config () =
  config,
  {
    frames = [[]];
    internal_frames = [[]];
    generated = Global.hTerms_create 257;
    generated_rewrites = Global.hTerms_create 257;
    generated_contains_splits = Global.hTerms_create 257;
    generated_symbolic_reductions = Global.hTerms_create 257;
    witnesses = HWitness.create 257;
    last_unknown = None;
    last_strings = [];
    next_witness_id = 0;
    refinement_limit = refinement_limit_from_env ();
    witness_limit = witness_limit_from_env ();
    witness_round_limit = witness_round_limit_from_env ();
    stats = empty_stats ();
  }

let reset state =
  state.frames <- [[]];
  state.internal_frames <- [[]];
  state.last_unknown <- None;
  state.last_strings <- [];
  state.next_witness_id <- 0;
  HWitness.clear state.witnesses;
  reset_stats state.stats;
  reset_generated state

let push state =
  state.frames <- [] :: state.frames;
  state.internal_frames <- [] :: state.internal_frames

let pop_one_frame = function
  | [] | [_] -> None
  | _ :: tail -> Some tail

let pop state =
  match pop_one_frame state.frames, pop_one_frame state.internal_frames with
  | Some frames, Some internal_frames ->
      state.frames <- frames;
      state.internal_frames <- internal_frames;
      reset_generated state
  | _ ->
      Yices2.High.ExceptionsErrorHandling.raise_bindings_error
        "String extension pop on empty assertion stack"

let goto state level =
  if level < 0 then
    Yices2.High.ExceptionsErrorHandling.raise_bindings_error
      "String extension goto expects non-negative level, got %d" level;
  while List.length state.frames - 1 < level do
    push state
  done;
  while List.length state.frames - 1 > level do
    pop state
  done

let remember_assertion state formula =
  match state.frames with
  | [] -> state.frames <- [[formula]]
  | frame :: tail -> state.frames <- (formula :: frame) :: tail

let remember_internal_assertion state formula =
  match state.internal_frames with
  | [] -> state.internal_frames <- [[formula]]
  | frame :: tail -> state.internal_frames <- (formula :: frame) :: tail

let is_seen_generated state term =
  if HTerms.mem state.generated term then true
  else (
    HTerms.add state.generated term ();
    false)

let scalar_length_of_literal term =
  match literal_info term with
  | Some info -> Some info.scalar_length
  | None ->
      match reveal_string term with
      | Some (Lit text) -> Some (utf8_scalar_length text)
      | _ -> None

let ground_concat_value terms =
  let rec aux acc = function
    | [] -> Some (String.concat "" (List.rev acc))
    | term :: tail ->
        match reveal_string term with
        | Some (Lit text) -> aux (text :: acc) tail
        | Some (Concat terms) -> aux acc (terms @ tail)
        | _ -> None
  in
  aux [] terms

let static_string_value term =
  match reveal_string term with
  | Some (Lit text) -> Some text
  | Some (Concat terms) -> ground_concat_value terms
  | _ -> None

let find_substring_from haystack needle start_byte =
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > hay_len then None
    else if String.equal (String.sub haystack i needle_len) needle then Some i
    else loop (i + 1)
  in
  if needle_len = 0 then Some start_byte
  else if start_byte < 0 || start_byte > hay_len then None
  else loop start_byte

let eval_replace_all_text haystack needle replacement =
  if String.equal needle "" then haystack
  else
    let hay_len = String.length haystack in
    let needle_len = String.length needle in
    let output = Buffer.create hay_len in
    let rec loop start =
      match find_substring_from haystack needle start with
      | None ->
          Buffer.add_substring output haystack start (hay_len - start)
      | Some index ->
          Buffer.add_substring output haystack start (index - start);
          Buffer.add_string output replacement;
          loop (index + needle_len)
    in
    loop 0;
    Buffer.contents output

let string_starts_with text prefix =
  let len_text = String.length text in
  let len_prefix = String.length prefix in
  len_prefix <= len_text && String.equal (String.sub text 0 len_prefix) prefix

let string_ends_with text suffix =
  let len_text = String.length text in
  let len_suffix = String.length suffix in
  len_suffix <= len_text
  && String.equal (String.sub text (len_text - len_suffix) len_suffix) suffix

let q_to_int q =
  if Z.equal (Q.den q) Z.one then
    try Some (Z.to_int (Q.num q)) with Z.Overflow -> None
  else
    None

let static_int_value term =
  try
    let Term tstruct = Term.reveal term in
    match tstruct with
    | A0 _ -> (
        match Term.const_value tstruct with
        | `Rational q -> q_to_int q
        | _ -> None)
    | _ -> None
  with _ -> None

let utf8_scalar_boundaries s =
  let len = String.length s in
  let byte i = Char.code s.[i] in
  let continuation i =
    i < len && byte i land 0xC0 = 0x80
  in
  let rec loop acc i =
    if i = len then List.rev (len :: acc)
    else
      let next =
        let b0 = byte i in
        if b0 <= 0x7F then i + 1
        else if b0 >= 0xC2 && b0 <= 0xDF && continuation (i + 1) then i + 2
        else if b0 = 0xE0
                && i + 2 < len
                && byte (i + 1) >= 0xA0 && byte (i + 1) <= 0xBF
                && continuation (i + 2)
        then i + 3
        else if ((b0 >= 0xE1 && b0 <= 0xEC) || (b0 >= 0xEE && b0 <= 0xEF))
                && continuation (i + 1) && continuation (i + 2)
        then i + 3
        else if b0 = 0xED
                && i + 2 < len
                && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x9F
                && continuation (i + 2)
        then i + 3
        else if b0 = 0xF0
                && i + 3 < len
                && byte (i + 1) >= 0x90 && byte (i + 1) <= 0xBF
                && continuation (i + 2)
                && continuation (i + 3)
        then i + 4
        else if b0 >= 0xF1 && b0 <= 0xF3
                && continuation (i + 1)
                && continuation (i + 2)
                && continuation (i + 3)
        then i + 4
        else if b0 = 0xF4
                && i + 3 < len
                && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x8F
                && continuation (i + 2)
                && continuation (i + 3)
        then i + 4
        else raise_invalid_utf8 s i
      in
      loop (i :: acc) next
  in
  loop [] 0

let scalar_length_from_boundaries boundaries =
  List.length boundaries - 1

let list_nth_opt list index =
  if index < 0 then None else List.nth_opt list index

let substring_by_scalars s start length =
  if start < 0 || length <= 0 then ""
  else
    let boundaries = utf8_scalar_boundaries s in
    let scalar_len = scalar_length_from_boundaries boundaries in
    if start >= scalar_len then ""
    else
      let stop = min scalar_len (start + length) in
      match list_nth_opt boundaries start, list_nth_opt boundaries stop with
      | Some start_byte, Some stop_byte ->
          String.sub s start_byte (stop_byte - start_byte)
      | _ -> ""

let scalar_index_of_byte s byte_index =
  let boundaries = utf8_scalar_boundaries s in
  let rec loop index = function
    | [] -> None
    | boundary :: tail ->
        if boundary = byte_index then Some index
        else if boundary > byte_index then None
        else loop (index + 1) tail
  in
  loop 0 boundaries

let eval_contains_text haystack needle =
  match find_substring_from haystack needle 0 with
  | Some _ -> true
  | None -> false

let eval_indexof_text haystack needle start =
  let boundaries = utf8_scalar_boundaries haystack in
  let scalar_len = scalar_length_from_boundaries boundaries in
  if start < 0 || start > scalar_len then -1
  else if String.equal needle "" then start
  else
    match list_nth_opt boundaries start with
    | None -> -1
    | Some start_byte -> (
        match find_substring_from haystack needle start_byte with
        | None -> -1
        | Some byte_index ->
            Option.value ~default:(-1) (scalar_index_of_byte haystack byte_index))

let eval_replace_text haystack needle replacement =
  if String.equal needle "" then replacement ^ haystack
  else
    match find_substring_from haystack needle 0 with
    | None -> haystack
    | Some start ->
        let prefix = String.sub haystack 0 start in
        let suffix_start = start + String.length needle in
        let suffix =
          String.sub haystack suffix_start (String.length haystack - suffix_start)
        in
        prefix ^ replacement ^ suffix

let eval_at_text string index =
  substring_by_scalars string index 1

let scalar_codes s =
  let boundaries = utf8_scalar_boundaries s in
  let codepoint start stop =
    let b0 = Char.code s.[start] in
    if stop - start = 1 then b0
    else if stop - start = 2 then
      ((b0 land 0x1F) lsl 6) lor (Char.code s.[start + 1] land 0x3F)
    else if stop - start = 3 then
      ((b0 land 0x0F) lsl 12)
      lor ((Char.code s.[start + 1] land 0x3F) lsl 6)
      lor (Char.code s.[start + 2] land 0x3F)
    else
      ((b0 land 0x07) lsl 18)
      lor ((Char.code s.[start + 1] land 0x3F) lsl 12)
      lor ((Char.code s.[start + 2] land 0x3F) lsl 6)
      lor (Char.code s.[start + 3] land 0x3F)
  in
  let rec pairs acc = function
    | start :: (stop :: _ as tail) -> pairs (codepoint start stop :: acc) tail
    | _ -> List.rev acc
  in
  pairs [] boundaries

let eval_to_code_text text =
  match scalar_codes text with
  | [code] -> code
  | _ -> -1

let eval_from_code_value code =
  if valid_scalar_code code then string_of_scalar_code code else ""

let rec regex_string_terms acc = function
  | ReToRe term -> scan_term acc term
  | ReConcat regexes
  | ReUnion regexes
  | ReInter regexes ->
      List.fold_left regex_string_terms acc regexes
  | ReStar regex
  | ReComp regex
  | RePlus regex
  | ReOpt regex
  | ReLoop (regex, _, _) ->
      regex_string_terms acc regex
  | ReEmpty | ReAll | ReAllChar | ReLit _ | ReRange _ -> acc

and scan_term acc term =
  let acc =
    if is_string_type (Term.type_of_term term) then StringTermSet.add term acc
    else acc
  in
  let Term tstruct = Term.reveal term in
  let acc =
    match reveal_string term with
    | Some (Len arg) -> scan_term acc arg
    | Some (Concat args) -> List.fold_left scan_term acc args
    | Some (Substr (string, start, length)) ->
        List.fold_left scan_term acc [string; start; length]
    | Some (Contains (haystack, needle))
    | Some (Prefixof (haystack, needle))
    | Some (Suffixof (haystack, needle)) ->
        List.fold_left scan_term acc [haystack; needle]
    | Some (Indexof (haystack, needle, start)) ->
        List.fold_left scan_term acc [haystack; needle; start]
    | Some (Replace (haystack, needle, replacement)) ->
        List.fold_left scan_term acc [haystack; needle; replacement]
    | Some (ReplaceAll (haystack, needle, replacement)) ->
        List.fold_left scan_term acc [haystack; needle; replacement]
    | Some (ToCode string) -> scan_term acc string
    | Some (FromCode code) -> scan_term acc code
    | Some (At (string, index)) ->
        List.fold_left scan_term acc [string; index]
    | Some (InRe (string, regex)) -> regex_string_terms (scan_term acc string) regex
    | Some (Lit _) | None -> acc
  in
  match tstruct with
  | A0 _ -> acc
  | A1 (_, t) -> scan_term acc t
  | A2 (_, t1, t2) -> scan_term (scan_term acc t1) t2
  | Astar (_, terms) -> List.fold_left scan_term acc terms
  | ITE (c, tb, eb) -> List.fold_left scan_term acc [c; tb; eb]
  | App (f, args) -> List.fold_left scan_term (scan_term acc f) args
  | Bindings { vars; body; _ } -> List.fold_left scan_term (scan_term acc body) vars
  | Update { array; index; value } ->
      List.fold_left scan_term (scan_term (scan_term acc array) value) index
  | Projection (_, _, t) -> scan_term acc t
  | BV_Sum terms ->
      List.fold_left
        (fun acc (_, term) -> Option.map_or ~default:acc (scan_term acc) term)
        acc terms
  | Sum terms
  | FF_Sum terms ->
      List.fold_left
        (fun acc (_, term) -> Option.map_or ~default:acc (scan_term acc) term)
        acc terms
  | Product (_, terms) ->
      List.fold_left (fun acc (term, _) -> scan_term acc term) acc terms

let sum_lengths terms =
  match terms with
  | [] -> Term.Arith.zero ()
  | term :: tail ->
      List.fold_left
        (fun acc term -> Term.Arith.(acc ++ len term))
        (len term)
        tail

let axioms_for_string_term term =
  let length = len term in
  let zero = Term.Arith.zero () in
  let empty = literal "" in
  let common =
    [
      Term.Arith.geq length zero;
      Term.iff Term.(length === zero) Term.(term === empty);
    ]
  in
  let empty_axiom =
    if Term.equal term empty then []
    else [Term.(len empty === zero)]
  in
  let literal_axiom =
    match scalar_length_of_literal term with
    | Some n -> [Term.(length === Term.Arith.int n)]
    | None -> []
  in
  let concat_axiom =
    match reveal_string term with
    | Some (Concat terms) ->
        let length_axiom = Term.(length === sum_lengths terms) in
        let content_axioms =
          match ground_concat_value terms with
          | Some text -> [Term.(term === literal text)]
          | None -> []
        in
        length_axiom :: content_axioms
    | _ -> []
  in
  let replace_all_axiom =
    match reveal_string term with
    | Some (ReplaceAll (haystack, needle, replacement)) -> (
        let content_axioms =
          match
            static_string_value haystack,
            static_string_value needle,
            static_string_value replacement
          with
          | Some haystack_text, Some needle_text, Some replacement_text ->
              [
                Term.(
                  term
                  === literal
                        (eval_replace_all_text
                           haystack_text
                           needle_text
                           replacement_text));
              ]
          | _, Some "", _ -> [Term.(term === haystack)]
          | _, Some needle_text, Some replacement_text
            when String.equal needle_text replacement_text ->
              [Term.(term === haystack)]
          | Some "", Some needle_text, _
            when not (String.equal needle_text "") ->
              [Term.(term === haystack)]
          | _ -> []
        in
        let length_axioms =
          match static_string_value needle, static_string_value replacement with
          | Some needle_text, Some replacement_text
            when not (String.equal needle_text "")
                 && utf8_scalar_length needle_text
                    = utf8_scalar_length replacement_text ->
              [Term.(length === len haystack)]
          | _ -> []
        in
        content_axioms @ length_axioms)
    | _ -> []
  in
  let from_code_axiom =
    match reveal_string term with
    | Some (FromCode code) -> (
        let valid_guard =
          Term.andN
            [
              Term.Arith.geq code (Term.Arith.zero ());
              Term.Arith.leq code (Term.Arith.int unicode_max);
              Term.orN
                [
                  Term.Arith.lt code (Term.Arith.int surrogate_lo);
                  Term.Arith.gt code (Term.Arith.int surrogate_hi);
                ];
            ]
        in
        let invalid_guard =
          Term.orN
            [
              Term.Arith.lt code (Term.Arith.zero ());
              Term.Arith.gt code (Term.Arith.int unicode_max);
              Term.andN
                [
                  Term.Arith.geq code (Term.Arith.int surrogate_lo);
                  Term.Arith.leq code (Term.Arith.int surrogate_hi);
                ];
            ]
        in
        let guarded_axioms =
          [
            Term.(valid_guard ==> (length === Term.Arith.int 1));
            Term.(invalid_guard ==> (term === empty));
          ]
        in
        match static_int_value code with
        | Some code -> Term.(term === literal (eval_from_code_value code)) :: guarded_axioms
        | None -> guarded_axioms)
    | _ -> []
  in
  empty_axiom @ common @ literal_axiom @ concat_axiom @ replace_all_axiom
  @ from_code_axiom

let conjoin = function
  | [] -> Term.true0 ()
  | [formula] -> formula
  | formulas -> Term.andN formulas

let disjoin = function
  | [] -> Term.false0 ()
  | [formula] -> formula
  | formulas -> Term.orN formulas

let imply_all premises conclusion =
  match premises with
  | [] -> conclusion
  | _ -> Term.(conjoin premises ==> conclusion)

let bool_value_axiom term value =
  if value then term else Term.not1 term

let bounded_by_length term upper =
  Term.Arith.leq (len term) upper

let one () = Term.Arith.int 1

let minus_one () = Term.Arith.int (-1)

let rewrite_axioms_for_term term =
  let empty = literal "" in
  match reveal_string term with
  | Some (Substr (string, start, length_term)) ->
      let length_axioms =
        [
          bounded_by_length term (len string);
        ]
      in
      let static_length_axioms =
        match static_int_value length_term with
        | Some n when n <= 0 -> [Term.(term === empty)]
        | Some n -> [Term.Arith.leq (len term) (Term.Arith.int n)]
        | None -> []
      in
      let static_start_axioms =
        match static_int_value start with
        | Some n when n < 0 -> [Term.(term === empty)]
        | _ -> []
      in
      let ground_axioms =
        match
          static_string_value string,
          static_int_value start,
          static_int_value length_term
        with
        | Some text, Some start, Some length ->
            [Term.(term === literal (substring_by_scalars text start length))]
        | _ -> []
      in
      length_axioms @ static_length_axioms @ static_start_axioms @ ground_axioms
  | Some (Contains (haystack, needle)) ->
      let length_axioms =
        [
          Term.(term ==> Term.Arith.leq (len needle) (len haystack));
          Term.(
            Term.Arith.gt (len needle) (len haystack) ==> Term.not1 term);
        ]
      in
      let static_axioms =
        match static_string_value haystack, static_string_value needle with
        | _, Some "" -> [term]
        | Some "", _ -> [Term.iff term Term.(needle === empty)]
        | Some haystack, Some needle ->
            [bool_value_axiom term (eval_contains_text haystack needle)]
        | _ -> []
      in
      length_axioms @ static_axioms
  | Some (Prefixof (prefix, string)) ->
      let length_axioms =
        [Term.(term ==> Term.Arith.leq (len prefix) (len string))]
      in
      let containment_axioms =
        [Term.(term ==> contains string prefix)]
      in
      let static_axioms =
        match static_string_value prefix, static_string_value string with
        | Some "", _ -> [term]
        | _, Some "" -> [Term.iff term Term.(prefix === empty)]
        | Some prefix, Some string ->
            [bool_value_axiom term (string_starts_with string prefix)]
        | _ -> []
      in
      length_axioms @ containment_axioms @ static_axioms
  | Some (Suffixof (suffix, string)) ->
      let length_axioms =
        [Term.(term ==> Term.Arith.leq (len suffix) (len string))]
      in
      let containment_axioms =
        [Term.(term ==> contains string suffix)]
      in
      let static_axioms =
        match static_string_value suffix, static_string_value string with
        | Some "", _ -> [term]
        | _, Some "" -> [Term.iff term Term.(suffix === empty)]
        | Some suffix, Some string ->
            [bool_value_axiom term (string_ends_with string suffix)]
        | _ -> []
      in
      length_axioms @ containment_axioms @ static_axioms
  | Some (Indexof (haystack, needle, start)) ->
      let bounds =
        [
          disjoin
            [
              Term.(term === minus_one ());
              conjoin
                [
                  Term.Arith.geq term (Term.Arith.zero ());
                  Term.Arith.leq term (len haystack);
                ];
            ];
        ]
      in
      let static_axioms =
        match
          static_string_value haystack,
          static_string_value needle,
          static_int_value start
        with
        | Some haystack, Some needle, Some start ->
            [Term.(term === Term.Arith.int (eval_indexof_text haystack needle start))]
        | _, _, Some start when start < 0 -> [Term.(term === minus_one ())]
        | Some haystack, Some needle, _
          when not (String.equal needle "")
               && utf8_scalar_length needle > utf8_scalar_length haystack ->
            [Term.(term === minus_one ())]
        | _ -> []
      in
      bounds @ static_axioms
  | Some (Replace (haystack, needle, replacement)) ->
      let static_axioms =
        match
          static_string_value haystack,
          static_string_value needle,
          static_string_value replacement
        with
        | Some haystack, Some needle, Some replacement ->
            [
              Term.(
                term === literal (eval_replace_text haystack needle replacement));
            ]
        | _, Some "", _ ->
            let empty_replace = concat [replacement; haystack] in
            axioms_for_string_term empty_replace @ [Term.(term === empty_replace)]
        | _, Some needle, Some replacement when String.equal needle replacement ->
            [Term.(term === haystack)]
        | _ -> []
      in
      let length_axioms =
        match static_string_value needle, static_string_value replacement with
        | Some needle, Some replacement
          when not (String.equal needle "")
               && utf8_scalar_length needle = utf8_scalar_length replacement ->
            [Term.(len term === len haystack)]
        | _ -> []
      in
      static_axioms @ length_axioms
  | Some (ReplaceAll _) ->
      []
  | Some (ToCode string) ->
      let valid_singleton =
        Term.(len string === Term.Arith.int 1)
      in
      let singleton_axioms =
        [
          Term.(
            valid_singleton
            ==> conjoin
                  [
                    Term.Arith.geq term (Term.Arith.zero ());
                    Term.Arith.leq term (Term.Arith.int unicode_max);
                    disjoin
                      [
                        Term.Arith.lt term (Term.Arith.int surrogate_lo);
                        Term.Arith.gt term (Term.Arith.int surrogate_hi);
                      ];
                  ]);
          Term.(Term.not1 valid_singleton ==> (term === minus_one ()));
        ]
      in
      let static_axioms =
        match static_string_value string with
        | Some text -> [Term.(term === Term.Arith.int (eval_to_code_text text))]
        | None -> []
      in
      singleton_axioms @ static_axioms
  | Some (FromCode code) ->
      let static_axioms =
        match static_int_value code with
        | Some code -> [Term.(term === literal (eval_from_code_value code))]
        | None -> []
      in
      static_axioms
  | Some (At (string, index)) ->
      let one = one () in
      let base_axioms = [Term.Arith.leq (len term) one] in
      let in_bounds =
        conjoin
          [
            Term.Arith.geq index (Term.Arith.zero ());
            Term.Arith.lt index (len string);
          ]
      in
      let length_axioms =
        [Term.(in_bounds ==> (len term === one))]
      in
      let static_axioms =
        match static_string_value string, static_int_value index with
        | Some string, Some index ->
            [Term.(term === literal (eval_at_text string index))]
        | _, Some index when index < 0 -> [Term.(term === empty)]
        | _ -> []
      in
      base_axioms @ length_axioms @ static_axioms
  | Some (Lit _ | Concat _ | Len _ | InRe _) | None -> []

let rec regex_extension_terms acc = function
  | ReToRe term -> collect_extension_terms acc term
  | ReConcat regexes
  | ReUnion regexes
  | ReInter regexes ->
      List.fold_left regex_extension_terms acc regexes
  | ReStar regex
  | ReComp regex
  | RePlus regex
  | ReOpt regex
  | ReLoop (regex, _, _) ->
      regex_extension_terms acc regex
  | ReEmpty | ReAll | ReAllChar | ReLit _ | ReRange _ -> acc

and collect_extension_terms acc term =
  let acc =
    match reveal_string term with
    | Some (Lit _) | None -> acc
    | Some (InRe (_, regex)) ->
        regex_extension_terms (StringTermSet.add term acc) regex
    | Some _ -> StringTermSet.add term acc
  in
  let Term tstruct = Term.reveal term in
  collect_extension_children acc tstruct

and collect_extension_children : type a.
    StringTermSet.t -> a YTypes.termstruct -> StringTermSet.t =
  fun acc -> function
  | A0 _ -> acc
  | A1 (_, t) -> collect_extension_terms acc t
  | A2 (_, t1, t2) ->
      collect_extension_terms (collect_extension_terms acc t1) t2
  | Astar (_, terms) -> List.fold_left collect_extension_terms acc terms
  | ITE (c, tb, eb) ->
      List.fold_left collect_extension_terms acc [c; tb; eb]
  | App (f, args) ->
      List.fold_left collect_extension_terms (collect_extension_terms acc f) args
  | Bindings { vars; body; _ } ->
      List.fold_left collect_extension_terms (collect_extension_terms acc body) vars
  | Update { array; index; value } ->
      List.fold_left
        collect_extension_terms
        (collect_extension_terms (collect_extension_terms acc array) value)
        index
  | Projection (_, _, t) -> collect_extension_terms acc t
  | BV_Sum terms ->
      List.fold_left
        (fun acc (_, term) ->
           Option.map_or ~default:acc (collect_extension_terms acc) term)
        acc
        terms
  | Sum terms
  | FF_Sum terms ->
      List.fold_left
        (fun acc (_, term) ->
           Option.map_or ~default:acc (collect_extension_terms acc) term)
        acc
        terms
  | Product (_, terms) ->
      List.fold_left
        (fun acc (term, _) -> collect_extension_terms acc term)
        acc
        terms

let is_seen_rewrite state term =
  if HTerms.mem state.generated_rewrites term then true
  else (
    HTerms.add state.generated_rewrites term ();
    false)

let concat_part_can_add_containment part =
  match static_string_value part with
  | Some "" -> false
  | _ -> true

let concat_containment_axioms atom whole concat_term =
  match reveal_string concat_term with
  | Some (Concat parts) ->
      parts
      |> List.filter concat_part_can_add_containment
      |> List.sort_uniq ~cmp:Term.compare
      |> List.map (fun part -> Term.(atom ==> contains whole part))
  | _ -> []

let containment_axioms_from_equality atom lhs rhs =
  if is_string_type (Term.type_of_term lhs) && is_string_type (Term.type_of_term rhs) then
    concat_containment_axioms atom lhs rhs
    @ concat_containment_axioms atom rhs lhs
  else
    []

let containment_axioms_from_assertion formula =
  let rec aux acc term =
    let Term tstruct = Term.reveal term in
    let acc =
      match tstruct with
      | A2 (`YICES_EQ_TERM, lhs, rhs) ->
          List.rev_append (containment_axioms_from_equality term lhs rhs) acc
      | _ -> acc
    in
    fold_children acc tstruct
  and fold_children : type a. Term.t list -> a YTypes.termstruct -> Term.t list =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc
          terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc
          terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  aux [] formula |> List.sort_uniq ~cmp:Term.compare

let find_witness state key =
  try Some (HWitness.find state.witnesses key) with Not_found -> None

let fresh_witness_name state key =
  let id = state.next_witness_id in
  state.next_witness_id <- id + 1;
  Format.sprintf "__yices_string_%s_%d" (witness_key_name key) id

let witness_for_key state created_this_round key =
  match find_witness state key with
  | Some term ->
      String_log.info
        "Stage 3 reused %s witness %a"
        (witness_key_name key)
        Term.pp
        term;
      Ok (term, created_this_round)
  | None ->
      if state.stats.generated_witnesses >= state.witness_limit then
        Error
          (Format.asprintf
             "Stage 3 witness limit %d prevents creating %s witness"
             state.witness_limit
             (witness_key_name key))
      else if created_this_round >= state.witness_round_limit then
        Error
          (Format.asprintf
             "Stage 3 per-round witness limit %d prevents creating %s witness"
             state.witness_round_limit
             (witness_key_name key))
      else
        let name = fresh_witness_name state key in
        let term = Term.new_uninterpreted ~name (string_type ()) in
        HWitness.add state.witnesses key term;
        state.stats.generated_witnesses <- state.stats.generated_witnesses + 1;
        String_log.info
          "Stage 3 created %s witness %a"
          (witness_key_name key)
          Term.pp
          term;
        Ok (term, created_this_round + 1)

let collect_contains_terms formulas =
  let rec aux acc term =
    let acc =
      match reveal_string term with
      | Some (Contains _) -> StringTermSet.add term acc
      | _ -> acc
    in
    let Term tstruct = Term.reveal term in
    fold_children acc tstruct
  and fold_children : type a. StringTermSet.t -> a YTypes.termstruct -> StringTermSet.t =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  List.fold_left aux StringTermSet.empty formulas |> StringTermSet.elements

type symbolic_reduction =
  | Symbolic_none
  | Symbolic_refine of refinement_operator * Term.t
  | Symbolic_blocked of string

let contains_split_reduction state contains_term haystack needle =
  match
    witness_for_key state 0 (ContainsPrefix (haystack, needle))
  with
  | Error reason -> Error reason
  | Ok (prefix, created) -> (
      match witness_for_key state created (ContainsSuffix (haystack, needle)) with
      | Error reason -> Error reason
      | Ok (suffix, _) ->
          let split = concat [prefix; needle; suffix] in
          let split_axioms =
            [prefix; suffix; split]
            |> List.map axioms_for_string_term
            |> List.flatten
          in
          let reduction = Term.(contains_term ==> (haystack === split)) in
          let lemma = conjoin (split_axioms @ [reduction]) in
          HTerms.replace state.generated_contains_splits contains_term ();
          remember_internal_assertion state lemma;
          Ok lemma)

let witness_pair state first_key second_key =
  match witness_for_key state 0 first_key with
  | Error reason -> Error reason
  | Ok (first, created) -> (
      match witness_for_key state created second_key with
      | Error reason -> Error reason
      | Ok (second, _) -> Ok (first, second))

let mark_symbolic_reduction state atom lemma =
  HTerms.replace state.generated_symbolic_reductions atom ();
  remember_internal_assertion state lemma;
  Ok lemma

let substr_split_reduction state eq string start length result =
  if HTerms.mem state.generated_symbolic_reductions eq.atom then Ok None
  else
    match
      witness_pair
        state
        (SubstrPrefix (string, start, length))
        (SubstrSuffix (string, start, length))
    with
    | Error reason -> Error reason
    | Ok (prefix, suffix) ->
        let zero = Term.Arith.zero () in
        let split = concat [prefix; result; suffix] in
        let normal_guard =
          conjoin
            [
              Term.Arith.geq start zero;
              Term.Arith.gt length zero;
              Term.Arith.lt start (len string);
            ]
        in
        let result_empty = Term.(len result === zero) in
        let possible_case = disjoin [result_empty; normal_guard] in
        let normal_split =
          conjoin
            [
              Term.(string === split);
              Term.(len prefix === start);
              Term.Arith.leq (len result) length;
            ]
        in
        let split_axioms =
          [prefix; suffix; split]
          |> List.map axioms_for_string_term
          |> List.flatten
        in
        let lemma =
          conjoin
            (split_axioms
             @ [
                 Term.implies eq.atom possible_case;
                 imply_all [eq.atom; normal_guard] normal_split;
               ])
        in
        Result.map (fun lemma -> Some lemma) (mark_symbolic_reduction state eq.atom lemma)

let indexof_split_reduction state eq haystack needle start result =
  if HTerms.mem state.generated_symbolic_reductions eq.atom then Ok None
  else
    match
      witness_pair
        state
        (IndexofPrefix (haystack, needle, start))
        (IndexofSuffix (haystack, needle, start))
    with
    | Error reason -> Error reason
    | Ok (prefix, suffix) ->
        let zero = Term.Arith.zero () in
        let split = concat [prefix; needle; suffix] in
        let found_guard = Term.Arith.geq result zero in
        let needle_empty = Term.(len needle === zero) in
        let needle_nonempty = Term.Arith.gt (len needle) zero in
        let empty_case =
          conjoin
            [
              Term.(result === start);
              Term.Arith.geq start zero;
              Term.Arith.leq start (len haystack);
            ]
        in
        let occurrence_case =
          conjoin
            [
              Term.(haystack === split);
              Term.(len prefix === result);
              Term.Arith.geq start zero;
              Term.Arith.leq start result;
            ]
        in
        let split_axioms =
          [prefix; suffix; split]
          |> List.map axioms_for_string_term
          |> List.flatten
        in
        let lemma =
          conjoin
            (split_axioms
             @ [
                 imply_all [eq.atom; found_guard; needle_empty] empty_case;
                 imply_all [eq.atom; found_guard; needle_nonempty] occurrence_case;
               ])
        in
        Result.map (fun lemma -> Some lemma) (mark_symbolic_reduction state eq.atom lemma)

let replace_split_reduction state eq haystack needle replacement result =
  if HTerms.mem state.generated_symbolic_reductions eq.atom then Ok None
  else
    match
      witness_pair
        state
        (ReplacePrefix (haystack, needle, replacement))
        (ReplaceSuffix (haystack, needle, replacement))
    with
    | Error reason -> Error reason
    | Ok (prefix, suffix) ->
        let zero = Term.Arith.zero () in
        let input_split = concat [prefix; needle; suffix] in
        let output_split = concat [prefix; replacement; suffix] in
        let empty_output = concat [replacement; haystack] in
        let needle_empty = Term.(len needle === zero) in
        let needle_nonempty = Term.Arith.gt (len needle) zero in
        let occurrence_case =
          conjoin
            [
              Term.(haystack === input_split);
              Term.(result === output_split);
            ]
        in
        let no_occurrence_case = Term.(result === haystack) in
        let split_axioms =
          [prefix; suffix; input_split; output_split; empty_output]
          |> List.map axioms_for_string_term
          |> List.flatten
        in
        let lemma =
          conjoin
            (split_axioms
             @ [
                 imply_all [eq.atom; needle_empty] Term.(result === empty_output);
                 imply_all
                   [eq.atom; needle_nonempty]
                   (disjoin [no_occurrence_case; occurrence_case]);
               ])
        in
        Result.map (fun lemma -> Some lemma) (mark_symbolic_reduction state eq.atom lemma)

let replace_all_split_reduction state eq haystack needle replacement result =
  if HTerms.mem state.generated_symbolic_reductions eq.atom then Ok None
  else if Option.is_none (static_string_value needle) then
    Error
      (Format.asprintf
         "unsupported symbolic str.replace_all needle in %a"
         Term.pp
         eq.atom)
  else
    match
      witness_pair
        state
        (ReplaceAllPrefix (haystack, needle, replacement))
        (ReplaceAllSuffix (haystack, needle, replacement))
    with
    | Error reason -> Error reason
    | Ok (prefix, suffix) ->
        let zero = Term.Arith.zero () in
        let input_split = concat [prefix; needle; suffix] in
        let tail_result = replace_all suffix needle replacement in
        let output_split = concat [prefix; replacement; tail_result] in
        let needle_empty = Term.(len needle === zero) in
        let needle_nonempty = Term.Arith.gt (len needle) zero in
        let no_occurrence_case =
          conjoin
            [
              Term.not1 (contains haystack needle);
              Term.(result === haystack);
            ]
        in
        let occurrence_case =
          conjoin
            [
              contains haystack needle;
              Term.not1 (contains prefix needle);
              Term.(haystack === input_split);
              Term.(result === output_split);
            ]
        in
        let split_axioms =
          [prefix; suffix; input_split; tail_result; output_split]
          |> List.map axioms_for_string_term
          |> List.flatten
        in
        let lemma =
          conjoin
            (split_axioms
             @ [
                 imply_all [eq.atom; needle_empty] Term.(result === haystack);
                 imply_all
                   [eq.atom; needle_nonempty]
                   (disjoin [no_occurrence_case; occurrence_case]);
               ])
        in
        Result.map (fun lemma -> Some lemma) (mark_symbolic_reduction state eq.atom lemma)

let translate_assertion (_ctx : Context.t) state formula =
  remember_assertion state formula;
  let string_terms = scan_term StringTermSet.empty formula in
  let extension_terms = collect_extension_terms StringTermSet.empty formula in
  String_log.debug
    "registered %d string term(s), %d extension term(s) from assertion %a"
    (StringTermSet.cardinal string_terms)
    (StringTermSet.cardinal extension_terms)
    Term.pp formula;
  let string_axioms =
    StringTermSet.fold
      (fun term axioms ->
         if is_seen_generated state term then axioms
         else List.rev_append (axioms_for_string_term term) axioms)
      string_terms
      []
  in
  let rewrite_axioms =
    StringTermSet.fold
      (fun term axioms ->
         if is_seen_rewrite state term then axioms
         else List.rev_append (rewrite_axioms_for_term term) axioms)
      extension_terms
      []
  in
  let containment_axioms = containment_axioms_from_assertion formula in
  List.rev string_axioms @ List.rev rewrite_axioms @ containment_axioms @ [formula]

let translate_assumption (_ctx : Context.t) _state formula =
  formula

let term_of_old _ term = term
let typ_of_old _ typ = typ
let param_to_old _ param = param
let smodel_to_old _ model = model.StringModel.base
let smodel_of_old _ base = { StringModel.base; strings = [] }

let string_terms_in_state state =
  current_terms state
  |> List.fold_left scan_term StringTermSet.empty
  |> StringTermSet.elements

let string_terms_in_public_state state =
  public_terms state
  |> List.fold_left scan_term StringTermSet.empty
  |> StringTermSet.elements

let dedup_assignments assignments =
  let add result (term, text) =
    match result with
    | Error _ as err -> err
    | Ok acc -> (
        match List.find_opt (fun (term', _) -> Term.equal term term') acc with
        | None -> Ok ((term, text) :: acc)
        | Some (_, old) when String.equal old text -> Ok acc
        | Some (_, old) ->
            Error
              (Format.asprintf
                 "inconsistent duplicate concrete values for %a: %S and %S"
                 Term.pp term old text))
  in
  Result.map List.rev (List.fold_left add (Ok []) assignments)

let assignment_find assignments term =
  List.find_map
    (fun (key, text) -> if Term.equal key term then Some text else None)
    assignments

let add_assignment assignments term text =
  match assignment_find assignments term with
  | None -> Ok ((term, text) :: assignments)
  | Some old when String.equal old text -> Ok assignments
  | Some old ->
      Error
        (Format.asprintf
           "inconsistent concrete values for %a: %S and %S"
           Term.pp term old text)

let force_assignment assignments term text =
  match static_string_value term with
  | Some known when not (String.equal known text) ->
      Error
        (Format.asprintf
           "cannot force %a to %S because it is the literal %S"
           Term.pp term text known)
  | _ ->
      let assignments =
        List.filter (fun (key, _) -> not (Term.equal key term)) assignments
      in
      Ok ((term, text) :: assignments)

let int_value_in_model smodel term =
  try
    match ModelValue.reveal (SModel.get_value smodel term) with
    | `Rational q -> q_to_int q
    | _ -> None
  with _ -> None

let string_length_in_model smodel term =
  match int_value_in_model smodel (len term) with
  | Some n when n >= 0 -> Some n
  | _ -> None

let eval_prefixof_text prefix string =
  string_starts_with string prefix

let eval_suffixof_text suffix string =
  string_ends_with string suffix

module ScalarSet = Set.Make(Int)

type character_set =
  | Characters_top
  | Characters of ScalarSet.t

let character_range_limit = 512

let character_set_of_text text =
  Characters
    (scalar_codes text
     |> List.fold_left (fun acc code -> ScalarSet.add code acc) ScalarSet.empty)

let character_union lhs rhs =
  match lhs, rhs with
  | Characters_top, _ | _, Characters_top -> Characters_top
  | Characters lhs, Characters rhs -> Characters (ScalarSet.union lhs rhs)

let character_inter lhs rhs =
  match lhs, rhs with
  | Characters_top, other | other, Characters_top -> other
  | Characters lhs, Characters rhs -> Characters (ScalarSet.inter lhs rhs)

let character_unions sets =
  List.fold_left character_union (Characters ScalarSet.empty) sets

let character_inters sets =
  List.fold_left character_inter Characters_top sets

let character_set_of_range lo hi =
  if hi < lo then
    Characters ScalarSet.empty
  else if hi - lo > character_range_limit then
    Characters_top
  else
    let rec loop acc code =
      if code > hi then Characters acc else loop (ScalarSet.add code acc) (code + 1)
    in
    loop ScalarSet.empty lo

let rec character_set_of_regex = function
  | ReEmpty -> Characters ScalarSet.empty
  | ReLit text -> character_set_of_text text
  | ReRange (lo, hi) -> character_set_of_range lo hi
  | ReToRe term -> (
      match static_string_value term with
      | Some text -> character_set_of_text text
      | None -> Characters_top)
  | ReConcat regexes
  | ReUnion regexes ->
      regexes |> List.map character_set_of_regex |> character_unions
  | ReInter [] -> Characters_top
  | ReInter regexes ->
      regexes |> List.map character_set_of_regex |> character_inters
  | ReStar regex
  | RePlus regex
  | ReOpt regex ->
      character_set_of_regex regex
  | ReLoop (regex, _, hi) ->
      if hi = 0 then Characters ScalarSet.empty else character_set_of_regex regex
  | ReAll | ReAllChar | ReComp _ -> Characters_top

let rec character_set_of_term term =
  match static_string_value term with
  | Some text -> character_set_of_text text
  | None -> (
      match reveal_string term with
      | Some (Concat terms) ->
          terms |> List.map character_set_of_term |> character_unions
      | Some (Substr (string, _, _))
      | Some (At (string, _)) ->
          character_set_of_term string
      | Some (Replace (haystack, _, replacement))
      | Some (ReplaceAll (haystack, _, replacement)) ->
          character_union
            (character_set_of_term haystack)
            (character_set_of_term replacement)
      | Some (FromCode code) -> (
          match static_int_value code with
          | Some code -> character_set_of_text (eval_from_code_value code)
          | None -> Characters_top)
      | Some (Lit _) -> Characters ScalarSet.empty
      | Some
          ( Len _
          | Contains _
          | Indexof _
          | ToCode _
          | Prefixof _
          | Suffixof _
          | InRe _ )
      | None ->
          Characters_top)

let character_set_excludes_text character_set text =
  match character_set with
  | Characters_top -> false
  | Characters set ->
      scalar_codes text |> List.exists (fun code -> not (ScalarSet.mem code set))

let rec automata_regex_of_regex = function
  | ReEmpty -> RA.Empty
  | ReAll -> RA.All
  | ReAllChar -> RA.AllChar
  | ReLit text -> RA.Lit text
  | ReToRe _ ->
      invalid_arg "symbolic str.to_re cannot be compiled as a concrete automaton"
  | ReRange (lo, hi) -> RA.Range (lo, hi)
  | ReConcat regexes -> RA.Concat (List.map automata_regex_of_regex regexes)
  | ReUnion regexes -> RA.Union (List.map automata_regex_of_regex regexes)
  | ReStar regex -> RA.Star (automata_regex_of_regex regex)
  | ReInter regexes -> RA.Inter (List.map automata_regex_of_regex regexes)
  | ReComp regex -> RA.Comp (automata_regex_of_regex regex)
  | RePlus regex -> RA.Plus (automata_regex_of_regex regex)
  | ReOpt regex -> RA.Opt (automata_regex_of_regex regex)
  | ReLoop (regex, lo, hi) -> RA.Loop (automata_regex_of_regex regex, lo, hi)

let regex_accepts_direct regex text =
  let boundaries = utf8_scalar_boundaries text in
  let boundary_count = List.length boundaries in
  let boundary_at index =
    match list_nth_opt boundaries index with
    | Some boundary -> boundary
    | None -> invalid_arg "regex_accepts: boundary index out of range"
  in
  let scalar_count = boundary_count - 1 in
  let text_between start_idx end_idx =
    let start_byte = boundary_at start_idx in
    let end_byte = boundary_at end_idx in
    String.sub text start_byte (end_byte - start_byte)
  in
  let rec match_from regex start =
    match regex with
    | ReEmpty -> []
    | ReAll -> [scalar_count]
    | ReAllChar ->
        if start < scalar_count then [start + 1] else []
    | ReLit literal ->
        let lit_len = utf8_scalar_length literal in
        let stop = start + lit_len in
        if stop <= scalar_count && String.equal (text_between start stop) literal then
          [stop]
        else
          []
    | ReToRe _ -> []
    | ReRange (lo, hi) -> (
        if start >= scalar_count then []
        else
          match scalar_codes (text_between start (start + 1)) with
          | [code] when lo <= code && code <= hi -> [start + 1]
          | _ -> [])
    | ReUnion regexes ->
        regexes
        |> List.concat_map (fun regex -> match_from regex start)
        |> List.sort_uniq ~cmp:Int.compare
    | ReInter [] ->
        List.init (scalar_count - start + 1) (fun offset -> start + offset)
    | ReInter (first :: rest) ->
        match_from first start
        |> List.filter (fun stop ->
               List.for_all
                 (fun regex -> List.exists (( = ) stop) (match_from regex start))
                 rest)
    | ReConcat regexes ->
        List.fold_left
          (fun starts regex ->
             starts
             |> List.concat_map (fun start -> match_from regex start)
             |> List.sort_uniq ~cmp:Int.compare)
          [start]
          regexes
    | ReStar regex ->
        let rec closure seen queue =
          match queue with
          | [] -> seen
          | start :: rest ->
              let next =
                match_from regex start
                |> List.filter
                     (fun stop -> stop > start && not (List.exists (( = ) stop) seen))
              in
              closure (List.rev_append next seen) (List.rev_append next rest)
        in
        closure [start] [start] |> List.sort_uniq ~cmp:Int.compare
    | ReComp regex ->
        let matched = match_from regex start in
        List.init (scalar_count - start + 1) (fun offset -> start + offset)
        |> List.filter (fun stop -> not (List.exists (( = ) stop) matched))
    | RePlus regex ->
        match_from (ReConcat [regex; ReStar regex]) start
    | ReOpt regex ->
        match_from (ReUnion [ReLit ""; regex]) start
    | ReLoop (regex, lo, hi) ->
        if lo < 0 || hi < lo then
          []
        else
          let rec repeat starts n =
            if n = 0 then starts
            else
              starts
              |> List.concat_map (fun start -> match_from regex start)
              |> List.sort_uniq ~cmp:Int.compare
              |> fun starts -> repeat starts (n - 1)
          in
          let rec collect acc n =
            if n > hi then acc
            else collect (List.rev_append (repeat [start] n) acc) (n + 1)
          in
          collect [] lo |> List.sort_uniq ~cmp:Int.compare
  in
  List.exists (( = ) scalar_count) (match_from regex 0)

let[@warning "-32"] regex_accepts regex text =
  match try Ok (automata_regex_of_regex regex) with Invalid_argument msg -> Error msg with
  | Error _ -> regex_accepts_direct regex text
  | Ok automata_regex -> (
  match RA.compile automata_regex with
  | Ok automaton -> RA.accepts automaton text
  | Error _ -> regex_accepts_direct regex text)

let filler_string seed length =
  if length <= 0 then ""
  else
    let code = Char.code 'a' + (seed mod 26) in
    String.make length (Char.chr code)

let all_distinct = function
  | [] | [_] -> true
  | values ->
      let rec loop seen = function
        | [] -> true
        | value :: tail ->
            if List.exists (String.equal value) seen then false
            else loop (value :: seen) tail
      in
      loop [] values

let sequence_options values =
  let rec aux acc = function
    | [] -> Some (List.rev acc)
    | None :: _ -> None
    | Some value :: tail -> aux (value :: acc) tail
  in
  aux [] values

let true_in_model smodel term =
  try SModel.formula_true_in_model smodel term with _ -> false

let positive_contains_reduction state smodel formulas =
  let rec find = function
    | [] -> Symbolic_none
    | term :: tail -> (
        if HTerms.mem state.generated_contains_splits term
           || not (true_in_model smodel term)
        then
          find tail
        else
          match reveal_string term with
          | Some (Contains (haystack, needle)) -> (
              match contains_split_reduction state term haystack needle with
              | Ok lemma -> Symbolic_refine (Op_contains, lemma)
              | Error reason -> Symbolic_blocked reason)
          | _ -> find tail)
  in
  find (collect_contains_terms formulas)

let is_string_equality lhs rhs =
  is_string_type (Term.type_of_term lhs) && is_string_type (Term.type_of_term rhs)

let collect_true_equalities smodel formulas =
  let rec aux acc term =
    let Term tstruct = Term.reveal term in
    let acc =
      match tstruct with
      | A2 (`YICES_EQ_TERM, lhs, rhs) when is_string_equality lhs rhs ->
          if true_in_model smodel term then { atom = term; lhs; rhs } :: acc else acc
      | _ -> acc
    in
    fold_children acc tstruct
  and fold_children : type a. eq_atom list -> a YTypes.termstruct -> eq_atom list =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  List.fold_left aux [] formulas |> List.rev

let is_int_term_type term =
  Type.equal (Term.type_of_term term) Type.(int ())

let has_reducible_stage3_equality lhs rhs =
  match reveal_string lhs, reveal_string rhs with
  | Some (Substr _), _ | _, Some (Substr _)
  | Some (Indexof _), _ | _, Some (Indexof _)
  | Some (Replace _), _ | _, Some (Replace _)
  | Some (ReplaceAll _), _ | _, Some (ReplaceAll _) -> true
  | _ -> false

let collect_true_stage3_equalities smodel formulas =
  let rec aux acc term =
    let Term tstruct = Term.reveal term in
    let acc =
      match tstruct with
      | A2 (`YICES_EQ_TERM, lhs, rhs)
        when has_reducible_stage3_equality lhs rhs && true_in_model smodel term ->
          { atom = term; lhs; rhs } :: acc
      | _ -> acc
    in
    fold_children acc tstruct
  and fold_children : type a. eq_atom list -> a YTypes.termstruct -> eq_atom list =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  List.fold_left aux [] formulas |> List.rev

type stage3_equality_reduction =
  | Reduce_substr of eq_atom * Term.t * Term.t * Term.t * Term.t
  | Reduce_indexof of eq_atom * Term.t * Term.t * Term.t * Term.t
  | Reduce_replace of eq_atom * Term.t * Term.t * Term.t * Term.t
  | Reduce_replace_all of eq_atom * Term.t * Term.t * Term.t * Term.t

let stage3_equality_reduction_of_eq eq =
  match reveal_string eq.lhs, reveal_string eq.rhs with
  | Some (Substr (string, start, length)), _ ->
      Some (Reduce_substr (eq, string, start, length, eq.rhs))
  | _, Some (Substr (string, start, length)) ->
      Some (Reduce_substr (eq, string, start, length, eq.lhs))
  | Some (Indexof (haystack, needle, start)), _ when is_int_term_type eq.rhs ->
      Some (Reduce_indexof (eq, haystack, needle, start, eq.rhs))
  | _, Some (Indexof (haystack, needle, start)) when is_int_term_type eq.lhs ->
      Some (Reduce_indexof (eq, haystack, needle, start, eq.lhs))
  | Some (Replace (haystack, needle, replacement)), _ ->
      Some (Reduce_replace (eq, haystack, needle, replacement, eq.rhs))
  | _, Some (Replace (haystack, needle, replacement)) ->
      Some (Reduce_replace (eq, haystack, needle, replacement, eq.lhs))
  | Some (ReplaceAll (haystack, needle, replacement)), _ ->
      Some (Reduce_replace_all (eq, haystack, needle, replacement, eq.rhs))
  | _, Some (ReplaceAll (haystack, needle, replacement)) ->
      Some (Reduce_replace_all (eq, haystack, needle, replacement, eq.lhs))
  | _ -> None

let stage3_equality_reduction_operator = function
  | Reduce_substr _ -> Op_substr
  | Reduce_indexof _ -> Op_indexof
  | Reduce_replace _ -> Op_replace
  | Reduce_replace_all _ -> Op_replace_all

let stage3_equality_reduction_base_priority = function
  | Reduce_substr _ -> 70
  | Reduce_indexof _ -> 70
  | Reduce_replace _ -> 80
  | Reduce_replace_all _ -> 90

let stage3_equality_reduction_rank state reduction =
  let op = stage3_equality_reduction_operator reduction in
  ( stage3_equality_reduction_base_priority reduction
    + min 10 (refinement_operator_count state.stats op),
    refinement_operator_name op )

let compare_stage3_equality_reductions state lhs rhs =
  Stdlib.compare
    (stage3_equality_reduction_rank state lhs)
    (stage3_equality_reduction_rank state rhs)

let symbolic_stage3_equality_reduction state smodel formulas =
  let rec find = function
    | [] -> Symbolic_none
    | reduction :: tail -> (
        match reduction with
        | Reduce_substr (eq, string, start, length, result) -> (
            match substr_split_reduction state eq string start length result with
            | Ok None -> find tail
            | Ok (Some lemma) -> Symbolic_refine (Op_substr, lemma)
            | Error reason -> Symbolic_blocked reason)
        | Reduce_indexof (eq, haystack, needle, start, result) -> (
            match indexof_split_reduction state eq haystack needle start result with
            | Ok None -> find tail
            | Ok (Some lemma) -> Symbolic_refine (Op_indexof, lemma)
            | Error reason -> Symbolic_blocked reason)
        | Reduce_replace (eq, haystack, needle, replacement, result) -> (
            match replace_split_reduction state eq haystack needle replacement result with
            | Ok None -> find tail
            | Ok (Some lemma) -> Symbolic_refine (Op_replace, lemma)
            | Error reason -> Symbolic_blocked reason)
        | Reduce_replace_all (eq, haystack, needle, replacement, result) -> (
            match replace_all_split_reduction state eq haystack needle replacement result with
            | Ok None -> find tail
            | Ok (Some lemma) -> Symbolic_refine (Op_replace_all, lemma)
            | Error reason -> Symbolic_blocked reason))
  in
  collect_true_stage3_equalities smodel formulas
  |> List.filter_map stage3_equality_reduction_of_eq
  |> List.stable_sort (compare_stage3_equality_reductions state)
  |> find

let fixed_concat_prefix_middle_suffix parts =
  let rec aux prefix = function
    | [] -> None
    | term :: tail -> (
        match static_string_value term with
        | Some text -> aux (prefix ^ text) tail
        | None -> (
            match ground_concat_value tail with
            | Some suffix -> Some (prefix, term, suffix)
            | None -> None))
  in
  aux "" parts

let concat_literal_refinement eq =
  let one_direction concat_term literal_text =
    let atom = eq.atom in
    match reveal_string concat_term with
    | Some (Concat parts) -> (
        match fixed_concat_prefix_middle_suffix parts with
        | None -> None
        | Some (prefix, middle, suffix) ->
            if string_starts_with literal_text prefix
               && string_ends_with literal_text suffix
               && String.length prefix + String.length suffix <= String.length literal_text
            then
              let middle_start = String.length prefix in
              let middle_len =
                String.length literal_text - middle_start - String.length suffix
              in
              let inferred = String.sub literal_text middle_start middle_len in
              Some Term.(atom ==> (middle === literal inferred))
            else
              Some Term.(not1 atom))
    | _ -> None
  in
  match static_string_value eq.lhs, static_string_value eq.rhs with
  | Some literal_text, None -> one_direction eq.rhs literal_text
  | None, Some literal_text -> one_direction eq.lhs literal_text
  | _ -> None

let concat_literal_assignment eq =
  let one_direction concat_term literal_text =
    match reveal_string concat_term with
    | Some (Concat parts) -> (
        match fixed_concat_prefix_middle_suffix parts with
        | None -> None
        | Some (prefix, middle, suffix) ->
            if string_starts_with literal_text prefix
               && string_ends_with literal_text suffix
               && String.length prefix + String.length suffix <= String.length literal_text
            then
              let middle_start = String.length prefix in
              let middle_len =
                String.length literal_text - middle_start - String.length suffix
              in
              Some (middle, String.sub literal_text middle_start middle_len)
            else
              None)
    | _ -> None
  in
  match static_string_value eq.lhs, static_string_value eq.rhs with
  | Some literal_text, None -> one_direction eq.rhs literal_text
  | None, Some literal_text -> one_direction eq.lhs literal_text
  | _ -> None

let refinement_lemma smodel equalities =
  List.find_map
    (fun eq ->
       match concat_literal_refinement eq with
       | None -> None
       | Some lemma ->
           if true_in_model smodel lemma then None
           else (
             String_log.debug "generated string refinement lemma %a" Term.pp lemma;
             Some lemma))
    equalities

let equality_classes terms equalities =
  let neighbors term =
    List.fold_left
      (fun acc eq ->
         if Term.equal term eq.lhs then eq.rhs :: acc
         else if Term.equal term eq.rhs then eq.lhs :: acc
         else acc)
      [] equalities
  in
  let rec bfs seen = function
    | [] -> seen
    | term :: queue ->
        if StringTermSet.mem term seen then bfs seen queue
        else bfs (StringTermSet.add term seen) (neighbors term @ queue)
  in
  let rec build remaining classes =
    if StringTermSet.is_empty remaining then classes
    else
      let term = StringTermSet.choose remaining in
      let cls = bfs StringTermSet.empty [term] in
      build (StringTermSet.diff remaining cls) (StringTermSet.elements cls :: classes)
  in
  build (List.fold_left (fun acc term -> StringTermSet.add term acc) StringTermSet.empty terms) []

let fixed_string_terms equalities =
  let terms =
    List.fold_left
      (fun acc eq -> StringTermSet.add eq.lhs (StringTermSet.add eq.rhs acc))
      StringTermSet.empty
      equalities
    |> StringTermSet.elements
  in
  equality_classes terms equalities
  |> List.fold_left
       (fun acc cls ->
          if List.exists (fun term -> Option.is_some (static_string_value term)) cls then
            List.fold_left (fun acc term -> StringTermSet.add term acc) acc cls
          else
            acc)
       StringTermSet.empty

let representative_length smodel cls =
  List.find_map (string_length_in_model smodel) cls

let static_values_for_class cls =
  cls
  |> List.filter_map static_string_value
  |> List.sort_uniq ~cmp:String.compare

let class_known_value forced cls =
  let values =
    let forced_values =
      List.filter_map
        (fun (term, text) ->
           if List.exists (Term.equal term) cls then Some text else None)
        forced
    in
    static_values_for_class cls |> List.rev_append forced_values
    |> List.sort_uniq ~cmp:String.compare
  in
  match values with
  | [] -> Ok None
  | [value] -> Ok (Some value)
  | values ->
      Error
        (Format.asprintf
           "conflicting literal values in string equality class: %a"
           (List.pp (fun fmt value -> Format.fprintf fmt "%S" value)) values)

let seed_for_class index cls =
  index + List.fold_left (fun acc term -> acc + Term.hash term) 0 cls

let initial_assignments smodel terms equalities forced =
  let classes = equality_classes terms equalities in
  let assign_class (index, result) cls =
    match result with
    | Error _ as err -> index + 1, err
    | Ok assignments -> (
        match class_known_value forced cls with
        | Error _ as err -> index + 1, err
        | Ok known ->
            let value =
              match known with
              | Some text -> text
              | None ->
                  let length = Option.value ~default:0 (representative_length smodel cls) in
                  filler_string (seed_for_class index cls) length
            in
            let result =
              List.fold_left
                (fun result term ->
                   match result with
                   | Error _ as err -> err
                   | Ok assignments -> add_assignment assignments term value)
                (Ok assignments)
                cls
            in
            index + 1, result)
  in
  snd (List.fold_left assign_class (0, Ok []) classes)

type regex_polarity =
  | Regex_pos
  | Regex_neg

type regex_constraint = {
  regex_atom : Term.t;
  regex_string : Term.t;
  regex_body : regex;
  regex_polarity : regex_polarity;
}

type automaton_constraint_source =
  | Source_regex
  | Source_literal_equality
  | Source_concat_literal_shell
  | Source_prefixof
  | Source_suffixof
  | Source_contains_literal
  | Source_at_literal

type automaton_constraint = {
  automaton : RA.t;
  premises : Term.t list;
  source : automaton_constraint_source;
}

type regex_class_info = {
  regex_terms : Term.t list;
  regex_premises : Term.t list;
  regex_constraints : regex_constraint list;
  regex_negative_constraints : regex_constraint list;
  regex_shape_constraints : automaton_constraint list;
  regex_automaton : RA.t;
}

let unique_terms terms =
  List.sort_uniq ~cmp:Term.compare terms

let add_unique_regex_constraint item constraints =
  if List.exists (fun old -> Term.equal old.regex_atom item.regex_atom) constraints then
    constraints
  else
    item :: constraints

let collect_regex_constraints smodel formulas =
  let rec aux acc term =
    let Term tstruct = Term.reveal term in
    let acc =
      match tstruct, reveal_string term with
      | A1 (`YICES_NOT_TERM, arg), _ when true_in_model smodel term -> (
          match reveal_string arg with
          | Some (InRe (string, regex)) ->
              add_unique_regex_constraint
                {
                  regex_atom = term;
                  regex_string = string;
                  regex_body = regex;
                  regex_polarity = Regex_neg;
                }
                acc
          | _ -> acc)
      | _, Some (InRe (string, regex)) when true_in_model smodel term ->
          add_unique_regex_constraint
            {
              regex_atom = term;
              regex_string = string;
              regex_body = regex;
              regex_polarity = Regex_pos;
            }
            acc
      | _ -> acc
    in
    fold_children acc tstruct
  and fold_children : type a.
      regex_constraint list -> a YTypes.termstruct -> regex_constraint list =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  List.fold_left aux [] formulas |> List.rev

let is_positive_regex_constraint constraint_ =
  match constraint_.regex_polarity with
  | Regex_pos -> true
  | Regex_neg -> false

let is_negative_regex_constraint constraint_ =
  match constraint_.regex_polarity with
  | Regex_pos -> false
  | Regex_neg -> true

let term_in_class term cls =
  List.exists (Term.equal term) cls

let equality_premises_for_class equalities cls =
  equalities
  |> List.filter_map (fun eq ->
         if term_in_class eq.lhs cls && term_in_class eq.rhs cls then Some eq.atom
         else None)
  |> unique_terms

let regex_constraints_for_class constraints cls =
  List.filter (fun constraint_ -> term_in_class constraint_.regex_string cls) constraints

let rec regex_has_symbolic = function
  | ReToRe _ -> true
  | ReConcat regexes
  | ReUnion regexes
  | ReInter regexes ->
      List.exists regex_has_symbolic regexes
  | ReStar regex
  | ReComp regex
  | RePlus regex
  | ReOpt regex
  | ReLoop (regex, _, _) ->
      regex_has_symbolic regex
  | ReEmpty | ReAll | ReAllChar | ReLit _ | ReRange _ -> false

let is_concrete_regex_constraint constraint_ =
  not (regex_has_symbolic constraint_.regex_body)

let symbolic_regex_branch_limit = 16

let rec regex_exact_word_terms regex =
  let append_limited lhs rhs =
    let combined = List.rev_append lhs rhs |> List.sort_uniq ~cmp:Term.compare in
    if List.length combined > symbolic_regex_branch_limit then
      Error
        (Format.asprintf
           "symbolic regex finite expansion exceeds branch limit %d"
           symbolic_regex_branch_limit)
    else
      Ok combined
  in
  let word_concat terms =
    let terms =
      List.filter
        (fun term ->
           match static_string_value term with
           | Some "" -> false
           | _ -> true)
        terms
    in
    match terms with
    | [] -> literal ""
    | [term] -> term
    | terms -> concat terms
  in
  let concat_products lhs rhs =
    let products =
      List.concat_map
        (fun left ->
           List.map (fun right -> word_concat [left; right]) rhs)
        lhs
    in
    append_limited [] products
  in
  match regex with
  | ReEmpty -> Ok []
  | ReLit text -> Ok [literal text]
  | ReToRe term -> Ok [term]
  | ReConcat regexes ->
      let rec aux acc = function
        | [] -> Ok acc
        | regex :: rest -> (
            match regex_exact_word_terms regex with
            | Error _ as err -> err
            | Ok terms -> (
                match concat_products acc terms with
                | Error _ as err -> err
                | Ok acc -> aux acc rest))
      in
      aux [literal ""] regexes
  | ReUnion regexes ->
      let rec aux acc = function
        | [] -> Ok acc
        | regex :: rest -> (
            match regex_exact_word_terms regex with
            | Error _ as err -> err
            | Ok terms -> (
                match append_limited acc terms with
                | Error _ as err -> err
                | Ok acc -> aux acc rest))
      in
      aux [] regexes
  | ReOpt regex ->
      regex_exact_word_terms (ReUnion [ReLit ""; regex])
  | ReLoop (regex, lo, hi) ->
      if lo < 0 || hi < lo then
        Ok []
      else
        let rec repeat acc n =
          if n = 0 then Ok acc
          else
            match regex_exact_word_terms regex with
            | Error _ as err -> err
            | Ok terms -> (
                match concat_products acc terms with
                | Error _ as err -> err
                | Ok acc -> repeat acc (n - 1))
        in
        let rec collect acc n =
          if n > hi then Ok acc
          else (
            match repeat [literal ""] n with
            | Error _ as err -> err
            | Ok terms -> (
                match append_limited acc terms with
                | Error _ as err -> err
                | Ok acc -> collect acc (n + 1)))
        in
        collect [] lo
  | ReAll | ReAllChar | ReRange _ ->
      Error "symbolic regex reduction supports only exact word languages"
  | ReStar _ ->
      Error "symbolic regex reduction does not support unbounded re.*"
  | RePlus _ ->
      Error "symbolic regex reduction does not support unbounded re.+"
  | ReInter _ ->
      Error "symbolic regex reduction does not support symbolic re.inter"
  | ReComp _ ->
      Error "symbolic regex reduction does not support symbolic re.comp"

let symbolic_regex_axioms terms =
  terms
  |> List.fold_left scan_term StringTermSet.empty
  |> StringTermSet.elements
  |> List.concat_map axioms_for_string_term
  |> List.sort_uniq ~cmp:Term.compare

let symbolic_regex_reduction_lemma constraint_ words =
  let equalities =
    List.map (fun word -> Term.(constraint_.regex_string === word)) words
  in
  let conclusion =
    match constraint_.regex_polarity with
    | Regex_pos -> disjoin equalities
    | Regex_neg -> conjoin (List.map Term.not1 equalities)
  in
  conjoin
    (symbolic_regex_axioms words
     @ [Term.(constraint_.regex_atom ==> conclusion)])

let symbolic_regex_reduction state smodel formulas =
  let constraints =
    collect_regex_constraints smodel formulas
    |> List.filter (fun constraint_ -> regex_has_symbolic constraint_.regex_body)
  in
  let rec find blocked = function
    | [] -> (
        match blocked with
        | None -> Symbolic_none
        | Some reason -> Symbolic_blocked reason)
    | constraint_ :: tail ->
        if HTerms.mem state.generated_symbolic_reductions constraint_.regex_atom then
          find blocked tail
        else (
          match regex_exact_word_terms constraint_.regex_body with
          | Error reason ->
              let blocked =
                Some
                  (Format.asprintf
                     "unsupported symbolic regex for %a: %s"
                     Term.pp
                     constraint_.regex_atom
                     reason)
              in
              find blocked tail
          | Ok words ->
              let lemma = symbolic_regex_reduction_lemma constraint_ words in
              if true_in_model smodel lemma then begin
                String_log.debug
                  "symbolic regex reduction skipped for %a; model already satisfies exact reduction"
                  Term.pp
                  constraint_.regex_atom;
                find blocked tail
              end else (
                String_log.debug
                  "symbolic regex reduction generated for %a with %d branch(es)"
                  Term.pp
                  constraint_.regex_atom
                  (List.length words);
                match mark_symbolic_reduction state constraint_.regex_atom lemma with
                | Ok lemma -> Symbolic_refine (Op_in_re, lemma)
                | Error reason -> Symbolic_blocked reason))
  in
  find None constraints

let compile_regex_body regex =
  match try Ok (automata_regex_of_regex regex) with Invalid_argument msg -> Error msg with
  | Error _ as err -> err
  | Ok regex -> RA.compile regex

let automaton_constraint source premises automaton =
  Result.map
    (fun automaton -> { automaton; premises = unique_terms premises; source })
    automaton

let regex_automaton_constraint constraint_ =
  compile_regex_body constraint_.regex_body
  |> automaton_constraint Source_regex [constraint_.regex_atom]

let option_min lhs rhs =
  match lhs, rhs with
  | None, other | other, None -> other
  | Some lhs, Some rhs -> Some (min lhs rhs)

let rec regex_min_length = function
  | ReEmpty -> None
  | ReAll -> Some 0
  | ReAllChar | ReRange _ -> Some 1
  | ReToRe _ -> Some 0
  | ReLit text -> (
      match RA.scalar_length text with
      | Ok length -> Some length
      | Error _ -> None)
  | ReUnion regexes ->
      List.fold_left
        (fun acc regex -> option_min acc (regex_min_length regex))
        None
        regexes
  | ReConcat regexes ->
      let rec aux acc = function
        | [] -> Some acc
        | regex :: rest -> (
            match regex_min_length regex with
            | None -> None
            | Some length -> aux (acc + length) rest)
      in
      aux 0 regexes
  | ReStar _ -> Some 0
  | ReInter regexes ->
      regexes
      |> List.map regex_min_length
      |> List.fold_left
        (fun acc min_length ->
           match acc, min_length with
           | None, _ | _, None -> None
           | Some lhs, Some rhs -> Some (max lhs rhs))
        (Some 0)
  | ReComp _ -> Some 0
  | RePlus regex -> regex_min_length regex
  | ReOpt _ -> Some 0
  | ReLoop (regex, lo, _hi) ->
      if lo <= 0 then
        Some 0
      else
        Option.map (fun length -> lo * length) (regex_min_length regex)

let combine_automaton_constraints constraints =
  let rec aux automaton = function
    | [] -> Ok automaton
    | constraint_ :: rest -> (
        match RA.intersect automaton constraint_.automaton with
        | Error _ as error -> error
        | Ok automaton -> aux automaton rest)
  in
  match constraints with
  | [] -> Error "internal error: empty automaton constraint class"
  | constraint_ :: rest -> aux constraint_.automaton rest

let singleton_scalar text =
  match scalar_codes text with
  | [code] -> Some code
  | _ -> None

let shape_exact atom text =
  RA.exact text |> automaton_constraint Source_literal_equality [atom]

let shape_prefix atom text =
  if String.equal text "" then Ok None
  else
    RA.prefix text
    |> automaton_constraint Source_concat_literal_shell [atom]
    |> Result.map Option.some

let shape_suffix atom text =
  if String.equal text "" then Ok None
  else
    RA.suffix text
    |> automaton_constraint Source_concat_literal_shell [atom]
    |> Result.map Option.some

let add_result_option result acc =
  match result, acc with
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok None, Ok acc -> Ok acc
  | Ok (Some item), Ok acc -> Ok (item :: acc)

let shape_constraints_from_concat_shell atom term =
  match reveal_string term with
  | Some (Concat parts) -> (
      match fixed_concat_prefix_middle_suffix parts with
      | None -> Ok []
      | Some (prefix, _, suffix) ->
          Ok []
          |> add_result_option (shape_prefix atom prefix)
          |> add_result_option (shape_suffix atom suffix))
  | _ -> Ok []

let shape_constraints_from_equality smodel cls eq =
  let add_exact_for_side term other acc =
    if term_in_class term cls then
      match static_string_value other with
      | Some text -> (
          match acc with
          | Error _ as err -> err
          | Ok acc -> (
              match shape_exact eq.atom text with
              | Error _ as err -> err
              | Ok constraint_ -> Ok (constraint_ :: acc)))
      | None -> acc
    else
      acc
  in
  let add_concat_for_side term acc =
    if term_in_class term cls then
      match acc with
      | Error _ as err -> err
      | Ok acc -> (
          match shape_constraints_from_concat_shell eq.atom term with
          | Error _ as err -> err
          | Ok constraints -> Ok (List.rev_append constraints acc))
    else
      acc
  in
  let add_at_for_side term other acc =
    match reveal_string term, static_string_value other with
    | Some (At (string, index)), Some text when term_in_class string cls -> (
        match singleton_scalar text, int_value_in_model smodel index with
        | Some scalar, Some index when index >= 0 -> (
            match acc with
            | Error _ as err -> err
            | Ok acc -> (
                match
                  RA.fixed_position ~index ~scalar
                  |> automaton_constraint Source_at_literal [eq.atom]
                with
                | Error _ as err -> err
                | Ok constraint_ -> Ok (constraint_ :: acc)))
        | _ -> acc)
    | _ -> acc
  in
  Ok []
  |> add_exact_for_side eq.lhs eq.rhs
  |> add_exact_for_side eq.rhs eq.lhs
  |> add_concat_for_side eq.lhs
  |> add_concat_for_side eq.rhs
  |> add_at_for_side eq.lhs eq.rhs
  |> add_at_for_side eq.rhs eq.lhs

let shape_constraint_from_formula smodel cls term =
  if not (true_in_model smodel term) then
    Ok None
  else
    match reveal_string term with
    | Some (Prefixof (prefix, string))
      when term_in_class string cls -> (
        match static_string_value prefix with
        | None -> Ok None
        | Some text ->
            RA.prefix text
            |> automaton_constraint Source_prefixof [term]
            |> Result.map Option.some)
    | Some (Suffixof (suffix, string))
      when term_in_class string cls -> (
        match static_string_value suffix with
        | None -> Ok None
        | Some text ->
            RA.suffix text
            |> automaton_constraint Source_suffixof [term]
            |> Result.map Option.some)
    | Some (Contains (haystack, needle))
      when term_in_class haystack cls -> (
        match static_string_value needle with
        | None -> Ok None
        | Some text ->
            RA.contains text
            |> automaton_constraint Source_contains_literal [term]
            |> Result.map Option.some)
    | _ -> Ok None

let shape_constraints_from_formulas smodel cls formulas =
  let rec aux result term =
    match result with
    | Error _ as err -> err
    | Ok constraints -> (
        match shape_constraint_from_formula smodel cls term with
        | Error _ as err -> err
        | Ok None ->
            let Term tstruct = Term.reveal term in
            fold_children (Ok constraints) tstruct
        | Ok (Some constraint_) ->
            let Term tstruct = Term.reveal term in
            fold_children (Ok (constraint_ :: constraints)) tstruct)
  and fold_children : type a.
      (automaton_constraint list, string) result -> a YTypes.termstruct ->
      (automaton_constraint list, string) result =
    fun result -> function
    | A0 _ -> result
    | A1 (_, t) -> aux result t
    | A2 (_, t1, t2) -> aux (aux result t1) t2
    | Astar (_, terms) -> List.fold_left aux result terms
    | ITE (c, tb, eb) -> List.fold_left aux result [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux result f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux result body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux result array) value) index
    | Projection (_, _, t) -> aux result t
    | BV_Sum terms ->
        List.fold_left
          (fun result (_, term) -> Option.map_or ~default:result (aux result) term)
          result terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun result (_, term) -> Option.map_or ~default:result (aux result) term)
          result terms
    | Product (_, terms) ->
        List.fold_left (fun result (term, _) -> aux result term) result terms
  in
  List.fold_left aux (Ok []) formulas |> Result.map List.rev

let shape_constraints_for_class smodel formulas equalities cls =
  let from_equalities =
    List.fold_left
      (fun result eq ->
         match result with
         | Error _ as err -> err
         | Ok acc -> (
             match shape_constraints_from_equality smodel cls eq with
             | Error _ as err -> err
             | Ok constraints -> Ok (List.rev_append constraints acc)))
      (Ok [])
      equalities
  in
  match from_equalities with
  | Error _ as err -> err
  | Ok equality_constraints -> (
      match shape_constraints_from_formulas smodel cls formulas with
      | Error _ as err -> err
      | Ok formula_constraints ->
          Ok (List.rev_append equality_constraints formula_constraints))

let source_is_shape = function
  | Source_regex -> false
  | Source_literal_equality
  | Source_concat_literal_shell
  | Source_prefixof
  | Source_suffixof
  | Source_contains_literal
  | Source_at_literal -> true

let regex_automaton_constraints constraints =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | constraint_ :: rest -> (
        match regex_automaton_constraint constraint_ with
        | Error _ as err -> err
        | Ok constraint_ -> aux (constraint_ :: acc) rest)
  in
  aux [] constraints

let base_automaton_constraints positives shape_constraints negatives =
  match positives, shape_constraints, negatives with
  | [], [], [] -> Error "internal error: unconstrained regex class"
  | [], _, _ ->
      RA.compile RA.All
      |> automaton_constraint Source_regex []
      |> Result.map (fun base -> base :: shape_constraints)
  | _ ->
      match regex_automaton_constraints positives with
      | Error _ as err -> err
      | Ok regex_automata -> Ok (regex_automata @ shape_constraints)

let apply_negative_regex_constraints automaton constraints =
  let rec aux automaton = function
    | [] -> Ok automaton
    | constraint_ :: rest -> (
        match compile_regex_body constraint_.regex_body with
        | Error _ as err -> err
        | Ok negative_automaton -> (
            match RA.difference automaton negative_automaton with
            | Error _ as err -> err
            | Ok automaton -> aux automaton rest))
  in
  aux automaton constraints

let regex_class_infos smodel formulas terms equalities =
  let all_constraints = collect_regex_constraints smodel formulas in
  let constraints = List.filter is_concrete_regex_constraint all_constraints in
  let positive_constraints = List.filter is_positive_regex_constraint constraints in
  let negative_constraints = List.filter is_negative_regex_constraint constraints in
  let regex_terms =
    constraints
    |> List.map (fun constraint_ -> constraint_.regex_string)
    |> List.rev_append terms
    |> unique_terms
  in
  let classes = equality_classes regex_terms equalities in
  let build result cls =
    match result with
    | Error _ as err -> err
    | Ok infos ->
        let positives = regex_constraints_for_class positive_constraints cls in
        let negatives = regex_constraints_for_class negative_constraints cls in
        if List.is_empty positives && List.is_empty negatives then
          Ok infos
        else
          match shape_constraints_for_class smodel formulas equalities cls with
          | Error reason -> Error reason
          | Ok shape_constraints -> (
              match base_automaton_constraints positives shape_constraints negatives with
              | Error reason -> Error reason
              | Ok automaton_constraints -> (
                  match combine_automaton_constraints automaton_constraints with
                  | Error reason -> Error reason
                  | Ok positive_automaton -> (
                      match
                        apply_negative_regex_constraints positive_automaton negatives
                      with
                      | Error reason -> Error reason
                      | Ok automaton ->
                          let regex_atoms =
                            positives
                            |> List.map (fun constraint_ -> constraint_.regex_atom)
                            |> unique_terms
                          in
                          let negative_atoms =
                            negatives
                            |> List.map (fun constraint_ -> constraint_.regex_atom)
                            |> unique_terms
                          in
                          let premises =
                            automaton_constraints
                            |> List.concat_map (fun constraint_ -> constraint_.premises)
                            |> List.rev_append regex_atoms
                            |> List.rev_append negative_atoms
                            |> List.rev_append
                                 (equality_premises_for_class equalities cls)
                            |> unique_terms
                          in
                          Ok
                            ({
                              regex_terms = cls;
                              regex_premises = premises;
                              regex_constraints = positives;
                              regex_negative_constraints = negatives;
                              regex_shape_constraints = shape_constraints;
                              regex_automaton = automaton;
                            }
                             :: infos))))
  in
  let result = Result.map List.rev (List.fold_left build (Ok []) classes) in
  begin
    match result with
    | Ok infos ->
        let constrained_negatives =
          List.fold_left
            (fun acc info -> acc + List.length info.regex_negative_constraints)
            0
            infos
        in
        let shape_constraints =
          List.fold_left
            (fun acc info -> acc + List.length info.regex_shape_constraints)
            0
            infos
        in
        let shape_sources =
          List.fold_left
            (fun acc info ->
               acc
               + List.length
                   (List.filter
                      (fun constraint_ -> source_is_shape constraint_.source)
                      info.regex_shape_constraints))
            0
            infos
        in
        String_log.debug
          "regex domain: %d active constraint(s), %d negative constraint(s), %d constrained negative(s), %d shape constraint(s), %d shape source(s), %d equality class(es), %d constrained class(es)"
          (List.length constraints)
          (List.length negative_constraints)
          constrained_negatives
          shape_constraints
          shape_sources
          (List.length classes)
          (List.length infos)
    | Error reason ->
        String_log.debug "regex domain construction failed: %s" reason
  end;
  result

type regex_length_lemma_kind =
  | Length_empty_kind
  | Length_finite_kind
  | Length_periodic_kind
  | Length_lower_bound_kind

let length_periodic_formula length_term base threshold period =
  if period <= 0 then
    None
  else
    let base =
      base
      |> List.filter (fun length -> length < threshold)
      |> List.sort_uniq ~cmp:Int.compare
    in
    let base_formula =
      base
      |> List.map (fun length -> Term.(length_term === Term.Arith.int length))
    in
    let tail =
      let offset = Term.Arith.(length_term -- Term.Arith.int threshold) in
      Term.(
        Term.Arith.geq length_term (Term.Arith.int threshold)
        &&&
        Term.Arith.divides_atom (Term.Arith.int period) offset)
    in
    Some (disjoin (base_formula @ [tail]))

let length_domain_formula length_term = function
  | RA.Length_empty | RA.Length_finite [] ->
      Some (Length_empty_kind, Term.false0 ())
  | RA.Length_finite lengths ->
      let lengths = List.sort_uniq ~cmp:Int.compare lengths in
      let conclusion =
        lengths
        |> List.map (fun length -> Term.(length_term === Term.Arith.int length))
        |> disjoin
      in
      Some (Length_finite_kind, conclusion)
  | RA.Length_periodic { base; threshold; period } ->
      Option.map
        (fun formula -> Length_periodic_kind, formula)
        (length_periodic_formula length_term base threshold period)
  | RA.Length_top -> None

let regex_length_lemma constraint_ =
  match compile_regex_body constraint_.regex_body with
  | Error _ -> None
  | Ok automaton -> (
      match length_domain_formula (len constraint_.regex_string) (RA.length_domain automaton) with
      | Some (Length_empty_kind, _) ->
          Some (Length_empty_kind, Term.not1 constraint_.regex_atom)
      | Some (kind, conclusion) ->
          Some (kind, Term.(constraint_.regex_atom ==> conclusion))
      | None -> (
          match regex_min_length constraint_.regex_body with
          | Some min_length when min_length > 0 ->
              Some
                ( Length_lower_bound_kind,
                Term.(
                  constraint_.regex_atom
                  ==> Term.Arith.geq
                        (len constraint_.regex_string)
                        (Term.Arith.int min_length)) )
          | None | Some _ -> None))

let record_length_lemma_kind stats = function
  | Length_empty_kind | Length_finite_kind -> record_length_finite_lemma stats
  | Length_periodic_kind -> record_length_periodic_lemma stats
  | Length_lower_bound_kind -> record_length_lower_bound_lemma stats

let remember_regex_refinement state smodel lemma =
  if true_in_model smodel lemma then None
  else begin
    remember_internal_assertion state lemma;
    Some lemma
  end

let regex_empty_intersection_refinement state smodel infos =
  List.find_map
    (fun info ->
       if RA.is_empty info.regex_automaton then
         let lemma = Term.not1 (conjoin info.regex_premises) in
         begin
           match remember_regex_refinement state smodel lemma with
           | Some _ as result ->
               String_log.debug
                 "regex refinement: empty intersection for %d term(s)"
                 (List.length info.regex_terms);
               result
           | None -> None
         end
       else
         None)
    infos

let regex_length_domain_refinement state smodel infos =
  infos
  |> List.concat_map (fun info -> info.regex_constraints)
  |> List.find_map (fun constraint_ ->
         match regex_length_lemma constraint_ with
         | None -> None
         | Some (kind, lemma) -> (
             match remember_regex_refinement state smodel lemma with
             | Some _ as result ->
                 record_length_lemma_kind state.stats kind;
                 String_log.debug
                   "regex refinement: length-domain lemma for %a"
                   Term.pp
                   constraint_.regex_string;
                 result
             | None -> None))

let combined_regex_length_lemma info =
  match info.regex_terms with
  | [] -> None
  | representative :: _ -> (
      match length_domain_formula (len representative) (RA.length_domain info.regex_automaton) with
      | Some (Length_empty_kind, _) ->
          Some (Length_empty_kind, Term.not1 (conjoin info.regex_premises))
      | Some (kind, conclusion) ->
          Some (kind, imply_all info.regex_premises conclusion)
      | None -> None)

let combined_regex_length_domain_refinement state smodel infos =
  List.find_map
    (fun info ->
       match combined_regex_length_lemma info with
       | None -> None
       | Some (kind, lemma) -> (
           match remember_regex_refinement state smodel lemma with
           | Some _ as result ->
               record_length_combined_lemma state.stats;
               record_length_lemma_kind state.stats kind;
               String_log.debug
                 "regex refinement: combined length-domain lemma for %d term(s)"
                 (List.length info.regex_terms);
               result
           | None -> None))
    infos

let regex_failed_length_refinement state smodel infos =
  List.find_map
    (fun info ->
       match representative_length smodel info.regex_terms with
       | None -> None
       | Some length ->
           if RA.has_length info.regex_automaton length then
             None
           else
             let conclusion =
               Term.not1 Term.(len (List.hd info.regex_terms) === Term.Arith.int length)
             in
             let lemma = imply_all info.regex_premises conclusion in
             begin
               match remember_regex_refinement state smodel lemma with
               | Some _ as result ->
                   record_length_failed_lemma state.stats;
                   String_log.debug
                     "regex refinement: blocked model length %d for %d term(s)"
                     length
                     (List.length info.regex_terms);
                   result
               | None -> None
             end)
    infos

let regex_domain_refinement state smodel infos =
  match regex_empty_intersection_refinement state smodel infos with
  | Some _ as lemma -> lemma
  | None -> (
      match combined_regex_length_domain_refinement state smodel infos with
      | Some _ as lemma -> lemma
      | None -> (
          match regex_length_domain_refinement state smodel infos with
          | Some _ as lemma -> lemma
          | None -> regex_failed_length_refinement state smodel infos))

let containment_domain_lemma smodel info contains_term haystack needle_text =
  if not (term_in_class haystack info.regex_terms)
     || RA.is_empty info.regex_automaton
  then
    None
  else
    match RA.contains needle_text with
    | Error reason ->
        String_log.debug
          "containment abstraction skipped for %a: %s"
          Term.pp
          contains_term
          reason;
        None
    | Ok contains_automaton ->
        let contains_true = true_in_model smodel contains_term in
        let contains_false = true_in_model smodel (Term.not1 contains_term) in
        if contains_true then
          match RA.intersect info.regex_automaton contains_automaton with
          | Ok intersection when RA.is_empty intersection ->
              Some (imply_all info.regex_premises (Term.not1 contains_term))
          | Ok _ -> None
          | Error reason ->
              String_log.debug
                "containment abstraction intersection skipped for %a: %s"
                Term.pp
                contains_term
                reason;
              None
        else if contains_false then
          match RA.difference info.regex_automaton contains_automaton with
          | Ok difference when RA.is_empty difference ->
              Some (imply_all info.regex_premises contains_term)
          | Ok _ -> None
          | Error reason ->
              String_log.debug
                "containment abstraction difference skipped for %a: %s"
                Term.pp
                contains_term
                reason;
              None
        else
          None

let containment_domain_refinement state smodel formulas infos =
  collect_contains_terms formulas
  |> List.find_map (fun contains_term ->
         match reveal_string contains_term with
         | Some (Contains (haystack, needle)) -> (
             match static_string_value needle with
             | None -> None
             | Some needle_text ->
                 infos
                 |> List.find_map (fun info ->
                        match
                          containment_domain_lemma
                            smodel
                            info
                            contains_term
                            haystack
                            needle_text
                        with
                        | None -> None
                        | Some lemma -> (
                            match remember_regex_refinement state smodel lemma with
                            | Some _ as result ->
                                String_log.debug
                                  "containment abstraction refinement for %a"
                                  Term.pp
                                  contains_term;
                                result
                            | None -> None)))
         | _ -> None)

let character_class_set cls =
  cls
  |> List.map character_set_of_term
  |> character_inters

let character_regex_set info =
  info.regex_constraints
  |> List.map (fun constraint_ -> character_set_of_regex constraint_.regex_body)
  |> character_inters

let character_refinement_from_sources state smodel contains_term sources =
  List.find_map
    (fun (character_set, premises) ->
       match reveal_string contains_term with
       | Some (Contains (_, needle)) -> (
           match static_string_value needle with
           | Some needle_text
             when true_in_model smodel contains_term
                  && character_set_excludes_text character_set needle_text ->
               let lemma = imply_all premises (Term.not1 contains_term) in
               begin
                 match remember_regex_refinement state smodel lemma with
                 | Some _ as result ->
                     String_log.debug
                       "character abstraction refinement for %a"
                       Term.pp
                       contains_term;
                     result
                 | None -> None
               end
           | _ -> None)
       | _ -> None)
    sources

let character_abstraction_refinement state smodel formulas terms equalities infos =
  let contains_terms = collect_contains_terms formulas in
  let classes = equality_classes terms equalities in
  let class_sources haystack =
    classes
    |> List.filter (term_in_class haystack)
    |> List.map (fun cls ->
           character_class_set cls, equality_premises_for_class equalities cls)
  in
  let regex_sources haystack =
    infos
    |> List.filter (fun info -> term_in_class haystack info.regex_terms)
    |> List.map (fun info ->
           let character_set =
             character_inter (character_class_set info.regex_terms) (character_regex_set info)
           in
           character_set, info.regex_premises)
  in
  contains_terms
  |> List.find_map (fun contains_term ->
         match reveal_string contains_term with
         | Some (Contains (haystack, _)) ->
             let direct_sources = [character_set_of_term haystack, []] in
             let sources =
               direct_sources @ class_sources haystack @ regex_sources haystack
             in
             character_refinement_from_sources state smodel contains_term sources
         | _ -> None)

let regex_assignment_hints smodel forced infos =
  List.fold_left
    (fun result info ->
       match result with
       | Error _ as err -> err
       | Ok hints -> (
           match class_known_value forced info.regex_terms with
           | Error _ as err -> err
           | Ok (Some _) -> Ok hints
           | Ok None -> (
               match representative_length smodel info.regex_terms with
               | None -> Ok hints
               | Some length -> (
                   match RA.witness_of_length info.regex_automaton length with
                   | None -> Ok hints
                   | Some text ->
                       String_log.debug
                         "regex witness: selected %d term(s) at length %d"
                         (List.length info.regex_terms)
                         length;
                       List.fold_left
                         (fun result term ->
                            match result with
                            | Error _ as err -> err
                            | Ok hints -> add_assignment hints term text)
                         (Ok hints)
                         info.regex_terms))))
    (Ok [])
    infos

let rec string_value smodel assignments term =
  match reveal_string term with
  | Some (Lit text) -> Some text
  | Some (Concat terms) ->
      let rec aux acc = function
        | [] -> Some (String.concat "" (List.rev acc))
        | term :: tail -> (
            match string_value smodel assignments term with
            | Some text -> aux (text :: acc) tail
            | None -> None)
      in
      aux [] terms
  | Some (Substr (string, start, length)) -> (
      match
        string_value smodel assignments string,
        int_value smodel assignments start,
        int_value smodel assignments length
      with
      | Some string, Some start, Some length ->
          Some (substring_by_scalars string start length)
      | _ -> None)
  | Some (Replace (haystack, needle, replacement)) -> (
      match
        string_value smodel assignments haystack,
        string_value smodel assignments needle,
        string_value smodel assignments replacement
      with
      | Some haystack, Some needle, Some replacement ->
          Some (eval_replace_text haystack needle replacement)
      | _ -> None)
  | Some (ReplaceAll (haystack, needle, replacement)) -> (
      match
        string_value smodel assignments haystack,
        string_value smodel assignments needle,
        string_value smodel assignments replacement
      with
      | Some haystack, Some needle, Some replacement ->
          Some (eval_replace_all_text haystack needle replacement)
      | _ -> None)
  | Some (FromCode code) -> (
      match int_value smodel assignments code with
      | Some code -> Some (eval_from_code_value code)
      | None -> None)
  | Some (At (string, index)) -> (
      match string_value smodel assignments string, int_value smodel assignments index with
      | Some string, Some index -> Some (eval_at_text string index)
      | _ -> None)
  | _ -> assignment_find assignments term

and int_value smodel assignments term =
  match reveal_string term with
  | Some (Len string) ->
      begin
        match string_value smodel assignments string with
        | Some text -> Some (utf8_scalar_length text)
        | None -> int_value_in_model smodel term
      end
  | Some (Indexof (haystack, needle, start)) -> (
      match
        string_value smodel assignments haystack,
        string_value smodel assignments needle,
        int_value smodel assignments start
      with
      | Some haystack, Some needle, Some start ->
          Some (eval_indexof_text haystack needle start)
      | _ -> None)
  | Some (ToCode string) -> (
      match string_value smodel assignments string with
      | Some string -> Some (eval_to_code_text string)
      | None -> None)
  | _ -> int_value_in_model smodel term

and bool_value smodel assignments term =
  match reveal_string term with
  | Some (Contains (haystack, needle)) -> (
      match
        string_value smodel assignments haystack,
        string_value smodel assignments needle
      with
      | Some haystack, Some needle -> Some (eval_contains_text haystack needle)
      | _ -> None)
  | Some (Prefixof (prefix, string)) -> (
      match string_value smodel assignments prefix, string_value smodel assignments string with
      | Some prefix, Some string -> Some (eval_prefixof_text prefix string)
      | _ -> None)
  | Some (Suffixof (suffix, string)) -> (
      match string_value smodel assignments suffix, string_value smodel assignments string with
      | Some suffix, Some string -> Some (eval_suffixof_text suffix string)
      | _ -> None)
  | Some (InRe (string, regex)) -> (
      match string_value smodel assignments string with
      | Some text -> regex_accepts_value smodel assignments regex text
      | None -> None)
  | _ -> None

and regex_accepts_value smodel assignments regex text =
  let boundaries = utf8_scalar_boundaries text in
  let scalar_count = List.length boundaries - 1 in
  let boundary_at index =
    match list_nth_opt boundaries index with
    | Some boundary -> boundary
    | None -> invalid_arg "regex_accepts_value: boundary index out of range"
  in
  let text_between start_idx end_idx =
    let start_byte = boundary_at start_idx in
    let end_byte = boundary_at end_idx in
    String.sub text start_byte (end_byte - start_byte)
  in
  let literal_match literal start =
    let lit_len = utf8_scalar_length literal in
    let stop = start + lit_len in
    if stop <= scalar_count && String.equal (text_between start stop) literal then
      Some [stop]
    else
      Some []
  in
  let sort_stops stops = List.sort_uniq ~cmp:Int.compare stops in
  let rec collect_matches regexes start =
    let rec aux acc = function
      | [] -> Some (sort_stops acc)
      | regex :: rest -> (
          match match_from regex start with
          | None -> None
          | Some stops -> aux (List.rev_append stops acc) rest)
    in
    aux [] regexes
  and match_all regexes start =
    let rec aux starts = function
      | [] -> Some starts
      | regex :: rest -> (
          match
            starts
            |> List.map (fun start -> match_from regex start)
            |> sequence_options
          with
          | None -> None
          | Some next ->
              aux (sort_stops (List.flatten next)) rest)
    in
    aux [start] regexes
  and match_from regex start =
    match regex with
    | ReEmpty -> Some []
    | ReAll -> Some [scalar_count]
    | ReAllChar ->
        Some (if start < scalar_count then [start + 1] else [])
    | ReLit literal -> literal_match literal start
    | ReToRe term -> (
        match string_value smodel assignments term with
        | Some literal -> literal_match literal start
        | None -> None)
    | ReRange (lo, hi) ->
        if start >= scalar_count then Some []
        else (
          match scalar_codes (text_between start (start + 1)) with
          | [code] when lo <= code && code <= hi -> Some [start + 1]
          | _ -> Some [])
    | ReUnion regexes -> collect_matches regexes start
    | ReInter [] ->
        Some (List.init (scalar_count - start + 1) (fun offset -> start + offset))
    | ReInter (first :: rest) -> (
        match match_from first start with
        | None -> None
        | Some first_stops -> (
            match collect_matches rest start with
            | None -> None
            | Some _ ->
                let rec keep stop = function
                  | [] -> Some true
                  | regex :: rest -> (
                      match match_from regex start with
                      | None -> None
                      | Some stops ->
                          if List.exists (( = ) stop) stops then keep stop rest
                          else Some false)
                in
                let rec filter acc = function
                  | [] -> Some (List.rev acc)
                  | stop :: stops -> (
                      match keep stop rest with
                      | None -> None
                      | Some true -> filter (stop :: acc) stops
                      | Some false -> filter acc stops)
                in
                filter [] first_stops))
    | ReConcat regexes -> match_all regexes start
    | ReStar regex ->
        let rec closure seen queue =
          match queue with
          | [] -> Some (sort_stops seen)
          | start :: rest -> (
              match match_from regex start with
              | None -> None
              | Some stops ->
                  let next =
                    stops
                    |> List.filter
                         (fun stop ->
                            stop > start && not (List.exists (( = ) stop) seen))
                  in
                  closure (List.rev_append next seen) (List.rev_append next rest))
        in
        closure [start] [start]
    | ReComp regex -> (
        match match_from regex start with
        | None -> None
        | Some matched ->
            Some
              (List.init (scalar_count - start + 1) (fun offset -> start + offset)
               |> List.filter (fun stop -> not (List.exists (( = ) stop) matched))))
    | RePlus regex ->
        match_from (ReConcat [regex; ReStar regex]) start
    | ReOpt regex ->
        match_from (ReUnion [ReLit ""; regex]) start
    | ReLoop (regex, lo, hi) ->
        if lo < 0 || hi < lo then
          Some []
        else
          let rec repeat starts n =
            if n = 0 then Some starts
            else
              match
                starts
                |> List.map (fun start -> match_from regex start)
                |> sequence_options
              with
              | None -> None
              | Some next ->
                  repeat (sort_stops (List.flatten next)) (n - 1)
          in
          let rec collect acc n =
            if n > hi then Some (sort_stops acc)
            else (
              match repeat [start] n with
              | None -> None
              | Some stops -> collect (List.rev_append stops acc) (n + 1))
          in
          collect [] lo
  in
  Option.map (List.exists (( = ) scalar_count)) (match_from regex 0)

let complete_contains_witness_assignments state smodel formulas assignments =
  List.fold_left
    (fun result term ->
       match result, reveal_string term with
       | Error _ as err, _ -> err
       | Ok assignments, Some (Contains (haystack, needle))
         when HTerms.mem state.generated_contains_splits term
              && true_in_model smodel term -> (
           match
             find_witness state (ContainsPrefix (haystack, needle)),
             find_witness state (ContainsSuffix (haystack, needle))
           with
           | Some prefix, Some suffix -> (
               match
                 string_value smodel assignments prefix,
                 string_value smodel assignments needle,
                 string_value smodel assignments suffix
               with
               | Some prefix_text, Some needle_text, Some suffix_text ->
                   let text = prefix_text ^ needle_text ^ suffix_text in
                   let split = concat [prefix; needle; suffix] in
                   begin
                     match force_assignment assignments split text with
                     | Error _ as err -> err
                     | Ok assignments -> force_assignment assignments haystack text
                   end
               | _ -> Ok assignments)
           | _ -> Ok assignments)
       | Ok assignments, _ -> Ok assignments)
    (Ok assignments)
    (collect_contains_terms formulas)

let complete_concat_shell_assignments smodel equalities assignments =
  let one_direction assignments whole concat_term =
    match string_value smodel assignments whole, reveal_string concat_term with
    | Some text, Some (Concat parts) -> (
        match fixed_concat_prefix_middle_suffix parts with
        | None -> Ok assignments
        | Some (prefix, middle, suffix) ->
            if string_starts_with text prefix
               && string_ends_with text suffix
               && String.length prefix + String.length suffix <= String.length text
            then
              let middle_start = String.length prefix in
              let middle_len =
                String.length text - middle_start - String.length suffix
              in
              force_assignment
                assignments
                middle
                (String.sub text middle_start middle_len)
            else
              Ok assignments)
    | _ -> Ok assignments
  in
  List.fold_left
    (fun result eq ->
       match result with
       | Error _ as err -> err
       | Ok assignments -> (
           match one_direction assignments eq.lhs eq.rhs with
           | Error _ as err -> err
           | Ok assignments -> one_direction assignments eq.rhs eq.lhs))
    (Ok assignments)
    equalities

let force_assignments assignments bindings =
  List.fold_left
    (fun result (term, text) ->
       match result with
       | Error _ as err -> err
       | Ok assignments -> force_assignment assignments term text)
    (Ok assignments)
    bindings

let witness_text smodel assignments term length seed =
  match string_value smodel assignments term with
  | Some text when utf8_scalar_length text = length -> text
  | _ -> filler_string seed length

let witness_length smodel assignments term default =
  match string_value smodel assignments term with
  | Some text -> utf8_scalar_length text
  | None -> Option.value ~default (string_length_in_model smodel term)

let filler_avoiding needle length =
  if length <= 0 then ""
  else
    let rec pick code =
      if code > 126 then filler_string 0 length
      else
        let candidate = String.make length (Char.chr code) in
        if eval_contains_text candidate needle then pick (code + 1)
        else candidate
    in
    pick 1

let split_text_on_first text needle =
  if String.equal needle "" then
    Some ("", text)
  else
    match find_substring_from text needle 0 with
    | None -> None
    | Some start ->
        let stop = start + String.length needle in
        let prefix = String.sub text 0 start in
        let suffix = String.sub text stop (String.length text - stop) in
        Some (prefix, suffix)

let complete_substr_reduction
    state smodel fixed_terms assignments eq string start length result =
  if not (HTerms.mem state.generated_symbolic_reductions eq.atom)
     || not (true_in_model smodel eq.atom)
  then Ok assignments
  else
    match
      find_witness state (SubstrPrefix (string, start, length)),
      find_witness state (SubstrSuffix (string, start, length))
    with
    | Some prefix, Some suffix -> (
        match
          int_value smodel assignments start,
          int_value smodel assignments length,
          string_value smodel assignments result,
          string_value smodel assignments string
        with
        | Some start_value, Some length_value, _, Some text
          when StringTermSet.mem string fixed_terms ->
            begin
              match
                force_assignment
                  assignments
                  result
                  (substring_by_scalars text start_value length_value)
              with
              | Ok assignments -> Ok assignments
              | Error _ -> Ok assignments
            end
        | Some start_value, Some length_value, Some result_text, _
          when start_value >= 0
               && length_value > 0
               && utf8_scalar_length result_text <= length_value ->
            let prefix_text =
              witness_text smodel assignments prefix start_value (Term.hash prefix)
            in
            let suffix_len = witness_length smodel assignments suffix 0 in
            let suffix_text =
              witness_text smodel assignments suffix suffix_len (Term.hash suffix)
            in
            let split = concat [prefix; result; suffix] in
            let string_text = prefix_text ^ result_text ^ suffix_text in
            force_assignments
              assignments
              [
                prefix, prefix_text;
                suffix, suffix_text;
                split, string_text;
                string, string_text;
              ]
        | Some start_value, Some length_value, _, Some text ->
            force_assignment
              assignments
              result
              (substring_by_scalars text start_value length_value)
        | Some start_value, Some length_value, _, _
          when start_value < 0 || length_value <= 0 ->
            force_assignment assignments result ""
        | _ -> Ok assignments)
    | _ -> Ok assignments

let complete_indexof_reduction
    state smodel fixed_terms assignments eq haystack needle start result =
  if not (HTerms.mem state.generated_symbolic_reductions eq.atom)
     || not (true_in_model smodel eq.atom)
  then Ok assignments
  else
    match
      find_witness state (IndexofPrefix (haystack, needle, start)),
      find_witness state (IndexofSuffix (haystack, needle, start))
    with
    | Some prefix, Some suffix -> (
        match
          int_value smodel assignments result,
          int_value smodel assignments start,
          string_value smodel assignments needle,
          string_value smodel assignments haystack
        with
        | _, _, _, Some _
          when StringTermSet.mem haystack fixed_terms ->
            Ok assignments
        | Some found_at, Some _start_value, Some needle_text, _
          when found_at >= 0 && not (String.equal needle_text "") ->
            let prefix_text = filler_avoiding needle_text found_at in
            let suffix_len = witness_length smodel assignments suffix 0 in
            let suffix_text =
              witness_text smodel assignments suffix suffix_len (Term.hash suffix)
            in
            let split = concat [prefix; needle; suffix] in
            let haystack_text = prefix_text ^ needle_text ^ suffix_text in
            force_assignments
              assignments
              [
                prefix, prefix_text;
                suffix, suffix_text;
                split, haystack_text;
                haystack, haystack_text;
              ]
        | _ -> Ok assignments)
    | _ -> Ok assignments

let complete_replace_reduction
    state smodel fixed_terms assignments eq haystack needle replacement result =
  if not (HTerms.mem state.generated_symbolic_reductions eq.atom)
     || not (true_in_model smodel eq.atom)
  then Ok assignments
  else
    match
      find_witness state (ReplacePrefix (haystack, needle, replacement)),
      find_witness state (ReplaceSuffix (haystack, needle, replacement))
    with
    | Some prefix, Some suffix -> (
        match
          string_value smodel assignments haystack,
          string_value smodel assignments needle,
          string_value smodel assignments replacement,
          string_value smodel assignments result
        with
        | Some _, _, _, _
          when StringTermSet.mem haystack fixed_terms ->
            Ok assignments
        | Some haystack_text, Some "", Some replacement_text, _ ->
            force_assignment assignments result (replacement_text ^ haystack_text)
        | _, Some needle_text, _, Some result_text
          when not (String.equal needle_text "")
               && not (eval_contains_text result_text needle_text)
               && true_in_model smodel Term.(haystack === result) ->
            force_assignment assignments haystack result_text
        | _, Some needle_text, Some replacement_text, Some result_text
          when not (String.equal needle_text "") -> (
            match split_text_on_first result_text replacement_text with
            | Some (prefix_text, suffix_text) ->
                let input_split = concat [prefix; needle; suffix] in
                let output_split = concat [prefix; replacement; suffix] in
                let haystack_text = prefix_text ^ needle_text ^ suffix_text in
                force_assignments
                  assignments
                  [
                    prefix, prefix_text;
                    suffix, suffix_text;
                    input_split, haystack_text;
                    output_split, result_text;
                    haystack, haystack_text;
                    result, result_text;
                  ]
            | None -> Ok assignments)
        | _, Some needle_text, Some replacement_text, _
          when not (String.equal needle_text "") -> (
            match
              string_value smodel assignments prefix,
              string_value smodel assignments suffix
            with
            | Some prefix_text, Some suffix_text ->
                let input_split = concat [prefix; needle; suffix] in
                let output_split = concat [prefix; replacement; suffix] in
                let haystack_text = prefix_text ^ needle_text ^ suffix_text in
                let result_text = prefix_text ^ replacement_text ^ suffix_text in
                force_assignments
                  assignments
                  [
                    input_split, haystack_text;
                    output_split, result_text;
                    haystack, haystack_text;
                    result, result_text;
                  ]
            | _ -> Ok assignments)
        | _ -> Ok assignments)
    | _ -> Ok assignments

let complete_replace_all_reduction
    state smodel fixed_terms assignments eq haystack needle replacement result =
  if not (HTerms.mem state.generated_symbolic_reductions eq.atom)
     || not (true_in_model smodel eq.atom)
  then Ok assignments
  else
    match
      find_witness state (ReplaceAllPrefix (haystack, needle, replacement)),
      find_witness state (ReplaceAllSuffix (haystack, needle, replacement))
    with
    | Some prefix, Some suffix -> (
        match
          string_value smodel assignments haystack,
          string_value smodel assignments needle,
          string_value smodel assignments replacement,
          string_value smodel assignments result
        with
        | Some haystack_text, Some needle_text, Some replacement_text, _
          when StringTermSet.mem haystack fixed_terms ->
            begin
              match
                force_assignment
                  assignments
                  result
                  (eval_replace_all_text haystack_text needle_text replacement_text)
              with
              | Ok assignments -> Ok assignments
              | Error _ -> Ok assignments
            end
        | _, Some needle_text, Some replacement_text, Some result_text
          when not (String.equal needle_text "")
               && not (String.equal replacement_text "")
               && not (StringTermSet.mem haystack fixed_terms) -> (
            let prefix_len = witness_length smodel assignments prefix 0 in
            let replacement_len = utf8_scalar_length replacement_text in
            let result_len = utf8_scalar_length result_text in
            if prefix_len >= 0 && prefix_len + replacement_len <= result_len then
              let prefix_text =
                substring_by_scalars result_text 0 prefix_len
              in
              let replacement_slice =
                substring_by_scalars result_text prefix_len replacement_len
              in
              let suffix_text =
                substring_by_scalars
                  result_text
                  (prefix_len + replacement_len)
                  (result_len - prefix_len - replacement_len)
              in
              if String.equal replacement_slice replacement_text
                 && not (eval_contains_text prefix_text needle_text)
                 && not (eval_contains_text suffix_text needle_text)
              then
                let input_split = concat [prefix; needle; suffix] in
                let tail_result = replace_all suffix needle replacement in
                let output_split = concat [prefix; replacement; tail_result] in
                let haystack_text = prefix_text ^ needle_text ^ suffix_text in
                let bindings =
                  [
                    prefix, prefix_text;
                    suffix, suffix_text;
                    input_split, haystack_text;
                    tail_result, suffix_text;
                    output_split, result_text;
                    haystack, haystack_text;
                    result, result_text;
                  ]
                in
                force_assignments assignments bindings
              else
                Ok assignments
            else
              Ok assignments)
        | _, Some "", _, Some result_text
          when not (StringTermSet.mem haystack fixed_terms) ->
            force_assignment assignments haystack result_text
        | _, Some needle_text, _, Some result_text
          when not (String.equal needle_text "")
               && not (StringTermSet.mem haystack fixed_terms)
               && not (eval_contains_text result_text needle_text)
               && true_in_model smodel Term.(haystack === result) ->
            force_assignment assignments haystack result_text
        | Some haystack_text, Some needle_text, Some replacement_text, _ ->
            begin
              match
                force_assignment
                  assignments
                  result
                  (eval_replace_all_text haystack_text needle_text replacement_text)
              with
              | Ok assignments -> Ok assignments
              | Error _ -> Ok assignments
            end
        | _, Some needle_text, Some replacement_text, _
          when not (String.equal needle_text "") -> (
            match
              string_value smodel assignments prefix,
              string_value smodel assignments suffix
            with
            | Some prefix_text, Some suffix_text
              when not (eval_contains_text prefix_text needle_text) ->
                let input_split = concat [prefix; needle; suffix] in
                let tail_result = replace_all suffix needle replacement in
                let output_split = concat [prefix; replacement; tail_result] in
                let haystack_text = prefix_text ^ needle_text ^ suffix_text in
                let tail_text =
                  eval_replace_all_text suffix_text needle_text replacement_text
                in
                let result_text = prefix_text ^ replacement_text ^ tail_text in
                let bindings =
                  [
                    input_split, haystack_text;
                    tail_result, tail_text;
                    output_split, result_text;
                    result, result_text;
                  ]
                in
                let bindings =
                  if StringTermSet.mem haystack fixed_terms then bindings
                  else (haystack, haystack_text) :: bindings
                in
                force_assignments assignments bindings
            | _ -> Ok assignments)
        | _ -> Ok assignments)
    | _ -> Ok assignments

let complete_stage3_witness_assignments state smodel formulas fixed_terms assignments =
  List.fold_left
    (fun result eq ->
       match result, stage3_equality_reduction_of_eq eq with
       | Error _ as err, _ -> err
       | Ok assignments, Some (Reduce_substr (eq, string, start, length, result)) ->
           complete_substr_reduction
             state
             smodel
             fixed_terms
             assignments
             eq
             string
             start
             length
             result
       | Ok assignments, Some (Reduce_indexof (eq, haystack, needle, start, result)) ->
           complete_indexof_reduction
             state
             smodel
             fixed_terms
             assignments
             eq
             haystack
             needle
             start
             result
       | Ok assignments, Some (Reduce_replace (eq, haystack, needle, replacement, result)) ->
           complete_replace_reduction
             state
             smodel
             fixed_terms
             assignments
             eq
             haystack
             needle
             replacement
             result
       | Ok assignments, Some (Reduce_replace_all (eq, haystack, needle, replacement, result)) ->
           complete_replace_all_reduction
             state
             smodel
             fixed_terms
             assignments
             eq
             haystack
             needle
             replacement
             result
       | Ok assignments, None -> Ok assignments)
    (Ok assignments)
    (collect_true_stage3_equalities smodel formulas)

let complete_concat_assignments smodel terms assignments =
  List.fold_left
    (fun result term ->
       match result, reveal_string term with
       | Error _ as err, _ -> err
       | Ok assignments, Some (Concat _) -> (
           match string_value smodel assignments term with
           | Some text -> force_assignment assignments term text
           | None -> Ok assignments)
       | Ok assignments, _ -> Ok assignments)
    (Ok assignments)
    terms

let validate_lengths smodel assignments =
  List.find_map
    (fun (term, text) ->
       match string_length_in_model smodel term with
       | None -> None
       | Some expected ->
           let actual = utf8_scalar_length text in
           if actual = expected then None
           else
             Some
               (Format.asprintf
                  "length mismatch for %a: concrete length %d, Yices length %d"
                  Term.pp term actual expected))
    assignments

let is_stage3_view = function
  | Substr _ | Contains _ | Indexof _ | Replace _ | ReplaceAll _ | Prefixof _
  | Suffixof _ | At _ | ToCode _ | FromCode _ | InRe _ -> true
  | Lit _ | Concat _ | Len _ -> false

let rec contains_stage3_view term =
  let self =
    match reveal_string term with
    | Some view -> is_stage3_view view
    | None -> false
  in
  if self then true
  else
    let Term tstruct = Term.reveal term in
    contains_stage3_view_children tstruct

and contains_stage3_view_children : type a. a YTypes.termstruct -> bool = function
  | A0 _ -> false
  | A1 (_, t) -> contains_stage3_view t
  | A2 (_, t1, t2) -> contains_stage3_view t1 || contains_stage3_view t2
  | Astar (_, terms) -> List.exists contains_stage3_view terms
  | ITE (c, tb, eb) ->
      List.exists contains_stage3_view [c; tb; eb]
  | App (f, args) ->
      contains_stage3_view f || List.exists contains_stage3_view args
  | Bindings { vars; body; _ } ->
      contains_stage3_view body || List.exists contains_stage3_view vars
  | Update { array; index; value } ->
      contains_stage3_view array
      || contains_stage3_view value
      || List.exists contains_stage3_view index
  | Projection (_, _, t) -> contains_stage3_view t
  | BV_Sum terms ->
      List.exists
        (fun (_, term) -> Option.map_or ~default:false contains_stage3_view term)
        terms
  | Sum terms
  | FF_Sum terms ->
      List.exists
        (fun (_, term) -> Option.map_or ~default:false contains_stage3_view term)
        terms
  | Product (_, terms) ->
      List.exists (fun (term, _) -> contains_stage3_view term) terms

let is_int_term term =
  Type.equal (Term.type_of_term term) Type.(int ())

let is_bool_term term =
  Type.equal (Term.type_of_term term) Type.(bool ())

let rec eval_bool smodel assignments formula =
  let fallback () =
    if contains_stage3_view formula then None
    else try Some (SModel.formula_true_in_model smodel formula) with _ -> None
  in
  match bool_value smodel assignments formula with
  | Some value -> Some value
  | None ->
  let Term tstruct = Term.reveal formula in
  match tstruct with
  | A2 (`YICES_EQ_TERM, lhs, rhs) when is_string_equality lhs rhs -> (
      match string_value smodel assignments lhs, string_value smodel assignments rhs with
      | Some lhs, Some rhs -> Some (String.equal lhs rhs)
      | _ -> None)
  | A2 (`YICES_EQ_TERM, lhs, rhs)
    when is_int_term lhs && is_int_term rhs && contains_stage3_view formula -> (
      match int_value smodel assignments lhs, int_value smodel assignments rhs with
      | Some lhs, Some rhs -> Some (lhs = rhs)
      | _ -> None)
  | A2 (`YICES_EQ_TERM, lhs, rhs)
    when is_bool_term lhs && is_bool_term rhs && contains_stage3_view formula -> (
      match eval_bool smodel assignments lhs, eval_bool smodel assignments rhs with
      | Some lhs, Some rhs -> Some (Bool.equal lhs rhs)
      | _ -> None)
  | A2 (`YICES_ARITH_GE_ATOM, lhs, rhs) when contains_stage3_view formula -> (
      match int_value smodel assignments lhs, int_value smodel assignments rhs with
      | Some lhs, Some rhs -> Some (lhs >= rhs)
      | _ -> None)
  | Astar (`YICES_DISTINCT_TERM, terms)
    when List.for_all (fun term -> is_string_type (Term.type_of_term term)) terms ->
      begin
        match sequence_options (List.map (string_value smodel assignments) terms) with
        | Some values -> Some (all_distinct values)
        | None -> None
      end
  | A1 (`YICES_NOT_TERM, arg) -> Option.map not (eval_bool smodel assignments arg)
  | Astar (`YICES_OR_TERM, terms) ->
      let values = List.map (eval_bool smodel assignments) terms in
      if List.exists (function Some true -> true | _ -> false) values then Some true
      else if List.for_all (function Some false -> true | _ -> false) values then Some false
      else None
  | Astar (`YICES_XOR_TERM, terms) ->
      begin
        match sequence_options (List.map (eval_bool smodel assignments) terms) with
        | Some values ->
            Some
              (List.fold_left
                 (fun parity value -> if value then not parity else parity)
                 false
                 values)
        | None -> None
      end
  | ITE (cond, then_branch, else_branch)
    when Type.equal (Term.type_of_term then_branch) Type.(bool ()) ->
      begin
        match eval_bool smodel assignments cond with
        | Some true -> eval_bool smodel assignments then_branch
        | Some false -> eval_bool smodel assignments else_branch
        | None -> None
      end
  | _ -> fallback ()

let validate_formulas smodel assignments formulas =
  List.find_map
    (fun formula ->
       match eval_bool smodel assignments formula with
       | Some true -> None
       | Some false ->
           Some
             (Format.asprintf
                "concrete string model does not satisfy %a"
                Term.pp formula)
       | None ->
           Some
             (Format.asprintf
                "concrete string model cannot evaluate %a"
                Term.pp formula))
    formulas

type concrete_value =
  | Value_string of string
  | Value_int of int
  | Value_bool of bool

let rec regex_argument_terms = function
  | ReToRe term -> [term]
  | ReConcat regexes
  | ReUnion regexes
  | ReInter regexes ->
      List.concat_map regex_argument_terms regexes
  | ReStar regex
  | ReComp regex
  | RePlus regex
  | ReOpt regex
  | ReLoop (regex, _, _) ->
      regex_argument_terms regex
  | ReEmpty | ReAll | ReAllChar | ReLit _ | ReRange _ -> []

let stage3_args = function
  | Substr (string, start, length) -> [string; start; length]
  | Contains (haystack, needle)
  | Prefixof (haystack, needle)
  | Suffixof (haystack, needle) -> [haystack; needle]
  | Indexof (haystack, needle, start) -> [haystack; needle; start]
  | Replace (haystack, needle, replacement) -> [haystack; needle; replacement]
  | ReplaceAll (haystack, needle, replacement) ->
      [haystack; needle; replacement]
  | ToCode string -> [string]
  | FromCode code -> [code]
  | At (string, index) -> [string; index]
  | InRe (string, regex) -> string :: regex_argument_terms regex
  | Lit _ | Concat _ | Len _ -> []

let stage3_value smodel assignments term =
  match reveal_string term with
  | Some view when is_stage3_view view ->
      let ty = Term.type_of_term term in
      if is_string_type ty then
        Option.map (fun text -> Value_string text) (string_value smodel assignments term)
      else if is_int_term term then
        Option.map (fun value -> Value_int value) (int_value smodel assignments term)
      else if is_bool_term term then
        Option.map (fun value -> Value_bool value) (bool_value smodel assignments term)
      else
        None
  | _ -> None

let premise_for_value term = function
  | Value_string text -> Term.(term === literal text)
  | Value_int value -> Term.(term === Term.Arith.int value)
  | Value_bool true -> term
  | Value_bool false -> Term.not1 term

let premise_for_arg smodel assignments term =
  if is_string_type (Term.type_of_term term) then
    Option.map
      (fun text -> premise_for_value term (Value_string text))
      (string_value smodel assignments term)
  else if is_int_term term then
    Option.map
      (fun value -> premise_for_value term (Value_int value))
      (int_value smodel assignments term)
  else if is_bool_term term then
    Option.map
      (fun value -> premise_for_value term (Value_bool value))
      (bool_value smodel assignments term)
  else
    None

let collect_stage3_terms formulas =
  let rec aux acc term =
    let acc =
      match reveal_string term with
      | Some view when is_stage3_view view -> StringTermSet.add term acc
      | _ -> acc
    in
    let Term tstruct = Term.reveal term in
    fold_children acc tstruct
  and fold_children : type a. StringTermSet.t -> a YTypes.termstruct -> StringTermSet.t =
    fun acc -> function
    | A0 _ -> acc
    | A1 (_, t) -> aux acc t
    | A2 (_, t1, t2) -> aux (aux acc t1) t2
    | Astar (_, terms) -> List.fold_left aux acc terms
    | ITE (c, tb, eb) -> List.fold_left aux acc [c; tb; eb]
    | App (f, args) -> List.fold_left aux (aux acc f) args
    | Bindings { vars; body; _ } -> List.fold_left aux (aux acc body) vars
    | Update { array; index; value } ->
        List.fold_left aux (aux (aux acc array) value) index
    | Projection (_, _, t) -> aux acc t
    | BV_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Sum terms
    | FF_Sum terms ->
        List.fold_left
          (fun acc (_, term) -> Option.map_or ~default:acc (aux acc) term)
          acc terms
    | Product (_, terms) ->
        List.fold_left (fun acc (term, _) -> aux acc term) acc terms
  in
  List.fold_left aux StringTermSet.empty formulas |> StringTermSet.elements

let extended_refinement_lemma smodel assignments fixed_terms formulas =
  let rec fixed_string_arg term =
    not (is_string_type (Term.type_of_term term))
    || Option.is_some (static_string_value term)
    || StringTermSet.mem term fixed_terms
    ||
    match reveal_string term with
    | Some (Lit _) -> true
    | Some (Concat terms) -> List.for_all fixed_string_arg terms
    | Some (FromCode _) -> true
    | Some (Substr (string, _, _))
    | Some (At (string, _)) ->
        fixed_string_arg string
    | Some (Replace (haystack, needle, replacement))
    | Some (ReplaceAll (haystack, needle, replacement)) ->
        List.for_all fixed_string_arg [haystack; needle; replacement]
    | Some
        ( Len _
        | Contains _
        | Indexof _
        | ToCode _
        | Prefixof _
        | Suffixof _
        | InRe _ )
    | None ->
        false
  in
  collect_stage3_terms formulas
  |> List.find_map (fun term ->
         match reveal_string term, stage3_value smodel assignments term with
         | Some view, Some value ->
             let op = refinement_operator_of_view view in
             Option.flat_map
               (fun op ->
                 let args = stage3_args view in
                 if not (List.for_all fixed_string_arg args) then
                   None
                 else
                 let premises = List.filter_map (premise_for_arg smodel assignments) args in
                 if List.length premises <> List.length args
                    || not (List.for_all (true_in_model smodel) premises)
                 then
                   None
                 else
                   let conclusion = premise_for_value term value in
                   if true_in_model smodel conclusion then None
                   else
                     let lemma =
                       match premises with
                       | [] -> conclusion
                       | [premise] -> Term.(premise ==> conclusion)
                       | premises -> Term.(andN premises ==> conclusion)
                     in
                     if true_in_model smodel lemma then None else Some (op, lemma))
               op
         | _ -> None)

type concrete_result =
  | Concrete_sat of (Term.t * string) list
  | Concrete_refine of refinement_operator * Term.t
  | Concrete_unknown of string

type refinement_candidate_outcome =
  | Candidate_none
  | Candidate_refine of refinement_operator * Term.t
  | Candidate_blocked of string

type refinement_candidate = {
  candidate_priority : int;
  candidate_operator : refinement_operator;
  candidate_label : string;
  candidate_produce : unit -> refinement_candidate_outcome;
}

let candidate_rank state candidate =
  ( candidate.candidate_priority
    + min 10 (refinement_operator_count state.stats candidate.candidate_operator),
    candidate.candidate_priority,
    refinement_operator_name candidate.candidate_operator,
    candidate.candidate_label )

let compare_refinement_candidates state lhs rhs =
  Stdlib.compare (candidate_rank state lhs) (candidate_rank state rhs)

let choose_refinement_candidate state candidates =
  let rec find blocked = function
    | [] -> (
        match blocked with
        | None -> Candidate_none
        | Some reason -> Candidate_blocked reason)
    | candidate :: tail -> (
        match candidate.candidate_produce () with
        | Candidate_none -> find blocked tail
        | Candidate_refine (op, lemma) ->
            String_log.debug
              "selected %s refinement candidate at priority %d"
              candidate.candidate_label
              candidate.candidate_priority;
            Candidate_refine (op, lemma)
        | Candidate_blocked reason ->
            String_log.debug
              "skipping blocked %s refinement candidate: %s"
              candidate.candidate_label
              reason;
            find (Some reason) tail)
  in
  candidates |> List.stable_sort (compare_refinement_candidates state) |> find None

let candidate_of_option op option =
  match option with
  | None -> Candidate_none
  | Some lemma -> Candidate_refine (op, lemma)

let candidate_of_symbolic = function
  | Symbolic_none -> Candidate_none
  | Symbolic_refine (op, lemma) -> Candidate_refine (op, lemma)
  | Symbolic_blocked reason -> Candidate_blocked reason

let concrete_of_candidate = function
  | Candidate_none -> None
  | Candidate_refine (op, lemma) -> Some (Concrete_refine (op, lemma))
  | Candidate_blocked reason -> Some (Concrete_unknown reason)

let early_refinement_candidate state smodel formulas terms equalities regex_infos =
  choose_refinement_candidate
    state
    [
      {
        candidate_priority = 20;
        candidate_operator = Op_concat;
        candidate_label = "concat literal refinement";
        candidate_produce =
          (fun () -> candidate_of_option Op_concat (refinement_lemma smodel equalities));
      };
      {
        candidate_priority = 25;
        candidate_operator = Op_contains;
        candidate_label = "character abstraction refinement";
        candidate_produce =
          (fun () ->
             candidate_of_option
               Op_contains
               (character_abstraction_refinement
                  state
                  smodel
                  formulas
                  terms
                  equalities
                  regex_infos));
      };
      {
        candidate_priority = 30;
        candidate_operator = Op_in_re;
        candidate_label = "regex domain refinement";
        candidate_produce =
          (fun () ->
             candidate_of_option
               Op_in_re
               (regex_domain_refinement state smodel regex_infos));
      };
      {
        candidate_priority = 45;
        candidate_operator = Op_contains;
        candidate_label = "containment domain refinement";
        candidate_produce =
          (fun () ->
             candidate_of_option
               Op_contains
               (containment_domain_refinement state smodel formulas regex_infos));
      };
    ]

let post_validation_refinement_candidate
    state smodel assignments formulas equalities fixed_terms =
  choose_refinement_candidate
    state
    [
      {
        candidate_priority = 20;
        candidate_operator = Op_concat;
        candidate_label = "concat literal refinement";
        candidate_produce =
          (fun () -> candidate_of_option Op_concat (refinement_lemma smodel equalities));
      };
      {
        candidate_priority = 50;
        candidate_operator = Op_replace;
        candidate_label = "model-based extended refinement";
        candidate_produce =
          (fun () ->
             match extended_refinement_lemma smodel assignments fixed_terms formulas with
             | Some (op, lemma) -> Candidate_refine (op, lemma)
             | None -> Candidate_none);
      };
      {
        candidate_priority = 60;
        candidate_operator = Op_in_re;
        candidate_label = "symbolic regex refinement";
        candidate_produce =
          (fun () ->
             candidate_of_symbolic
               (symbolic_regex_reduction state smodel formulas));
      };
      {
        candidate_priority = 90;
        candidate_operator = Op_contains;
        candidate_label = "positive contains split refinement";
        candidate_produce =
          (fun () ->
             candidate_of_symbolic
               (positive_contains_reduction state smodel formulas));
      };
      {
        candidate_priority = 80;
        candidate_operator = Op_substr;
        candidate_label = "stage3 equality split refinement";
        candidate_produce =
          (fun () ->
             candidate_of_symbolic
               (symbolic_stage3_equality_reduction state smodel formulas));
      };
    ]

let build_concrete_strings state smodel support =
  let formulas = current_terms state in
  let terms =
    List.fold_left
      (fun acc term -> StringTermSet.add term acc)
      (List.fold_left scan_term StringTermSet.empty formulas)
      support
    |> StringTermSet.elements
  in
  let equalities = collect_true_equalities smodel formulas in
  let fixed_terms = fixed_string_terms equalities in
  match regex_class_infos smodel formulas terms equalities with
  | Error reason -> Concrete_unknown reason
  | Ok regex_infos -> (
      match
        early_refinement_candidate
          state
          smodel
          formulas
          terms
          equalities
          regex_infos
        |> concrete_of_candidate
      with
      | Some result -> result
      | None ->
          let concat_forced =
            List.filter_map concat_literal_assignment equalities
          in
          match regex_assignment_hints smodel concat_forced regex_infos with
          | Error reason -> Concrete_unknown reason
          | Ok regex_forced ->
              let forced = List.rev_append regex_forced concat_forced in
              match initial_assignments smodel terms equalities forced with
              | Error reason -> Concrete_unknown reason
              | Ok assignments -> (
                  match
                    complete_concat_shell_assignments smodel equalities assignments
                  with
                  | Error reason -> Concrete_unknown reason
                  | Ok assignments -> (
                      match
                        complete_contains_witness_assignments
                          state
                          smodel
                          formulas
                          assignments
                      with
                      | Error reason -> Concrete_unknown reason
                      | Ok assignments -> (
                          match
                            complete_stage3_witness_assignments
                              state
                              smodel
                              formulas
                              fixed_terms
                              assignments
                          with
                          | Error reason -> Concrete_unknown reason
                          | Ok assignments -> (
                              match complete_concat_assignments smodel terms assignments with
                              | Error reason -> Concrete_unknown reason
                              | Ok assignments -> (
                                  match dedup_assignments assignments with
                                  | Error reason -> Concrete_unknown reason
                                  | Ok assignments -> (
                                      match validate_lengths smodel assignments with
                                      | Some reason -> Concrete_unknown reason
                                      | None -> (
                                          match
                                            validate_formulas
                                              smodel
                                              assignments
                                              formulas
                                          with
                                          | None -> Concrete_sat assignments
                                          | Some reason -> (
                                              match
                                                post_validation_refinement_candidate
                                                  state
                                                  smodel
                                                  assignments
                                                  formulas
                                                  equalities
                                                  fixed_terms
                                                |> concrete_of_candidate
                                              with
                                              | Some result -> result
                                              | None -> Concrete_unknown reason)))))))))

let check state smodel =
  if state.stats.active_iterations >= state.refinement_limit then begin
    let reason =
      Format.asprintf
        "Stage 3 string extension exceeded refinement limit of %d iteration(s)"
        state.refinement_limit
    in
    state.last_unknown <- Some reason;
    ignore state.last_unknown;
    state.last_strings <- [];
    reset_active_iterations state;
    String_log.info "%s" reason;
    log_stats state "unknown";
    Unknown reason
  end else begin
    state.stats.active_iterations <- state.stats.active_iterations + 1;
    state.stats.refinement_iterations <- state.stats.refinement_iterations + 1;
    state.last_strings <- [];
    let support = string_terms_in_state state in
    match build_concrete_strings state smodel support with
    | Concrete_sat strings ->
        state.last_unknown <- None;
        state.last_strings <- strings;
        reset_active_iterations state;
        log_stats state "sat";
        Sat smodel
    | Concrete_refine (op, lemma) ->
        record_refinement_lemma state.stats op;
        String_log.info "Stage 3 generated %s refinement lemma %a"
          (refinement_operator_name op)
          Term.pp
          lemma;
        log_stats state "refinement";
        Unsat lemma
    | Concrete_unknown reason ->
        state.last_unknown <- Some reason;
        ignore state.last_unknown;
        reset_active_iterations state;
        String_log.info "Stage 3 string extension is incomplete: %s" reason;
        log_stats state "unknown";
        Unknown reason
  end

let enrich_smodel state ?support base =
  let support =
    match support with
    | None -> string_terms_in_public_state state
    | Some support -> support
  in
  let strings =
    match state.last_strings with
    | [] -> (
        match build_concrete_strings state base support with
        | Concrete_sat strings -> strings
        | Concrete_refine _ | Concrete_unknown _ -> [])
    | strings -> strings
  in
  let strings =
    List.filter
      (fun (term, _) -> List.exists (Term.equal term) support)
      strings
  in
  { StringModel.base = SModel.with_support support base; strings }

let interpolant _ old_interpolant = old_interpolant

let pp_string_literal fmt s =
  Format.fprintf fmt "%S" s

let regex_code_string code =
  if 0 <= code && code <= 0x7F then String.make 1 (Char.chr code)
  else Format.sprintf "\\u{%X}" code

let rec pp_regex fmt = function
  | ReEmpty -> Format.fprintf fmt "re.none"
  | ReAll -> Format.fprintf fmt "re.all"
  | ReAllChar -> Format.fprintf fmt "re.allchar"
  | ReLit text -> Format.fprintf fmt "@[<2>(str.to_re@ %S)@]" text
  | ReToRe term -> Format.fprintf fmt "@[<2>(str.to_re@ %a)@]" Term.pp term
  | ReRange (lo, hi) ->
      Format.fprintf fmt "@[<2>(re.range@ %S@ %S)@]"
        (regex_code_string lo)
        (regex_code_string hi)
  | ReConcat regexes ->
      Format.fprintf fmt "@[<2>(re.++@ %a)@]" (List.pp pp_regex) regexes
  | ReUnion regexes ->
      Format.fprintf fmt "@[<2>(re.union@ %a)@]" (List.pp pp_regex) regexes
  | ReStar regex ->
      Format.fprintf fmt "@[<2>(re.*@ %a)@]" pp_regex regex
  | ReInter regexes ->
      Format.fprintf fmt "@[<2>(re.inter@ %a)@]" (List.pp pp_regex) regexes
  | ReComp regex ->
      Format.fprintf fmt "@[<2>(re.comp@ %a)@]" pp_regex regex
  | RePlus regex ->
      Format.fprintf fmt "@[<2>(re.+@ %a)@]" pp_regex regex
  | ReOpt regex ->
      Format.fprintf fmt "@[<2>(re.opt@ %a)@]" pp_regex regex
  | ReLoop (regex, lo, hi) ->
      Format.fprintf fmt "@[<2>((_ re.loop %d %d)@ %a)@]" lo hi pp_regex regex

let pp_term fmt term =
  match reveal_string term with
  | Some (Lit text) -> pp_string_literal fmt text
  | Some (Concat terms) ->
      Format.fprintf fmt "@[<2>(str.++@ %a)@]" (List.pp Term.pp) terms
  | Some (Len term) ->
      Format.fprintf fmt "@[<2>(str.len@ %a)@]" Term.pp term
  | Some (Substr (string, start, length)) ->
      Format.fprintf fmt "@[<2>(str.substr@ %a@ %a@ %a)@]"
        Term.pp string Term.pp start Term.pp length
  | Some (Contains (haystack, needle)) ->
      Format.fprintf fmt "@[<2>(str.contains@ %a@ %a)@]"
        Term.pp haystack Term.pp needle
  | Some (Indexof (haystack, needle, start)) ->
      Format.fprintf fmt "@[<2>(str.indexof@ %a@ %a@ %a)@]"
        Term.pp haystack Term.pp needle Term.pp start
  | Some (Replace (haystack, needle, replacement)) ->
      Format.fprintf fmt "@[<2>(str.replace@ %a@ %a@ %a)@]"
        Term.pp haystack Term.pp needle Term.pp replacement
  | Some (ReplaceAll (haystack, needle, replacement)) ->
      Format.fprintf fmt "@[<2>(str.replace_all@ %a@ %a@ %a)@]"
        Term.pp haystack Term.pp needle Term.pp replacement
  | Some (ToCode string) ->
      Format.fprintf fmt "@[<2>(str.to_code@ %a)@]" Term.pp string
  | Some (FromCode code) ->
      Format.fprintf fmt "@[<2>(str.from_code@ %a)@]" Term.pp code
  | Some (Prefixof (prefix, string)) ->
      Format.fprintf fmt "@[<2>(str.prefixof@ %a@ %a)@]"
        Term.pp prefix Term.pp string
  | Some (Suffixof (suffix, string)) ->
      Format.fprintf fmt "@[<2>(str.suffixof@ %a@ %a)@]"
        Term.pp suffix Term.pp string
  | Some (At (string, index)) ->
      Format.fprintf fmt "@[<2>(str.at@ %a@ %a)@]" Term.pp string Term.pp index
  | Some (InRe (string, regex)) ->
      Format.fprintf fmt "@[<2>(str.in_re@ %a@ %a)@]" Term.pp string pp_regex regex
  | None -> Term.pp fmt term

let pp_type fmt typ =
  if is_string_type typ then Format.fprintf fmt "String"
  else Type.pp fmt typ

let rec term_to_sexp ?smt2arrays term =
  match reveal_string term with
  | Some (Lit text) -> Sexp.Atom (Format.sprintf "%S" text)
  | Some (Concat terms) ->
      Sexp.List (Sexp.Atom "str.++" :: List.map (term_to_sexp ?smt2arrays) terms)
  | Some (Len term) ->
      Sexp.List [Sexp.Atom "str.len"; term_to_sexp ?smt2arrays term]
  | Some (Substr (string, start, length)) ->
      Sexp.List
        [
          Sexp.Atom "str.substr";
          term_to_sexp ?smt2arrays string;
          term_to_sexp ?smt2arrays start;
          term_to_sexp ?smt2arrays length;
        ]
  | Some (Contains (haystack, needle)) ->
      Sexp.List
        [
          Sexp.Atom "str.contains";
          term_to_sexp ?smt2arrays haystack;
          term_to_sexp ?smt2arrays needle;
        ]
  | Some (Indexof (haystack, needle, start)) ->
      Sexp.List
        [
          Sexp.Atom "str.indexof";
          term_to_sexp ?smt2arrays haystack;
          term_to_sexp ?smt2arrays needle;
          term_to_sexp ?smt2arrays start;
        ]
  | Some (Replace (haystack, needle, replacement)) ->
      Sexp.List
        [
          Sexp.Atom "str.replace";
          term_to_sexp ?smt2arrays haystack;
          term_to_sexp ?smt2arrays needle;
          term_to_sexp ?smt2arrays replacement;
        ]
  | Some (ReplaceAll (haystack, needle, replacement)) ->
      Sexp.List
        [
          Sexp.Atom "str.replace_all";
          term_to_sexp ?smt2arrays haystack;
          term_to_sexp ?smt2arrays needle;
          term_to_sexp ?smt2arrays replacement;
        ]
  | Some (ToCode string) ->
      Sexp.List [Sexp.Atom "str.to_code"; term_to_sexp ?smt2arrays string]
  | Some (FromCode code) ->
      Sexp.List [Sexp.Atom "str.from_code"; term_to_sexp ?smt2arrays code]
  | Some (Prefixof (prefix, string)) ->
      Sexp.List
        [
          Sexp.Atom "str.prefixof";
          term_to_sexp ?smt2arrays prefix;
          term_to_sexp ?smt2arrays string;
        ]
  | Some (Suffixof (suffix, string)) ->
      Sexp.List
        [
          Sexp.Atom "str.suffixof";
          term_to_sexp ?smt2arrays suffix;
          term_to_sexp ?smt2arrays string;
        ]
  | Some (At (string, index)) ->
      Sexp.List
        [
          Sexp.Atom "str.at";
          term_to_sexp ?smt2arrays string;
          term_to_sexp ?smt2arrays index;
        ]
  | Some (InRe (string, regex)) ->
      Sexp.List
        [
          Sexp.Atom "str.in_re";
          term_to_sexp ?smt2arrays string;
          regex_to_sexp ?smt2arrays regex;
        ]
  | None -> Term.to_sexp ?smt2arrays term

and regex_to_sexp ?smt2arrays = function
  | ReEmpty -> Sexp.Atom "re.none"
  | ReAll -> Sexp.Atom "re.all"
  | ReAllChar -> Sexp.Atom "re.allchar"
  | ReLit text -> Sexp.List [Sexp.Atom "str.to_re"; Sexp.Atom (Format.sprintf "%S" text)]
  | ReToRe term -> Sexp.List [Sexp.Atom "str.to_re"; term_to_sexp ?smt2arrays term]
  | ReRange (lo, hi) ->
      Sexp.List
        [
          Sexp.Atom "re.range";
          Sexp.Atom (Format.sprintf "%S" (regex_code_string lo));
          Sexp.Atom (Format.sprintf "%S" (regex_code_string hi));
        ]
  | ReConcat regexes ->
      Sexp.List (Sexp.Atom "re.++" :: List.map (regex_to_sexp ?smt2arrays) regexes)
  | ReUnion regexes ->
      Sexp.List
        (Sexp.Atom "re.union" :: List.map (regex_to_sexp ?smt2arrays) regexes)
  | ReStar regex ->
      Sexp.List [Sexp.Atom "re.*"; regex_to_sexp ?smt2arrays regex]
  | ReInter regexes ->
      Sexp.List
        (Sexp.Atom "re.inter" :: List.map (regex_to_sexp ?smt2arrays) regexes)
  | ReComp regex ->
      Sexp.List [Sexp.Atom "re.comp"; regex_to_sexp ?smt2arrays regex]
  | RePlus regex ->
      Sexp.List [Sexp.Atom "re.+"; regex_to_sexp ?smt2arrays regex]
  | ReOpt regex ->
      Sexp.List [Sexp.Atom "re.opt"; regex_to_sexp ?smt2arrays regex]
  | ReLoop (regex, lo, hi) ->
      Sexp.List
        [
          Sexp.List
            [
              Sexp.Atom "_";
              Sexp.Atom "re.loop";
              Sexp.Atom (string_of_int lo);
              Sexp.Atom (string_of_int hi);
            ];
          regex_to_sexp ?smt2arrays regex;
        ]

let type_to_sexp ?smt2arrays typ =
  if is_string_type typ then Sexp.Atom "String"
  else Type.to_sexp ?smt2arrays typ

let smodel_to_sexp ?smt2arrays model =
  let string_bindings =
    model.StringModel.strings
    |> List.map (fun (term, text) ->
           Sexp.List
             [
               Sexp.Atom ":=";
               term_to_sexp ?smt2arrays term;
               Sexp.Atom (Format.sprintf "%S" text);
             ])
  in
  match SModel.as_map model.base with
  | [] -> Sexp.List (Sexp.Atom "model" :: string_bindings)
  | base_bindings ->
      let base_bindings =
        List.map
          (fun (lhs, rhs) ->
             Sexp.List
               [
                 Sexp.Atom ":=";
                 Term.to_sexp ?smt2arrays lhs;
                 Term.to_sexp ?smt2arrays rhs;
               ])
          base_bindings
      in
      Sexp.List (Sexp.Atom "model" :: string_bindings @ base_bindings)

module Type = struct
  include Type

  let string = string_type
  let is_string = is_string_type
end

module Term = struct
  include Term

  let str = literal

  let string_var ?name () =
    Term.new_uninterpreted ?name (string_type ())

  let concat = concat
  let len = len
  let substr = substr
  let contains = contains
  let indexof = indexof
  let replace = replace
  let replace_all = replace_all
  let to_code = to_code
  let from_code = from_code
  let prefixof = prefixof
  let suffixof = suffixof
  let at = at
  let in_re = in_re
  let string_reveal = reveal_string
end

module Regex = struct
  type t = regex

  let empty = ReEmpty
  let all = ReAll
  let all_char = ReAllChar
  let str text =
    ignore (utf8_scalar_length text);
    ReLit text
  let to_re term =
    check_string_term term;
    match reveal_string term with
    | Some (Lit text) -> ReLit text
    | _ -> ReToRe term
  let range lo hi =
    let lo_codes = scalar_codes lo in
    let hi_codes = scalar_codes hi in
    match lo_codes, hi_codes with
    | [lo], [hi] when lo <= hi -> ReRange (lo, hi)
    | [_], [_] ->
        Yices2.High.ExceptionsErrorHandling.raise_bindings_error
          "invalid regex range: lower bound %S is greater than upper bound %S"
          lo hi
    | _ ->
        Yices2.High.ExceptionsErrorHandling.raise_bindings_error
          "regex range endpoints must be single Unicode scalar values: %S %S"
          lo hi
  let concat regexes =
    match regexes with
    | [] -> ReLit ""
    | [regex] -> regex
    | _ -> ReConcat regexes
  let union regexes =
    match regexes with
    | [] -> ReEmpty
    | [regex] -> regex
    | _ -> ReUnion regexes
  let star regex = ReStar regex
  let inter regexes =
    match regexes with
    | [] -> ReAll
    | [regex] -> regex
    | _ -> ReInter regexes
  let comp regex = ReComp regex
  let plus regex = RePlus regex
  let opt regex = ReOpt regex
  let loop ~lo ~hi regex =
    if lo < 0 || hi < lo then
      Yices2.High.ExceptionsErrorHandling.raise_bindings_error
        "invalid regex loop bounds: %d %d" lo hi
    else
      ReLoop (regex, lo, hi)
end

module Arg = struct
  type term = Term.t
  type typ = Type.t
  type old_context = Context.t
  type config = Config.t
  type param = Param.t
  type smodel = StringModel.t

  type nonrec t = t

  let translate_assertion_impl = translate_assertion
  let translate_assumption_impl = translate_assumption

  let malloc = malloc
  let reset = reset
  let push = push
  let pop = pop
  let goto = goto
  let translate_assertion (ctx : old_context) state formula =
    translate_assertion_impl ctx state formula
  let translate_assumption (ctx : old_context) state formula =
    translate_assumption_impl ctx state formula
  let check = check
  let term_of_old = term_of_old
  let typ_of_old = typ_of_old
  let param_to_old = param_to_old
  let smodel_to_old = smodel_to_old
  let smodel_of_old = smodel_of_old
  let enrich_smodel = enrich_smodel
  let interpolant = interpolant
  let pp_term = pp_term
  let pp_type = pp_type
  let term_to_sexp = term_to_sexp
  let type_to_sexp = type_to_sexp
  let smodel_to_sexp = smodel_to_sexp
end

module Context = Make (Context) (Arg)
