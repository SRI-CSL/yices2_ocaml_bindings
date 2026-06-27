type interval = {
  lo : int;
  hi : int;
}

type regex =
  | Empty
  | All
  | AllChar
  | Lit of string
  | Range of int * int
  | Concat of regex list
  | Union of regex list
  | Star of regex
  | Inter of regex list
  | Comp of regex
  | Plus of regex
  | Opt of regex
  | Loop of regex * int * int

type length_domain =
  | Length_empty
  | Length_finite of int list
  | Length_periodic of {
      base : int list;
      threshold : int;
      period : int;
    }
  | Length_top

module IntSet = Set.Make(Int)
module StateSet = IntSet
module PairSet = Set.Make(struct
    type t = int * int
    let compare = compare
  end)
module StateListMap = Map.Make(struct
    type t = int list
    let compare = compare
  end)
module ConfigSet = Set.Make(struct
    type t = int list * int
    let compare = compare
  end)
module CostConfigSet = Set.Make(struct
    type t = int list * int * int
    let compare = compare
  end)

type label =
  | Eps
  | RangeLabel of interval list

type transition = {
  src : int;
  label : label;
  dst : int;
}

type nfa = {
  start : int;
  finals : StateSet.t;
  transitions : transition list;
  state_count : int;
}

type t = {
  nfa : nfa;
  source : regex option;
  mutable length_domain_cache : length_domain option;
}

let make ?source nfa =
  { nfa; source; length_domain_cache = None }

let unicode_max = 0x10FFFF
let surrogate_lo = 0xD800
let surrogate_hi = 0xDFFF

let scalar_intervals = [
  { lo = 0; hi = surrogate_lo - 1 };
  { lo = surrogate_hi + 1; hi = unicode_max };
]

let valid_scalar code =
  0 <= code
  && code <= unicode_max
  && not (surrogate_lo <= code && code <= surrogate_hi)

let compare_interval a b =
  let c = compare a.lo b.lo in
  if c <> 0 then c else compare a.hi b.hi

let normalize_intervals intervals =
  let intervals =
    intervals
    |> List.filter (fun i -> i.lo <= i.hi)
    |> List.sort compare_interval
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | i :: rest -> (
        match acc with
        | { lo; hi } :: tail when i.lo <= hi + 1 ->
            aux ({ lo; hi = max hi i.hi } :: tail) rest
        | _ -> aux (i :: acc) rest)
  in
  aux [] intervals

let intersect_intervals lhs rhs =
  let rec aux acc lhs rhs =
    match lhs, rhs with
    | [], _ | _, [] -> List.rev acc
    | l :: ls, r :: rs ->
        let lo = max l.lo r.lo in
        let hi = min l.hi r.hi in
        let acc = if lo <= hi then { lo; hi } :: acc else acc in
        if l.hi < r.hi then aux acc ls rhs else aux acc lhs rs
  in
  aux [] (normalize_intervals lhs) (normalize_intervals rhs)

let label_intersection lhs rhs =
  match lhs, rhs with
  | Eps, Eps -> Some Eps
  | RangeLabel lhs, RangeLabel rhs ->
      let ranges = intersect_intervals lhs rhs in
      if ranges = [] then None else Some (RangeLabel ranges)
  | Eps, RangeLabel _ | RangeLabel _, Eps -> None

let interval_contains code interval =
  interval.lo <= code && code <= interval.hi

let scalar_of_printable intervals =
  let rec loop code =
    if code > 126 then None
    else if List.exists (interval_contains code) intervals then Some code
    else loop (code + 1)
  in
  loop 32

let representative_scalar intervals =
  match scalar_of_printable intervals with
  | Some code -> Some code
  | None ->
      intervals
      |> List.find_map (fun interval ->
             if valid_scalar interval.lo then Some interval.lo else None)

let utf8_add buf code =
  if not (valid_scalar code) then
    invalid_arg "utf8_add: invalid Unicode scalar";
  if code <= 0x7F then
    Buffer.add_char buf (Char.chr code)
  else if code <= 0x7FF then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end else if code <= 0xFFFF then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (code lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end

let string_of_scalars scalars =
  let buf = Buffer.create (List.length scalars) in
  List.iter (utf8_add buf) scalars;
  Buffer.contents buf

let utf8_scalars input =
  let len = String.length input in
  let continuation index =
    index < len && (Char.code input.[index] land 0xC0) = 0x80
  in
  let byte index = Char.code input.[index] in
  let rec loop acc index =
    if index = len then Ok (List.rev acc)
    else
      let b0 = byte index in
      if b0 <= 0x7F then
        loop (b0 :: acc) (index + 1)
      else if 0xC2 <= b0 && b0 <= 0xDF then
        if index + 1 < len && continuation (index + 1) then
          let code = ((b0 land 0x1F) lsl 6) lor (byte (index + 1) land 0x3F) in
          loop (code :: acc) (index + 2)
        else Error "invalid UTF-8 continuation byte"
      else if 0xE0 <= b0 && b0 <= 0xEF then
        if index + 2 < len
           && continuation (index + 1)
           && continuation (index + 2)
        then
          let b1 = byte (index + 1) in
          let b2 = byte (index + 2) in
          let code =
            ((b0 land 0x0F) lsl 12)
            lor ((b1 land 0x3F) lsl 6)
            lor (b2 land 0x3F)
          in
          if valid_scalar code && code >= 0x800 then
            loop (code :: acc) (index + 3)
          else Error "invalid UTF-8 scalar value"
        else Error "invalid UTF-8 continuation byte"
      else if 0xF0 <= b0 && b0 <= 0xF4 then
        if index + 3 < len
           && continuation (index + 1)
           && continuation (index + 2)
           && continuation (index + 3)
        then
          let b1 = byte (index + 1) in
          let b2 = byte (index + 2) in
          let b3 = byte (index + 3) in
          let code =
            ((b0 land 0x07) lsl 18)
            lor ((b1 land 0x3F) lsl 12)
            lor ((b2 land 0x3F) lsl 6)
            lor (b3 land 0x3F)
          in
          if valid_scalar code && code >= 0x10000 then
            loop (code :: acc) (index + 4)
          else Error "invalid UTF-8 scalar value"
        else Error "invalid UTF-8 continuation byte"
      else
        Error "invalid UTF-8 leading byte"
  in
  loop [] 0

let scalar_length input =
  match utf8_scalars input with
  | Ok scalars -> Ok (List.length scalars)
  | Error _ as error -> error

let offset_transition offset transition =
  {
    src = transition.src + offset;
    label = transition.label;
    dst = transition.dst + offset;
  }

let offset_nfa offset nfa =
  {
    start = nfa.start + offset;
    finals = StateSet.map (( + ) offset) nfa.finals;
    transitions = List.map (offset_transition offset) nfa.transitions;
    state_count = nfa.state_count;
  }

let empty_nfa =
  { start = 0; finals = StateSet.empty; transitions = []; state_count = 1 }

let epsilon_nfa =
  { start = 0; finals = StateSet.singleton 0; transitions = []; state_count = 1 }

let range_nfa intervals =
  {
    start = 0;
    finals = StateSet.singleton 1;
    transitions = [{ src = 0; label = RangeLabel (normalize_intervals intervals); dst = 1 }];
    state_count = 2;
  }

let concat_nfa nfas =
  match nfas with
  | [] -> epsilon_nfa
  | first :: rest ->
      let first = offset_nfa 0 first in
      let offset, nfas =
        List.fold_left
          (fun (offset, acc) nfa ->
             let nfa = offset_nfa offset nfa in
             offset + nfa.state_count, nfa :: acc)
          (first.state_count, [first])
          rest
      in
      let nfas = List.rev nfas in
      let transitions =
        List.concat_map (fun nfa -> nfa.transitions) nfas
      in
      let epsilon_links =
        let rec aux acc = function
          | lhs :: (rhs :: _ as tail) ->
              let links =
                StateSet.fold
                  (fun final acc -> { src = final; label = Eps; dst = rhs.start } :: acc)
                  lhs.finals
                  acc
              in
              aux links tail
          | [] | [_] -> acc
        in
        aux [] nfas
      in
      let last = List.hd (List.rev nfas) in
      {
        start = first.start;
        finals = last.finals;
        transitions = transitions @ epsilon_links;
        state_count = offset;
      }

let union_nfa nfas =
  match nfas with
  | [] -> empty_nfa
  | [nfa] -> nfa
  | _ ->
      let start = 0 in
      let offset, nfas =
        List.fold_left
          (fun (offset, acc) nfa ->
             let nfa = offset_nfa offset nfa in
             offset + nfa.state_count, nfa :: acc)
          (1, [])
          nfas
      in
      let nfas = List.rev nfas in
      {
        start;
        finals =
          List.fold_left
            (fun acc nfa -> StateSet.union acc nfa.finals)
            StateSet.empty
            nfas;
        transitions =
          List.concat_map (fun nfa -> nfa.transitions) nfas
          @ List.map (fun nfa -> { src = start; label = Eps; dst = nfa.start }) nfas;
        state_count = offset;
      }

let star_nfa nfa =
  let nfa = offset_nfa 1 nfa in
  let start = 0 in
  let loop_links =
    StateSet.fold
      (fun final acc ->
         { src = final; label = Eps; dst = nfa.start } :: acc)
      nfa.finals
      []
  in
  {
    start;
    finals = StateSet.add start nfa.finals;
    transitions = { src = start; label = Eps; dst = nfa.start } :: loop_links @ nfa.transitions;
    state_count = nfa.state_count + 1;
  }

let rec repeat_list item n =
  if n <= 0 then [] else item :: repeat_list item (n - 1)

let loop_nfa nfa lo hi =
  let required = repeat_list nfa lo in
  let optional = union_nfa [epsilon_nfa; nfa] in
  concat_nfa (required @ repeat_list optional (hi - lo))

let rec compile_nfa = function
  | Empty -> Ok empty_nfa
  | All -> compile_nfa (Star AllChar)
  | AllChar -> Ok (range_nfa scalar_intervals)
  | Lit text -> (
      match utf8_scalars text with
      | Error _ as error -> error
      | Ok scalars ->
          scalars
          |> List.map (fun code -> range_nfa [{ lo = code; hi = code }])
          |> concat_nfa
          |> fun nfa -> Ok nfa)
  | Range (lo, hi) ->
      if not (valid_scalar lo && valid_scalar hi) then
        Error "regex range endpoint is not a valid Unicode scalar"
      else if lo > hi then
        Error "regex range lower endpoint exceeds upper endpoint"
      else
        let intervals = intersect_intervals [{ lo; hi }] scalar_intervals in
        Ok (range_nfa intervals)
  | Concat regexes ->
      compile_list regexes |> Result.map concat_nfa
  | Union regexes ->
      compile_list regexes |> Result.map union_nfa
  | Star regex ->
      compile_nfa regex |> Result.map star_nfa
  | Inter _ | Comp _ ->
      Error "regex intersection/complement requires automata-level compilation"
  | Plus regex ->
      compile_nfa (Concat [regex; Star regex])
  | Opt regex ->
      compile_nfa (Union [Lit ""; regex])
  | Loop (regex, lo, hi) ->
      if lo < 0 || hi < lo then
        Error "invalid regex loop bounds"
      else
        compile_nfa regex |> Result.map (fun nfa -> loop_nfa nfa lo hi)

and compile_list regexes =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | regex :: rest -> (
        match compile_nfa regex with
        | Ok nfa -> aux (nfa :: acc) rest
        | Error _ as error -> error)
  in
  aux [] regexes

let state_transitions nfa state =
  List.filter (fun transition -> transition.src = state) nfa.transitions

let epsilon_closure nfa states =
  let rec loop seen work =
    match work with
    | [] -> seen
    | state :: rest ->
        let next =
          state_transitions nfa state
          |> List.filter_map (function
            | { label = Eps; dst; _ } when not (StateSet.mem dst seen) -> Some dst
            | _ -> None)
        in
        let seen =
          List.fold_left (fun acc state -> StateSet.add state acc) seen next
        in
        loop seen (next @ rest)
  in
  loop states (StateSet.elements states)

let range_step nfa states code =
  StateSet.fold
    (fun state acc ->
       state_transitions nfa state
       |> List.fold_left
         (fun acc transition ->
            match transition.label with
            | RangeLabel intervals when List.exists (interval_contains code) intervals ->
                StateSet.add transition.dst acc
            | Eps | RangeLabel _ -> acc)
         acc)
    states
    StateSet.empty

let step nfa states code =
  states
  |> epsilon_closure nfa
  |> fun states -> range_step nfa states code
  |> epsilon_closure nfa

let start_closure nfa =
  epsilon_closure nfa (StateSet.singleton nfa.start)

let accepts automaton input =
  match utf8_scalars input with
  | Error _ -> false
  | Ok scalars ->
      let states =
        List.fold_left
          (fun states code -> step automaton.nfa states code)
          (start_closure automaton.nfa)
          scalars
      in
      not (StateSet.is_empty (StateSet.inter states automaton.nfa.finals))

let outgoing_ranges nfa states =
  StateSet.fold
    (fun state acc ->
       state_transitions nfa state
       |> List.fold_left
         (fun acc transition ->
            match transition.label with
            | Eps -> acc
            | RangeLabel intervals -> intervals @ acc)
         acc)
    states
    []

let partition_intervals intervals =
  let intervals =
    intervals
    |> List.filter (fun interval -> interval.lo <= interval.hi)
    |> List.sort compare_interval
  in
  let cuts =
    intervals
    |> List.fold_left
      (fun acc interval ->
         let acc = interval.lo :: acc in
         if interval.hi < unicode_max then interval.hi + 1 :: acc else acc)
      []
    |> List.sort_uniq compare
  in
  let rec cells acc = function
    | lo :: (hi :: _ as rest) ->
        let cell = { lo; hi = hi - 1 } in
        cells (cell :: acc) rest
    | [lo] ->
        let max_hi =
          intervals
          |> List.filter (fun interval -> interval.lo <= lo && lo <= interval.hi)
          |> List.fold_left (fun acc interval -> max acc interval.hi) lo
        in
        List.rev ({ lo; hi = max_hi } :: acc)
    | [] -> List.rev acc
  in
  cells [] cuts
  |> List.filter (fun cell ->
       List.exists
         (fun interval -> interval.lo <= cell.lo && cell.hi <= interval.hi)
         intervals)

let cut_after code =
  if code >= unicode_max then None else Some (code + 1)

let full_alphabet_partition intervals =
  let add_interval_cuts acc interval =
    let acc = interval.lo :: acc in
    match cut_after interval.hi with
    | None -> acc
    | Some cut -> cut :: acc
  in
  let cuts =
    List.fold_left add_interval_cuts [] (scalar_intervals @ intervals)
    |> List.sort_uniq compare
  in
  let rec cells acc = function
    | lo :: (next :: _ as rest) ->
        let raw = { lo; hi = next - 1 } in
        let parts = intersect_intervals [raw] scalar_intervals in
        cells (List.rev_append parts acc) rest
    | [lo] ->
        let raw = { lo; hi = unicode_max } in
        let parts = intersect_intervals [raw] scalar_intervals in
        List.rev_append parts acc |> List.rev
    | [] -> List.rev acc
  in
  cells [] cuts
  |> List.filter (fun interval -> interval.lo <= interval.hi)

let next_symbol_choices nfa states =
  outgoing_ranges nfa states
  |> partition_intervals
  |> List.filter_map (fun interval ->
       representative_scalar [interval]
       |> Option.map (fun code -> code, interval))
  |> List.sort (fun (lhs, _) (rhs, _) -> compare lhs rhs)

let intervals_without_scalar interval scalar =
  if scalar < interval.lo || scalar > interval.hi then
    [interval]
  else
    [
      { lo = interval.lo; hi = scalar - 1 };
      { lo = scalar + 1; hi = interval.hi };
    ]
    |> List.filter (fun interval -> interval.lo <= interval.hi)

let cost_symbol_choices nfa states scalar =
  let choices_for_interval interval =
    let target_choice =
      if interval_contains scalar interval then [scalar, interval] else []
    in
    let non_target_choice =
      intervals_without_scalar interval scalar
      |> List.find_map (fun interval ->
             representative_scalar [interval]
             |> Option.map (fun code -> code, interval))
      |> Option.to_list
    in
    target_choice @ non_target_choice
  in
  outgoing_ranges nfa states
  |> partition_intervals
  |> List.concat_map choices_for_interval
  |> List.sort_uniq (fun (lhs, _) (rhs, _) -> compare lhs rhs)

let state_key states =
  StateSet.elements states

let witness_of_length automaton length =
  if length < 0 then None
  else
    let nfa = automaton.nfa in
    let rec bfs seen queue =
      match queue with
      | [] -> None
      | (states, depth, scalars) :: rest ->
          let key = state_key states, depth in
          if ConfigSet.mem key seen then
            bfs seen rest
          else
            let seen = ConfigSet.add key seen in
            if depth = length then
              if StateSet.is_empty (StateSet.inter states nfa.finals) then
                bfs seen rest
              else
                Some (string_of_scalars (List.rev scalars))
            else
              let next =
                next_symbol_choices nfa states
                |> List.filter_map (fun (code, _) ->
                     let states = step nfa states code in
                     if StateSet.is_empty states then None
                     else Some (states, depth + 1, code :: scalars))
              in
              bfs seen (rest @ next)
    in
    bfs ConfigSet.empty [start_closure nfa, 0, []]

let has_length automaton length =
  Option.is_some (witness_of_length automaton length)

let witness_of_length_with_scalar_count automaton ~length ~scalar ~count =
  if length < 0 || count < 0 || count > length || not (valid_scalar scalar) then
    None
  else
    let nfa = automaton.nfa in
    let rec bfs seen queue =
      match queue with
      | [] -> None
      | (states, depth, scalar_count, scalars) :: rest ->
          let key = state_key states, depth, scalar_count in
          if CostConfigSet.mem key seen then
            bfs seen rest
          else
            let seen = CostConfigSet.add key seen in
            if depth = length then
              if scalar_count = count
                 && not (StateSet.is_empty (StateSet.inter states nfa.finals))
              then
                Some (string_of_scalars (List.rev scalars))
              else
                bfs seen rest
            else
              let remaining = length - depth in
              let min_possible = scalar_count in
              let max_possible = scalar_count + remaining in
              if count < min_possible || count > max_possible then
                bfs seen rest
              else
                let next =
                  cost_symbol_choices nfa states scalar
                  |> List.filter_map (fun (code, _) ->
                         let next_count =
                           scalar_count + if code = scalar then 1 else 0
                         in
                         if next_count > count then
                           None
                         else
                           let states = step nfa states code in
                           if StateSet.is_empty states then None
                           else Some (states, depth + 1, next_count, code :: scalars))
                in
                bfs seen (rest @ next)
    in
    bfs CostConfigSet.empty [start_closure nfa, 0, 0, []]

let witness automaton =
  let rec loop length =
    if length > automaton.nfa.state_count then None
    else
      match witness_of_length automaton length with
      | Some _ as result -> result
      | None -> loop (length + 1)
  in
  loop 0

let is_empty automaton =
  Option.is_none (witness automaton)

let intersect lhs rhs =
  let lhs = lhs.nfa in
  let rhs = rhs.nfa in
  let pair_id =
    let table = Hashtbl.create 32 in
    let next = ref 0 in
    fun pair ->
      match Hashtbl.find_opt table pair with
      | Some id -> id
      | None ->
          let id = !next in
          incr next;
          Hashtbl.add table pair id;
          id
  in
  let add_pair pair queue seen =
    if PairSet.mem pair seen then queue, seen
    else pair :: queue, PairSet.add pair seen
  in
  let rec explore queue seen transitions finals =
    match queue with
    | [] ->
        let state_count =
          PairSet.fold (fun pair acc -> max acc (pair_id pair + 1)) seen 0
        in
        Ok (make {
            start = pair_id (lhs.start, rhs.start);
            finals;
            transitions;
            state_count;
          })
    | ((lstate, rstate) as pair) :: rest ->
        let id = pair_id pair in
        let finals =
          if StateSet.mem lstate lhs.finals && StateSet.mem rstate rhs.finals then
            StateSet.add id finals
          else
            finals
        in
        let ltrans = state_transitions lhs lstate in
        let rtrans = state_transitions rhs rstate in
        let queue, seen, transitions =
          let queue, seen, transitions =
            List.fold_left
              (fun (queue, seen, transitions) transition ->
                 match transition.label with
                 | Eps ->
                     let next_pair = transition.dst, rstate in
                     let queue, seen = add_pair next_pair queue seen in
                     queue, seen,
                     { src = id; label = Eps; dst = pair_id next_pair } :: transitions
                 | RangeLabel _ -> queue, seen, transitions)
              (rest, seen, transitions)
              ltrans
          in
          List.fold_left
            (fun (queue, seen, transitions) transition ->
               match transition.label with
               | Eps ->
                   let next_pair = lstate, transition.dst in
                   let queue, seen = add_pair next_pair queue seen in
                   queue, seen,
                   { src = id; label = Eps; dst = pair_id next_pair } :: transitions
               | RangeLabel _ -> queue, seen, transitions)
            (queue, seen, transitions)
            rtrans
        in
        let queue, seen, transitions =
          List.fold_left
            (fun acc ltransition ->
               match ltransition.label with
               | Eps -> acc
               | RangeLabel _ ->
                   List.fold_left
                     (fun (queue, seen, transitions) rtransition ->
                        match label_intersection ltransition.label rtransition.label with
                        | None | Some Eps -> queue, seen, transitions
                        | Some label ->
                            let next_pair = ltransition.dst, rtransition.dst in
                            let queue, seen = add_pair next_pair queue seen in
                            queue, seen,
                            { src = id; label; dst = pair_id next_pair } :: transitions)
                     acc
                     rtrans)
            (queue, seen, transitions)
            ltrans
        in
        explore queue seen transitions finals
  in
  let start_pair = lhs.start, rhs.start in
  explore [start_pair] (PairSet.singleton start_pair) [] StateSet.empty

let automata_state_limit () =
  match Sys.getenv_opt "YICES_STRING_REGEX_AUTOMATA_STATE_LIMIT" with
  | None -> 10000
  | Some raw -> (
      match int_of_string_opt raw with
      | Some n when n >= 0 -> n
      | _ -> 10000)

let check_state_limit limit count =
  if limit > 0 && count > limit then
    Error
      (Printf.sprintf
         "regex automata state limit exceeded (%d > %d)"
         count
         limit)
  else
    Ok ()

let determinize ?(complement = false) automaton =
  let nfa = automaton.nfa in
  let limit = automata_state_limit () in
  let subset_key states = StateSet.elements states in
  let original_accepting states =
    not (StateSet.is_empty (StateSet.inter states nfa.finals))
  in
  let dfa_accepting states =
    if complement then not (original_accepting states) else original_accepting states
  in
  let state_id =
    let next = ref 0 in
    fun table states ->
      let key = subset_key states in
      match StateListMap.find_opt key !table with
      | Some id -> Ok (id, false)
      | None ->
          let id = !next in
          incr next;
          begin
            match check_state_limit limit !next with
            | Error _ as err -> err
            | Ok () ->
                table := StateListMap.add key id !table;
                Ok (id, true)
          end
  in
  let table = ref StateListMap.empty in
  let start = start_closure nfa in
  match state_id table start with
  | Error _ as err -> err
  | Ok (start_id, _) ->
      let rec explore queue transitions finals =
        match queue with
        | [] ->
            Ok (make {
                start = start_id;
                finals;
                transitions = List.rev transitions;
                state_count = StateListMap.cardinal !table;
              })
        | states :: rest -> (
            match state_id table states with
            | Error _ as err -> err
            | Ok (src, _) ->
                let finals =
                  if dfa_accepting states then StateSet.add src finals else finals
                in
                let cells = full_alphabet_partition (outgoing_ranges nfa states) in
                let add_cell result cell =
                  match result with
                  | Error _ as err -> err
                  | Ok (queue, transitions) -> (
                      match representative_scalar [cell] with
                      | None -> Ok (queue, transitions)
                      | Some code -> (
                          let dst_states = step nfa states code in
                          match state_id table dst_states with
                          | Error _ as err -> err
                          | Ok (dst, is_new) ->
                              let queue =
                                if is_new then queue @ [dst_states] else queue
                              in
                              Ok
                                ( queue,
                                  {
                                    src;
                                    label = RangeLabel [cell];
                                    dst;
                                  }
                                  :: transitions )))
                in
                match List.fold_left add_cell (Ok (rest, transitions)) cells with
                | Error _ as err -> err
                | Ok (queue, transitions) -> explore queue transitions finals)
      in
      explore [start] [] StateSet.empty

let complement automaton =
  determinize ~complement:true automaton

let difference lhs rhs =
  match complement rhs with
  | Error _ as err -> err
  | Ok complement_rhs -> intersect lhs complement_rhs

let finite_union lhs rhs =
  match lhs, rhs with
  | Length_empty, other | other, Length_empty -> other
  | Length_finite lhs, Length_finite rhs ->
      Length_finite (List.sort_uniq compare (lhs @ rhs))
  | Length_top, _ | _, Length_top -> Length_top
  | Length_periodic _, _ | _, Length_periodic _ -> Length_top

let finite_sum lhs rhs =
  match lhs, rhs with
  | Length_empty, _ | _, Length_empty -> Length_empty
  | Length_finite lhs, Length_finite rhs ->
      lhs
      |> List.concat_map (fun l -> List.map (fun r -> l + r) rhs)
      |> List.sort_uniq compare
      |> fun xs -> Length_finite xs
  | Length_top, _ | _, Length_top -> Length_top
  | Length_periodic _, _ | _, Length_periodic _ -> Length_top

let rec repeat_length_domain domain n =
  if n = 0 then
    Length_finite [0]
  else
    finite_sum domain (repeat_length_domain domain (n - 1))

let loop_length_domain domain lo hi =
  if lo < 0 || hi < lo then
    Length_empty
  else
    let rec loop acc n =
      if n > hi then acc
      else loop (finite_union acc (repeat_length_domain domain n)) (n + 1)
    in
    loop Length_empty lo

let rec int_gcd a b =
  let a = abs a in
  let b = abs b in
  if b = 0 then a else int_gcd b (a mod b)

let gcd_list = function
  | [] -> 0
  | first :: rest -> List.fold_left int_gcd first rest

let semilinear_star_domain lengths =
  let positive =
    lengths
    |> List.filter (fun n -> n > 0)
    |> List.sort_uniq compare
  in
  match positive with
  | [] -> Length_finite [0]
  | [n] -> Length_periodic { base = []; threshold = 0; period = n }
  | lengths ->
      let period = gcd_list lengths in
      if period <= 0 then
        Length_top
      else
        let scaled = List.map (fun n -> n / period) lengths in
        let min_step = List.fold_left min max_int scaled in
        let limit =
          match Sys.getenv_opt "YICES_STRING_REGEX_LENGTH_DOMAIN_LIMIT" with
          | None -> 200
          | Some raw -> (
              match int_of_string_opt raw with
              | Some n when n >= 0 -> n
              | _ -> 200)
        in
        let reachable = Array.make (limit + min_step + 1) false in
        reachable.(0) <- true;
        for i = 0 to Array.length reachable - 1 do
          if reachable.(i) then
            List.iter
              (fun step ->
                 let next = i + step in
                 if next < Array.length reachable then reachable.(next) <- true)
              scaled
        done;
        let rec find_threshold i =
          if i + min_step - 1 >= Array.length reachable || i > limit then
            None
          else
            let rec window offset =
              offset = min_step || (reachable.(i + offset) && window (offset + 1))
            in
            if window 0 then Some i else find_threshold (i + 1)
        in
        match find_threshold 0 with
        | None -> Length_top
        | Some threshold ->
            let base =
              let rec collect acc i =
                if i >= threshold then List.rev acc
                else if reachable.(i) then collect ((i * period) :: acc) (i + 1)
                else collect acc (i + 1)
              in
              collect [] 0
            in
            Length_periodic
              {
                base;
                threshold = threshold * period;
                period;
              }

let rec regex_length_domain = function
  | Empty -> Length_empty
  | All -> Length_top
  | AllChar | Range _ -> Length_finite [1]
  | Lit text -> (
      match scalar_length text with
      | Ok length -> Length_finite [length]
      | Error _ -> Length_empty)
  | Union regexes ->
      List.fold_left
        (fun acc regex -> finite_union acc (regex_length_domain regex))
        Length_empty
        regexes
  | Concat regexes ->
      List.fold_left
        (fun acc regex -> finite_sum acc (regex_length_domain regex))
        (Length_finite [0])
        regexes
  | Star regex -> (
      match regex_length_domain regex with
      | Length_empty | Length_finite [] | Length_finite [0] -> Length_finite [0]
      | Length_finite lengths -> semilinear_star_domain lengths
      | Length_periodic _ | Length_top -> Length_top)
  | Inter _ | Comp _ -> Length_top
  | Plus regex ->
      finite_sum (regex_length_domain regex) (regex_length_domain (Star regex))
  | Opt regex ->
      finite_union (Length_finite [0]) (regex_length_domain regex)
  | Loop (regex, lo, hi) ->
      loop_length_domain (regex_length_domain regex) lo hi

let transition_targets nfa state =
  state_transitions nfa state
  |> List.map (fun transition -> transition.dst)

let reachable_states nfa =
  let rec loop seen = function
    | [] -> seen
    | state :: rest ->
        if StateSet.mem state seen then
          loop seen rest
        else
          loop
            (StateSet.add state seen)
            (transition_targets nfa state @ rest)
  in
  loop StateSet.empty [nfa.start]

let co_reachable_states nfa =
  let reverse =
    List.fold_left
      (fun acc transition ->
         let old = Hashtbl.find_opt acc transition.dst |> Option.value ~default:[] in
         Hashtbl.replace acc transition.dst (transition.src :: old);
         acc)
      (Hashtbl.create nfa.state_count)
      nfa.transitions
  in
  let rec loop seen = function
    | [] -> seen
    | state :: rest ->
        if StateSet.mem state seen then
          loop seen rest
        else
          let predecessors = Hashtbl.find_opt reverse state |> Option.value ~default:[] in
          loop (StateSet.add state seen) (predecessors @ rest)
  in
  loop StateSet.empty (StateSet.elements nfa.finals)

let useful_states nfa =
  StateSet.inter (reachable_states nfa) (co_reachable_states nfa)

let useful_consuming_cycle nfa useful =
  let index = ref 0 in
  let stack = ref [] in
  let indices = Hashtbl.create nfa.state_count in
  let lowlinks = Hashtbl.create nfa.state_count in
  let on_stack = Hashtbl.create nfa.state_count in
  let has_consuming_cycle = ref false in
  let successors state =
    state_transitions nfa state
    |> List.filter_map (fun transition ->
         if StateSet.mem transition.dst useful then Some transition.dst else None)
  in
  let rec strongconnect state =
    Hashtbl.add indices state !index;
    Hashtbl.add lowlinks state !index;
    incr index;
    stack := state :: !stack;
    Hashtbl.replace on_stack state true;
    List.iter
      (fun succ ->
         if not (Hashtbl.mem indices succ) then begin
           strongconnect succ;
           let low_state = Hashtbl.find lowlinks state in
           let low_succ = Hashtbl.find lowlinks succ in
           Hashtbl.replace lowlinks state (min low_state low_succ)
         end else if Hashtbl.find_opt on_stack succ = Some true then begin
           let low_state = Hashtbl.find lowlinks state in
           let index_succ = Hashtbl.find indices succ in
           Hashtbl.replace lowlinks state (min low_state index_succ)
         end)
      (successors state);
    if Hashtbl.find lowlinks state = Hashtbl.find indices state then begin
      let rec pop acc =
        match !stack with
        | [] -> acc
        | top :: rest ->
            stack := rest;
            Hashtbl.replace on_stack top false;
            let acc = top :: acc in
            if top = state then acc else pop acc
      in
      let component = pop [] in
      let component_set =
        List.fold_left
          (fun acc state -> StateSet.add state acc)
          StateSet.empty
          component
      in
      let internal_consuming =
        List.exists
          (fun state ->
             state_transitions nfa state
             |> List.exists (fun transition ->
                    StateSet.mem transition.dst component_set
                    &&
                    match transition.label with
                    | RangeLabel _ -> true
                    | Eps -> false))
          component
      in
      if internal_consuming then has_consuming_cycle := true
    end
  in
  StateSet.iter
    (fun state ->
       if not !has_consuming_cycle && not (Hashtbl.mem indices state) then
         strongconnect state)
    useful;
  !has_consuming_cycle

let transition_length = function
  | Eps -> 0
  | RangeLabel _ -> 1

let finite_lengths_for_acyclic_automaton nfa useful =
  let max_length = StateSet.cardinal useful in
  let rec loop seen lengths = function
    | [] -> Some (StateSet.elements lengths)
    | (state, length) :: rest ->
        if length > max_length then
          None
        else if ConfigSet.mem ([state], length) seen then
          loop seen lengths rest
        else
          let seen = ConfigSet.add ([state], length) seen in
          let lengths =
            if StateSet.mem state nfa.finals then StateSet.add length lengths
            else lengths
          in
          let next =
            state_transitions nfa state
            |> List.filter_map (fun transition ->
                 if not (StateSet.mem transition.dst useful) then
                   None
                 else
                   match transition.label with
                   | Eps -> Some (transition.dst, length)
                   | RangeLabel _ -> Some (transition.dst, length + 1))
          in
          loop seen lengths (rest @ next)
  in
  loop ConfigSet.empty StateSet.empty [nfa.start, 0]

let min_accepted_length nfa useful =
  let distances = Array.make nfa.state_count max_int in
  let rec loop = function
    | [] -> ()
    | state :: rest ->
        let distance = distances.(state) in
        let rest =
          state_transitions nfa state
          |> List.fold_left
            (fun rest transition ->
               if not (StateSet.mem transition.dst useful) then
                 rest
               else
                 let next_distance = distance + transition_length transition.label in
                 if next_distance < distances.(transition.dst) then begin
                   distances.(transition.dst) <- next_distance;
                   transition.dst :: rest
                 end else
                   rest)
            rest
        in
        loop rest
  in
  distances.(nfa.start) <- 0;
  loop [nfa.start];
  StateSet.fold
    (fun final acc ->
       let distance = distances.(final) in
       if distance = max_int then acc else min acc distance)
    nfa.finals
    max_int
  |> fun min_length ->
  if min_length = max_int then None else Some min_length

let strongly_connected_components nfa useful =
  let index = ref 0 in
  let stack = ref [] in
  let indices = Hashtbl.create nfa.state_count in
  let lowlinks = Hashtbl.create nfa.state_count in
  let on_stack = Hashtbl.create nfa.state_count in
  let components = ref [] in
  let successors state =
    state_transitions nfa state
    |> List.filter_map (fun transition ->
         if StateSet.mem transition.dst useful then Some transition.dst else None)
  in
  let rec strongconnect state =
    Hashtbl.add indices state !index;
    Hashtbl.add lowlinks state !index;
    incr index;
    stack := state :: !stack;
    Hashtbl.replace on_stack state true;
    List.iter
      (fun succ ->
         if not (Hashtbl.mem indices succ) then begin
           strongconnect succ;
           let low_state = Hashtbl.find lowlinks state in
           let low_succ = Hashtbl.find lowlinks succ in
           Hashtbl.replace lowlinks state (min low_state low_succ)
         end else if Hashtbl.find_opt on_stack succ = Some true then begin
           let low_state = Hashtbl.find lowlinks state in
           let index_succ = Hashtbl.find indices succ in
           Hashtbl.replace lowlinks state (min low_state index_succ)
         end)
      (successors state);
    if Hashtbl.find lowlinks state = Hashtbl.find indices state then begin
      let rec pop acc =
        match !stack with
        | [] -> acc
        | top :: rest ->
            stack := rest;
            Hashtbl.replace on_stack top false;
            let acc = top :: acc in
            if top = state then acc else pop acc
      in
      components := pop [] :: !components
    end
  in
  StateSet.iter
    (fun state ->
       if not (Hashtbl.mem indices state) then strongconnect state)
    useful;
  !components

let component_period nfa component =
  let component_set =
    List.fold_left
      (fun acc state -> StateSet.add state acc)
      StateSet.empty
      component
  in
  let distances = Hashtbl.create (List.length component) in
  let period = ref 0 in
  let rec visit state =
    let state_distance = Hashtbl.find distances state in
    state_transitions nfa state
    |> List.iter (fun transition ->
           if StateSet.mem transition.dst component_set then begin
             let edge_length = transition_length transition.label in
             match Hashtbl.find_opt distances transition.dst with
             | None ->
                 Hashtbl.add distances transition.dst (state_distance + edge_length);
                 visit transition.dst
             | Some dst_distance ->
                 period := int_gcd !period (state_distance + edge_length - dst_distance)
           end)
  in
  List.iter
    (fun state ->
       if not (Hashtbl.mem distances state) then begin
         Hashtbl.add distances state 0;
         visit state
       end)
    component;
  !period

let automaton_cycle_period nfa useful =
  strongly_connected_components nfa useful
  |> List.map (component_period nfa)
  |> List.filter (fun period -> period > 0)
  |> gcd_list

let accepted_residues nfa useful period =
  let module ResidueSet = Set.Make(struct
      type t = int * int
      let compare = compare
    end)
  in
  let rec loop seen residues = function
    | [] -> residues
    | (state, residue) :: rest ->
        if ResidueSet.mem (state, residue) seen then
          loop seen residues rest
        else
          let seen = ResidueSet.add (state, residue) seen in
          let residues =
            if StateSet.mem state nfa.finals then IntSet.add residue residues
            else residues
          in
          let next =
            state_transitions nfa state
            |> List.filter_map (fun transition ->
                 if not (StateSet.mem transition.dst useful) then
                   None
                 else
                   let residue =
                     (residue + transition_length transition.label) mod period
                   in
                   Some (transition.dst, residue))
          in
          loop seen residues (rest @ next)
  in
  loop ResidueSet.empty IntSet.empty [nfa.start, 0]

let automaton_length_domain automaton =
  let nfa = automaton.nfa in
  let useful = useful_states nfa in
  if StateSet.is_empty useful then
    Length_empty
  else if useful_consuming_cycle nfa useful then
    match min_accepted_length nfa useful with
    | None -> Length_empty
    | Some threshold ->
        let period = automaton_cycle_period nfa useful in
        if period > 1 && IntSet.cardinal (accepted_residues nfa useful period) = 1 then
          Length_periodic { base = []; threshold; period }
        else
          Length_periodic { base = []; threshold; period = 1 }
  else
    match finite_lengths_for_acyclic_automaton nfa useful with
    | Some lengths -> Length_finite lengths
    | None -> Length_top

let rec compile_many regexes =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | regex :: rest -> (
        match compile regex with
        | Ok automaton -> aux (automaton :: acc) rest
        | Error _ as error -> error)
  in
  aux [] regexes

and compile_intersection = function
  | [] -> compile All
  | first :: rest -> (
      match compile first with
      | Error _ as error -> error
      | Ok first ->
          List.fold_left
            (fun result regex ->
               match result with
               | Error _ as error -> error
               | Ok automaton -> (
                   match compile regex with
                   | Error _ as error -> error
                   | Ok next -> intersect automaton next))
            (Ok first)
            rest)

and compile regex =
  match regex with
  | Empty -> Ok (make ~source:regex empty_nfa)
  | All -> Ok (make ~source:regex (star_nfa (range_nfa scalar_intervals)))
  | AllChar -> Ok (make ~source:regex (range_nfa scalar_intervals))
  | Lit _ | Range _ -> compile_nfa regex |> Result.map (make ~source:regex)
  | Concat regexes ->
      compile_many regexes
      |> Result.map (fun automata ->
             automata
             |> List.map (fun automaton -> automaton.nfa)
             |> concat_nfa
             |> make ~source:regex)
  | Union regexes ->
      compile_many regexes
      |> Result.map (fun automata ->
             automata
             |> List.map (fun automaton -> automaton.nfa)
             |> union_nfa
             |> make ~source:regex)
  | Star body -> (
      match compile body with
      | Error _ as error -> error
      | Ok automaton -> Ok (make ~source:regex (star_nfa automaton.nfa)))
  | Inter regexes ->
      compile_intersection regexes
  | Comp body -> (
      match compile body with
      | Error _ as error -> error
      | Ok automaton -> complement automaton)
  | Plus body ->
      compile (Concat [body; Star body])
      |> Result.map (fun automaton ->
             { automaton with source = Some regex; length_domain_cache = None })
  | Opt body ->
      compile (Union [Lit ""; body])
      |> Result.map (fun automaton ->
             { automaton with source = Some regex; length_domain_cache = None })
  | Loop (body, lo, hi) ->
      if lo < 0 || hi < lo then
        Error "invalid regex loop bounds"
      else
        match compile body with
        | Error _ as error -> error
        | Ok automaton ->
            Ok (make ~source:regex (loop_nfa automaton.nfa lo hi))

let exact text =
  compile (Lit text)

let prefix text =
  compile (Concat [Lit text; All])

let suffix text =
  compile (Concat [All; Lit text])

let contains text =
  compile (Concat [All; Lit text; All])

let fixed_position ~index ~scalar =
  if index < 0 then
    Error "fixed-position index is negative"
  else if not (valid_scalar scalar) then
    Error "fixed-position scalar is not a valid Unicode scalar"
  else
    let rec repeat acc n =
      if n = 0 then acc else repeat (AllChar :: acc) (n - 1)
    in
    compile (Concat (List.rev (repeat [] index) @ [Range (scalar, scalar); All]))

let fixed_position_regex ~index regex =
  if index < 0 then
    Error "fixed-position index is negative"
  else
    let rec repeat acc n =
      if n = 0 then List.rev acc else repeat (AllChar :: acc) (n - 1)
    in
    match compile regex with
    | Error _ as err -> err
    | Ok automaton ->
        let one_char = Inter [regex; AllChar] in
        let fixed = Concat (repeat [] index @ [one_char; All]) in
        let source_regex =
          if accepts automaton "" then
            Union [Loop (AllChar, 0, index); fixed]
          else
            fixed
        in
        compile source_regex

let length_domain automaton =
  match automaton.length_domain_cache with
  | Some domain -> domain
  | None ->
      let domain =
        match automaton.source with
        | Some regex -> regex_length_domain regex
        | None -> automaton_length_domain automaton
      in
      automaton.length_domain_cache <- Some domain;
      domain
