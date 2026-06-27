type t =
  | Replace_all of {
      needle : string;
      replacement : string;
    }

let replace_all ~needle ~replacement =
  Replace_all { needle; replacement }

let preimage transducer automaton =
  match transducer with
  | Replace_all { needle; replacement } ->
      Regex_automata.replace_all_preimage ~needle ~replacement automaton
