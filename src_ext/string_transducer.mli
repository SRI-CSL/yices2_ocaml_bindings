type t

val replace_all : needle:string -> replacement:string -> t
val preimage : t -> Regex_automata.t -> (Regex_automata.t, string) result
