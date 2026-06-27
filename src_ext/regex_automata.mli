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

type t

val scalar_length : string -> (int, string) result
val compile : regex -> (t, string) result
val exact : string -> (t, string) result
val prefix : string -> (t, string) result
val suffix : string -> (t, string) result
val contains : string -> (t, string) result
val fixed_position : index:int -> scalar:int -> (t, string) result
val fixed_position_regex : index:int -> regex -> (t, string) result
val accepts : t -> string -> bool
val intersect : t -> t -> (t, string) result
val left_quotient : t -> by:t -> (t, string) result
val right_quotient : t -> by:t -> (t, string) result
val replace_all_preimage : needle:string -> replacement:string -> t -> (t, string) result
val complement : t -> (t, string) result
val difference : t -> t -> (t, string) result
val is_empty : t -> bool
val witness : t -> string option
val witness_of_length : t -> int -> string option
val witness_of_length_with_scalar_count :
  t -> length:int -> scalar:int -> count:int -> string option
val has_length : t -> int -> bool
val length_domain : t -> length_domain
