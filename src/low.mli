(* Abbreviation for composition *)
val ( <.> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

include Bindings_types.Low with type 'a checkable = 'a

