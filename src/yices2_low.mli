open Yices2_low_types

module type API  = API

include API with type 'a checkable = 'a

