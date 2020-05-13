open Yices2_low_types

module type API  = API
module BaseTypes = BaseTypes

include API with type 'a checkable = 'a

