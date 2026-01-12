external set_out_of_mem_callback :
  _ Cstubs_internals.fatfunptr -> unit
  = "caml_yices_set_out_of_mem_callback"

let yices_set_out_of_mem_callback callback =
  let Ctypes_static.Static_funptr fn = callback in
  set_out_of_mem_callback fn
