external test_out_of_mem_callback_addr : unit -> nativeint
  = "caml_yices_test_out_of_mem_callback_addr"

let test_out_of_mem_callback () =
  Ctypes.funptr_of_raw_address (test_out_of_mem_callback_addr ())
