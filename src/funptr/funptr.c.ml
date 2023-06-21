let%c () = header {|
                   #include <yices.h>
                   |}

external yices_set_out_of_mem_callback
         : (void -> void) static_funptr -> void
  =
  "yices_set_out_of_mem_callback"

