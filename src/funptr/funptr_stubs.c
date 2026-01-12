#include <caml/mlvalues.h>
#include <ctypes_cstubs_internals.h>
#include <ctypes_raw_pointer.h>
#include <yices.h>

CAMLprim value caml_yices_set_out_of_mem_callback(value cb) {
  yices_set_out_of_mem_callback((void (*)(void))CTYPES_ADDR_OF_FATPTR(cb));
  return Val_unit;
}
