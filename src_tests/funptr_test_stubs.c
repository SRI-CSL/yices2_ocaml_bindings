#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

static void yices_test_out_of_mem_callback(void) {
  /* no-op: used only for testing registration via a C callback */
}

CAMLprim value caml_yices_test_out_of_mem_callback_addr(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_nativeint((intnat)&yices_test_out_of_mem_callback));
}
