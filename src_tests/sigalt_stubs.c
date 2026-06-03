#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <caml/mlvalues.h>

CAMLprim value caml_yices_sigalt_onstack(value unit)
{
  stack_t ss;
  (void)unit;
  if (sigaltstack(NULL, &ss) != 0) return Val_int(-1);
  return Val_int((ss.ss_flags & SS_ONSTACK) ? 1 : 0);
}

CAMLprim value caml_yices_raw_exit(value code)
{
  fflush(NULL);
  _exit(Int_val(code));
  return Val_unit;
}
