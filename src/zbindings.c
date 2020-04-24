#include <stdlib.h>
#include <stdint.h>
#include <gmp.h>
#include <zarith.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* sets rop to the value in op (limbs are copied) */
CAMLprim value ml_z_mpz_set_z_ml(value rop, value op) {
  CAMLparam2(rop, op);
  mpz_ptr z = (mpz_ptr) (Data_custom_val(rop));
  /* void ml_z_mpz_set_z(mpz_t rop, value op); */
  ml_z_mpz_set_z(z, op);
  CAMLreturn(Val_unit);
}

/* inits and sets rop to the value in op (limbs are copied) */
CAMLprim value ml_z_mpz_init_set_z_ml(value rop, value op) {
  CAMLparam2(rop, op);
  mpz_ptr z = (mpz_ptr) (Data_custom_val(rop));
  /* void ml_z_mpz_init_set_z(mpz_t rop, value op); */
  ml_z_mpz_init_set_z(z, op);
  CAMLreturn(Val_unit);
}

/* returns a new z objects equal to op (limbs are copied) */
CAMLprim value ml_z_from_mpz_ml(value rop) {
  CAMLparam1(rop);
  mpz_ptr z = (mpz_ptr) (Data_custom_val(rop));
  /* value ml_z_from_mpz(mpz_t op); */
  CAMLreturn(ml_z_from_mpz(z));
}
