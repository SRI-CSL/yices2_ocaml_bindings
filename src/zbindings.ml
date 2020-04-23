open Gmp
  
external ml_z_mpz_set_z_ml : mpz_ptr -> Z.t -> unit
  = "ml_z_mpz_set_z_ml"

external ml_z_mpz_init_set_z_ml : mpz_ptr -> Z.t -> unit
  = "ml_z_mpz_init_set_z_ml"

external ml_z_from_mpz_ml : mpz_ptr -> Z.t 
  = "ml_z_from_mpz_ml"
