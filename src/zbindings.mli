open Gmp
  
val ml_z_mpz_set_z_ml : mpz_ptr -> Z.t -> unit
  (* = external "ml_z_mpz_set_z_ml" *)

val ml_z_mpz_init_set_z_ml : mpz_ptr -> Z.t -> unit
  (* = external "ml_z_mpz_init_set_z_ml" *)

val ml_z_from_mpz_ml : mpz_ptr -> Z.t 
  (* = external "ml_z_from_mpz_ml" *)
