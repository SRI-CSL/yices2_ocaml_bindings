open Yices2_high

module EH1 = Make(ExceptionsErrorHandling)

let test () =
  print_endline "Types tests";
  let open EH1 in
  let open Global in
  init();
  
  let bool_t = Type.bool() in
  let int_t  = Type.int() in
  assert(not Type.(equal bool_t int_t));
  let real_t = Type.real() in
  assert(not Type.(equal real_t bool_t));
  assert(not Type.(equal real_t int_t));
  let bv_t    = Type.bv 8 in
  let scal_t  = Type.new_scalar ~card:12 in
  let unint_t = Type.new_uninterpreted() in
  let tup1_t  = Type.tuple [bool_t] in
  let tup2_t  = Type.tuple [int_t; real_t] in
  let tup3_t  = Type.tuple [bv_t; scal_t; unint_t] in
  let ta4     = [bool_t; tup1_t; tup2_t; tup3_t] in
  let tup4_t  = Type.tuple ta4 in
  let fun1_t  = Type.func [int_t] bool_t in
  let _fun2_t  = Type.func [real_t; bv_t] scal_t in
  let fun3_t  = Type.func [tup1_t; tup2_t; tup3_t] fun1_t in
  let fun4_t  = Type.func ta4 fun3_t in

  assert(Type.is_bool bool_t);
  assert(not (Type.is_bool int_t));
  assert(Type.is_int int_t);
  assert(Type.is_real real_t);
  assert(Type.is_arithmetic real_t);
  assert(Type.is_bitvector bv_t);
  assert(Type.is_tuple tup1_t);
  assert(Type.is_function fun4_t);
  assert(Type.is_scalar scal_t);
  assert(Type.is_uninterpreted unint_t);
  assert(Type.test_subtype int_t real_t);
  assert(not (Type.test_subtype real_t int_t));
  assert(Types.equal_ytype (Type.reveal bv_t) (BV 8));
  assert(Type.scalar_card scal_t = 12);
  assert(Type.num_children(tup3_t) = 3);
  assert(Type.child tup3_t 1 = scal_t);
  let type_v = Type.children tup4_t in
  assert(List.length type_v = 4);
  assert(List.nth type_v 0 = bool_t);
  assert(List.nth type_v 1 = tup1_t);
  assert(List.nth type_v 2 = tup2_t);
  assert(List.nth type_v 3 = tup3_t);
  print_endline "Done with Types tests";
  exit()
