open Yices2_high

module EH1 = Make(ExceptionsErrorHandling)

let test () =
  print_endline "Error tests";
  let open EH1 in
  let open Global in
  init();
  
  (* First with no error *)
  let errcode = Error.code() in
  assert(Types.equal_error_code errcode `NO_ERROR);
  let errep = Error.report() in
  assert(Types.equal_error_code errep.code `NO_ERROR);
  Error.clear();
  let errstr = ErrorPrint.string() in
  assert(String.equal errstr "no error");
  (* ErrorPrint.print_fd 1; *)

  (* Illegal - only scalar or uninterpreted types allowed *)
  let bool_t = Type.bool() in
  assert(match Type.reveal bool_t with Bool -> true | _ -> false);
  begin
    try
      let _const1 = Term.constant bool_t ~id:0 in
      assert false;
    with _ ->
      let error_string = ErrorPrint.string() in
      assert(String.equal error_string "invalid type in constant creation")
  end;
  Error.clear();
  assert(Types.equal_error_code (Error.code()) `NO_ERROR);
  let errpt = Error.report() in
  assert(Types.equal_error_code (errpt.code) `NO_ERROR);
  let errstr = ErrorPrint.string() in
  assert(String.equal errstr "no error");
  (* ErrorPrint.print_fd 1; *)
  Error.clear();
  assert(Types.equal_error_code (Error.code()) `NO_ERROR);
  print_endline "Done with Error tests";
  exit()
