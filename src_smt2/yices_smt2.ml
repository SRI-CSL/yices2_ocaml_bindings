open Containers

open Yices2.Ext

module SMT2 = Yices2.SMT2.Make(WithExceptionsErrorHandling)
open SMT2

let args = ref []
let description = "Executable for solving smt2 files. One filename as argument.";;

Arg.parse [] (fun a->args := a::!args) description;;

match !args with
| [filename] ->
  (try
     SMT2.process_file filename
  with
    ExceptionsErrorHandling.YicesException(_,report) as exc ->
    let bt = Printexc.get_backtrace() in
    Format.(fprintf stderr) "@[<v>%a@]%!" Types.pp_error_report report;
    Format.(fprintf stderr) "@[<v>%s@]%!" bt;
    raise exc
 )
| [] -> failwith "Too few arguments in the command"
| _ -> failwith "Too many arguments in the command";;
