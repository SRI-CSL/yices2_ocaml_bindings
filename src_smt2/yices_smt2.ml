open Containers
open Arg

open Yices2_high
open Yices2_SMT2
open Bindings

let args = ref []
let description = "Executable for solving smt2 files. One filename as argument.";;

Arg.parse [] (fun a->args := a::!args) description;;

match !args with
| [filename] ->
  (try
     SMT2.process_file filename
  with
    ExceptionsErrorHandling.YicesException(_,report) as exc
    ->
    Format.(fprintf stderr) "@[<v>%a@]%!" Types.pp_error_report report;
    raise exc
 )
| [] -> failwith "Too few arguments in the command"
| _ -> failwith "Too many arguments in the command";;
