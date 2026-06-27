type level =
  | Error
  | Warn
  | Info
  | Debug
  | Trace

let rank = function
  | Error -> 0
  | Warn -> 1
  | Info -> 2
  | Debug -> 3
  | Trace -> 4

let level_of_string = function
  | "ERROR" | "error" -> Some Error
  | "WARN" | "warn" -> Some Warn
  | "INFO" | "info" -> Some Info
  | "DEBUG" | "debug" -> Some Debug
  | "TRACE" | "trace" -> Some Trace
  | _ -> None

let configured_level () =
  match Sys.getenv_opt "YICES_STRING_LOG_LEVEL" with
  | Some value ->
      begin
        match level_of_string value with
        | Some level -> level
        | None -> Error
      end
  | None -> Error

let enabled level =
  rank level <= rank (configured_level ())

let log level fmt =
  if enabled level then
    Format.kasprintf
      (fun msg -> Format.eprintf "[yices-string] %s@." msg)
      fmt
  else
    Format.ifprintf Format.err_formatter fmt

let error fmt = log Error fmt
let warn fmt = log Warn fmt
let info fmt = log Info fmt
let debug fmt = log Debug fmt
let trace fmt = log Trace fmt
