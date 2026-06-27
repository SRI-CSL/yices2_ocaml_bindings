let literal_tag = "__yices_smt2_string_hex"

let error fmt =
  Format.ksprintf (fun msg -> Error msg) fmt

let hex_value = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let add_utf8_scalar buf start scalar =
  if scalar < 0 || scalar > 0x10FFFF then
    error "invalid Unicode scalar at position %d: U+%X exceeds 0x10FFFF" start scalar
  else if scalar >= 0xD800 && scalar <= 0xDFFF then
    error "invalid Unicode scalar at position %d: U+%X is a surrogate code point" start scalar
  else begin
    if scalar <= 0x7F then
      Buffer.add_char buf (Char.chr scalar)
    else if scalar <= 0x7FF then begin
      Buffer.add_char buf (Char.chr (0xC0 lor (scalar lsr 6)));
      Buffer.add_char buf (Char.chr (0x80 lor (scalar land 0x3F)))
    end else if scalar <= 0xFFFF then begin
      Buffer.add_char buf (Char.chr (0xE0 lor (scalar lsr 12)));
      Buffer.add_char buf (Char.chr (0x80 lor ((scalar lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (scalar land 0x3F)))
    end else begin
      Buffer.add_char buf (Char.chr (0xF0 lor (scalar lsr 18)));
      Buffer.add_char buf (Char.chr (0x80 lor ((scalar lsr 12) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor ((scalar lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (scalar land 0x3F)))
    end;
    Ok ()
  end

let decode_hex hex =
  let len = String.length hex in
  if len mod 2 <> 0 then
    error "malformed encoded string literal: odd number of hex digits"
  else
    let buf = Buffer.create (len / 2) in
    let rec loop i =
      if i >= len then Ok (Buffer.contents buf)
      else
        match hex_value hex.[i], hex_value hex.[i + 1] with
        | Some hi, Some lo ->
           Buffer.add_char buf (Char.chr ((hi lsl 4) lor lo));
           loop (i + 2)
        | _ ->
           error "malformed encoded string literal at byte %d" i
    in
    loop 0

let decode_literal_content content =
  let len = String.length content in
  let pos = ref 0 in
  let buf = Buffer.create len in
  let parse_braced_unicode start =
    let rec loop scalar digits =
      if !pos >= len then
        error "unterminated Unicode escape at position %d" start
      else
        match content.[!pos] with
        | '}' ->
           incr pos;
           if digits = 0 then
             error "malformed Unicode escape at position %d: empty scalar" start
           else
             Ok scalar
        | c -> (
            incr pos;
            match hex_value c with
            | Some value ->
               let scalar = (scalar lsl 4) lor value in
               if scalar > 0x10FFFF then
                 error
                   "invalid Unicode scalar at position %d: U+%X exceeds 0x10FFFF"
                   start
                   scalar
               else
                 loop scalar (digits + 1)
            | None ->
               error
                 "malformed Unicode escape at position %d: non-hex character %c"
                 (!pos - 1)
                 c)
    in
    loop 0 0
  in
  let parse_fixed_unicode start =
    let scalar = ref 0 in
    let rec loop remaining =
      if remaining = 0 then Ok !scalar
      else if !pos >= len then
        error "unterminated Unicode escape at position %d" start
      else
        let c = content.[!pos] in
        incr pos;
        match hex_value c with
        | Some value ->
           scalar := (!scalar lsl 4) lor value;
           loop (remaining - 1)
        | None ->
           error
             "malformed Unicode escape at position %d: non-hex character %c"
             (!pos - 1)
             c
    in
    loop 4
  in
  let rec loop () =
    if !pos >= len then Ok (Buffer.contents buf)
    else
      match content.[!pos] with
      | '"' when !pos + 1 < len && Char.equal content.[!pos + 1] '"' ->
         pos := !pos + 2;
         Buffer.add_char buf '"';
         loop ()
      | '\\' when !pos + 1 < len && Char.equal content.[!pos + 1] 'u' ->
         let start = !pos in
         pos := !pos + 2;
         let scalar =
           if !pos < len && Char.equal content.[!pos] '{' then begin
             incr pos;
             parse_braced_unicode start
           end else
             parse_fixed_unicode start
         in
         begin match scalar with
         | Error _ as err -> err
         | Ok scalar -> (
            match add_utf8_scalar buf start scalar with
            | Error _ as err -> err
            | Ok () -> loop ())
         end
      | c ->
         incr pos;
         Buffer.add_char buf c;
         loop ()
  in
  loop ()

let decode_hex_literal_content hex =
  match decode_hex hex with
  | Error _ as err -> err
  | Ok content -> decode_literal_content content
