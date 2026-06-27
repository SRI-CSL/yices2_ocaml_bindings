module S = Extensions.Strings
module Y = Yices2.Ext.WithExceptionsErrorHandling

exception String_smt2_error of string

let raise_smt2 fmt =
  Format.ksprintf (fun msg -> raise (String_smt2_error msg)) fmt

type sexp =
  | Atom of string
  | StringLit of string
  | List of sexp list

let pp_status fmt = function
  | `STATUS_SAT -> Format.fprintf fmt "sat"
  | `STATUS_UNSAT -> Format.fprintf fmt "unsat"
  | `STATUS_UNKNOWN -> Format.fprintf fmt "unknown"
  | status -> Yices2.Ext.Types.pp_smt_status fmt status

let load_file filename =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let parse_sexps input =
  let len = String.length input in
  let pos = ref 0 in
  let peek () =
    if !pos < len then Some input.[!pos] else None
  in
  let bump () =
    let c = input.[!pos] in
    incr pos;
    c
  in
  let rec skip () =
    match peek () with
    | Some (' ' | '\n' | '\r' | '\t') ->
        incr pos;
        skip ()
    | Some ';' ->
        while !pos < len && input.[!pos] <> '\n' do
          incr pos
        done;
        skip ()
    | _ -> ()
  in
  let parse_string () =
    let buf = Buffer.create 16 in
    let rec loop () =
      if !pos >= len then raise_smt2 "unterminated string literal";
      match bump () with
      | '"' ->
          begin
            match peek () with
            | Some '"' ->
                incr pos;
                Buffer.add_char buf '"';
                loop ()
            | _ -> Buffer.contents buf
          end
      | c ->
          Buffer.add_char buf c;
          loop ()
    in
    loop ()
  in
  let parse_atom first =
    let buf = Buffer.create 16 in
    Buffer.add_char buf first;
    let rec loop () =
      match peek () with
      | Some (' ' | '\n' | '\r' | '\t' | '(' | ')' | ';') | None ->
          Buffer.contents buf
      | Some c ->
          incr pos;
          Buffer.add_char buf c;
          loop ()
    in
    loop ()
  in
  let rec parse_one () =
    skip ();
    match peek () with
    | None -> raise_smt2 "unexpected end of input"
    | Some '(' ->
        incr pos;
        let rec list acc =
          skip ();
          match peek () with
          | Some ')' ->
              incr pos;
              List (List.rev acc)
          | None -> raise_smt2 "unterminated list"
          | _ -> list (parse_one () :: acc)
        in
        list []
    | Some ')' -> raise_smt2 "unexpected ')'"
    | Some '"' ->
        incr pos;
        StringLit (parse_string ())
    | Some c ->
        incr pos;
        Atom (parse_atom c)
  in
  let rec all acc =
    skip ();
    if !pos >= len then List.rev acc
    else all (parse_one () :: acc)
  in
  all []

type session = {
  vars : (string, S.Term.t) Hashtbl.t;
  types : (string, S.Type.t) Hashtbl.t;
  mutable ctx : S.Context.t option;
}

let create_session () =
  { vars = Hashtbl.create 32; types = Hashtbl.create 8; ctx = None }

let context session =
  match session.ctx with
  | Some ctx -> ctx
  | None ->
      let ctx = S.Context.malloc () in
      session.ctx <- Some ctx;
      ctx

let reset_context session =
  session.ctx <- Some (S.Context.malloc ())

let rec parse_type session = function
  | Atom "String" -> S.Type.string ()
  | Atom "Bool" -> S.Type.bool ()
  | Atom "Int" -> S.Type.int ()
  | Atom "Real" -> S.Type.real ()
  | Atom name when Hashtbl.mem session.types name -> Hashtbl.find session.types name
  | List [Atom "_"; Atom "BitVec"; Atom width] -> S.Type.bv (int_of_string width)
  | sexp -> raise_smt2 "unsupported type %s" (sexp_to_string sexp)

and sexp_to_string = function
  | Atom s -> s
  | StringLit s -> Format.sprintf "%S" s
  | List l -> "(" ^ String.concat " " (List.map sexp_to_string l) ^ ")"

let is_int_atom s =
  match int_of_string_opt s with
  | Some _ -> true
  | None -> false

let parse_int_atom s =
  match int_of_string_opt s with
  | Some n -> S.Term.Arith.int n
  | None -> raise_smt2 "expected integer constant, got %s" s

let parse_nonnegative_int_atom s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> n
  | Some _ | None -> raise_smt2 "expected non-negative integer constant, got %s" s

let chain op = function
  | [] | [_] -> raise_smt2 "chainable operator expects at least two arguments"
  | first :: rest ->
      let rec aux acc last = function
        | [] -> S.Term.andN (List.rev acc)
        | term :: tail -> aux (op last term :: acc) term tail
      in
      aux [] first rest

let rec parse_term session = function
  | StringLit s -> S.Term.str s
  | Atom "true" -> S.Term.true0 ()
  | Atom "false" -> S.Term.false0 ()
  | Atom s when Hashtbl.mem session.vars s -> Hashtbl.find session.vars s
  | Atom s when is_int_atom s -> parse_int_atom s
  | Atom s -> raise_smt2 "unknown symbol %s" s
  | List (Atom head :: args) ->
      begin
        match head, args with
        | "str.++", args ->
            S.Term.concat (List.map (parse_term session) args)
        | "str.len", [arg] ->
            S.Term.len (parse_term session arg)
        | "str.substr", [string; start; length] ->
            S.Term.substr
              (parse_term session string)
              (parse_term session start)
              (parse_term session length)
        | "str.contains", [haystack; needle] ->
            S.Term.contains (parse_term session haystack) (parse_term session needle)
        | "str.indexof", [haystack; needle; start] ->
            S.Term.indexof
              (parse_term session haystack)
              (parse_term session needle)
              (parse_term session start)
        | "str.replace", [haystack; needle; replacement] ->
            S.Term.replace
              (parse_term session haystack)
              (parse_term session needle)
              (parse_term session replacement)
        | "str.prefixof", [prefix; string] ->
            S.Term.prefixof (parse_term session prefix) (parse_term session string)
        | "str.suffixof", [suffix; string] ->
            S.Term.suffixof (parse_term session suffix) (parse_term session string)
        | "str.at", [string; index] ->
            S.Term.at (parse_term session string) (parse_term session index)
        | "str.in_re", [string; regex] ->
            S.Term.in_re (parse_term session string) (parse_regex session regex)
        | "=", args ->
            chain S.Term.eq (List.map (parse_term session) args)
        | "distinct", args ->
            S.Term.distinct (List.map (parse_term session) args)
        | "not", [arg] ->
            S.Term.not1 (parse_term session arg)
        | "and", args ->
            S.Term.andN (List.map (parse_term session) args)
        | "or", args ->
            S.Term.orN (List.map (parse_term session) args)
        | "=>", [lhs; rhs] ->
            S.Term.implies (parse_term session lhs) (parse_term session rhs)
        | "+", args ->
            begin
              match List.map (parse_term session) args with
              | [] -> S.Term.Arith.zero ()
              | first :: rest -> List.fold_left S.Term.Arith.add first rest
            end
        | "-", [arg] ->
            S.Term.Arith.neg (parse_term session arg)
        | "-", first :: rest ->
            List.fold_left
              S.Term.Arith.sub
              (parse_term session first)
              (List.map (parse_term session) rest)
        | "<=", args ->
            chain S.Term.Arith.leq (List.map (parse_term session) args)
        | "<", args ->
            chain S.Term.Arith.lt (List.map (parse_term session) args)
        | ">=", args ->
            chain S.Term.Arith.geq (List.map (parse_term session) args)
        | ">", args ->
            chain S.Term.Arith.gt (List.map (parse_term session) args)
        | _, _ when Hashtbl.mem session.vars head ->
            let f = Hashtbl.find session.vars head in
            S.Term.application f (List.map (parse_term session) args)
        | _ ->
            raise_smt2 "unsupported term %s" (sexp_to_string (List (Atom head :: args)))
      end
  | List [] -> raise_smt2 "empty term"
  | List (head :: args) ->
      let f = parse_term session head in
      S.Term.application f (List.map (parse_term session) args)

and parse_regex session = function
  | Atom "re.none" -> S.Regex.empty
  | Atom "re.all" -> S.Regex.all
  | Atom "re.allchar" -> S.Regex.all_char
  | List [Atom "str.to_re"; StringLit text] -> S.Regex.str text
  | List (Atom "re.++" :: regexes) ->
      S.Regex.concat (List.map (parse_regex session) regexes)
  | List (Atom "re.union" :: regexes) ->
      S.Regex.union (List.map (parse_regex session) regexes)
  | List [Atom "re.*"; regex] ->
      S.Regex.star (parse_regex session regex)
  | List (Atom "re.inter" :: regexes) ->
      S.Regex.inter (List.map (parse_regex session) regexes)
  | List [Atom "re.comp"; regex] ->
      S.Regex.comp (parse_regex session regex)
  | List [Atom "re.+"; regex] ->
      S.Regex.plus (parse_regex session regex)
  | List [Atom "re.opt"; regex] ->
      S.Regex.opt (parse_regex session regex)
  | List [List [Atom "_"; Atom "re.loop"; Atom lo; Atom hi]; regex] ->
      S.Regex.loop
        ~lo:(parse_nonnegative_int_atom lo)
        ~hi:(parse_nonnegative_int_atom hi)
        (parse_regex session regex)
  | List [Atom "re.range"; StringLit lo; StringLit hi] ->
      S.Regex.range lo hi
  | sexp ->
      raise_smt2 "unsupported regex %s" (sexp_to_string sexp)

let print_model session =
  let ctx = context session in
  let support =
    Hashtbl.to_seq_values session.vars
    |> Seq.filter (fun term -> S.Type.is_string (S.Term.type_of_term term))
    |> List.of_seq
  in
  let model = S.Context.get_model ~support ctx in
  let pp_binding fmt (term, text) =
    let name =
      if S.Term.Names.has_name term then S.Term.Names.to_name term
      else Format.asprintf "%a" S.Term.pp term
    in
    Format.fprintf fmt "(define-fun %s () String %S)" name text
  in
  Format.printf "(@[<v>%a@])@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_binding)
    model.S.StringModel.strings

let parse_instruction session = function
  | List (Atom "set-info" :: _) ->
      ()
  | List (Atom "set-option" :: _) ->
      ()
  | List [Atom "set-logic"; Atom _logic] ->
      reset_context session
  | List [Atom "reset"] ->
      Y.Global.reset ();
      session.ctx <- None;
      Hashtbl.clear session.vars;
      Hashtbl.clear session.types
  | List [Atom "push"; Atom n] ->
      for _ = 1 to int_of_string n do
        S.Context.push (context session)
      done
  | List [Atom "pop"; Atom n] ->
      for _ = 1 to int_of_string n do
        S.Context.pop (context session)
      done
  | List [Atom "declare-sort"; Atom name; Atom "0"] ->
      Hashtbl.replace session.types name (S.Type.new_uninterpreted ~name ())
  | List [Atom "declare-const"; Atom name; typ] ->
      let term = S.Term.new_uninterpreted ~name (parse_type session typ) in
      Hashtbl.replace session.vars name term
  | List [Atom "declare-fun"; Atom name; List domain; codom] ->
      let domain = List.map (parse_type session) domain in
      let codom = parse_type session codom in
      let typ = match domain with
        | [] -> codom
        | _ -> S.Type.func domain codom
      in
      let term = S.Term.new_uninterpreted ~name typ in
      Hashtbl.replace session.vars name term
  | List [Atom "assert"; formula] ->
      S.Context.assert_formula (context session) (parse_term session formula)
  | List [Atom "check-sat"] ->
      let status = S.Context.check (context session) in
      Format.printf "%a@." pp_status status
  | List [Atom "get-model"] ->
      print_model session
  | List [Atom "exit"] ->
      ()
  | sexp ->
      raise_smt2 "unsupported command %s" (sexp_to_string sexp)

let process_file filename =
  Printexc.record_backtrace true;
  Y.Global.init ();
  let session = create_session () in
  Fun.protect
    ~finally:(fun () -> Y.Global.exit ())
    (fun () ->
       filename
       |> load_file
       |> parse_sexps
       |> List.iter (parse_instruction session))

let () =
  let args = ref [] in
  let description =
    "Executable for SMT-LIB string benchmarks. One filename as argument."
  in
  Arg.parse [] (fun arg -> args := arg :: !args) description;
  match List.rev !args with
  | [filename] ->
      begin
        try process_file filename with
        | String_smt2_error msg ->
            Format.eprintf "string SMT2 error: %s@." msg;
            exit 1
        | Yices2.High.ExceptionsErrorHandling.YicesException (_, report) as exc ->
            Format.eprintf "%a@." Yices2.Ext.Types.pp_error_report report;
            raise exc
      end
  | [] -> failwith "Too few arguments in the command"
  | _ -> failwith "Too many arguments in the command"
