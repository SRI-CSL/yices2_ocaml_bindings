open Containers

open Ext_bindings
open Extension_builder

module AddArrays = struct

  type term = Term.t             
  type config = Config.t
  type model  = Model.t

  let config_set = Config.set

  include Trivial

  type array_symb = {
      write : Term.t;
      read  : Term.t;
    }

  (* Maps a function type to its symbols *)
  let table  = Global.hTypes_create 10
  (* Maps a write symbol to its function type *)
  let symbols : ([ `Read | `Write ] * Type.t) HTerms.t = Global.hTerms_create 10

  type write = {
      warray  : Term.t;
      windex  : Term.t list;
      wvalue  : Term.t
    }

  type read = {
      rarray  : Term.t;
      rindex  : Term.t list;
    } [@@deriving ord]

  module ReadSet = Set.Make(struct type t = read [@@deriving ord] end)

  type t = {
      writes : write HTerms.t;
      reads  : ReadSet.t HTypes.t;
    }

  let malloc ?config () =
    config,
    { writes = Global.hTerms_create 100;
      reads  = Global.hTypes_create 100 }

  let free _ = ()

  module ExtraType = struct

    let f typ =
      let typ = Purification.Type.get_body typ in
      match Type.reveal typ with
      | Fun {dom; codom} ->
         let old_typ, _ = Purification.Type.get_var typ in
         let name = Format.sprintf "write_%a" Type.pp typ in
         let write =
           Term.new_uninterpreted ~name (Type.(func (old_typ::codom::dom) old_typ))
         in
         HTerms.add symbols write (`Write, typ);
         let name = Format.sprintf "read_%a" Type.pp typ in
         let read =
           Term.new_uninterpreted ~name (Type.(func (old_typ::dom) codom))
         in
         HTerms.add symbols read (`Read, typ);
         {write; read}
         
      | _ -> High.ExceptionsErrorHandling.raise_bindings_error
               "Trying to get the write symbol of what should be a functional type, not %a" Type.pp typ
    
    (* get the write symbol of type typ *)
    let write typ =
      (* let k, _ = Purification.Type.get_var typ in *)
      let symbs = HTypes.get_or_add table ~f ~k:typ in
      symbs.write

    (* get the read symbol of type typ *)
    let read typ =
      (* let k, _ = Purification.Type.get_var typ in *)
      let symbs = HTypes.get_or_add table ~f ~k:typ in
      symbs.read

  end

  module ExtraTerm = struct

    let write ?typ array index value =
      let typ = match typ with
        | Some typ -> typ
        | None     -> Term.type_of_term array
      in
      let r = Term.application (ExtraType.write typ) (array::value::index) in
      Term.notation r "%a[%a <- %a]" Term.pp array (List.pp Term.pp) index Term.pp value;
      r

    let read ?typ array index =
      let typ = match typ with
        | Some typ -> typ
        | None     -> Term.type_of_term array
      in
      let r = Term.application (ExtraType.read typ) (array::index) in
      Term.notation r "%a[%a]" Term.pp array (List.pp Term.pp) index;
      r
    
    let update state array ~value ~index =
      let result = write array index value in
      HTerms.replace state result {warray=array; windex=index; wvalue=value};
      result

    let application state array ~index =
      let typ = array |> Term.type_of_term in
      let l = HTypes.get_or state typ ~default:ReadSet.empty in
      HTypes.replace state typ (ReadSet.add {rarray=array; rindex=index} l);
      read ~typ array index

    let reveal : 'a. 'a Types.termstruct ->
                 [ `Write of Term.t * Term.t list * Term.t
                 | `Read of Term.t * Term.t list
                 | `Var of Term.t
                 | `Other ] =
      fun (type a) (tstruct : a Types.termstruct) ->
      let open Types in
      match tstruct with
      | A0(`YICES_UNINTERPRETED_TERM, t) -> `Var t
      | App(f, l) when HTerms.mem symbols f ->
         begin
           match HTerms.find symbols f, l with
           | (`Write, _), array::value::index -> `Write (array, index, value)
           | (`Read, _),  array::index        -> `Read  (array, index)
           | _ -> failwith "not typable"
         end
      | _ -> `Other

    let rec old2new t =
      let Types.Term ts = Term.reveal t in
      match reveal ts with
      | `Write(array, index, value) ->
         let array = old2new array in
         let value = old2new value in
         let index = List.map old2new index in
         Term.update array index value
      | `Read(array, index) ->
         let array = old2new array in
         let index = List.map old2new index in
         Term.application array index
      | `Var t -> Purification.Term.get_body t
      | `Other -> ts |> Term.map old2new |> Term.build

    let rec new2old state t =
      (* print_endline(Format.sprintf "new2old %a" Term.pp t); *)
      let Types.Term ts = Term.reveal t in
      match ts with
      | A0(`YICES_UNINTERPRETED_TERM, _) when Type.is_function(Term.type_of_term t) ->
         let get_typ typ = Purification.Type.get_var typ |> fst in
         let r = Purification.Term.get_var ~get_typ t |> fst in
         (* print_endline(Format.sprintf "purify %a into %a" Term.pp t Term.pp r); *)
         r
      | Update { array; index; value } ->  
         let array = new2old state array in
         let value = new2old state value in
         let index = List.map (new2old state) index in
         update state.writes array ~value ~index
      | App(array, index) ->  
         let array = new2old state array in
         let index = List.map (new2old state) index in
         application state.reads array ~index
      | _ -> ts |> Term.map (new2old state) |> Term.build

  end


  let assert_formula old_assert state f =
    old_assert (ExtraTerm.new2old state f)

  let check old_model updated warray windex read l =
    let form =
      Term.(
        orN(List.map2 (fun v1 v2 -> v1 =/= v2) windex read.rindex)
        ==> (ExtraTerm.read updated read.rindex === ExtraTerm.read warray read.rindex))
    in
    if Model.formula_true_in_model old_model form
    then l
    else TermSet.add form l

  let check old_model all_reads updated write l =
    let typ   = Term.type_of_term updated in
    let reads = HTypes.get_or all_reads typ ~default:ReadSet.empty in
    let l    = ReadSet.fold (check old_model updated write.warray write.windex) reads l in
    let form = Term.(ExtraTerm.read updated write.windex === write.wvalue) in
    if Model.formula_true_in_model old_model form
    then l
    else TermSet.add form l

  let check t old_model =
    print_endline (Format.sprintf "@[<v>Model is@, @[<v>%a@]@]" (SModel.pp()) (SModel.make old_model));
    let violated = HTerms.fold (check old_model t.reads) t.writes TermSet.empty
                   |> TermSet.to_list
    in
    print_endline(Format.sprintf "@[<v>Violated:@,%a@]" (List.pp Term.pp) violated);
    match violated with
    | [] -> Sat old_model
    | _ -> Unsat(Term.andN violated)

  let interpolant _t old_interpolant = ExtraTerm.old2new old_interpolant

end

module Arrays = Make(Context)(AddArrays)
