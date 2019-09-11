let run_interpreter ?(buffer_size = 4096)
    ~command_line ~module_name commander =
  let channels = Unix.open_process_full command_line (Unix.environment ()) in
  Interface_tools.try_close
    ~close:(fun () ->
      assert (channels |> Unix.close_process_full = Unix.WEXITED 0))
    @@ fun () ->
      let in_channel, out_channel, err_channel = channels in
      let buffer = Buffer.create buffer_size in
      let rec wait_for_prompt () =
        let _ : int =
          Interface_tools.Buffer.add_channel_no_wait buffer in_channel
            buffer_size in
        if not (Interface_tools.Buffer.has_suffix buffer "# ") then
          wait_for_prompt () in
      wait_for_prompt ();
      let prolog = Buffer.contents buffer in
      let linefeed_index = String.index prolog '\n' in
      let version_line = String.sub prolog 0 linefeed_index in
      let version = Interface_tools.Version.of_string version_line in
      commander version (fun phrase ->
        let str =
          String.trim (Format.asprintf "%a@." Pprintast.top_phrase phrase) in
        let str =
          if Interface_tools.String.has_prefix ~prefix:";;" str then
            Interface_tools.String.suffix_from str 2
          else
            str in
        Printf.fprintf out_channel "\n%s\n\n" str;
        flush out_channel;
        Buffer.clear buffer;
        Interface_tools.Buffer.add_channel_to_the_end buffer in_channel
          ~continue:(fun () ->
            not (Interface_tools.Buffer.has_suffix buffer "#   "));
        if Buffer.length buffer > 4 then
          Buffer.truncate buffer (Buffer.length buffer - 4);
        Buffer.contents buffer)

let module_type_of_string ~module_name s =
  match
    let lexbuf = s |> Lexing.from_string in
    Interface_tools.Lexing.set_filename lexbuf module_name;
    match Parse.interface lexbuf with
    | [{ psig_desc =
         Psig_module
           { pmd_type = module_type } }] ->
             module_type
    | [{ psig_desc =
         Psig_modtype
           { pmtd_type = Some module_type }}] ->
             module_type
    | _ -> failwith "Unexpected result"
  with
  | exception Syntaxerr.Error(err) ->
      prerr_endline s;
      Syntaxerr.report_error Format.err_formatter err;
      { pmty_desc = Pmty_signature []; pmty_loc = Location.none;
        pmty_attributes = [] }
  | exception e ->
      prerr_endline s;
      prerr_endline (Printexc.to_string e);
      { pmty_desc = Pmty_signature []; pmty_loc = Location.none;
        pmty_attributes = [] }
  | s ->
      s

let refine_signature_item ~module_name
    (interpret : Parsetree.toplevel_phrase -> string)
    (signature_item : Parsetree.signature_item) =
  match signature_item.psig_desc with
  | Psig_value value_description ->
      let pstr_desc : Parsetree.structure_item_desc =
        Pstr_eval ({
          pexp_desc = Pexp_ident {
            txt = Longident.Ldot (
              Lident module_name, value_description.pval_name.txt);
            loc = Location.none };
          pexp_loc = Location.none;
          pexp_attributes = [] }, []) in
      let s = interpret (Ptop_def [{ pstr_desc; pstr_loc = Location.none }]) in
      let lines = String.split_on_char '\n' s in
      let rec chop_warning lines =
        match lines with
        | s :: s' :: _ when
            Interface_tools.String.has_prefix ~prefix:"Warning 3:" s
        || Interface_tools.String.has_prefix ~prefix:"[24mWarning 3:" s ->
            Some s'
        | _ :: tl -> chop_warning tl
        | [] -> None in
      let warning = chop_warning lines in
      let signature_item =
        match warning with
        | None -> signature_item
        | Some warning ->
            let value_description = { value_description with
              pval_attributes =
                ({ txt = "ocaml.deprecated"; loc = Location.none },
                 PStr [{ pstr_loc = Location.none; pstr_desc = Pstr_eval ({
                   pexp_desc = Pexp_constant (Pconst_string (warning, None));
                   pexp_loc = Location.none;
                   pexp_attributes = [];
                 }, [])}])
                :: value_description.pval_attributes } in
            { signature_item with psig_desc = Psig_value value_description } in
      signature_item
  | _ -> signature_item

let refine_module_type ~module_name interpret
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_signature s ->
      let s = List.map (refine_signature_item ~module_name interpret) s in
      { module_type with pmty_desc = Pmty_signature s }
  | _ -> module_type

let self_name ~(module_name : Longident.t) (type_declaration : Parsetree.type_declaration)
    : Longident.t =
  if module_name = Lident "Pervasives" then
    Lident type_declaration.ptype_name.txt
  else
    Ldot (module_name, type_declaration.ptype_name.txt)

let rec remove_self_aliases_of_type_declaration ~module_name
    (type_declaration : Parsetree.type_declaration) =
  match type_declaration.ptype_manifest with
  | Some { ptyp_desc = Ptyp_constr ({ txt = ident }, _args) }
      when ident = self_name ~module_name type_declaration ->
        { type_declaration with ptype_manifest = None }
  | _ -> type_declaration

let rec remove_self_aliases_of_module_type ~module_name
    (module_type : Parsetree.module_type) =
  match module_type.pmty_desc with
  | Pmty_functor (var, arg, body) ->
      let module_name : Longident.t = Lapply (module_name, Lident var.txt) in
      let body = remove_self_aliases_of_module_type ~module_name body in
      { module_type with pmty_desc = Pmty_functor (var, arg, body) }
  | Pmty_signature s ->
      let s =
        s |> List.map @@ remove_self_aliases_of_signature_item ~module_name in
      { module_type with pmty_desc = Pmty_signature s }
  | _ -> module_type

and remove_self_aliases_of_signature_item ~module_name
    (item : Parsetree.signature_item) =
  match item.psig_desc with
  | Psig_type (rec_flag, list) ->
      let list = list |>
        List.map @@ remove_self_aliases_of_type_declaration ~module_name in
      { item with psig_desc = Psig_type (rec_flag, list) }
  | Psig_module module_declaration ->
      let module_name : Longident.t =
        Ldot (module_name, module_declaration.pmd_name.txt) in
      let pmd_type = remove_self_aliases_of_module_type ~module_name
          module_declaration.pmd_type in
      { item with psig_desc = Psig_module { module_declaration with pmd_type }}
  | _ -> item

let module_type_of_name ~command_line ~module_name =
  run_interpreter ~command_line ~module_name (fun version interpret ->
    let pstr_desc : Parsetree.structure_item_desc =
        let stdlib_module_name =
          if Interface_tools.Version.compare version
              { major = 4; minor = 7; patch = 0 } >= 0
              && module_name <> "Pervasives" && module_name <> "Stdlib" then
            Printf.sprintf "Stdlib__%s" (String.uncapitalize_ascii module_name)
          else
            module_name in
      let module_expr : Parsetree.module_expr = {
        pmod_desc = Pmod_ident {
          txt = Lident stdlib_module_name; loc = Location.none };
        pmod_loc = Location.none;
        pmod_attributes = [] } in
      if Interface_tools.Version.compare version
          { major = 4; minor = 2; patch = 0 } >= 0 then
        Pstr_modtype {
          pmtd_name = { txt = module_name; loc = Location.none };
          pmtd_type = Some ({
            pmty_desc = Pmty_typeof module_expr;
            pmty_loc = Location.none;
            pmty_attributes = [] });
          pmtd_attributes = [
            { Location.txt = "ocaml.warning"; loc = Location.none },
            Parsetree.PStr [{Parsetree.pstr_desc = Pstr_eval ({ Parsetree.pexp_desc = Pexp_constant (Parsetree.Pconst_string ("-3", None)); pexp_attributes = []; pexp_loc = Location.none }, []); pstr_loc = Location.none }]
      ];
          pmtd_loc = Location.none; }
      else
        Pstr_module {
          pmb_name = { txt = module_name; loc = Location.none };
          pmb_expr = module_expr;
          pmb_attributes = [];
          pmb_loc = Location.none; } in
    let s = interpret (Ptop_def [{ pstr_desc; pstr_loc = Location.none }]) in
    let module_type = module_type_of_string ~module_name s in
    let module_type =
      if Interface_tools.Version.compare version
          { major = 4; minor = 2; patch = 0 } >= 0 then
        module_type
      else
        remove_self_aliases_of_module_type ~module_name:(Lident module_name)
          module_type in
    let module_type = refine_module_type ~module_name interpret module_type in
    let _ : string = interpret (Ptop_def [{
      pstr_loc = Location.none;
      pstr_desc = Pstr_eval ({
          pexp_desc = Pexp_apply ({
             pexp_desc = Pexp_ident {
               txt = Lident "exit"; loc = Location.none };
             pexp_loc = Location.none;
             pexp_attributes = [] }, [Nolabel, {
               pexp_desc = Pexp_constant (Pconst_integer ("0", None));
               pexp_loc = Location.none;
               pexp_attributes = [] }]);
          pexp_loc = Location.none;
          pexp_attributes = [] }, [])}]) in
    module_type)

let main () =
  let module_name, command_line =
    match Sys.argv with
    | [| _; module_name; command_line |] -> module_name, command_line
    | _ -> failwith "Bad usage" in
  let module_type = module_type_of_name ~command_line ~module_name in
  let signature =
    match module_type.pmty_desc with
    | Pmty_signature signature -> signature
    | _ -> failwith "Unexpected module type" in
  Format.fprintf Format.std_formatter "%a@."
    Pprintast.signature signature

let () =
  if not !Sys.interactive then
    main ()
