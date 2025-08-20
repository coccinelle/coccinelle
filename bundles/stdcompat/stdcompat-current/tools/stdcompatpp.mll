(* Preprocesses stdcompat modules *)

{

let error message =
  Printf.eprintf "%s\n%!" message;
  exit 2

let unknown what arg = Printf.sprintf "Unknown %s %s" what arg |> error

type source_type = OCaml | C

let source_types = [
  "c", C;
  "ocaml", OCaml;
]

let start_comment language =
  (if language = OCaml then "(*" else "#if 0") |> print_string
  
let end_comment language = 
  (if language = OCaml then "*)" else "#endif") |> print_string

type options = {
  compiler_version : Compiler_version.t;
  source_type : source_type;
  debug : bool;
}

let init_options () = {
  compiler_version = Compiler_version.of_string Sys.ocaml_version;
  source_type = OCaml;
  debug = false;
}

let dprintf options fmt =
  let printer =
    if options.debug then Printf.fprintf else Printf.ifprintf
  in
  printer stderr fmt

let set_compiler_version opts version =
  let compiler_version =
    try Compiler_version.of_string version with
    _ -> unknown "compiler version" version
  in
  opts := { !opts with compiler_version  }

let set_source_type opts source_type =
  let source_type =
    try List.assoc source_type source_types with
    Not_found -> unknown "source type" source_type
  in
  opts := { !opts with source_type }

let set_debug opts =
  opts := { !opts with debug = true }

let parse_commandline options =
  let opts = ref options in
  let args =
  [
    ("--compiler-version", Arg.String (set_compiler_version opts),
      " Set compiler version");
    ("--source-type", Arg.String (set_source_type opts),
      " Set source type (ocaml or c)");
    ("--debug", Arg.Unit (fun () -> set_debug opts),
      " Turn on debugging");
  ] in
  Arg.parse args (unknown "argument") Sys.argv.(0);
  !opts

type cst = Before | From | After

type block_info = {
  start_lineno : int; (* Line number where the current block starts *)
  tag : string; (* tag used to start this block *)
  keep : bool;
}

type lexer_state = {
  lineno : int;
  blocks : block_info list;
}

let next_line state = { state with lineno = state.lineno + 1 }

type lexer =
  options -> lexer_state -> Lexing.lexbuf -> unit

let initial_lexer_state () = {
  lineno = 1;
  blocks = [];
}
  
let cst_of_string = function
  | "BEFORE" -> Before
  | "FROM" -> From
  | "AFTER" -> After
  | c -> error ("Unknown constraint " ^ c)

let tag_mismatch (start_lineno, start_tag) (end_lineno, end_tag) =
  Printf.sprintf
    "Line %d: expected END_%s, got END_%s, block starts at line %d"
    end_lineno start_tag end_tag start_lineno
  |> error

let check_block_end state end_tag = match state.blocks with
  | [] ->
    Printf.sprintf
      "Line %d: the %s block has not been started"
      state.lineno end_tag
    |> error
  | { start_lineno; tag = start_tag; _ } :: _ ->
    if start_tag <> end_tag then
      tag_mismatch (start_lineno, start_tag) (state.lineno, end_tag)

let known_tags compiler_version = [
  "BIG_ENDIAN", string_of_bool Sys.big_endian;
  "OCAML_DEVELOPMENT_VERSION", "false";
  "OCAML_RELEASE_EXTRA",
    Compiler_version.ocaml_of_extra_info_opt
      (Compiler_version.extra_info compiler_version);
  "OCAML_VERSION_MAJOR",
    string_of_int (Compiler_version.major compiler_version);
  "OCAML_VERSION_MINOR",
    string_of_int (Compiler_version.minor compiler_version);
  "OCAML_VERSION_PATCHLEVEL",
    string_of_int (Compiler_version.patch_level compiler_version);
]

let features = [
  "CYGWIN", Sys.os_type = "Cygwin";
  "FLAMBDA2", false;
  "MAGIC", true;
  "RESULT_PKG", true;
  "SEQ_PKG", true;
  "UCHAR_PKG", true;
  "UNIX", Sys.os_type = "Unix";
  "WIN32", Sys.os_type = "Win32";
]

}

let cst = "BEFORE"|"FROM"|"AFTER"

let digit = ['0'-'9']

let number = digit+

let tag_char = ['A'-'Z''a'-'z''0'-'9''_']

let version = (number as major)'_'(number as minor)('_'(number as patch_level))?

let compiler_version_constraint_block = (cst as cst) '_' (version as _version) as _tag

let compiler_version_constraint_block_begin = "@BEGIN_" compiler_version_constraint_block '@'
let compiler_version_constraint_c_block_begin = "@C_BEGIN_" compiler_version_constraint_block '@'

let compiler_version_constraint_block_end = "@END_" compiler_version_constraint_block '@'
let compiler_version_constraint_c_block_end = "@C_END_" compiler_version_constraint_block '@'

let with = ("WITH"|"WITHOUT") as _enabled
let feature = ((tag_char+) as _feature)

let begin_feature_block = "@BEGIN_" ((with '_' feature) as _tag) '@'
let end_feature_block = "@END_" ((with '_' feature) as _tag) '@'

let other_tag = '@' ((tag_char+) as tag) '@'

rule ocaml_lexer options state = parse
  | compiler_version_constraint_block_begin
    {
      dprintf options "block_begin\n%!";
      let patch_level =
        match patch_level with
        | None -> "0"
        | Some pl -> pl
      in
      dprintf options "Line %d: start of block with cst=%s, version=%s major=%s minor=%s patch_level=%s\n%!"
        state.lineno cst _version major minor patch_level;
      let cst = cst_of_string cst in
      let f c = if c = '_' then '.' else c in
      let version = _version |> (String.map f) |> Compiler_version.of_string in
      let cmp = Compiler_version.compare options.compiler_version version in
      let keep =
        match cst with
        | Before -> cmp < 0
        | From -> cmp >= 0
        | After -> cmp > 0
      in
      dprintf options "keep=%b\n%!" keep;
      if not keep then start_comment OCaml;
      let block = { start_lineno = state.lineno; tag = _tag; keep } in
      let state = { state with blocks = block::state.blocks } in
      ocaml_lexer options state lexbuf
    }
  | compiler_version_constraint_block_end
    {
      dprintf options "block_end\n%!";
      let patch_level =
        match patch_level with
        | None -> "0"
        | Some pl -> pl
      in
      dprintf options "Line %d: end of block with cst=%s, version=%s major=%s minor=%s patch_level=%s\n%!"
        state.lineno cst _version major minor patch_level;
      check_block_end state _tag;
      let (keep, state) = match state.blocks with
        | [] -> (true, state)
        | block::blocks -> (block.keep, {state with blocks})
      in
      if not keep then end_comment OCaml;
      ocaml_lexer options state lexbuf
    }
  | begin_feature_block
    {
      dprintf options "begin_feature_block\n%!";
      if not (List.mem_assoc _feature features) then
        Printf.sprintf "Line %d: unknown feature %s" state.lineno _feature |> error;
      let keep = List.assoc _feature features in
      let keep = if _enabled = "WITH" then keep else not keep in
      if not keep then start_comment OCaml;
      let block = { start_lineno = state.lineno; tag = _tag; keep } in
      let state = { state with blocks = block::state.blocks } in
      ocaml_lexer options state lexbuf
    }
  | end_feature_block
    {
      dprintf options "end_feature_block\n%!";
      check_block_end state _tag;
      let (keep, state) = match state.blocks with
        | [] -> (true, state)
        | block::blocks -> (block.keep, {state with blocks})
      in
      if not keep then end_comment OCaml;
      ocaml_lexer options state lexbuf
    }
  | other_tag as quoted_tag
    {
      dprintf options "other_tag: %s\n%!" quoted_tag;
      (try (List.assoc tag (known_tags options.compiler_version) |> print_string)
      with Not_found -> Printf.sprintf "Line %d: unknknown tag %s " state.lineno quoted_tag |> error);
      ocaml_lexer options state lexbuf
    }
  | _ as ch
    {
      dprintf options "character: '%s'\n%!" (Char.escaped ch);
      print_char ch;
      let state = if ch='\n' then next_line state else state in
      ocaml_lexer options state lexbuf
    }
  | eof
    {
      dprintf options "eof\n%!";
      match state.blocks with
      | [] -> ()
      | block::_ ->
        begin
          let message =
            Printf.sprintf "The %s block started at line %d has not been ended"
            block.tag block.start_lineno
          in
          error message
        end
    }
and c_lexer options state = parse
  | compiler_version_constraint_c_block_begin
    {
      dprintf options "c_block_begin\n%!";
      let patch_level =
        match patch_level with
        | None -> "0"
        | Some pl -> pl
      in
      dprintf options "Line %d: start of block with cst=%s, version=%s major=%s minor=%s patch_level=%s\n%!"
        state.lineno cst _version major minor patch_level;
      let cst = cst_of_string cst in
      let f c = if c = '_' then '.' else c in
      let version = _version |> (String.map f) |> Compiler_version.of_string in
      let cmp = Compiler_version.compare options.compiler_version version in
      let keep =
        match cst with
        | Before -> cmp < 0
        | From -> cmp >= 0
        | After -> cmp > 0
      in
      if not keep then start_comment C;
      let block = { start_lineno = state.lineno; tag = _tag; keep } in
      let state = { state with blocks = block::state.blocks } in
      c_lexer options state lexbuf
    }
  | compiler_version_constraint_c_block_end
    {
      dprintf options "c_block_end\n%!";
      let patch_level =
        match patch_level with
        | None -> "0"
        | Some pl -> pl
      in
      dprintf options "Line %d: end of block with cst=%s, version=%s major=%s minor=%s patch_level=%s\n%!"
        state.lineno cst _version major minor patch_level;
      check_block_end state _tag;
      let (keep, state) = match state.blocks with
        | [] -> (true, state)
        | block::blocks -> (block.keep, {state with blocks})
      in
      if not keep then end_comment C;
      c_lexer options state lexbuf
    }
  | other_tag as quoted_tag
    {
      dprintf options "other_tag: %s\n%!" quoted_tag;
      try (List.assoc tag (known_tags options.compiler_version)) |> print_string
      with Not_found -> Printf.sprintf "Line %d: unknknown tag %s " state.lineno quoted_tag |> error
    }
  | _ as ch
    {
      dprintf options "char: '%s'\n%!" (Char.escaped ch);
      print_char ch;
      let state = if ch='\n' then next_line state else state in
      c_lexer options state lexbuf
    }
  | eof
    {
      dprintf options "eof\n%!";
      match state.blocks with
      | [] -> ()
      | block::_ ->
        begin
          let message =
            Printf.sprintf "The %s block started at line %d has not been ended"
            block.tag block.start_lineno
          in
          error message
        end
    }

{

let main () =
  let options = init_options () |> parse_commandline in
  let lexer = match options.source_type with
    | OCaml -> ocaml_lexer
    | C -> c_lexer
  in
  let state = initial_lexer_state () in
  Lexing.from_channel stdin |> lexer options state

let _ = main ()

}
