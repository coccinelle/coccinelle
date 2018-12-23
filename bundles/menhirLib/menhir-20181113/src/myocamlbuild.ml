(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Ocamlbuild_plugin
open Command

(* ---------------------------------------------------------------------------- *)

(* This compatibility layer allows us to support both OCaml 4.02 and 4.03, with
   deprecation errors activated. We define our own copies of certain 4.03
   functions. *)

module Compatibility = struct

  module Char = struct

    let lowercase_ascii c =
      if (c >= 'A' && c <= 'Z')
      then Char.chr (Char.code c + 32)
      else c

    let uppercase_ascii c =
      if (c >= 'a' && c <= 'z')
      then Char.chr (Char.code c - 32)
      else c

  end

  module Bytes = struct

    include Bytes

    let apply1 f s =
      if Bytes.length s = 0 then s else begin
        let r = Bytes.copy s in
        Bytes.unsafe_set r 0 (f (Bytes.unsafe_get s 0));
        r
      end

    let capitalize_ascii s =
      apply1 Char.uppercase_ascii s

    let uncapitalize_ascii s =
      apply1 Char.lowercase_ascii s

  end

  module String = struct

    let capitalize_ascii s =
      Bytes.unsafe_to_string (Bytes.capitalize_ascii (Bytes.unsafe_of_string s))

    let uncapitalize_ascii s =
      Bytes.unsafe_to_string (Bytes.uncapitalize_ascii (Bytes.unsafe_of_string s))

  end

end

(* ---------------------------------------------------------------------------- *)
(* The following rules can be copied into other projects. *)
(* ---------------------------------------------------------------------------- *)

(* The auxiliary function [lines] reads a file, line by line. *)

let lines filename : string list =
  let c = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := input_line c :: !lines
    done;
    assert false
  with End_of_file ->
    close_in c;
    List.rev !lines

(* The auxiliary function [noncomment] recognizes a non-blank non-comment line. *)

let rec noncomment s i n =
  i < n && match s.[i] with
  | ' ' | '\t' | '\r' | '\n' ->
      noncomment s (i + 1) n
  | '#' ->
      false
  | _ ->
      true

let noncomment s =
  noncomment s 0 (String.length s)

(* ---------------------------------------------------------------------------- *)

(* If [m] is the name of a module, [cmx m] are the possible names of its
   [.cmx] file. There are two candidate names, because of OCaml's convention
   where the first letter of the file name is capitalized to obtain the module
   name. We do *not* decide between them by accessing the file system, because
   we do not understand or control when ocamlbuild copies files to the build
   directory. *)

let cmx (m : string) : string list =
  let candidate = m ^ ".cmx" in
  [ candidate; Compatibility.String.uncapitalize_ascii candidate ]

(* ---------------------------------------------------------------------------- *)

(* If there is a file [foo.mlpack], then the modules that are listed in this
   file are meant to be part of the library [Foo], and should receive the tag
   [for-pack(Foo)]. ocamlbuild doesn't do this automatically, so we program
   it. *)

(* The argument [basename] should be the basename of the [.mlpack] file. *)

let for_pack (basename : string) =
  let filename = basename ^ ".mlpack" in
  let modules = List.filter noncomment (lines filename) in
  let library = Compatibility.String.capitalize_ascii basename in
  let tags = [ Printf.sprintf "for-pack(%s)" library ] in
  List.iter (fun m ->
    List.iter (fun candidate ->
      tag_file candidate tags
    ) (cmx m)
  ) modules

(* ---------------------------------------------------------------------------- *)
(* The following rules can be copied into other projects. *)
(* ---------------------------------------------------------------------------- *)

(* This rule generates an .ml file [target] from an .mly file [grammar] and a
   .messages file [messages]. *)

(* If the name of a witness file is passed, it is made an additional
   dependency. This triggers a separate rule (see below) which performs a
   completeness check, that is, which checks that the .messages file lists
   every possible syntax error. *)

let compile_errors grammar messages (witness : string list) target =
  rule
    "menhir/compile_errors"
    ~prod:target
    ~deps:([ grammar; messages ] @ witness)
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--compile-errors"; P (env messages);
        Sh ">"; Px (env target);
      ]))

(* A generic version of the above rule, with uniform naming. *)

let generic_compile_errors (check_completeness : bool) =
  compile_errors
    (* sources: *)
    "%.mly" "%Messages.messages"
    (* if present, this dependency forces a completeness check: *)
    (if check_completeness then [ "%Messages.witness" ] else [])
    (* target: *)
    "%Messages.ml"

(* ---------------------------------------------------------------------------- *)

(* This rule generates a .messages file [messages] from an .mly file
   [grammar]. *)

let list_errors grammar messages =
  rule
    "produce a list of messages"
    ~prod:messages
    ~dep:grammar
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--list-errors";
        Sh ">"; Px (env messages);
      ]))

(* ---------------------------------------------------------------------------- *)

(* This rule compares the .messages files [messages1] and [messages2]. This is
   used to ensure complete coverage, i.e., check that every possible error is
   covered. The file [witness] is used as a witness that the comparison has
   been carried out. *)

let compare_errors grammar messages1 messages2 witness =
  rule
    "compare two lists of messages"
    ~stamp:witness
    ~deps:[ grammar; messages1; messages2 ]
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--compare-errors"; P (env messages1);
        A "--compare-errors"; P (env messages2);
      ]))

(* ---------------------------------------------------------------------------- *)

(* This rule combines the above two rules and makes sure that the [messages]
   file is complete, i.e., covers all possible errors. This rule creates a
   witness file. *)

let completeness_check grammar messages witness =
  (* We need a name for a temporary [.messages] file, which we produce,
     and which lists all possible errors. *)
  let complete_messages = grammar ^ ".auto.messages" in
  (* Use the above two rules. *)
  list_errors grammar complete_messages;
  compare_errors grammar complete_messages messages witness

(* A generic version of the above rule, with uniform naming. *)

let generic_completeness_check () =
  completeness_check
    (* sources: *)
    "%.mly" "%Messages.messages"
    (* target: *)
    "%Messages.witness"

(* ---------------------------------------------------------------------------- *)
(* The following rules and settings are specific to the compilation of Menhir.  *)
(* ---------------------------------------------------------------------------- *)

(* Dealing with the two parsers. *)

(* Just for fun, Menhir comes with two parsers for its own input files. One is
   called [yacc-parser.mly] and is built using [ocamlyacc]. The other is called
   [fancy-parser.mly] and is built using Menhir. It depends on [standard.mly].
   The choice between the two parsers is determined by the presence of the tag
   [fancy_parser]. *)

let fancy () : bool =
  mark_tag_used "fancy_parser";
  Tags.mem "fancy_parser" (tags_of_pathname "")

let parser_configuration () =
  (* Create [parser.mly] by copying the appropriate source file. *)
  copy_rule "create parser.mly"
    (* source: *)
    (if fancy() then "fancy-parser.mly" else "yacc-parser.mly")
    (* target: *)
    "parser.mly"
  ;
  (* Create [Driver.ml] by copying the appropriate source file. *)
  copy_rule "create Driver.ml"
    (* source: *)
    (if fancy() then "fancyDriver.ml" else "yaccDriver.ml")
    (* target: *)
    "Driver.ml"
  ;
  (* In the fancy case, use Menhir to generate [parserMessages.ml] based
     on [parserMessages.messages], which is maintained by hand. Also, check
     that [parserMessages.messages] covers all possible syntax errors. *)
  if fancy() then begin
    generic_compile_errors true;
    (* We might wish to perform the completeness check only if [Sys.word_size]
       is at least 64. Indeed, on a 32-bit machine, [menhir --list-errors] is
       restricted to small grammars. For the moment, this works, because our
       grammar is small enough. *)
    generic_completeness_check()
  end

(* ---------------------------------------------------------------------------- *)

(* If the tag [sdk] is provided, then the modules listed in [menhirSdk.mlpack]
   must be built using [for-pack(MenhirSdk)]. Otherwise, we are building Menhir
   and menhirLib, so the modules listed in [menhirLib.mlpack] must be built using
   [for-pack(MenhirLib)]. There could be a nonempty intersection between the two,
   which is why we do not supply both sets of flags at once. *)

let sdk () : bool =
  mark_tag_used "sdk";
  Tags.mem "sdk" (tags_of_pathname "")

(* ---------------------------------------------------------------------------- *)

(* Compilation flags for Menhir. *)

let flags () =
  (* -noassert (if enabled by tag) *)
  flag ["ocaml"; "compile"; "noassert"] (S [A "-noassert"]);
  (* nazi warnings *)
  flag ["ocaml"; "compile"; "my_warnings"] (S[A "-w"; A "@1..60-4-9-41-44-60"])

(* ---------------------------------------------------------------------------- *)

(* Define custom compilation rules. *)

let () =
  dispatch (function After_rules ->
    (* Add our rules after the standard ones. *)
    parser_configuration();
    flags();
    if sdk() then for_pack "menhirSdk" else for_pack "menhirLib"
  | _ -> ()
  )
