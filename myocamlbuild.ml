# 0 "myocamlbuild.ml.in"

(*
 * This file is a plugin that provides the needed customization of
 * calls to the ocaml compiler needed for components of coccinelle.
 * The classification of particular components is done by tags, which
 * are specified in the _tags file.
 *
 * This file is also a compromise: some aspects of coccocinelle's
 * build process are somehwat complicated due to packaging some
 * bundled software, having no requirement on ocamlfind, etc.
 * We therefore let 'configure' find out the configuration and
 * paths to tools and libraries, and this plugin is transformed
 * by that configuration to customize ocamlbuild accordingly.
 *)

(* Some useful commandline arguments to ocamlbuild are:
 *   -yaccflag -v      verbose ocamlyacc and menhir output
 *   -classic-display  see the individual build steps
 *   -j 0              parallel building
 *   -tag "-custom"    pure bytecode building
 *   -tag "-dtypes"    no type annotation generation
 *)


(* Configuration of this build plugin
 *)

let ocamlc_path     = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamlc.opt"
let ocamlopt_path   = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamlopt.opt"
let ocamldep_path   = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamldep"
let ocamldoc_path   = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamldoc"
let ocamlyacc_path  = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamlyacc"
let ocamllex_path   = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamllex.opt"
let ocamlmklib_path = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamlmklib"
let ocamlmktop_path = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/ocamlmktop"
let camlp4o_path    = "/nix/store/xmz1ppj1vbjdkqvj3pkz80qrb4pdh77z-ocaml-3.12.1/bin/camlp4o"
let menhir_path     = "/nix/store/kzw2w7v8gycv2l103q0kzm2nf9afb2sk-menhir-20120123/bin/menhir"

let pycaml_path     = "/nix/store/3lm00zsnqzmrxv8l24fww9j3adr2n18v-pycaml-0.82-14/lib/ocaml/3.12.1/site-lib/pycaml"
let pcre_path       = "/nix/store/qimwlnrmjdwghwh7b4pybnqr4fzrn77a-ocaml-pcre-6.2.5/lib/ocaml/3.12.1/site-lib/pcre"
let menhirLib_path  = "/nix/store/kzw2w7v8gycv2l103q0kzm2nf9afb2sk-menhir-20120123/lib/ocaml/3.12.1/site-lib/menhirLib"

let pcre_cflags     = "-I/nix/store/1zh3nk0zsz7pxzihqwjd701dm7an2sah-pcre-8.21/include  "
let pcre_ldflags    = "-L/nix/store/1zh3nk0zsz7pxzihqwjd701dm7an2sah-pcre-8.21/lib -lpcre  "
let python_cflags   = "-I/nix/store/5qmz5wc596kl7rywx56phyl2gb0052w7-python-2.7.3/include/python2.7  "
let python_ldflags  = "-L/nix/store/5qmz5wc596kl7rywx56phyl2gb0052w7-python-2.7.3/lib -lpython2.7  "
let python_major_version = "2"


(* The plugin code starts here. *)
open Ocamlbuild_plugin

(* Removes double separators and single dots from
 * a path. It does not resolve symlinks or turn
 * relative paths in absolute paths.
 *)
let rec normalize_path path =
  let parent = Pathname.dirname path in
  if Pathname.equal parent path
  then path
  else let name = Pathname.basename path in
       if Pathname.equal name "."
       then normalize_path parent
       else normalize_path parent / name

(* Makes path relative and implicit, if it is a child of the
 * current directory. Relative paths are a must when dealing
 * with the build directory.
 * Todo: find out if there is a library function for
 * exactly this purpose.
 *)
let relative_path path =
  let current = normalize_path Pathname.pwd in
  let target = normalize_path path in
  if Pathname.is_prefix current target
  then let len_current = String.length current in
       let len_target = String.length target in
       if len_current == len_target
       then "."
       else let len_tail = len_target - len_current - 1 in
            let ind_tail = len_current + 1 in
	    String.sub target ind_tail len_tail
  else target

let add_flags flag_ref flags =
  flag_ref := List.append flags !flag_ref


let mk_use_tag name = "use_" ^ name


(* Sets up a tag for compiling c and library files against
 * an external c library.
 *)
let setup_clib name compile_flags link_flags =
  let tag = mk_use_tag name in
  flag [tag; "c"; "compile"] (S[A "-ccopt"; A compile_flags]);
  flag [tag; "c"; "ocamlmklib"] (S[A "-ldopt"; A link_flags]);
  flag [tag; "ocaml"; "link"] (S[A "-ccopt"; A compile_flags]);
  flag [tag; "ocaml"; "link"] (S[A "-ccopt"; A link_flags])

(* Sets up a tag for declaring a dependency on a stubs library,
 * and linking it in. The dependency includes both a .a archive
 * and a .so dll.
 *)
let setup_stubs name stubs_dir =
  let tag     = mk_use_tag name in
  let path_a  = Printf.sprintf "%s/lib%s_stubs.a" stubs_dir name in
  if not (Pathname.exists path_a) then
    dep [tag; "link"; "ocaml"] [path_a];
  let stubs_arg = Printf.sprintf "-l%s_stubs" name in
  flag [tag; "ocaml"; "link"; "byte"]
    (S[A "-I"; A stubs_dir; A "-dllib"; A stubs_arg; A "-cclib"; A stubs_arg]);
  flag [tag; "ocaml"; "link"; "native"]
    (S[A "-I"; A stubs_dir; A "-cclib"; A stubs_arg])

(* The use of bundled software is simply the
 * inclusion of the appropriate source directory.
 * The build system can find automatically how to
 * deal with the bundled sources.
 *)
let setup_bundle rootdir =
  tag_file rootdir ["include"; "traverse"]

(* Sets up a tag that adds the given module directory and module
 * as additional argument to ocaml when it processes a
 * file with that tag.
 * Todo: it may be beneficial to add a dependency on the target
 * module.
 *)
let setup_module name modname rootdir =
  let tag = mk_use_tag name in
  let link_args isNative = S [A "-I"; P rootdir; A (modname isNative) ] in
  let compile_args = S [A "-I"; P rootdir] in
  flag [tag; "ocaml"; "compile"] compile_args;
  flag [tag; "ocaml"; "byte"; "link"; "program"]   (link_args false);
  flag [tag; "ocaml"; "native"; "link"; "program"] (link_args true)

(* Sets up the use of either a bundled source package or precompiled module. *)
let setup_package name modname rootdir =
  let exists_path isNative = Pathname.exists (rootdir / modname isNative) in
  let is_binary = exists_path false || exists_path true in
  if is_binary
  then setup_module name modname rootdir
  else setup_bundle rootdir


(* The menhir package provides individual object files
 * instead of an archive.
 *)
let setup_menhirLib () =
  let menhirLib_dir = relative_path menhirLib_path in
  let modname isNative = match isNative with
        true  -> "menhirLib.cmx"
      | false -> "menhirLib.cmo" in

  setup_package "menhirLib" modname menhirLib_dir

(* Pycaml is a stubs library with some conditional
 * code that depends on the python version. We
 * additionally introduce a tag pp_pycaml which
 * runs the appropriate preprocessors.
 *)
let setup_pycaml () =
  let pycaml_dir = relative_path pycaml_path in
  let modname isNative = match isNative with
        true  -> "pycaml.cmxa"
      | false -> "pycaml.cma" in

  setup_package "pycaml" modname pycaml_dir;
  setup_stubs "pycaml" pycaml_dir;
  setup_clib "pycaml" python_cflags python_ldflags;

  let macrodef = Printf.sprintf "-D PYMAJOR%s" python_major_version in
  flag ["pp_pycaml"; "c"; "compile"] (S[A "-ccopt"; A macrodef]);
  let camlp4cmd = Printf.sprintf "%s -parser Camlp4MacroParser -D PYMAJOR%s"
    camlp4o_path python_major_version in
  flag ["pp_pycaml"; "ocaml"; "pp"] (Sh camlp4cmd)

(* Pcre is a standard stub library. *)
let setup_pcre () =
  let pcre_dir = relative_path pcre_path in
  let modname isNative = match isNative with
        true  -> "pcre.cmxa"
      | false -> "pcre.cma" in

  setup_package "pcre" modname pcre_dir;
  setup_stubs "pcre" pcre_dir;
  setup_clib "pcre" pcre_cflags pcre_ldflags

let is_path_configured path =
  String.length path > 0 && Pathname.exists path

(* Note: the setup of the modules is done before the hygiene phase
 * in order to benefit from additional "include" tags that may be
 * given to directories.
 *)
let _ = dispatch begin
  function
    | Before_options ->
        Options.hygiene      := true;
        Options.sanitize     := true;
        Options.make_links   := false;
        Options.catch_errors := true;
	Options.use_menhir   := true;

	let menhir_wrapper = Printf.sprintf
          "%s/setup/wrapper-menhir.sh" Pathname.pwd in
        Options.ocamlc       := Sh ocamlc_path;
	Options.ocamlopt     := Sh ocamlopt_path;
	Options.ocamldep     := Sh ocamldep_path;
	Options.ocamldoc     := Sh ocamldoc_path;
	Options.ocamlyacc    := S[P menhir_wrapper; P ocamlyacc_path; P menhir_path];
	Options.ocamllex     := Sh ocamllex_path;
	Options.ocamlmklib   := Sh ocamlmklib_path;
	Options.ocamlmktop   := Sh ocamlmktop_path;
        ()

    | Before_hygiene ->
	if is_path_configured menhirLib_path then
          setup_menhirLib ();

	if is_path_configured pcre_path then
          setup_pcre ();

        if is_path_configured pycaml_path then
          setup_pycaml ();
        ()

    | After_rules ->
	(* produces a slightly faster native version *)
	flag ["ocaml"; "compile"; "native"] (A "-unsafe");

	(* adds debugging info (including exception backtraces) *)
	flag ["ocaml"; "compile"] (A "-g");

	(* flags to parameterize ocamldoc to produce web pages *)
	flag ["gen_html"; "ocaml"; "doc"]
	  (S [A "-colorize-code"; A "-short-functors"; A "-all-params"]);
        flag ["gen_man"; "ocaml"; "doc"]
	  (S [A "-man"; A "-man-mini"]);
        ()

    | _ -> ()
end
