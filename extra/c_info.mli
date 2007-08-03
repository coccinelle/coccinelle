open Common

type entities = {
  macros : string hashset;
  variables : string hashset;
  static_variables : string hashset;
  functions : string hashset;
  static_functions : string hashset;
  structs : string hashset;
  typedefs : string hashset;
  include_c : filename hashset;
}
val print_entities : entities -> unit

type idx_entities = {
  idx_macros : (string, filename hashset) Hashtbl.t;
  idx_variables : (string, filename hashset) Hashtbl.t;
  idx_functions : (string, filename hashset) Hashtbl.t;
  idx_structs : (string, filename hashset) Hashtbl.t;
  idx_typedefs : (string, filename hashset) Hashtbl.t;
}

type file_info = { 
  used : entities; 
  defined : entities; 
  is_module : bool; 
}

type global_definitions = idx_entities

type dependencies_graph =
    ((filename * file_info) * string, bool) Ograph_extended.ograph_mutable


val defined_stuff : (Ast_c.toplevel * 'a) list -> entities
val used_stuff : (Ast_c.toplevel * 'a) list -> entities
(* is_module *)
val extra_stuff : (Ast_c.toplevel * 'a) list -> bool

val adjust_used_only_external : entities -> entities -> unit

val mk_global_definitions_index :
  (filename * file_info) list -> idx_entities
val check_no_duplicate_global_definitions : 
  idx_entities -> unit


val build_graph :
  (filename * file_info) list -> idx_entities -> filename (*outfile*) ->
  ((filename * file_info) * string, bool) Ograph_extended.ograph_mutable


val generate_makefile : dependencies_graph -> filename -> unit
