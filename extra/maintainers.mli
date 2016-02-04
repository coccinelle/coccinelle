open Common

(* used my tools/meta_files and tools/split_patch *)


(* correspond usually to a kernel_dirs.meta *)
type subsystem_info = subsystem list
   and subsystem = Subsystem of (dir * maintainers) *
                                (dir * maintainers) list (* subdirs *)
      and dir = string
      and maintainers = string list

val mk_inverted_index_subsystem : subsystem_info -> (dir,dir) Hashtbl.t
val subsystem_to_hash :
  subsystem_info -> (dir, (maintainers * (dir * maintainers) list)) Hashtbl.t


val unparse_subsystem_info : subsystem_info -> filename (*outfile*) -> unit
val parse_subsystem_info : filename -> subsystem_info


val generate_naive_subsystem_info : string list -> subsystem_info
val check_up_to_date : subsystem_info -> subsystem_info -> unit


