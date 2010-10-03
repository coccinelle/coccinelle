open Common

(* used my tools/meta_files *)


(* correspond usually to a kernel_files.meta *)
type kbuild_info = directory list
   and directory = Directory of string (*dirname*) * group list
   and group = Group of filename list

val unparse_kbuild_info : kbuild_info -> filename (*outfile*) -> unit
val parse_kbuild_info : filename -> kbuild_info


val generate_naive_kbuild_info : string list -> kbuild_info
val generate_less_naive_kbuild_info : string list -> kbuild_info
val generate_kbuild_info_from_depcocci :
  string list -> filename (*out*) -> unit
val check_up_to_date : kbuild_info -> kbuild_info -> unit

(* get the relevant groups from dirs given a kbuild_info *)
val files_in_dirs : string list (* dirs *) -> kbuild_info -> group list


(* remove the .git directory, wrong include, from the list of directories
 * passed in parameter
 *)
val adjust_dirs : string list -> string list
