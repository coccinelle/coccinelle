open Common

type kbuild_info = directory list
   and directory = Directory of string (*dirname*) * group list
   and group = Group of filename list

val unparse_kbuild_info : kbuild_info -> filename (*outfile*) -> unit
val parse_kbuild_info : filename -> kbuild_info


val generate_naive_kbuild_info : string list -> kbuild_info
val check_up_to_date : kbuild_info -> kbuild_info -> unit

val files_in_dirs : string list -> kbuild_info -> group list
