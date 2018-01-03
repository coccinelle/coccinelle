val library_patterns: (int -> int -> string, unit, string) format list

val library_suffix: string

val ensure_executable_suffix: string -> string

val which: string

val fd_of_int: int -> Unix.file_descr

val path_separator: string
